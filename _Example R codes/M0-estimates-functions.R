

sim_ch<- function(
    nprim = 10,
    phi=NULL,#rep(0.8,nprim),## SURVIVAL
    f=NULL,#rep(0.1,nprim),# FECUNDITY
    n=1000, # initial population size
    n_inn=300, # initial number in bend
    ## RANDOM MOVEMENT PRIMARY
    gam_prime=rep(0.7,nprim), # UNOBSERVABLE[t-1]--> UNOBSERVABLE[t]
    nsec=NULL,#rep(4,nprim),## SECONDARY OCCASIONS
    ## RANDOM MOVEMENT SECONDARY
    gam_d_prime2=NULL,#rep(0.1,nprim), # OBSERVABLE[t-1] --> UNOBSERVABLE[t]
    ## CAPTURE PROBABILITY
    p=NULL,#rep(0.3,nprim)
    bend=1,
    segment=2)
    {   
    library(rowr)
    gam_d_prime=1-gam_prime # OBSERVABLE[t-1] --> UNOBSERVABLE[t]
    gam_prime2<- 1-gam_d_prime2# UNOBSERVABLE[t-1]--> UNOBSERVABLE[t]
    ## SUPER POPULATION
    Z<- matrix(0,n,nprim)
    Z_inn<-Z
    Z_out<-Z

    Z[,1]<-1
    Z_inn[1:n_inn,1]<-1
    Z_out[,1]<- 1*(1-Z_inn[,1])

    # SIMULATE CAPTURE HISTORY
    ## MORE THAN 1 CAPTURE OCCASIONS
    ch<- matrix(0,nrow(Z),sum(nsec))


     
    ## CAPTURE HISTORY FOR T=1    
    ## UPDATE WHO IS IN AND OUT
    ## AFTER DAY TO DAY MOVEMENT
    i<-1       
    ch<-Z_inn2<-Z_out2<- c()
    for(k in 1:nsec[i])
        {
        ch<- cbind(ch,rbinom(nrow(Z_inn),1,p[i]*Z_inn[,i]*Z[,i]))    
        ## MOVEMENT OUT OF STUDY AREA
        innToOut<- rbinom(n=nrow(Z),
            size=1,
            prob=gam_d_prime2[i]*Z[,i]*Z_inn[,i])
        innToInn<- (1-innToOut)*(Z[,i]*Z_inn[,i])
        ## REMAINING OUT OF STUDY AREA
        outToOut<- rbinom(n=nrow(Z),
            size=1,
            prob=gam_prime2[i]*Z[,i]*Z_out[,i])
        outToInn<-  (1-outToOut)*(Z[,i]*Z_out[,i])
        Z_inn[,i]<-0 
        Z_inn[which(innToInn==1 | outToInn==1),i]<- 1
        Z_out[,i]<-0 
        Z_out[which(outToOut==1 | innToOut==1),i]<- 1
        Z_inn2<- cbind(Z_inn2,Z_inn[,i])
        Z_out2<- cbind(Z_out2,Z_out[,i])
        }    

 

    ## SIMULATE POPULATION DYNAMICS
    for(i in 2:nprim)
        {
        ## SURVIVAL
        Z[,i]<- rbinom(n=nrow(Z),
            size=1,
            prob=phi[i-1]*Z[,i-1])
        
        ## MOVEMENT OUT OF STUDY AREA
        innToOut<- rbinom(n=nrow(Z),
            size=1,
            prob=gam_d_prime[i-1]*Z[,i]*Z_inn[,i-1])
        innToInn<- (1-innToOut)*(Z[,i]*Z_inn[,i-1])
        
        ## REMAINING OUT OF STUDY AREA
        outToOut<- rbinom(n=nrow(Z),
            size=1,
            prob=gam_prime[i-1]*Z[,i]*Z_out[,i-1])
        outToInn<-  (1-outToOut)*(Z[,i]*Z_out[,i-1])
        Z_inn[,i]<-0 
        Z_inn[which(innToInn==1 | outToInn==1),i]<- 1
        Z_out[,i]<-0 
        Z_out[which(outToOut==1 | innToOut==1),i]<- 1

       
        ## SIMULATE DAY TO DAY MOVEMENT AND
        ## CAPTURE HISTORY    
        for(k in 1:nsec[i])
            {
            ch<- cbind.fill(ch,rbinom(nrow(Z_inn),1,p[i]*Z_inn[,i]*Z[,i]))        
            ## MOVEMENT OUT OF STUDY AREA
            innToOut<- rbinom(n=nrow(Z),
                size=1,
                prob=gam_d_prime2[i]*Z[,i]*Z_inn[,i])
            innToInn<- (1-innToOut)*(Z[,i]*Z_inn[,i])
            ## REMAINING OUT OF STUDY AREA
            outToOut<- rbinom(n=nrow(Z),
                size=1,
                prob=gam_prime2[i]*Z[,i]*Z_out[,i])
            outToInn<-  (1-outToOut)*(Z[,i]*Z_out[,i])
            Z_inn[,i]<-0 
            Z_inn[which(innToInn==1 | outToInn==1),i]<- 1
            Z_out[,i]<-0 
            Z_out[which(outToOut==1 | innToOut==1),i]<- 1
            Z_inn2<- cbind.fill(Z_inn2,Z_inn[,i])
            Z_out2<- cbind.fill(Z_out2,Z_out[,i])
            }  
        
        ## RECRUITMENT
        ## ASSUME RECRUITS SETTLE OUTSIDE OF 
        ## STUDY AREA
        R<-rpois(1,sum(Z[,i])*f[i-1])
        app<- matrix(0,R,nprim)
        app[,i]<-1
        Z<- rbind(Z,app)
        Z_out<- rbind(Z_out,app)
        app[]<-0
        Z_inn<- rbind(Z_inn,app)
        }


        
    ch$fishId<- c(1:nrow(ch))   

    ## SET UP TIME INTERRVALS
    ## FOR PROGRAM MARK
    ends<-cumsum(nsec) # last sampling occasion
    occs<- rep(0,sum(nsec))
    occs[ends]<-1# last occasion in primary
    occs<- occs[-length(occs)]# drop last 1 for processing 

    ## TO LONG FORMAT FOR OUTPUT
    out<-data.frame()
    for(i in 1:length(ends))
        {
        indx<- (ends[i]-nsec[i]+1):ends[i]
        for(k in 1:length(indx))
            {
            tmp<- data.frame(fishId=ch$fishId,
                ch=ch[,indx[k]],
                occ=k,
                year=i,
                bend=bend,
                segment=segment) 
            out<- rbind(out, tmp)
            }
        }
    out<-subset(out, ch==1)

    return(list(out=out,
        N=data.frame(year=1:nprim,
                bend=bend,
                segment=segment,
                N=c(colSums(Z))),
        nprim=nprim,
        nsec=nsec)
    )}
    
S
    