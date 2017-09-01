# CODE TO SIMULATE CAPTURE
# HISTORIES FOR VIOLATING CLOSURE
# DURING THE SECONDARY PERIOD


library(RMark)
## make a ZZ nrow=inds, ncol=nprim*nsec

robustDesign<-function(
    ## PRIMARY OCCASIONS
    nprim = 10,

    ## SURVIVAL
    phi=0.8,
    # FECUNDITY
    f=0.1,
    n=1000, # initial population size
    n_inn=300, # initial number in study area
    ## RANDOM MOVEMENT
    gam_prime=0.3, # UNOBSERVABLE[t-1]--> UNOBSERVABLE[t]
    gam_d_prime=0.3, # OBSERVABLE[t-1] --> UNOBSERVABLE[t]
    ## SECONDARY OCCASIONS
    nsec=4,
    gam_sec=0,# MOVEMENT AMONG SECONDARY OCCASIONS
    ## CAPTURE PROBABILITY
    p=0.3,...)
    {
    ## SUPER POPULATION
    Z<- matrix(0,n,nprim)
    Z_inn<-Z
    Z_out<-Z

    Z[,1]<-1
    Z_inn[1:n_inn,1]<-1
    Z_out[,1]<- 1*(1-Z_inn[,1])

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
            prob=gam_d_prime*Z[,i]*Z_inn[,i-1])
        innToInn<- (1-innToOut)*(Z[,i]*Z_inn[,i-1])
        
        ## REMAINING OUT OF STUDY AREA
        outToOut<- rbinom(n=nrow(Z),
            size=1,
            prob=gam_prime*Z[,i]*Z_out[,i-1])
        outToInn<-  (1-outToOut)*(Z[,i]*Z_out[,i-1])
        Z_inn[which(innToInn+outToInn>0),i]<- 1
        Z_out[which(outToOut+innToOut>0),i]<- 1

        ## RECRUITMENT
        ## ASSUME RECRUITS SETTLE OUTSIDE OF 
        ## STUDY AREA
        R<-rpois(1,sum(Z[,i])*f)
        app<- matrix(0,R,nprim)
        app[,i]<-1
        Z<- rbind(Z,app)
        Z_out<- rbind(Z_out,app)
        app[]<-0
        Z_inn<- rbind(Z_inn,app)
        }
        
    # SIMULATE CAPTURE HISTORY
    ## MORE THAN 1 CAPTURE OCCASIONS
    ch<- matrix(0,nrow(Z),sum(nsec))

    ## LINK SECONDARY OCCASION TO PRIMARY OCCASION
    primsec<- matrix(cbind(rep(1:nprim,nsec),
        c(1:sum(nsec))), ncol=2)
        
    ## CAPTURE HISTORY CONDITIONAL ON 
    ## BEING PRESENT (I.E., NOT EMIGRATED)
    for(j in 1:nrow(primsec))
        {
        primocc<- primsec[j,1]
        ch[,j]<- rbinom(n=nrow(Z),
            size=1,
            prob=p[j]*Z[,primocc]*Z_inn[,primocc])
        }

    ## SUBSET OUT FISH THAT ARE NEVER CAPTURED
    ch<- ch[which(apply(ch,1,sum)!=0),]
    ## NEED THIS FOR RMARK
    # prep data for processing
    ch<- data.frame(ch=apply(ch,1,paste0,collapse=""),
        freq=1,stringsAsFactors=FALSE)
        
    ## SET UP TIME INTERRVALS
    ends<-cumsum(nsec) # last sampling occasion
    occs<- rep(0,sum(nsec))
    occs[ends]<-1# last occasion in primary
    occs<- occs[-length(occs)]# drop last 1 for processing 
    return(list(ch=ch,occs=occs,Z=Z,Z_inn=Z_inn,
        Z_out=Z_out))}

nprim<-10
nsec<-c(3,4,5,3,4,5,4,5,6,3)
out<-robustDesign(
    nprim = nprim,
    phi=rep(0.8,nprim-1),
    f=0.4,
    n=1000, # initial population size
    n_inn=300, # initial number in study area
    gam_prime=0.3, 
    gam_d_prime=0.3,
    nsec=nsec,
    p=rep(0.3,sum(nsec)))
    
    
    
rd<-process.data(data=out$ch, 
    model="Robust", 
    time.intervals=out$occs)
S=list(formula=~1)# SURVIVAL
# SHARE = TRUE TO SET C = P
p=list(formula=~1,share=TRUE)# CAPTURE PROBABILITY
f0<- list(formula=~time) # NUMBER NOT ENCOUNTERED
GammaDoublePrime=list(formula=~1,share=TRUE)
GammaPrime=list(formula=~1)
fit<-mark(data = rd, 
	model = "Robust", 
    time.intervals=time.intervals,
	model.parameters=list(
		S=S,
		GammaDoublePrime=GammaDoublePrime,
		# GammaPrime=GammaPrime, # not needed when share=TRUE
		p=p),
	threads=2,
	brief=TRUE)
derived<- fit$results$derived$`N Population Size`
matplot(cbind(colSums(out$Z_inn),derived$estimate),type='b')

#RDPdfClosed

plogis(summary(fit)$beta$estimate)[1:3]
#f0 = beta[1] + effect of session then exp
#exp(summary(fit)$beta$estimate)[-c(1:3)]
#unlist(summary(fit)$real$f0)

