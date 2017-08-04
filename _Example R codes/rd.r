RMark::cleanup(ask=FALSE)    

input<-list()
input$nprim = 10
input$phi=rep(0.8,input$nprim-1)## SURVIVAL
input$f=rep(0,input$nprim-1)# RECRUITMENT
input$n=500 # initial population size
## RANDOM MOVEMENT PRIMARY
#input$gam_prime=rep(0.3,input$nprim-1) # UNOBSERVABLE[t-1]--> UNOBSERVABLE[t]
input$gam_d_prime=rep(0.3,input$nprim-1) # OBSERVABLE[t-1] --> UNOBSERVABLE[t]; TEMP EMIGRATION
input$nsec=rep(4,input$nprim)## SECONDARY OCCASIONS
input$p=rep(0.3,input$nprim)## CAPTURE PROBABILITY

inputs<-input

    
sim_ch<-function(inputs,...)
    {
    ## FUNCTION TO SIMULATE A POPULATION WITH TEMPORARY EMIGRATION
    library(rowr)# NEED TO HANDLE RECRUITMENT
    
    ## SET UP TIME INTERRVALS
    ## FOR PROGRAM MARK
    ends<-cumsum(inputs$nsec) # last sampling occasion
    occs<- rep(0,sum(inputs$nsec))
    occs[ends]<-1# last occasion in primary
    occs<- occs[-length(occs)]# drop last 1 for processing
    
    ## MAKE THE SUPER POPULATION AT T=1
    Z<- matrix(0,inputs$n,inputs$nprim)
    Z_inn<-Z
    Z_out<-Z
    Z[,1]<-1 # SET ALL AS ALIVE
    # ASSIGN A PROPORTION AS BEING IN BEND
    Z_inn[,1]<-rbinom(nrow(Z),1, 1-inputs$gam_d_prime[1])   
    # THOSE OUTSIDE THE BEND
    Z_out[,1]<-1-Z_inn[,1]  
    
    ## SIMULATE POPULATION DYNAMICS
    for(i in 2:ncol(Z))
        {
        ## SURVIVAL
        Z[,i]<- rbinom(n=nrow(Z),
            size=1,
            prob=input$phi[i-1]*Z[,i-1])
        ## MOVEMENT: RANDOM TEMPORARY EMIGRATION
        Z_inn[,i]<- rbinom(nrow(Z),1,1-inputs$gam_d_prime[i-1])*Z[,i]
        Z_out[,i]<- (1-Z_inn[,i])*Z[,i]      
        
        ## RECRUITMENT
        ## ASSUME RECRUITS SETTLE OUTSIDE OF 
        ## STUDY AREA
        R<-rpois(1,sum(Z[,i])*inputs$f[i-1])
        app<- matrix(0,R,ncol(Z))
        app[,i]<-1
        Z<- rbind(Z,app)
        Z_out<- rbind(Z_out,app)# OUTSIDE STUDY AREA
        app[]<-0
        Z_inn<- rbind(Z_inn,app)# INSIDE STUDY AREA
        }

    ## SIMULATE CAPTURE HISTORIES
    ## CAPTURE IS CONDITIONAL ON BEING IN THE STUDY AREA
    ch<- matrix(0,nrow(Z),sum(inputs$nsec))
    primary_occasion<- rep(1:inputs$nprim,inputs$nsec)
    for(i in 1:sum(inputs$nsec))
        {
        ch[,i]<-rbinom(nrow(Z),1,p[primary_occasion[i]]*Z_inn[,primary_occasion[i]])
        }
       
    ## SUBSET OUT FISH THAT ARE NEVER CAPTURED
    ch<- ch[which(apply(ch,1,sum)!=0),]
    ## NEED THIS FOR RMARK
    # prep data for processing
    ch<- data.frame(ch=apply(ch,1,paste0,collapse=""),
        freq=1,stringsAsFactors=FALSE)
    out<-(list(ch=ch,
        occs=occs,
        nprim=inputs$nprim,
        nsec=inputs$nsec,
        Z=Z,
        trueN=colSums(Z_inn),
        Z_inn=Z_inn,
        Z_out=Z_out))
    return(out)
    } #DONE SIMULATING CAPTURE HISTORIES
    

## ESIMATE ABUNDANCE AND SURVIVAL WITH rrd    
est_rd<-function(inputs,...)
    {
    library(RMark)# NEED TO COMMUNICATION TO PROGRAM MARK
    
    # SET UP DESIGN MATRIX AND CAPTURE HISTORIES FOR PROGRAM MARK
    rd<-process.data(data=inputs$ch, # CAPTURE HISTORIES FOR EACH FISH
        model="Robust", # MODEL TYPE
        time.intervals=inputs$occs) # FUNKY INPUTS TO DEFINE PRIMARY AND SECONDARY OCCASIONS

    # SET UP THE FORMULA AND UNDERLYING DESIGN MATRIX FOR SURIVAL 
    # WE SIMULATED CONSTANT S, THEREFORE INTERCEPT ONLY MODEL
    S=list(formula=~1)# SURVIVAL

    # SET UP THE FORMULA AND UNDERLYING DESIGN MATRIX FOR CAPTURE PROBABILIYT
    # RD CAN DO CAPTURE (C) AND RECAPTURE (P) PROBABILITY BUT 
    # WE SIMULATED CONSTANT P, THEREFORE WE CAN FIX C = P USING 
    # THE SHARE STATEMENT
    # SHARE = TRUE TO SET C = P
    p=list(formula=~1,share=TRUE)# CAPTURE PROBABILITY
    # f0 IS THE THE ESTIMATED NUMBER OF FISH NOT ENCOUNTERED
    # AS A LINEAR COMBINATION, NEED TO ALLOW IT TO VARY 
    # AMONG EACH PRIMARY OCCASION WHICH IS DONE BY SETTING UP 
    # A DESIGN MATRIX THAT WILL ESTIMATE AN EFFECT OF EACH PRIMARY 
    # OCCASION, THEN TOTAL ABUNDANDANCE IS DERIVED AS THE LINEAR COMBO 
    # OF THE INTERCEPT, TIME SPECIFIC EFFECT AND NUMBER OF TAGGED FISH
    # CAPTURED AT THAT TIME
    f0<- list(formula=~time) # NUMBER NOT ENCOUNTERED
    
    # HERE ARE THE MOVEMENT PARAMETERS. BY SETTING GAMMA DBL PRIME
    # EQUAL TO GAMMA PRIME WE GET RANDOM TEMPORARY EMIGRATION
    # USING THE SHARE = TRUE FIXES THEM TO EQUAL
    # SIMULATED CONSTANT MOVEMENT RATE THEREFORE USE AN INTERCEPT
    # ONLY MODEL
    GammaDoublePrime=list(formula=~1,share=TRUE)
    # GammaPrime=list(formula=~1) NOT NEEDED FOR RANDOM TE
    
    # OK FIT THE MODEL ALREADY
    # FITS USING ANOTHER PROGRAM, PROGRAM MARK 
    fit<-mark(data = rd, 
        model = "Robust", 
        time.intervals=time.intervals,
        model.parameters=list(
            S=S,# THESE ARE THE MODELS SET UP ABOVE
            GammaDoublePrime=GammaDoublePrime,
            p=p),
        threads=2,
        brief=TRUE)
    # COBBLE UP THE OUTPUTS FROM THE MODEL FIT AND RETURN    
    outp<-list(
        abundance=data.frame(
            derived=fit$results$derived$`N Population Size`,
            trueN=colSums(inputs$Z_inn)),
        parameters=data.frame(
            parameter=rownames(summary(fit)$beta)[1:3],
            ests=plogis(summary(fit)$beta$estimate[1:3])))
    return(outp)
    fit<-NA; cleanup(ask=FALSE)    
    }
    
    
    
    
# FUNCTION TO SIMULATE KNOW DYNAMICS
ch<-sim_ch(inputs=input)
# FIT THE MODEL TO COMPARE KNOWN TO ESTIMATES
ests<-est_rd(inputs=ch)


# THE GOODIES

## POPULATION DYNAMICS
matplot(ests$abundance[,c(1,5)],type='l',main="Estimated and true dynamics")

## CRUDE LOOK AT PARAMETER ESTIMATES
plot(ests$parameters$ests, c(input$phi[1],input$gam_d_prime[1], input$p[1]));abline(0,1)