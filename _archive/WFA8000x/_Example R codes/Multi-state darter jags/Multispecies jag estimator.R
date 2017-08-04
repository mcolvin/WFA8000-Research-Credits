

##      >>>>>> NOTE REQUIRES R PACKAGE R2JAGS 
##                  AND 
##      >>>>>> JAGS SOFTWARE INSTALLED (available: http://mcmc-jags.sourceforge.net/)
##

require(R2jags)

### THE JAGS MODEL 
jag.model<- function ()
    {
    ### the site loop begins
    for(ii in 1:nobs) {
      # state 1 only occupied by dominant species (D)
      Psi[ii,1] <- psiD[ii]*(1-psiSD[ii])
      #state 2 only occupied by subordinate species (S) no YOY
      Psi[ii,2] <- (1-psiD[ii])*psiSd[ii]*(1- psiYOY[ii])
      #state 3 only occupied by subordinate species (S) YOY
      Psi[ii,3] <- (1-psiD[ii])*psiSd[ii]*psiYOY[ii]
      #state 4 occupied by both species no YOY
      Psi[ii,4] <- psiD[ii]*psiSD[ii]*(1- psiDYOY[ii])
      #state 5 occupied by both species with YOY
      Psi[ii,5] <- psiD[ii]*psiSD[ii]*psiDYOY[ii]
      #state 6 unoccupied
      Psi[ii,6] <- (1-psiD[ii])*(1-psiSd[ii])
      
      ### Model occupancy as function of covariates cobble 
      logit(psiD.z[ii])<- eta.D[patch.no[ii]]
      psiD[ii]<-max(0.0001,min(0.9999, psiD.z[ii]))
      
      logit(psiSd.z[ii])<- eta.Sd[patch.no[ii]]
      psiSd[ii]<-max(0.0001,min(0.9999, psiSd.z[ii]))
      
      logit(psiSD.z[ii])<- eta.SD[patch.no[ii]]
      psiSD[ii]<-max(0.0001,min(0.9999, psiSD.z[ii]))
      
      logit(psiYOY.z[ii])<- eta.YOY[patch.no[ii]]
      psiYOY[ii]<-max(0.0001,min(0.9999, psiYOY.z[ii]))
      
      logit(psiDYOY.z[ii])<- eta.YOY[patch.no[ii]] + eta.D.YOY.eff
      psiDYOY[ii]<-max(0.0001,min(0.9999, psiDYOY.z[ii]))
      
      #categorical 1 draw from a multinomial
      Occ[ii] ~ dcat(Psi[ii,])
      
      for (jj in 1:k) {		   
        ## detection probabilities indexed by site (ii), visit (jj), true state, observed state
        p[ii,jj,1,1]  <- pD[ii,jj]
        p[ii,jj,1,2] <- 0
        p[ii,jj,1,3] <- 0
        p[ii,jj,1,4] <- 0
        p[ii,jj,1,5] <- 0
        p[ii,jj,1,6] <- 1- pD[ii,jj]
        
        p[ii,jj,2,1] <- 0
        p[ii,jj,2,2] <- pS[ii,jj]
        p[ii,jj,2,3] <- 0
        p[ii,jj,2,4] <- 0
        p[ii,jj,2,5] <- 0
        p[ii,jj,2,6] <- 1 - pS[ii,jj]
        
        p[ii,jj,3,1] <- 0
        p[ii,jj,3,2] <- pS[ii,jj]*(1-pYOY[ii,jj])
        p[ii,jj,3,3] <- pS[ii,jj]*pYOY[ii,jj]
        p[ii,jj,3,4] <- 0
        p[ii,jj,3,5] <- 0
        p[ii,jj,3,6] <- 1-(pS[ii,jj])
        
        p[ii,jj,4,1] <- pD[ii,jj]*(1-pS[ii,jj])
        p[ii,jj,4,2] <- (1-pD[ii,jj])*pS[ii,jj]
        p[ii,jj,4,3] <- 0
        p[ii,jj,4,4] <- pD[ii,jj]*pS[ii,jj]
        p[ii,jj,4,5] <- 0
        p[ii,jj,4,6] <- (1-pD[ii,jj])*(1-pS[ii,jj])
        
        p[ii,jj,5,1] <- pD[ii,jj]*(1-pS[ii,jj])
        p[ii,jj,5,2] <- (1-pD[ii,jj])*pS[ii,jj]
        p[ii,jj,5,3] <- (1-pD[ii,jj])*pS[ii,jj]*pDYOY[ii,jj]
        p[ii,jj,5,4] <- pD[ii,jj]*pS[ii,jj]*(1-pDYOY[ii,jj])
        p[ii,jj,5,5] <-  pD[ii,jj]*pS[ii,jj]*pDYOY[ii,jj]
        p[ii,jj,5,6] <- (1-pD[ii,jj])*(1-pS[ii,jj])
        
        p[ii,jj,6,1] <- 0
        p[ii,jj,6,2] <- 0
        p[ii,jj,6,3] <- 0
        p[ii,jj,6,4] <- 0
        p[ii,jj,6,5] <- 0
        p[ii,jj,6,6] <- 1
        
        # reading in the response variable: 1 for dominant present, 2 for subordinate present 
        #   3 for both present, and 4 for both absent
        #  the first k columns of the dataset are capture histories
        detect[ii,jj] ~ dcat(p[ii,jj,Occ[ii], ])
        
        ### Model detection as function of covariates 
        logit(pD.z[ii,jj]) <- beta.pD
        logit(pS.z[ii,jj]) <- beta.pS
        logit(pYOY.z[ii,jj]) <- beta.pYOY
        logit(pDYOY.z[ii,jj]) <-beta.pYOY 
        
        ### THIS JUST KEEPS THINGS IN BOUNDS IGNORE
        pD[ii,jj] <-max(0.0001,min(0.9999, pD.z[ii,jj]))
        pS[ii,jj] <-max(0.0001,min(0.9999, pS.z[ii,jj]))
        pYOY[ii,jj] <-max(0.0001,min(0.9999, pYOY.z[ii,jj]))
        pDYOY[ii,jj] <-max(0.0001,min(0.9999, pDYOY.z[ii,jj]))
        
      }
    }
    
    ### PRIOR DISTRIBUTION FOR DETECTION PARMS
    ## detection more two parms except for rSD
    beta.pD ~ dnorm(0,0.37)
    beta.pS ~ dnorm(0,0.37)
    beta.pYOY ~ dnorm(0,0.37)
    beta.D.YOY.eff ~ dnorm(0,0.37)
    
    for(xx in 1:no.ptch) {
      eta.D[xx] ~ dnorm(D.bar,D.tau)
      eta.SD[xx] ~ dnorm(SD.bar,SD.tau)
      eta.Sd[xx] ~ dnorm(Sd.bar,Sd.tau)
      eta.YOY[xx] ~ dnorm(YOY.bar,YOY.tau)
    }
    
    
    D.bar ~ dnorm(0,0.37)
    D.tau <- pow(D.ss,-2)
    D.ss ~ dunif(0,6)
    
    SD.bar ~ dnorm(0,0.37)
    SD.tau <- pow(SD.ss,-2)
    SD.ss ~ dunif(0,6)
    
    Sd.bar ~ dnorm(0,0.37)
    Sd.tau <- pow(Sd.ss,-2)
    Sd.ss ~ dunif(0,6)
    
    YOY.bar ~ dnorm(0,0.37)
    YOY.tau <- pow(YOY.ss,-2)
    YOY.ss ~ dunif(0,6)
    
    eta.D.YOY.eff ~ dnorm(0,0.37)
    
    logit(pred.psi.D)<- D.bar
    logit(pred.psi.SD)<- SD.bar
    logit(pred.psi.Sd)<- Sd.bar
    logit(pred.psi.YOYd) <- YOY.bar
    logit(pred.psi.YOYD) <- YOY.bar + eta.D.YOY.eff
    
    ## estimate species interaction factor
    SIF <- pred.psi.D*pred.psi.SD/(pred.psi.D*(pred.psi.D*pred.psi.SD + (1-pred.psi.D)*pred.psi.Sd))
  }
setwd("C:/Users/peterjam/Desktop")
dater<-read.csv("multistate.dater.csv")

dater$drop<- rowSums(is.na(dater)) < 5

dater<-subset(dater,drop == T)

no.ptch<-nrow(as.data.frame(table(dater$patch.no)))

nobs = nrow(dater); k = 5; 

detect<-c(dater[ ,1],dater[ ,2],dater[ ,3],dater[ ,4],dater[ ,5])

detect<-array(detect,dim=c(nobs,k))

patch.no<-dater[,8]

#these are the parameters we want to monitor
  params<- c("pred.psi.D", "pred.psi.SD", "pred.psi.Sd", "pred.psi.YOYd","pred.psi.YOYD","eta.D.YOY.eff","SIF")
## the data for jags
jdata<- list(nobs=nobs, k=k, no.ptch = no.ptch, patch.no =patch.no, detect =detect)

# the initial values
inits<-function(){list(beta.D.YOY.eff = 0, beta.pD = 0, beta.pS = 0, beta.pYOY = 0, 
                         eta.D.YOY.eff = 0, D.bar = 0, D.ss = 1, SD.bar = 0, SD.ss = 1, Sd.bar = 0, Sd.ss = 1, 
                         YOY.bar = 0, YOY.ss = 1, Occ = rep(5,nobs))}

#Invoking jags
ZZ<-jags(data =jdata, inits=inits, parameters.to.save=params, model.file=jag.model,
         n.thin=1, n.chains=2, n.burnin=200, n.iter=10000)


### this provides summary data
ZZ$BUGSoutput$summary

ZZ$BUGSoutput$DIC
ZZ$BUGSoutput$pD

