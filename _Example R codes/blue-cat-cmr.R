
N<-100000
p<-500/100000 # CAPTURE PROBABILITY

# MATRIX TO HOLD CAPTURED [1] OR NOT [0]
ch<-matrix(0,nrow=N,ncol=5)

# SIMULATE CAPTURE HISTORY
# ASSUMING CONSTANT CAPTURE PROBABILITY
for(i in 1:ncol(ch))
    {
    ch[,i]<- rbinom(N,1,p)
    }
    
## CAPTURE HISTORY  
## FIGURE OUT FISH WITH 00000 HISTORY, NEVER CAPTURED
fishCaptured<-which(rowSums(ch)>=1)
## DROP FISH THAT WERE NEVER CAPTURED [00000]
ch<-ch[fishCaptured,]

# install.packages("Rcapture")
library(Rcapture)# PACKAGE TO FIT CLOSED CMR MODELS
fits<-closedp.t(ch)## ESTIMATE POPULATION USING CLOSE ESTIMATORS
## THE ONE WITH THE LOWEST AIC IS YOUR HUCKLEBERRY
## THE MODEL USED TO GENERATE THE DATA WAS THE M0 MODEL
fits
closedpCI.t(ch,m=c("M0"))




### SAME AS ABOVE BUT CAPTURE PROBABILITIES VARY BY DAY
N<- 100000
p<-c(0.005,# first day
    0.007, # Jeff kicked ass
    0.002, # Jeff partied to hard last nigh and did bad
    0.005, # Ok back at it
    0.004)
    
# MATRIX TO HOLD CAPTURED [1] OR NOT [0]
ch<-matrix(0,nrow=N,ncol=5)

# SIMULATE CAPTURE HISTORY
# ASSUMING CONSTANT CAPTURE PROBABILITY
for(i in 1:ncol(ch))
    {
    ch[,i]<- rbinom(N,1,p[i])
    }
    
## CAPTURE HISTORY  
## FIGURE OUT FISH WITH 00000 HISTORY, NEVER CAPTURED
fishCaptured<-which(rowSums(ch)>=1)
## DROP FISH THAT WERE NEVER CAPTURED [00000]
ch<-ch[fishCaptured,]

fits<-closedp.t(ch)## ESTIMATE POPULATION USING CLOSE ESTIMATORS
## THE ONE WITH THE LOWEST AIC IS YOUR HUCKLEBERRY
## THE MODEL USED TO GENERATE THE DATA WAS THE Mt MODEL
fits
closedpCI.t(ch,m=c("Mt"))
    
# PARAMETER ESTIMATES
fits$parameters$Mt
