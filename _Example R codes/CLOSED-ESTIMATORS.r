library(closedp)
# Closed population estimator

N<- 500 # true abundance
P<- 0.2 # capture probability

## MAKE A MATRIX OF LIVE INVIDIALS AT TIME T
Z<- matrix(1,nrow=N,ncol=1)

## CONSTRUCT CAPTURE HISTORY 
## FISH CAN ONLY BE CAPTURED IF ALIVE
## THERE P IS CONDITIONED BY Z

occasions<- 3 # number of capture occasions

ch<- matrix(0,nrow=N,ncol=occasions)# matrix for capture history of individual fish

for(i in 1:occasions)
    {
    ch[,i]<-rbinom(N,1,P*Z)
    }
## NEED TO GET RID OF FISH THAT WERE NEVER CAPTURED

ch<- ch[which(rowSums(ch)>0),]
nrow(ch)## HOW MANY FISH WERE CAUGHT

 
## FIT CLOSED POPULATION MODELS TO DATA
## TO ESTIMATE ABUDNANCE 

fits<- closedp.t(ch)
fits ## NOT TOO SHABBY, THE TRUE MODEL WAS M0






## TREND
## TO DO TREND WE NEED TO LOOK ADD A SURIVIAL
## LETS LOOK AT 2 MORE YEARS.

phi<- 0.9 ## survival rate

for(i in 1:2)
    {
    Z<- cbind(Z,rbinom(N,1,Z[,i]*phi)) # SURVIVAL CONDITIONAL ON BEING PREVIOUSLY ALIVE
    }
N_true<-colSums(Z) # TRUE ABUNDANCE   

## YEAR 1
ch<- matrix(0,nrow=N,ncol=occasions)# matrix for capture history of individual fish
for(i in 1:occasions)
    {
    ch[,i]<-rbinom(N,1,P*Z[,1])
    }
ch<- ch[which(rowSums(ch)>0),]
nrow(ch)## HOW MANY FISH WERE CAUGHT
fits<- closedp.t(ch)
N1_EST<- as.data.frame(fits$results)$abundance[1]## M0 ESTIMATE

## YEAR 2
ch<- matrix(0,nrow=N,ncol=occasions)# matrix for capture history of individual fish
for(i in 1:occasions)
    {
    ch[,i]<-rbinom(N,1,P*Z[,2])
    }
ch<- ch[which(rowSums(ch)>0),]
nrow(ch)## HOW MANY FISH WERE CAUGHT
fits<- closedp.t(ch)
N2_EST<- as.data.frame(fits$results)$abundance[1]## M0 ESTIMATE

## YEAR 3
ch<- matrix(0,nrow=N,ncol=occasions)# matrix for capture history of individual fish
for(i in 1:occasions)
    {
    ch[,i]<-rbinom(N,1,P*Z[,3])
    }
ch<- ch[which(rowSums(ch)>0),]
nrow(ch)## HOW MANY FISH WERE CAUGHT
fits<- closedp.t(ch)
N3_EST<- as.data.frame(fits$results)$abundance[1]## M0 ESTIMATE

cbind(N_true,c(N1_EST,N2_EST,N3_EST))
