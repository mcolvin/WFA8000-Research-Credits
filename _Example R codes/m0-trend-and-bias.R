library(reshape2)
library(Rcapture)
library(plyr)
library(lattice)

## TRUE SEGEMENT AND BEND ABUNDANCE
true<- readRDS("true.RDS")
true<- aggregate(N~year+segment,true,sum)

## BEND DATA
bendData<- readRDS("bends.RDS")

## READ IN SOME LONG DATA
## CAPTURES ONLY FOR ALL 
## BENDS WITHIN SEGMENTS
yyy<- readRDS("yyy.Rds") 
names(yyy)[5]<-"bendId"

## MASSAGE DATA INTO SHAPE FOR 
## CAPTURE HISTORIES
pp<- dcast(yyy, 
    segment+bendId+year+fishId~occ,
    value.var="ch",sum)   
## ADD BEND RKM
pp<- merge(pp,bendData, by=c("segment","bendId"))
    
## SUBSET OUT SAMPLED BENDS 
## WITHIN SEGMENTS 
biggie<- data.frame(
    segments=c(2,3,4,7,8,9,10,13,14),
    bends=c(40,91,24,34,61,81,39,45,56))
nBendsToSampled<- 12
nYears<- 10 
samps<- lapply(1:nrow(biggie),function(x)
    {
    xx<-replicate(nYears,sample(1:biggie$bends[x],
        nBendsToSampled,
        replace=FALSE))
    xx<- c(xx)
    year<- sort(rep(1:nYears,nBendsToSampled))
    segment<- biggie$segments[x]
    return(data.frame(year=year,
        segment=segment,
        bendId=xx,
        sampled=1))
    })   
samps<- do.call("rbind", samps)
## ADD BEND RKM
samps<- merge(samps,bendData, by=c("segment","bendId"))
## MERGE DATA WITH WHAT WAS SAMPLED
dat<-merge(pp,samps,by=c("year",
    "segment","bendId"),all.x=TRUE)
## FILL NAs WITH 0 FOR NOT SAMPLED
dat[is.na(dat)]<- 0
dat[dat==-99]<-0
## SUBSET OUT SAMPLED BENDS
dat<-subset(dat,sampled==1)
## MERGE BENDDATA TO CAPTURE DATA
dat<- merge(dat,bendData, by=c("segment","bendId"))





## ESTIMATE ABUNDANCE FOR EACH BEND

## 
out<- lapply(1:nrow(samps),function(x)
    {
    ## SUBSET BEND AND YEAR CAPTURE DATA
    bend_dat<-subset(pp, 
        segment==samps$segment[x] & 
            bendId==samps$bendId[x] & 
        year==samps$year[x])
    ## FIT M0 MODEL TO ESTIMATE ABUNDANCE
    if(nrow(bend_dat)>5){# only estimate of 5 or more fish captured
        tmp<- closedp.t(bend_dat[,-c(1:4,9)])## estimate abundance
        tmp<- data.frame(## collect up relevant bits for M0
            year=samps$year[x],
            segment=samps$segment[x],
            bendId=samps$bendId[x],
            rkm=samps$rkm[x],
            Nhat=tmp$parameters$M0[1],
            p=tmp$parameters$M0[2],
            fit=tmp$results[1,7])}
    ## FILL FOR NOT ENOUGH DATA
    if(nrow(bend_dat)<=5){# not enough data
        tmp<- data.frame(
            year=samps$year[x],
            segment=samps$segment[x],
            bendId=samps$bendId[x],
            rkm=samps$rkm[x],
            Nhat=-99,
            p=-99,
            fit=-99)}    
    return(tmp)    
    })
out<- do.call("rbind", out)
out[out$fit>=1,]$Nhat<-NA ## make non converged models NA
out[out$Nhat==-99,]$Nhat<-NA ## assume not enough fish is NA
## CALCULATE BEND LEVEL DENSITY FROM ESTIMATES
out$dens<-out$Nhat/out$rkm    

## SUMMARIZE ESTIMATES 
ests<-ddply(out,.(segment,year),
    summarize,
    mn=mean(dens,na.rm = TRUE))
## CALCULATE SEGMENT LENGTH
segmentDistance<- aggregate(rkm~segment,bendData,sum)
## MERGE
ests<- merge(ests,segmentDistance, by="segment")
## ESTIMATE SEGMENT LEVEL ABUNDANCE
ests$Nhat<- ests$rkm*ests$mn   
## MERGE ESTIMATES WITH TRUE VALUES
ests<-merge(ests,true,by=c("year","segment"))
## MERGE WITH TRUE VALUES TO CALCULATE BIAS
ests<- ests[order(ests$segment,ests$year),]


## LOOK AT DATA...
xyplot(Nhat~year,ests,type='l',
    group=segment)


## trend
### bias 0.8 was true trend
fit<-lm(log(Nhat)~year+as.factor(segment),ests)
bias<-exp(coef(fit)[2])-0.8
### precision as coefficient of variation around trend
cv<-  summary(fit)$coefficients[2,2]/coef(fit)[2]   

## abundance
### bias
mean(ests$Nhat-ests$N)

### precision [needs work to propagate variance]



