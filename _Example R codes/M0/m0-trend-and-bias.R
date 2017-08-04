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
            SE_Nhat=tmp$results[1,2],
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
            SE_Nhat=-99,
            p=-99,
            fit=-99)}    
    return(tmp)    
    })
out<- do.call("rbind", out)
out[out$fit>=1,]$Nhat<-NA ## make non converged models NA
out[out$fit>=1,]$SE_Nhat<-NA ## make non converged models NA
out[which(out$Nhat==-99),]$Nhat<-NA ## assume not enough fish is NA
out[which(out$SE_Nhat==-99),]$SE_Nhat<-NA ## assume not enough fish is NA
## CALCULATE BEND LEVEL DENSITY FROM ESTIMATES
out$dens<-out$Nhat/out$rkm    

## SUMMARIZE ESTIMATES 
ests<-ddply(out,.(segment,year),
    summarize,
    mn=mean(dens,na.rm = TRUE),
    n_st=length(which(is.na(dens)==FALSE)),
    v_tmp=sum((1/rkm)^2*SE_Nhat^2,na.rm=TRUE),
    N_sst=sum(Nhat,na.rm=TRUE),
    d_ss=sum(rkm),
    v_tmp2=sum(SE_Nhat^2, na.rm=TRUE))
## CALCULATE SEGMENT LENGTH
segmentDistance<- aggregate(rkm~segment,bendData,sum)
## MERGE
ests<- merge(ests,segmentDistance, by="segment")
## CALCULATE SAMPLED SEGMENT DENSITY
ests$dens_sst<-ests$N_sst/ests$d_ss
## ESTIMATE SEGMENT LEVEL ABUNDANCE
ests$Nhat_1<- ests$rkm*ests$mn  
ests$Nhat_2<- ests$rkm*ests$dens_sst
## MERGE ESTIMATES WITH TRUE VALUES
ests<-merge(ests,true,by=c("year","segment"))
ests<- ests[order(ests$segment,ests$year),]
## CALCULATE ABUNDANCE BIAS
ests$abund_bias_1<-ests$Nhat_1-ests$N
ests$abund_bias_2<-ests$Nhat_2-ests$N

plot(Nhat_2~Nhat_1,ests)

## CALCULATE ABUNDANCE PRECISION
ests$abund_var_1<-(ests$rkm/ests$n_st)^2*ests$v_tmp
ests$abund_var_2<-(ests$rkm/ests$d_ss)^2*ests$v_tmp2
ests$abund_cv_1<-sqrt(ests$abund_var_1)/ests$Nhat_1
ests$abund_cv_2<-sqrt(ests$abund_var_2)/ests$Nhat_2

# COMPARE 2 ESTIMATES
## BIAS
mean(ests$abund_bias_1)
  # [1] 3204.129
mean(ests$abund_bias_2)
  # [1] 71.29493
### WHAT PERCENTAGE OF THE SEGMENTS WAS METHOD 2 BIAS SMALLER IN ABS VALUE:
length(which(abs(ests$abund_bias_2)<abs(ests$abund_bias_1)))/nrow(ests)
### LOOK AT DATA NOT SMALLER FOR:
ests[which(abs(ests$abund_bias_1)<abs(ests$abund_bias_2)),]
## PRECISION
mean(ests$abund_cv_1)
  #[1] 0.03997948
mean(ests$abund_cv_2)
  #[1] 0.03997948
### WHAT PERCENTAGE OF THE SEGMENTS WAS METHOD 2 BIAS SMALLER IN ABS VALUE:
length(which(abs(ests$abund_cv_2)<abs(ests$abund_cv_1)))/nrow(ests)
### LOOK AT DATA NOT SMALLER FOR:
ests[which(abs(ests$abund_cv_1)<abs(ests$abund_cv_2)),]


## LOOK AT DATA...
xyplot(Nhat_1~year,ests,type='l',
    group=segment)

xyplot(Nhat_2~year,ests,type='l',
       group=segment)

xyplot(log(Nhat_1)~year,ests,type='l',
       group=segment)

xyplot(log(Nhat_2)~year,ests,type='l',
       group=segment)


## trend
### bias 0.8 was true trend
fit<-lm(log(Nhat_1)~year+as.factor(segment),ests)
bias<-exp(coef(fit)[2])-0.8
### precision as coefficient of variation around trend
cv<-  summary(fit)$coefficients[2,2]/coef(fit)[2]   

fit2<-lm(log(Nhat_2)~year+as.factor(segment),ests)
bias2<-exp(coef(fit2)[2])-0.8
### precision as coefficient of variation around trend
cv2<-  summary(fit2)$coefficients[2,2]/coef(fit2)[2] 

