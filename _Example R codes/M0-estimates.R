source("M0-estimates-functions.R")
biggie<- data.frame(
    segments=c(2,3,4,7,8,9,10,13,14),
    bends=c(40,91,24,34,61,81,39,45,56))
nprim=10

biggie<- lapply(1:nrow(biggie),
    function(x) 
    {
    y<-biggie[rep(x,biggie$bends[x]),]
    y$bendId=c(1:biggie$bends[x])
    return(y)})
biggie<-do.call("rbind",biggie)
biggie$rkm<- runif(nrow(biggie),1,20)
dens<- 20
biggie$N0<- rpois(nrow(biggie),biggie$rkm*dens)
saveRDS(biggie,"biggie.RDS")
bendData<- biggie
names(bendData)[1]<-"segment"
saveRDS(bendData[,-c(2,5)],"bends.RDS")
yyy<-nnn<-data.frame()
for(i in 1:nrow(biggie))
    {
    xxx<- sim_ch(
        nprim = nprim,
        phi=rep(0.8,nprim),## SURVIVAL
        f=rep(0,nprim),# FECUNDITY
        n=biggie$N0[i], # initial population size
        n_inn=biggie$N0[i], # initial number in bend
        ## RANDOM MOVEMENT PRIMARY
        gam_prime=rep(1,nprim), # UNOBSERVABLE[t-1]--> UNOBSERVABLE[t]
        nsec=rep(4,nprim),## SECONDARY OCCASIONS
        ## RANDOM MOVEMENT SECONDARY
        gam_d_prime2=rep(0,nprim), # OBSERVABLE[t-1] --> UNOBSERVABLE[t]
        ## CAPTURE PROBABILITY
        p=rep(0.3,nprim),
        bend=biggie$bendId[i],
        segment=biggie$segment[i])
    yyy<- rbind(yyy,xxx$out)
    nnn<-rbind(nnn,xxx$N)
    print(i/nrow(biggie))
    }
    
    
## to here for gen data for trend and bias
    
    
    
    
    
saveRDS(yyy,"yyy.Rds")  
saveRDS(nnn,"nnn.Rds")  
saveRDS(nnn,"true.Rds")  

library(reshape2)
library(Rcapture)

pp<- dcast(yyy, segment+bend+year+fishId~occ,value.var="ch",sum)   

biggie2<- biggie[rep(1:nrow(biggie),nprim),]
biggie2$year<- rep(1:nprim,nrow(biggie))
out<- lapply(1:nrow(biggie2),function(x)
    {
    dat<-subset(pp, segment==biggie2$segment[x] & bend==biggie2$bendId[x] & 
        year==biggie2$year[x])
    if(nrow(dat)>5){
        tmp<- closedp.t(dat[,-c(1:4)])
        tmp<- data.frame(Nhat=tmp$parameters$M0[1],
            p=tmp$parameters$M0[2],
            fit=tmp$results[1,7])}
    if(nrow(dat)<=5){
        tmp<- data.frame(Nhat=-99,
            p=-99,
            fit=-99)}    
    return(tmp)    
    })

xxxx<-do.call("rbind",out)
xxxx<- cbind(biggie2,xxxx)
saveRDS(xxxx,"output.RDS")





xxxx<- readRDS("output.RDS")
yyy<-readRDS("yyy.Rds")  
nnn<-readRDS("nnn.Rds") 
biggie<- data.frame(segments=c(2,3,4,7,8,9,10,13,14),
    bends=c(40,91,24,34,61,81,39,45,56))


    
    
    
    
    
samps<- lapply(1:nrow(biggie),function(x)
    {
    xx<-replicate(10,sample(1:biggie$bends[x],10,
        replace=FALSE))
    xx<- c(xx)
    year<- sort(rep(1:10,10))
    segment<- biggie$segments[x]
    return(data.frame(year=year,
        segments=segment,
        bendId=xx,
        sampled=1))
    })
samps<- do.call("rbind", samps)

xxxx<-merge(xxxx,samps,by=c("year",
    "segments","bendId"),all.x=TRUE)
xxxx[is.na(xxxx)]<- 0
xxxx[xxxx==-99]<-0
xxxx[xxxx$fit>=1,]$Nhat<-0
xxxx$dens<-xxxx$Nhat/xxxx$rkm

## mean density
library(reshape2)
library(plyr)
ests<-ddply(xxxx,.(segments,year,sampled),
    summarize,
    mn=mean(dens))
ests<-subset(ests,sampled==1)
names(ests)[1]<-"segment"

N<- aggregate(N~year+segment,nnn,sum)
true<-ddply(xxxx,.(segments,year),
    summarize,
    rkm=sum(rkm))
names(true)[1]<-"segment"
true<-merge(true,N, by=c("year",
    "segment"))
yy<- merge(true,ests, by=c("year",
    "segment"))   
yy$Nhat<- yy$rkm*yy$mn   

head(yy)    

library(lattice)
yy<- yy[order(yy$segment,yy$year),]
xyplot(Nhat~year,yy,type='l',
    group=segment)

fit<-lm(log(Nhat)~year+as.factor(segment),yy)


## trend
### bias
bias<-exp(coef(fit)[2])-0.8
### precision
cv<-  summary(fit)$coefficients[2,2]/ coef(fit)[2]   

## abundance
mean(yy$Nhat-yy$N)   
    
    