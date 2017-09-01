library(Rcapture)
library(dcast)
nyears<-10
phi<- 0.8
N0<-10000

Z<- matrix(0,N0,nyears)
Z[,1]<-1
B<- sample(c(1:50),N0,replace=TRUE)

for(i in 2:nyears)
    {
    Z[,i]<-rbinom(nrow(Z),1,phi*Z[,i-1])
    }

p<- c(0.1,0.2,0.1,0.3,0.23,
    0.1,0.2,0.1,0.3,0.23)
ch<- data.frame()
for(i in 1:nyears)
    {
    for(j in 1:4)
        {
        app<-data.frame(year=i,occ=j,
            ind=c(1:nrow(Z)),
            catch=rbinom(nrow(Z),1,p[i]*Z[,i]),
            bend=B)
        ch<- rbind(ch,app)
        }
    }

## SAMPLE REACHES
## 10 REACHES
SB<-replicate(nyears,
    sample(1:50,10,replace=FALSE))
res<-data.frame()
for(i in 1:nyears)
    {
    for(j in 1:10)
        {
        dat<-ch[which(ch$year==i & 
            ch$bend == SB[j,i]),]
        dat<- dcast(dat,ind~occ,
            value.var="catch",
            sum)
        dat<-dat[,-1]            
        if(sum(apply(dat,1,max))<5)
            {
            Nhat<- -99
            Nhat_stderr<- -99
            }
        if(sum(apply(dat,1,max))>5)
            {
            dat<- dat[which(rowSums(dat)>0),]
            fits<- closedp.t(dat)
            Nhat<- fits$results[1,1]
            Nhat_stderr<- fits$results[1,2]
            }
        app<- data.frame(
            year=i,
            bend=SB[j,i],
            Nhat=Nhat,
            Nhat_stderr=Nhat_stderr)
        res<- rbind(res,app)
        }
    }
res$Nhat_var<- res$Nhat_stderr^2    
mns<- aggregate(cbind(Nhat,Nhat_var)~year,
    res,mean)
res$tau_hat<- 50*res$mns# estimate of the total population
res$tau<- sum(pop) # total population
res$var_mu_hat 	<- var(x)/n # variance of the estimator
res$var_tau_hat 	<- N^2*y$var_mu_hat # variance of the estimator


    
    
## No weighting


## Weighting