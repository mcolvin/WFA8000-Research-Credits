


p<- 0.4
nn<-1000
Z<- rbinom(nn,1,p)
eff<- runif(nn,1,13)
a<- 2
y<- log(eff) + a
C<- rpois(nn,exp(y)*Z)

hist(C)
mn<- mean(C)
mn*nn
sum(C)

CPUE<- C/eff
hist(CPUE)

H<-mean(eff)*mean(CPUE)*nn
H
sum(C)

x<- rnbinom(1000, mu=0.3, size=1500)
hist(x)
mn<- mean(x)

mn*1000
sum(x)





