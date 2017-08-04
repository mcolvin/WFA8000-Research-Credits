
nsites<- 450

# occupancy
b0<- 0.5
b1<- -0.02
set.seed(1111)
x<- round(runif(nsites, 10,200))# elevation
y<- b0+b1*x
plot(x,plogis(y))
psi<- plogis(y)


# detection
c0<- 0.4
c1=-1.5
xx<- round(runif(nsites, 0,1),2)# depth cv
yy<- c0+c1*xx
plot(xx,plogis(yy))
p<-plogis(yy)


Z<- rbinom(nsites,1,psi)
k<- 3
y<-matrix(0,nrow=nsites,ncol=k)
for(i in 1:k)
    {
    y[,i]<-rbinom(nsites,1,p*Z)
    }
y
mean(Z)
out<- data.frame(y=y,ele=x,dcv=xx)

write.csv(out,"out.csv")

library(unmarked)

