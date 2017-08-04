

library(nlme)

a<-c(0.0001,0.0002,0.0003)
b<-c(3,3.1,3.2)

x<-runif(300,200,800)
y<-rep(0,300)
y[1:100]<- a[1]*x[1:100]^b[1]
y[101:200]<-a[2]*x[101:200]^b[2]
y[201:300]<-a[3]*x[201:300]^b[3]



dat<- data.frame(trt=sort(rep(c(1:3),100)),x=x,y=rnorm(300,y,10000))
dat$trt<- as.factor(dat$trt)
plot(y~x, dat)

fit<- gnls(y~a*x^b, dat, start=list(a=c(0.0001,0.0002,0.0003), b=c(3,3.1,3.2)), 
	params=list(a~trt-1, b~trt-1))

data(Puromycin)

fit<- gnls(rat~Vm*conc/(K+conc), data=Puromycin, start=list(Vm=c(200,200), K=c(0.1, 0.1), para

dat_grp<- groupedData(~trt)
