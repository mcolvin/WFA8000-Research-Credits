### prediction uncertainty


a=0.000001
b=3
er=0.2
l<-c(1:1000)
w<- a*l^b


plot(l,w)
 
low<-qlnorm(0.025,log(w),er)
high<-qlnorm(0.975,log(w),er)
points(l,low)
points(l,high)




l<-runif(1000,1,1000)
w_obs<-rlnorm(length(l),log(a*l^b),er)
plot(l,w_obs)
dat<-data.frame(l=l, w_obs=w_obs)
fit<- lm(log(w_obs)~log(l))

exp(predict(fit,data.frame(l=400),interval = "predict"))

qlnorm(0.025,log(a*400^b),er)



