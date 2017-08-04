
library(R2WinBUGS)

# ZIP
x<- runif(1000, 0, 30)
yevent<- -3.5 + 0.2*x

pevent<- exp(yevent)/(1+ exp(yevent))

plot(x,pevent)

z<- rbinom(1000,1,pevent)


xx<- runif(1000, 50, 89)

yexp<- exp(-3 + 0.065*xx)
yobs<- rpois(1000, yexp*z)

plot(xx, yobs)

setwd("C:/Users/mcolvin/Documents/Example R codes")
require(R2WinBUGS)

model<- function()
	{
	for(i in 1:1000)
		{
		logit(p[i])<-  a1 + b1*X[i,1]
		p_z[i]<- max(0.001, min(p[i],0.999))
		Z[i] ~ dbern(p_z[i])
		
		log(mu[i])<- a2 + b2*X[i,2]
		mu_x[i]<- mu[i]*Z[i]
		y[i]~dpois(mu_x[i])
		}
		a1 ~ dnorm(0,0.37)
		a2 ~ dnorm(0,0.37)
		b1 ~ dnorm(0,0.37)
		b2 ~ dnorm(0,0.37)
	}
filename <- paste(getwd(),"base.bug", sep="/")
write.model(model, filename)	
	

param <- c('a1','a2','b1','b2')
dat<- list(y=yobs,X= as.matrix(cbind(x,xx)))
inits_base <- function(t) 
	{
	list(a1=rnorm(1) ,a2=rnorm(1), b1=rnorm(1),b2=rnorm(1),Z=ifelse(yobs>=1,1,0))	
	list(a1=rnorm(1) ,a2=rnorm(1), b1=rnorm(1),b2=rnorm(1),Z=ifelse(yobs>=1,1,0))	
	list(a1=rnorm(1) ,a2=rnorm(1), b1=rnorm(1),b2=rnorm(1),Z=ifelse(yobs>=1,1,0))	
	}	
out <- bugs(data=dat,inits=inits_base,parameters=param,model = filename,
	n.chains = 3,n.iter = 6000,n.burnin = 3000,	debug=TRUE,	
	bugs.directory="C:/Users/mcolvin/Documents/WinBUGS14 - 1",n.thin=1)

