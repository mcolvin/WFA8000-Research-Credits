



x<- runif(1000)
y<- -2+0.3*x
y_obs<- rlnorm(1000, y,0.02)
plot(x,y_obs)
library(R2WinBUGS)	
bugsdirr<- c("C:/Users/mcolvin/Documents/WinBUGS14 - 1","C:/Users/mcolvin/Documents/WinBUGS14 - 2",
		"C:/Users/mcolvin/Documents/WinBUGS14 - 3","C:/Users/mcolvin/Documents/WinBUGS14 - 4")
setwd("C:/Users/mcolvin/Desktop/tmp")
dat<- list(x=x, y=y_obs)
	
mod<- function()
	{
	for(i in 1:1000)
		{
		y[i]~dlnorm(mu[i],prec)
		mu[i]<- a + b*x[i]
		}
	prec~dgamma(1,1) #   sigma <- 1/sqrt(tau)
	a~ dnorm(0, 0.01)
	b~ dnorm(0, 0.01)
	}
fn <- paste(getwd(),"model.bug", sep="/")
write.model(mod, fn)	
inits<- function(t)
	{	
	list(a=0,b=0,prec=0.02)
	list(a=0,b=0,prec=0.02)
	list(a=0,b=0,prec=0.02)
	}
wd<- getwd()
params<- c("a","b")
out <- bugs(data=dat,
	inits=inits,
	parameters=params,	model = fn,
	n.chains = 3,	n.iter = 7500,	
	n.burnin = 2500, 
	debug=TRUE,	n.thin=1,
	bugs.directory=bugsdirr[1],
	working.directory=wd,codaPkg=FALSE)
out$mean	
	

## does log transforming do the same thing
dat<- list(x=x, y=log(y_obs))  ## note the log y_obs
mod<- function()
	{
	for(i in 1:1000)
		{
		y[i]~dnorm(mu[i],prec)
		mu[i]<- a + b*x[i]
		}
	prec~dgamma(1,1) #   sigma <- 1/sqrt(tau)
	a~ dnorm(0, 0.01)
	b~ dnorm(0, 0.01)
	}
fn <- paste(getwd(),"model.bug", sep="/")
write.model(mod, fn)	
inits<- function(t)
	{	
	list(a=0,b=0,prec=0.02)
	list(a=0,b=0,prec=0.02)
	list(a=0,b=0,prec=0.02)
	}
wd<- getwd()
params<- c("a","b")
out <- bugs(data=dat,
	inits=inits,
	parameters=params,	model = fn,
	n.chains = 3,	n.iter = 7500,	
	n.burnin = 2500, 
	debug=TRUE,	n.thin=1,
	bugs.directory=bugsdirr[1],
	working.directory=wd,codaPkg=FALSE)
out$mean	

