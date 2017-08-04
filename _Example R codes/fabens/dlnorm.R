library(R2WinBUGS)	
bugsdirr<- c("C:/Users/mcolvin/Documents/Research/WinBUGS14 - 1",
	"C:/Users/mcolvin/Documents/Research/WinBUGS14 - 2",
	"C:/Users/mcolvin/Documents/Research/WinBUGS14 - 3","C:/Users/mcolvin/Documents/Research/WinBUGS14 - 4")
setwd("C:/Users/mcolvin/Desktop/tmp/fabens")

# FABANS METHOD
# V02 do as lognormal

# this works
# feed dlnorm a mean and sigma is a cv
N<-100
mn<- 30
cv<-0.3
dL<- rlnorm(N,log(mn),cv)
dist(dL)
mean(dL)


mod<- function()
	{
	for(i in 1:N)
		{
		dL[i]~dlnorm(lmn,prec)
		}
	# PRIORS
	mn~ dnorm(0,0.1)
	lmn<- log(mn)
	sigma<- 1/sqrt(prec)
	prec ~ dgamma(0.001,0.001)# PRECISION
	}

# BUNDLE UP DATA
fn <- paste(getwd(),"model.bug", sep="/")
write.model(mod, fn)
dat<- list(dL=dL,N=length(dY))
	
inits<- function(t)
	{	
	list(mn=30,prec=0.02)
	list(mn=30,prec=0.02)
	list(mn=30,prec=0.02)	
	}
	
params<- c("mn","sigma","prec")

out <- bugs(data=dat,
	inits=inits,
	parameters=params,	model = fn,
	n.chains = 3,	n.iter = 7500,	
	n.burnin = 2500, 
	debug=TRUE,	n.thin=1,
	bugs.directory=bugsdirr[1],
	working.directory=getwd(),codaPkg=FALSE)
out$mean


