library(R2jags)	

# length-weight model
b_fec=0.0005
a=0.02
n=2000
len=runif(n,50,300)
wgt<- 0.0001*len^3
fec_ln <- a + b_fec*wgt + log(len) + rnorm(n,0,0.3)

fec<-rpois(n,exp(fec_ln))

plot(wgt,fec)
plot(len,fec)
glm(fec~wgt, family="poisson",offset=log(len))


mod<- function()
	{
	# length-weight model
	# weight-fecundity model with offset
	for(i in 1:n)
		{
		log(fec_mu[i])<- a + b_fec*wgh[i] + log(len[i]) + disp[i]
		disp[i]~dnorm(0,prec_sigma)
		fec[i]~dpois(fec_mu[i])	
		}
	# PRIORS
	a~dnorm(0,0.001)
	b_fec~dnorm(0,0.001)
	sigma~dunif(0.00001, 5)
	prec_sigma <-pow(sigma,-2)
	}
	
dat<- list(wgh=wgt, len=len,fec=fec,n=n)

inits<- function(t)
	{	
	list(a=0.1,b_fec=0.0005,sigma=.25)
	list(a=0.0,b_fec=0.0005,sigma=.25)
	list(a=-0.1,b_fec=0.0005,sigma=.25)
	}
params<- c("a","b_fec","sigma")	

out <- jags(data=dat,
	inits=inits,
	parameters=params,	
	model.file=mod,
	n.chains = 3,	
	n.iter = 1500,	
	n.burnin = 600, 
	n.thin=2,
	working.directory=getwd())
print(out)
traceplot(out)
(out$BUGSoutput$mean)	
	
	
	
	
	