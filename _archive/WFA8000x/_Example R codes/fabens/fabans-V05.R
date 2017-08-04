library(R2jags)	
setwd("C:/Users/mcolvin/Desktop/tmp/fabens")

# FABANS METHOD
# V02 do as lognormal [works]
# V03 do with indvidual linf as normal [works, needs lots of samples]
# V04 linf as lognormal
# V05 k and linf as normal, varying among individuals
N<-300
Linf<-rlnorm(300,log(800),0.2)
k<- rlnorm(300,log(0.05),0.2)
dY<- runif(N,2,10)# years
L1<- runif(N,0.1,0.87)*Linf
L2<- (Linf-L1)*(1-exp(-k*dY)) + L1
L2<- rlnorm(N,log(L2),0.2)
plot(L1,L2)

mod<- function()
	{
	for(i in 1:N)
		{
		# MODEL		
		L2[i]<- (Linfi[i]-L1[i])*(1-exp(-ki[i]*dY[i]))+L1[i]
		LL2[i]<- log(L2[i])
		# LIKLIHOOD
		Y[i]~dlnorm(LL2[i],prec_obs)
		Linfi[i]~dlnorm(LLinf,prec_Linf)
		ki[i]~dlnorm(Lk,prec_k)
		}
	# HYPER PARAMETER PRIORS
	Lk~dnorm(0.00, 0.000001)
	k<- exp(Lk)
	sigma_k ~ dunif (0, 10)
	prec_k <-  pow(sigma_k,-2)
	
	LLinf~dnorm(0,0.000001)# log scale
	Linf<- exp(LLinf)
	sigma_Linf ~ dunif (0, 10)
	prec_Linf <-  pow(sigma_Linf,-2)
	
	sigma_obs  ~ dunif (0, 10)
	prec_obs <-pow(sigma_obs,-2)
	}

# BUNDLE UP DATA
dat<- list(L1=L1,Y=L2,dY=dY,N=length(dY))
	
inits<- function(t)
	{	
	list(Lk=log(0.03),LLinf=log(800),sigma_obs=.25,Linfi=L2*1.5,sigma_Linf=.25,sigma_k=0.3, ki=rep(0.05,length(L2)))
	list(Lk=log(0.03),LLinf=log(800),sigma_obs=.25,Linfi=L2*1.5,sigma_Linf=.25,sigma_k=0.3, ki=rep(0.05,length(L2)))
	list(Lk=log(0.03),LLinf=log(800),sigma_obs=.25,Linfi=L2*1.5,sigma_Linf=.25,sigma_k=0.3, ki=rep(0.05,length(L2)))
	}
	
params<- c("k","Linf","sigma_obs","sigma_Linf","sigma_k")

out <- jags(data=dat,
	inits=inits,
	parameters=params,	model.file=mod,
	n.chains = 3,	
	n.iter = 1500,	
	n.burnin = 600, 
	n.thin=2,
	working.directory=getwd())
print(out)
traceplot(out)
out