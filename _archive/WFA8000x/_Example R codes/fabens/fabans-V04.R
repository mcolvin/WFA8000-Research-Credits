library(R2jags)	
bugsdirr<- c("C:/Users/mcolvin/Documents/Research/WinBUGS14 - 1",
	"C:/Users/mcolvin/Documents/Research/WinBUGS14 - 2",
	"C:/Users/mcolvin/Documents/Research/WinBUGS14 - 3","C:/Users/mcolvin/Documents/Research/WinBUGS14 - 4")
setwd("C:/Users/mcolvin/Desktop/tmp/fabens")

# FABANS METHOD
# V02 do as lognormal [works]
# V03 do with indvidual linf as normal [works, needs lots of samples]
# V04 linf as lognormal
N<-300
Linf<-rlnorm(300,log(800),0.2)
k<- 0.05 	# rlnorm(300,log(0.03),0.2)
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
		L2[i]<- (Linfi[i]-L1[i])*(1-exp(-k*dY[i]))+L1[i]
		LL2[i]<-log(L2[i])	# 

		# LIKLIHOOD
		Y[i]~dlnorm(LL2[i],prec_obs)
		Linfi[i]~dlnorm(LLinf,prec_Linf)
		}
	# HYPER PARAMETER PRIORS
	k~dunif(0.001,0.2)
	Linf~dunif(500,1500)
	LLinf<- log(Linf)
		
	sigma_obs  ~ dunif (0, 10)
	sigma_Linf ~ dunif (0, 10)
	prec_obs <-pow(sigma_obs,-2)
	prec_Linf <-  pow(sigma_Linf,-2)
	}

# BUNDLE UP DATA
dat<- list(L1=L1,Y=L2,dY=dY,N=length(dY))
	
inits<- function(t)
	{	
	list(k=0.1,Linf=800,sigma_obs=.25,Linfi=L2*1.5,sigma_Linf=.25)
	list(k=0.05,Linf=800,sigma_obs=.25,Linfi=L2*1.5,sigma_Linf=.25)
	list(k=0.01,Linf=800,sigma_obs=.25,Linfi=L2*1.5,sigma_Linf=.25)
	}
	
params<- c("k","Linf","sigma_obs","sigma_Linf")

out <- jags(data=dat,
	inits=inits,
	parameters=params,	model.file=mod,
	n.chains = 3,	
	n.iter = 15000,	
	n.burnin = 6000, 
	n.thin=2,
	working.directory=getwd())
print(out)
traceplot(out)
out