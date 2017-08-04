library(R2WinBUGS)	
bugsdirr<- c("C:/Users/mcolvin/Documents/Research/WinBUGS14 - 1",
	"C:/Users/mcolvin/Documents/Research/WinBUGS14 - 2",
	"C:/Users/mcolvin/Documents/Research/WinBUGS14 - 3","C:/Users/mcolvin/Documents/Research/WinBUGS14 - 4")
setwd("C:/Users/mcolvin/Desktop/tmp/fabens")

# FABANS METHOD
# V02 do as lognormal [works]
# V03 do with indvidual linf as normal [works, needs lots of samples]
N<-300
Linf<-rnorm(300,800,20)
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
		Linfi[i]~dnorm(Linf,prec_Linf)
		}
	# HYPER PARAMETER PRIORS
	k~dunif(0.001,0.2)
	Linf~dnorm(900,0.00000000001)
		
	sigma_obs <- 1/sqrt(prec_obs) # cv
	sigma_Linf <- 1/sqrt(prec_Linf) # cv
	prec_obs ~ dgamma(0.001,0.0001)## PRECISION
	prec_Linf ~ dgamma(0.001,0.0001)# PRECISION
	}

		
# BUNDLE UP DATA
fn <- paste(getwd(),"model.bug", sep="/")
write.model(mod, fn)
dat<- list(L1=L1,Y=L2,dY=dY,N=length(dY))
	
inits<- function(t)
	{	
	list(k=0.01,Linf=1700,prec_obs=25,Linfi=L2+110,prec_Linf=0.0025)
	list(k=0.02,Linf=1800,prec_obs=25,Linfi=L2+110,prec_Linf=0.0025)
	list(k=0.03,Linf=1900,prec_obs=25,Linfi=L2+110,prec_Linf=0.0025)
	}
	
params<- c("k","Linf","sigma_obs","sigma_Linf")

out <- bugs(data=dat,
	inits=inits,
	parameters=params,	model = fn,
	n.chains = 3,	n.iter = 15000,	
	n.burnin = 6000, 
	debug=TRUE,	n.thin=1,
	bugs.directory=bugsdirr[3],
	working.directory=getwd(),codaPkg=FALSE)
out$mean


