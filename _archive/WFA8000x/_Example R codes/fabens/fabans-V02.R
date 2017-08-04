library(R2WinBUGS)	
bugsdirr<- c("C:/Users/mcolvin/Documents/Research/WinBUGS14 - 1",
	"C:/Users/mcolvin/Documents/Research/WinBUGS14 - 2",
	"C:/Users/mcolvin/Documents/Research/WinBUGS14 - 3","C:/Users/mcolvin/Documents/Research/WinBUGS14 - 4")
setwd("C:/Users/mcolvin/Desktop/tmp/fabens")

# FABANS METHOD
# V02 do as lognormal
N<-300
Linf<- 800 	# rlnorm(300,log(800),0.2)
k<- 0.03 	# rlnorm(300,log(0.03),0.2)
dY<- runif(N,2,10)# years
L_t<- runif(N,200,600)
dL<- (Linf-L_t)*(1-exp(-k*dY)) 
dL<- rlnorm(N,log(dL),0.2)
plot(L_t,dL)

mod<- function()
	{
	for(i in 1:N)
		{
		# MODEL		
		dL_hat[i]<- (Linf-L_t[i])*(1-exp(-k*dY[i]))# throws inf?
		dL_hatt[i]<-log(dL_hat[i])	# 

		# LIKLIHOOD
		dL[i]~dlnorm(dL_hatt[i],prec_obs)
		}
	# PRIORS
	k~dunif(0.001,0.2)
	Linf~dunif(700,900)
	sigma_obs <- 1/sqrt(prec_obs)
	prec_obs ~ dgamma(0.001,0.001)# PRECISION
	}


	
	
	

	
	
# BUNDLE UP DATA
fn <- paste(getwd(),"model.bug", sep="/")
write.model(mod, fn)
dat<- list(dL=dL,dY=dY,L_t=L_t,N=length(dY))
	
inits<- function(t)
	{	
	list(k=0.02,Linf=800,prec_obs=0.02)
	list(k=0.02,Linf=800,prec_obs=0.02)
	list(k=0.02,Linf=800,prec_obs=0.02)
	}
	
params<- c("k","Linf","sigma_obs","prec_obs")

out <- bugs(data=dat,
	inits=inits,
	parameters=params,	model = fn,
	n.chains = 3,	n.iter = 7500,	
	n.burnin = 2500, 
	debug=TRUE,	n.thin=1,
	bugs.directory=bugsdirr[1],
	working.directory=getwd(),codaPkg=FALSE)
out$mean


