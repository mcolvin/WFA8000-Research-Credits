library(R2jags)	
library(MASS)
setwd("C:/Users/mcolvin/Desktop/tmp/fabens")

# FABANS METHOD
# V02 do as lognormal [works]
# V03 do with indvidual linf as normal [works, needs lots of samples]
# V04 linf as lognormal
# V05 k and linf as normal, varying among individuals
# V06:  k and linf as correlated...
N<-300





mns<- c(log(0.05),log(800))
var_vals<- c(0.2^2, 0.3^2)# var = exp(cv)^2
Sigma<- matrix(0,2,2)
diag(Sigma)<- var_vals
# assumes a correlation of 0.6
# cor = [cov(a,b)]/[sd(a)*sd(b)
Sigma[cbind(c(1,2),c(2,1))]<- prod(sqrt(var_vals),-0.9)
xx<-mvrnorm(300,mu=mns,Sigma=Sigma)
plot(exp(xx[,1]),exp(xx[,2]))
plot(xx[,1],xx[,2])

Linf<-exp(xx[,2])
k<- exp(xx[,1])

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
		L2[i]<- (exp(lbeta[i,2])-L1[i])*(1-exp(-exp(lbeta[i,1])*dY[i]))+L1[i]
		#LL2[i]<- log(max(L2[i],0.000001))
		# LIKLIHOOD
		Y[i]~dnorm(L2[i],prec_obs)
		lbeta[i,1:2]~dmnorm(mu.beta[], R[ , ])# draw correlated k and linf
		}
	# HYPER PARAMETER PRIORS
	#beta[1]~dnorm(0.00, 0.000001)# k
	#beta[2]~dnorm(0.00, 0.000001)# linft

	mu.beta[1 : 2] ~ dmnorm(mean[], prec[ , ])
	R[1:2 , 1:2] ~ dwish(Omega[ , ], 2)
	
	prec_Sigma[1:2, 1:2] ~ dwish(Omega[,], 2)
	Sigma[1:2,1:2] <- inverse(prec_Sigma[,])
	
	mean[1]<-0
	mean[2]<-0
	Omega[1,1] <- 1
	Omega[2,2] <- 1
	Omega[1,2] <- 0
	Omega[2,1] <- 0 
	
	prec[1,1]<- 1.0E-6
	prec[1,2]<- 0
	prec[2,1]<- 0 
	prec[2,2]<- 1.0E-6
	
	sigma_obs ~ dunif (0.000001, 10)
	prec_obs <-pow(sigma_obs,-2)	
		
	# DERIVED PARAMETERS
	k<- exp(mu.beta[2])
	Linf<- exp(mu.beta[1])
	}

# BUNDLE UP DATA
dat<- list(L1=L1,Y=L2,dY=dY,N=length(dY))
	
inits<- function(t)
	{	
	list(beta=c(0,0),sigma_obs=.25,prec_Sigma=matrix(c(1,0,0,1),nrow=2, byrow=TRUE))
	list(beta=c(0,0),sigma_obs=.25,prec_Sigma=matrix(c(1,0,0,1),nrow=2, byrow=TRUE))
	list(beta=c(0,0),sigma_obs=.25,prec_Sigma=matrix(c(1,0,0,1),nrow=2, byrow=TRUE))
	}
	
params<- c("beta","k","Linf","sigma_obs","prec_Sigma")

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