library(R2jags)	
library(MASS)
setwd("C:/Users/mcolvin/Desktop/tmp/fabens")

# FABANS METHOD
# V02 do as lognormal [works]
# V03 do with indvidual linf as normal [works, needs lots of samples]
# V04 linf as lognormal
# V05 k and linf as normal, varying among individuals
# V06:  k and linf as correlated...
N<-1500
mns<- c(log(1000),log(0.05))
var_vals<- c(0.2^2, 0.3^2)# var = exp(cv)^2
Sigma<- matrix(0,2,2)
diag(Sigma)<- var_vals
# assumes a correlation of 0.6
# cor = [cov(a,b)]/[sd(a)*sd(b)
rho<- -0.6
Sigma[cbind(c(1,2),c(2,1))]<- sqrt(var_vals[1])*sqrt(var_vals[2]) * rho
xx<-mvrnorm(N,mu=mns,Sigma=Sigma)
plot(exp(xx[,1]),exp(xx[,2]))
plot(xx[,1],xx[,2])
cor(xx[,1],xx[,2])


Linf<-exp(xx[,1])
k<- exp(xx[,2])

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
		B_hat[i, 2] <- mu_k
		B_hat[i, 1] <- mu_Linf
		B[i,1:2]~dmnorm(B_hat[i, ], Tau_B[,]) # the pairs of correlated random effects
		
		Linf[i]<- exp(B[i,1])
		k[i]<- exp(B[i,2])
		
		L2[i]<- (Linf[i]-L1[i])*(1-exp(-k[i]*dY[i]))+L1[i]
		LL2[i]<- log(max(L2[i],0.000001))
		# LIKLIHOOD
		Y[i]~dlnorm(LL2[i],prec_obs)
		}
	# HYPER PARAMETER PRIORS
	mu_k~dnorm(0, 0.0001) # mean for random intercepts
	mu_Linf~dnorm(0, 0.0001) # mean for random slopes
	sigma_k~dunif(0, 10) # SD of intercepts
	sigma_Linf~dunif(0, 10) # SD of slopes
	rho~dunif(-1, 1) # correlation between intercepts and slopes
	Sigma_B[1, 1] <- pow(sigma_Linf, 2) # We start assembling the var-covar matrix for the random effects
	Sigma_B[2, 2] <- pow(sigma_k, 2)
	Sigma_B[1, 2] <- rho*sigma_k*sigma_Linf
	Sigma_B[2, 1] <- Sigma_B[1, 2]
	covariance <- Sigma_B[1, 2]
	Tau_B[1:2, 1:2] <- inverse(Sigma_B[,])

	sigma_obs ~ dunif (0.000001, 10)
	prec_obs <-pow(sigma_obs,-2)	
		
	# DERIVED PARAMETERS
	k0<- exp(mu_k)
	Linf0<- exp(mu_Linf)
	}

# BUNDLE UP DATA
dat<- list(L1=L1,Y=L2,dY=dY,N=length(dY))
	
inits<- function(t)
	{	
	list(mu_k=log(0.05), mu_Linf=log(1000), sigma_k=0.3, sigma_Linf=0.3, rho=-0.9,sigma_obs=0.3)
	list(mu_k=log(0.05), mu_Linf=log(1000), sigma_k=0.3, sigma_Linf=0.3, rho=-0.9,sigma_obs=0.3)
	list(mu_k=log(0.05), mu_Linf=log(1000), sigma_k=0.3, sigma_Linf=0.3, rho=-0.9,sigma_obs=0.3)
	}
	
params<- c("Linf0","k0","sigma_k","sigma_Linf","rho","sigma_obs")

out <- jags(data=dat,#
	inits=inits,
	parameters=params,	model.file=mod,
	n.chains = 3,	
	n.iter = 3500,	
	n.burnin = 1000, 
	n.thin=2,
	working.directory=getwd())
	
print(out)
traceplot(out)
out

