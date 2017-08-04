
library(mvtnorm)
library(R2jags)	
# k
k<-0.3
# Linft
Linf<-1000

vcov<- matrix(-0.9,ncol=2, nrow=2)
diag(vcov)<- c(20, 0.2)

mn<-c(Linf, log(k))

-0.9/(sqrt(vcov[1,1])*sqrt(vcov[2,2]))




xx<- rmvnorm(200, mn, vcov)


plot(xx[,1],xx[,2])

cor(xx)


out<-data.frame()

for(i in 1:200)
	{
	x<- runif(5,0,10)
	y_true<- xx[i,1]+xx[i,2]*x
	tmp<- data.frame(id=i,x=x,y=y_true)
	out<- rbind(out, tmp)
	}
	

plot(y~x,out)
xyplot(y~x,out, group=id,type='l')

out$y<- rnorm(nrow(out),out$y,2)
plot(y~x,out)

xyplot(y~x,out, group=id,type='l')


mod<- function()
	{
	for (i in 1:1000)
		{
		mu[i] <- v[x[i,1],1] + v[x[i,1],2]*x[i,2]
		x[i,3] ~ dnorm(mu[i], prec.sigma2)
		}
	for (j in 1:200)
		{
		v[j,1:2] ~ dmnorm(beta[], prec.Sigma[,])
		}
	
	beta[1] ~ dnorm(0.0,1.0E-6)
	beta[2] ~ dnorm(0.0,1.0E-6)
	prec.sigma2 ~ dgamma(0.001,0.001)
	prec.Sigma[1:2, 1:2] ~ dwish(Omega[,], 2)
	Sigma[1:2,1:2] <- inverse(prec.Sigma[,])
	sigma2 <- 1/prec.sigma2
	
	Omega[1,1] <- 1
	Omega[2,2] <- 1
	Omega[1,2] <- 0
	Omega[2,1] <- 0
	}

dat<- list(x=out)


inits<- function(t)
	{	
	list(beta=c(1000,-1),prec.sigma2=2)
	}
params<- c("beta","Sigma")	
## LOWER BASIN	
out_lower <- jags(data=dat,
	inits=inits,
	parameters=params,	model.file=mod,
	n.chains = 3,	
	n.iter = 20000,	
	n.burnin = 5000,
	n.thin=2,
	working.directory=getwd())