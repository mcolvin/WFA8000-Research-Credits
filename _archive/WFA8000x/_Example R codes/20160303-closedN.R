library(R2jags)

setwd("C:/Users/mcolvin/Desktop")


mod<- function()
	{
	for(i in 1:M)
		{
		z[i]~dbern(omega) # LATENT VARIABLE, DATA AUGMENTATION
		for(j in 1:4)
			{
			p_eff[i,j]<- z[i]*p_cap[j] # CONDITIONAL CAPTURE PROBABILITY
			ch[i,j]~dbern(p_eff[i,j])			
			}#i
		}#j
	# ACOUSTICALLY TAGGED FISH
	for(i in 1:Ncha)
		{
		for(j in 1:4)
			{
			cha[i,j]~dbern(p_cap[j])
			}
		}
#	# CAPTURE PROBABILITY AS A FUNCTION OF EFFORT
	for(occ in 1:4)
		{
		y[occ]<- a
		p_cap[occ]<- exp(y[occ])/(1+exp(y[occ]))
		}
	# DERIVED PARAMETERS
	N<-sum(z[]) + Ncha
		
	# PRIORS
	omega~dunif(0,1)
	a~dnorm(0,0.37)
    }

## number of rows to augment capture histories
## this should be more than you expect the population to be
dat_aug<- 100
xxx<- read.csv("./Population Matrix from 3-2-16 Sampling.csv")
cha<- as.matrix(subset(xxx,dat==1)[,c(2:10)])
ch<- as.matrix(subset(xxx,dat==2)[,c(2:10)])

## vectors of 0s and 1s indicating whether fish is 
## in the pool or not
z<- c(rep(1,nrow(ch)),rep(0,dat_aug-nrow(ch)))
## capture history plus extra rows of 0s 



ch<-rbind(ch,matrix(0,nrow=dat_aug-nrow(ch) ,ncol=9))# data augmentation needs to be matrix of 0s not NAs for JAGs

## bundle up data for JAGs
dat<- list(ch=ch,# capture history of non-acoustic fish
	cha=cha, # capture history of acoustic fish
	Ncha=nrow(cha), # number of acoustically tagged fish in pool at sampling
	M=nrow(ch))# Number of rows in the augmented capture history matrix
	
## initial values
## set for each chain
inits<- function()
	{
	list(a=-2.5,omega=0.5,z=z)
	}
	
## WHAT PARAMTERS TO KEEP TRACK OF DURING ESTIMATION
params<-c("a","N","omega","p_cap")	

# THIS WILL ONLY RUN IF YOU HAVE JAGS INSTALLED 
# AND THE R2jags PACKAGE
out <- jags(data=dat,
	inits=inits,
	parameters=params,	
	model.file=mod,
	n.chains = 3,	
	n.iter = 15000,	
	n.burnin = 6000, 
	n.thin=2,
	working.directory=getwd())
out  # OUTPUT
out$BUGSoutput$mean$N # ESTIMATED NUMBER OF FISH IN POOL
traceplot(out)# LOOK AT TRACEPLOTS FOR CONVERGENCE.
	
	