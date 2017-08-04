library(R2WinBUGS)
bugsdirr<- c("C:/Users/mcolvin/Documents/WinBUGS14 - 1","C:/Users/mcolvin/Documents/WinBUGS14 - 2",
		"C:/Users/mcolvin/Documents/WinBUGS14 - 3","C:/Users/mcolvin/Documents/WinBUGS14 - 4")
setwd("C:/Users/mcolvin/Desktop/tmp")
N<-5000
T=4
n_segs<- 10

# EXPAND TO INDVIDUALS AND ASSIGN A SEGMENT
pop<- data.frame(ind=c(1:N),seg=sample(c(1:n_segs),N,replace=TRUE))


# OCCASION 1
# INDVIDUALS SUSCPECTIBLE TO CAPTURE IN SEGMENT
pop$suscpeptible<- rbinom(N,1,0.1)
p_cap<- 0.1
pop$cap1<- rbinom(N,1,p_cap*pop$suscpeptible)
pop$cap2<- rbinom(N,1,p_cap*pop$suscpeptible)
pop$cap3<- rbinom(N,1,p_cap*pop$suscpeptible)
pop$cap4<- rbinom(N,1,p_cap*pop$suscpeptible)

dat<- subset(pop, cap1==1 | cap2==2 | cap3==1)

dat_aug<- rbind(as.matrix(dat[,-c(1:3)]),
	matrix(NA,M,T)) # DATA AUGMENTATION

M<- 10000 # NUMBER OF SPOTS TO AUGMENT

# DATA TO MODEL
mod_dat<- list(M=nrow(dat_aug), 
	T=ncol(dat_aug),
	obs=dat_aug)



mod<- function()
	{

	for(i in 1:M)
		{
		z[i]~dbin(omega,1) # LATENT VARIABLE, IS THIS AN INDVIDUAL?
		for(j in 1:T)
			{
			p_eff[i,j]<- z[i]*p_cap # CONDITIONAL SPECIFICATION
			obs[i,j]~dbern(p_eff[i,j])			
			}#i
		}#j
	
	# DERIVED PARAMETERS
	N<-sum(z[]) 
	
	# PRIORS
	omega~dunif(0,1)
	p_cap~dunif(0,1)
    }
	
	
fn <- paste(getwd(),"model.bug", sep="/")
write.model(mod, fn)
	
inits<-function()
	{
	list(z=rep(1,nrow(dat_aug), p=runif(1,0,1)))
	}

wd<- getwd()
params<- c("p_cap","N")
out <- bugs(data=mod_dat,
	inits=inits,
	parameters=params,	model = fn,
	n.chains = 3,	n.iter = 7500,	
	n.burnin = 2500, 
	debug=TRUE,	n.thin=1,
	bugs.directory=bugsdirr[1],
	working.directory=wd,codaPkg=FALSE)
out$mean	
	
	
### SIMULATE Mo
N<- 100
T<-3
p<- 0.5
yfull<- yobs<- matrix(NA,N,T)
for(j in 1:T)
	{
	yfull[,j]<- rbinom(N,1,p)	
	}
ever_detected<- apply(yfull, 1, max)
C<- sum(ever_detected)
yobs<- yfull[ever_detected==1,]

nz<- 150 # add some 0s to augment
yaug<- rbind(yobs, matrix(0,nz,T))

mod<- function()
	{
	for(i in 1:M)
		{
		Z[i]~dbern(omega)
		for(j in 1:T)
			{
			peff[i,j]<- Z[i]*p
			yaug[i,j]~dbern(peff[i,j])
			}
		}
	
	N<- sum(Z[])
	omega~dunif(0,1)
	p~dunif(0,1)
	}
fn <- paste(getwd(),"model.bug", sep="/")
write.model(mod, fn)

dat<- list(M=nrow(yaug),
	T=T,
	yaug=yaug)
	
inits<-function()
	{
	list(Z=matrix(1,nrow(data_aug),T), 
		p=runif(1,0,1),
		omega=runif(1,0,1))
	}

wd<- getwd()
params<- c("N","p","omega")
out <- bugs(data=dat,
	inits=inits,
	parameters=params,	model = fn,
	n.chains = 3,	n.iter = 500,	
	n.burnin = 100, 
	debug=TRUE,	n.thin=1,
	bugs.directory=bugsdirr[1],
	working.directory=wd,codaPkg=FALSE)
out$median
	
	
	

	
	
	
	
	
	
	
### SIMULATE A JOLLY-SEBER
n_occ<- 10
phi<- rep(0.8,n_occ-1)
p<- rep(0.3,n_occ)
f<- 0.3

n<- 100 # initial population size
R<- c(n)
for(i in 2:n_occ)
	{
	R<- c(R,rpois(1,n[i-1]*f))
	n<- c(n, rbinom(1,n[i-1],phi)+R[i])	
	}
super_n<- sum(R) # total population
# WHEN DO CRITTERS RECRUIT TO THE POPULATION?
ent<-rep(c(1:length(R)),R)


ch<- Z<- matrix(0,super_n,n_occ)
for(i in 1:super_n)
	{
	Z[i,ent[i]]<-1
	indx<- ent[i]+1
	if(indx<=n_occ)
		{
		for(j in indx:n_occ)
			{
			Z[i,j]<- rbinom(1,1,phi[j-1]*Z[i,j-1])
			}
		}		
	}
# SIMULATE CAPTURE HISTORY
for(i in 1:super_n)
	{
	for(j in 1:n_occ)
		{
		ch[i,j]<- rbinom(1,1,p[j]*Z[i,j])
		}
	}
ch<- ch[which(apply(ch,1,sum)!=0),]

mod<- function()
	{
	for(i in 1:M)
		{
		W[i]~dbern(psi)
		Z[i,1]~dbern(nu[1])
		for(j in 2:n_occ)
			{
			Q[i,j-1]<- 1-Z[i,j-1] # 0 cannot be recruited, 1 can be recruited
			mu[i,j]<- phi[j-1]*Z[i,j-1]+nu[j]*prod(Q[i,1:(j-1)])
			Z[i,j]~dbern(mu[i,j])
			}
		}
		
		
	# OBERVATION MODEL
	for(i in 1:n_occ)
		{
		for(j in 1:n_occ)
			{
			p_cond[i,j]<- Z[i,j]*p_cap[j]*W[i]
			y[i,j]~dbern(p_cond[i,j])
			}
		}

	# DERIVED PARAMETERS
	for(j in 1:n_occ)
		{
		N[j]<- sum(Z[1:M,j])
		}
	# PRIORS
	mn_phi~dunif(0,1)
	mn_p~dunif(0,1)	
	psi~dunif(0,1)
	for(i in 1:(n_occ-1))
		{
		phi[i]<-mn_phi# survival
		}
	for(i in 1:n_occ)
		{
		beta[i]~dgamma(1,1)
		b[i]<- beta[i]/sum(beta[1:n_occ])
		p_cap[i]<-mn_p  # capture probability
		}
	nu[1]<- b[1]
	for(i in 2:n_occ)
		{
		nu[i]<- b[i]/(1-sum(b[1:(i-1)]))		
		}

		
    }
	
	
fn <- paste(getwd(),"model.bug", sep="/")
write.model(mod, fn)

M<-500
data_aug<- rbind(ch, matrix(0,M-nrow(ch),n_occ))	
dat<- list(M=M,
	n_occ=n_occ,
	y=data_aug,
	psi=runif(1))
	
inits<-function()
	{
	list(Z=matrix(1,nrow(data_aug),n_occ), 
		mn_p=runif(1,0,1),
		beta=rgamma(n_occ,1,1),
		mn_phi=runif(1,0,1),
		W=rep(1, nrow(dat$y)))
	}

wd<- getwd()
params<- c("N","mn_p","mn_phi")
out <- bugs(data=dat,
	inits=inits,
	parameters=params,	model = fn,
	n.chains = 3,	n.iter = 25000,	
	n.burnin = 10000, 
	debug=TRUE,	n.thin=1,
	bugs.directory=bugsdirr[1],
	working.directory=wd,codaPkg=FALSE)
out$median	
	
		
	



































