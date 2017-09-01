	require(msm)
	nfish=30 # n
	nant=6 # J
	nocc = 10 # T
	antenna.loc = sort(rep(runif(nant,3,60))) #x(j)
	caphist<- array(0,dim=c(nfish, nant, nocc))
	x_lower<- 0
	x_upper<- 60
	first<- rep(1, nfish) #assume all fish tagged at week 1
	phi<- 0.8
	mu<- 0.1
	beta<- 2.5
	flow<- runif(nocc,0,10)
	tau<- 2
	mrate<- 5
	Z<- matrix(0,nfish,nocc)
	# assume all tagged at t = 1
	Z[,1]<- 1
	# TRUE SURVIVAL
	for(i in 2:nocc){Z[,i]<- rbinom(nfish,1,Z[,i-1]*phi)}

	# MOVEMENT MODEL
	sigma<- rep(0,nocc) # MOVEMENT PARAMETER
	for(i in 1:nocc){sigma[i]<- log((mu + beta*flow[i] + rnorm(1,0,1)))}
	sigma2<- sigma*sigma
	S<- matrix(0,nfish, nocc)
	D2<- array(0, dim=c(nfish, nant, nocc)) # matirix of squared differnces of area from antenna
	#tmp<- array(0, dim=c(nfish, nant, nocc)) # matirix of squared differnces of area from antenna
	
	S[,1]<- runif(nfish, 0,3) # LOCATIONS PRIOR TO TAGGING
	
	for(i in 1:nfish){
	S_hat<- rep(0,nfish)
		for(t in 2:nocc){
			#S[i,t]<-  rtnorm(1,S[i,t-1], tau, x_lower, x_upper)
			S_hat[i]<- mrate+S[i,t-1]
			S[i,t]<-  rtnorm(1,S_hat[i], tau, x_lower, x_upper)
			}
	}

		
	# OBSERVATION MODEL
	lam0<- rep(2000,nocc)# basline antenna encounter rate if a fish's activity area was centered on antenna
	y<- array(0, dim=c(nfish, nant, nocc)) 
	lam<- array(0, dim=c(nfish, nant, nocc))

	for(occ in  1:nocc){
		for(ant in 1:nant){
			D2[,ant,occ]<- (S[,occ]-antenna.loc[ant])^2
			lam[,ant,occ]<- lam0*exp((-D2[,ant,occ])/(2*sigma2[occ]))		
		}
	}
	
	for(i in 1:nant){y[,i,1]<- rpois(nfish,lam[,i,1])}
	# t = 2:6
	for(occ in  2:nocc){
		for(ant in 1:nant){
		y[,ant,occ]<- rpois(nfish,lam[,ant,occ]*Z[,occ])}
		}
		
		
require(R2jags)		
		
# THE MODEL

model<- function() 
    { #model
	# Priors
	phi ~ dunif(0,1)      # Survival (constant)
	for (t in 1:nocc)
        { #t
		w_lam0[t] ~ dunif(0, 1) # Weekly weir encounter rate
		sigma[t] ~ dunif(0,8) # Weekly sigma
		sigma2[t] <-sigma[t] * sigma[t] # Weekly sigma2
		}#
	mrate~ dunif(0, 10)
	# END PRIORS
	
	
	# MODEL
	for (i in 1:nfish)
        { # m, FILL S AND z WITH 0 PRIOR TO FIRST ENCOUNTER 
		for(t in 1:(first[i]-1)) 
            {
			S[i,t] <-0 # Individual not in river, needed to follow node in JAGS
			z[i,t] <-0 # Individual not in river, needed to follow node in JAGS
			}
		}
	for(i in 1:nfish)
        {# Individual’s first period 
		z[i,first[i]] ~ dbern(1) # Individual known to be alive at entry into study area
		S[i,first[i]] ~ dunif(0,82) # No prior information on individual’s location
		}
	
	# First period, antennas
	for(i in 1:nfish){
		for(j in 1:nant) { #j
			D2[i,j,first[i]]  <-pow(S[i,first[i]]-antenna.loc[j], 2) 
			lam[i,j,first[i]] <-lam0[first[i]]*exp(-D2[i,j,first[i]]/(2*sigma2[first[i]])) 
			tmp[i,j,first[i]] <-lam[i,j,first[i]]
			# Subsequent periods
				for (t in (first[i]+1):last[i]) 
                    { #t
                    S_hat[i,t]<- mrate*S[i,t-1]
                    S[i,t] ~ dnorm(S[i,t-1], tau);T(0,60) # truncated normal, correlated S
                    D2[i,1,t] <-pow(S[i,t]-antenna.loc[1], 2) 
                    w_lam[i,1,t] <-(1-exp(-w_lam0[t])) * exp(-D2[i,1,t]/(2*sigma2[t]))
                    tmp[i,1,t] <-z[i,t]*w_lam[i,1,t]*ON[1,t]
                    }
                    # LIKLIHOOD FOR WIER
                    for(j in 2:nant) { #j
                        D2[i,j,t] <-pow(S[i,t]-antenna.loc[j], 2) 
                        lam[i,j,t] <-lam0[t] * exp(-D2[i,j,t]/(2*sigma2[t]))
                        tmp[i,j,t] <-z[i,t]*lam[i,j,t]*ON[j,t]
                        y[i,j,t] ~ dpois(tmp[i,j,t])# LIKLIHOOD FOR ANTENNA
                        } #j
		phiUP[i,t] <-z[i,t-1]*phi # Estimate overall weekly survival rate 
		z[i,t] ~ dbern(phiUP[i,t]) # Estimate individual alive state
		} # t

	} #i
} #model


###### INPUT DATA
	fl<- Z
	for(i in 1:ncol(fl)){fl[,i]<- fl[,i]*i}
	fl[fl==0]<- NA
	# day of tagging
	first<- apply(fl,1, min,na.rm=TRUE)
	# last detection
	last<- apply(fl,1, max,na.rm=TRUE)
	dat<- list(y=y, first=first, last=last, nfish=nfish, nocc=nocc, nant=nant, 
		antenna.loc=antenna.loc)
###### PARAMETERS TO KEEP TRACK OF
param <- c('phi')
###### INITIAL VALUES
inits = function(){list(z=Z, phi=runif(1,0,1), lam0=runif(12,1,3),mrate=runif(1,0,10) ) }
init <- function() {
	list(mu = matrix(0,nrow=3, ncol=2), tau = c(1,1),Z)
	list(mu = matrix(1,nrow=3, ncol=2), tau = c(1,1))
	list(mu = matrix(-1,nrow=3, ncol=2), tau = c(1,1))
}

out <- jags(data=dat,inits=init,parameters=param,	model = filename,
	n.chains = 3,
	n.iter = 100,
	n.burnin = 30,
	debug=TRUE,	bugs.directory=bugsdir)
out$mean




#Set up a data input
dat<-list(y=y, first=first, last=last, nfish=nfish, nocc=nocc, nant=nant, 
	antenna.loc=antenna.loc)
#Set initial values
inits = function(){list(z=z, phi=runif(1,0,1), lam0=runif(12,1,3), tauv=runif(1,0,30), 
mu=runif(1,1,3), beta=runif(1,0,1)) }
# Parameters to follow
parameters <-c("sigma", "phi", "w_lam0", "lam0", "mu", "beta", "tauv", "S", "z")