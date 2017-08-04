	## Simulate data

	# Parameters
	lam <- 30       # MEAN INITIAL ABUNDANCE IN YEAR 1
	p <- 0.35       # DETECTION PROBABILITY

	# STUDY DESIGN STUFF
	nSites <- 60
	nReps <- 10
	nYears <- 5
	
	# A COVARIATE TO MODEL TREND
	xx<- matrix(runif(nYears*nSites,0,1000),nSites,nYears)
	b0<-0.03
	b1<--0.0002 # EFFECT OF COVARIATE XX ON CHANCE IN ABUND.  
	r<- b0+b1*xx
	
	# CHANGE IN ABUNDANCE FROM TIME TO TIME 
	# ASSUMING A RELATIONSHIP WITH A COVARIATE XX
	# 1 = NO CHANGE, >1 = INCREASE, <1 = DECREASE
	plot(xx,exp(r)) # LOOK AT VALUES TO MAKE SURE THEY ARE REASONABLE

	# SIMULATE TRUE ABUNDANCE DYNAMICS GIVEN R
	N <- matrix(NA, nSites, nYears)
	N[,1] <- rpois(nSites, lam)
	for(t in 2:nYears) 
		{
		N[,t] <- rpois(nSites, exp(r[,t])*N[,t-1])
		}
	# END SIMULATING ABUNANCE DYNAMICS
	
	
	# LOOK AT TRUE ABUNANCE FOR EACH SITE
	matplot(t(N), xlab="Year", ylab="Population size",
	     type="l")
		
	# SIMULATE OBSERVATION PROCESS
	y <- array(NA, c(nSites, nReps, nYears))
	for(t in 1:nYears) 
		{
		for(k in 1:nReps) 
			{
			y[,k,t] <- rbinom(nSites, N[,t], p)
			}
		}
	yy<- y[,,1]
	for(i in 2:nYears)
		{
		yy<- cbind(yy,y[,,i])
		}
	# END MAKING OBERVERATIONS
	
	# BUNDLE UP YEARLY COVARIATES
	yscovs<- list(year=data.frame(
		matrix(as.factor(c(1:nYears)),
		nrow=nSites,ncol=nYears,byrow=TRUE)),
		x=xx)
	# END YEARLY COVARIATES		
		
	## Fit the model in unmarked
	library(unmarked)
	umf.exp <- unmarkedFramePCO(y=yy, 
		yearlySiteCovs=yscovs,
		numPrimary=nYears)
	fm.exp <- pcountOpen(~1,  # COVARIATES FOR INITIAL ABUNDANCE
		~x, # COVARIATES FOR TREND
		~1, # NOTHING GOES HERE
		~1, # COVARIATES FOR DETECTION
		data=umf.exp, dynamics="trend",
		K=max(N)+10, se=TRUE)
		
	coef(fm.exp,type="lambda") # INITIAL ABUNDANCE
	coef(fm.exp,type="gamma") # ESIMATES OF B0 AND B1
	coef(fm.exp,type="det") # ESTIMATES OF DETECTION PROB
		
		