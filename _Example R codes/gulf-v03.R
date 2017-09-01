require(msm)
M = 40 # n
nantenna = 6 # J
nocc = 12 # T WEEKS
socc = 1:(7*nocc) # days within weeks
week=sort(rep(1:nocc,7))
antenna.loc = sort(rep(runif(nantenna,3,60))) #x(j)
xl<- 0 
xu<- 60
first<- sample(1:30,M,replace=TRUE) 
last<- first+sample(sample(7:76,M,replace=TRUE))# make sure fish are around at least 2 weeks
last<- ifelse(last>max(socc)-8,max(socc)-8,last)
flow<- runif(length(socc),0,4)
T<-max(socc)


#######################################################################
# INPUTS
#######################################################################

beta0 <-0.25 	        # Intercept in sigma estimate
beta1 <- 2.2	        # Coefficient for flow in sigma estimate
tauv <- 27 		        # Normal dist spread for correlated S's, delete for uniform model
tau <- 1/(tauv*tauv)  	# Normal dist spread for correlated S's, delete for uniform model
lam0<-rep(NA,T)
for (t_ in 1:T)         # daily antenna baseline encounter rate
    {        
    lam0[t_] <- rgamma(1,20,5)  
    }

#######################################################################
# FLOW MODEL: DAILY
#######################################################################
sigma<- rep(0,T) # MOVEMENT PARAMETER
sigma2<- rep(0,T) # MOVEMENT PARAMETER
for (d in 1:T)
    { #t
    # daily sigma, flow covariate
    sigma[d] <- exp(beta0 + beta1 * flow[d] )
    sigma2[d] <- sigma[d] * sigma[d]  
    } #t

   

    
#######################################################################
# MOVEMENT MODEL: DAILY
#######################################################################   

## ACTIVITY CENTERS
S<- matrix(NA,M, length(socc))
for (i in 1:M)
    {    
    for(d in 1:(first[i]-1)) 
        { 
        S[i,d] <- 0  # Individual not in river, needed to follow node in JAGS
        }  
    S[i,first[i]] <- runif(1,xl,xu)  
    for (d in (first[i]+1):last[i]) 
        { 
        S[i,d] <- rtnorm(1,S[i,d-1], tauv,xl,xu) # truncated normal, correlated S 
        }
    for(d in (last[i]+1):T) 
        { 
        S[i,d] <- 0 
        }           
    } 

#######################################################################
# OBSERVATION MODEL
#######################################################################
# ARRAY OF DIFF^2 FROM ANTENNA    
D2<- array(0, dim=c(M, nantenna, length(socc)))
lam<- array(0, dim=c(M, nantenna, length(socc)))
tmp<- array(0, dim=c(M, nantenna, length(socc)))
y<- array(0L, dim=c(M, nantenna, length(socc)))
for(i in 1:M)
    {
    for(d in first[i]:last[i])
        {
        for(j in 1:nantenna)
            {
            D2[i,j,d] <- (S[i,d]-antenna.loc[j])^2
            lam[i,j,d] <- lam[i,j,d] 
            tmp[i,j,d] <- lam0[d] * exp(-D2[i,j,d]/(2*sigma2[d]))
            # Fit Poisson mode
            y[i,j,d] <-rpois(1,tmp[i,j,d])  
            }
        }
    }
    
    
model<-function()
    { #model
    # Priors
    beta0 ~ dunif(-1,3) 	# Intercept in sigma estimate
    beta1 ~ dunif(-1,3)	# Coefficient for flow in sigma estimate
    tauv ~ dunif(0,40) 		# Normal dist spread for correlated S's, delete for uniform model
    tau <- 1/(tauv*tauv)  	# Normal dist spread for correlated S's, delete for uniform model
    for (d in 1:T)         # daily antenna baseline encounter rate
        {        
        lam0[d] ~ dgamma(0.1, 0.1)  
        }
    ###################################################################
    # PROCESS MODEL
    ###################################################################
    for (dd in 1:T)
        { 
        # daily sigma, flow covariate
        log(sigma[dd]) <- beta0 + beta1 * flow[dd]
        sigma2[dd] <- sigma[dd] * sigma[dd]
        }    # DAILY MOVEMENT
    for (i in 1:M)
        {    
        for(d in 1:(first[i]-1)) 
            { 
            S[i,d] <- 0  # Individual not in river, needed to follow node in JAGS
            }  
        S[i,first[i]] ~ dunif(xl,xu)   
        for (d in (first[i]+1):last[i]) 
            { 
            S[i,d] ~ dnorm(S[i,d-1], tau);T(xl,xu) # truncated normal, correlated S 
            }
        for(d in (last[i]+1):T) 
            { 
            S[i,d] <- 0 
            }           
        }
    for(i in 1:M)
        {
        for(d in first[i]:last[i]) 
            {
            for(j in 1:nantenna)
                {
                D2[i,j,d] <-pow(S[i,d]-antenna.loc[j],2)
                lam[i,j,d] <- lam0[d]* exp(-D2[i,j,d]/(2*sigma2[d])) 
                tmp[i,j,d] <- lam[i,j,d]
                y[i,j,d] ~ dpois(tmp[i,j,d])  
                }
            }
        }
    
 
    }
  
		
###### INPUT DATA
dat<- list(y=y, 
    xl=xl,
    xu=xu,
    first=first,
    last=last,
    M=M, 
    T=T,
    flow=flow,
    nantenna=nantenna, 
    antenna.loc=antenna.loc)


inits <- function(){list(lam0=runif(T,0.1,10),
    tauv=runif(1,0,40), 
    beta0=runif(1,-1,3),
    beta1=runif(1,-1,3)) }
parameters <- c(#"lam0",
    "beta0", "beta1", "tauv")
library(R2jags)

# IF IT THROWS THE WARNING "Error in node y[X,X,X]
# Node inconsistent with parents" THEN RERUN THE JAGS
# FUNCTION WHICH WILL GENERATE NEW INITIAL VALUES
# IT MIGHT BE A BIT SQUIRELLY WITH THE INITIAL VALUES.
# IT MAY TAKE 5 TIMES OR MORE TO GET INITIAL VALUES THAT
# WILL WORK, BUT THAT IS OK.
out <- jags(data=dat,
    inits=inits,
    parameters=parameters,	
    model = model,
	n.chains = 3,
	n.iter = 500,
	n.burnin = 150)

out$BUGSoutput$mean

