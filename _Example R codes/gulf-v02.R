require(msm)
M = 100#315 # n
nantenna = 6 # J
nocc = 12 # T WEEKS
socc = 1:(7*nocc) # days within weeks
week=sort(rep(1:nocc,7))
antenna.loc = sort(rep(runif(nantenna,3,60))) #x(j)
xl<- 0 
xu<- 60
first<- sample(1:30,nfish,replace=TRUE) 
last<- first+sample(sample(7:76,nfish,replace=TRUE))# make sure fish are around at least 2 weeks
last<- ifelse(last>max(socc)-8,max(socc)-8,last)
flow<- runif(length(socc),0,10)
T<-max(socc)
W<-max(week)
#######################################################################
# INPUTS
#######################################################################
phi <- 0.8   		# Survival (constant)
beta0 <-0.25 	        # Intercept in sigma estimate
beta1 <- 2.2	        # Coefficient for flow in sigma estimate
tauv <- 27 		        # Normal dist spread for correlated S's, delete for uniform model
tau <- 1/(tauv*tauv)  	# Normal dist spread for correlated S's, delete for uniform model
for (t_ in 1:T)         # daily antenna baseline encounter rate
    {        
    lam0[t_] <- rgamma(1,20,5)  
    }

#######################################################################
# FLOW MODEL: DAILY
#######################################################################
for (t_ in 1:T)
    { #t
    # daily sigma, flow covariate
    sigma[t_] <- exp(beta0 + beta1 * flow[t_] )
    sigma2[t_] <- sigma[t_] * sigma[t_]  
    } #t
sigma<- rep(0,length(socc)) # MOVEMENT PARAMETER
for(i in 1:length(socc))
    {
    sigma[i]<- exp(beta0 + beta1*flow[i])
    }
sigma2<- sigma*sigma    

#######################################################################
# SUVIVAL: WEEKLY
#######################################################################
Z<- matrix(0,nfish,nocc)
Z[,1]<- 1
# TRUE SURVIVAL
for(i in 1:M)
    {
    for(W_ in 2:W)
        {
        Z[i,W_]<- rbinom(1,1,Z[i,W_-1]*phi)
        }
    }
    
    
#######################################################################
# MOVEMENT MODEL: DAILY
#######################################################################   

## ACTIVITY CENTENTERS
S<- matrix(NA,nfish, length(socc))
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
lambda<- array(0, dim=c(M, nantenna, length(socc)))
y<- array(0, dim=c(M, nantenna, length(socc)))
for(i in 1:M)
    {
    for(d in first[i]:last[i])
        {
        for(j in 1:nantenna)
            {
            D2[i,j,d] <- (S[i,d]-antenna.loc[j])^2
            lambda[i,j,d] <- lam0[d] * exp(-D2[i,j,d]/(2*sigma2[d]))
            # Fit Poisson mode
            y[i,j,d] <-rpois(1,lambda[i,j,d]*Z[i,week[d]])  
            }
        }
    }
    
    
i=1
j=1
d=first[i]
last[i]
    y[1,,]
    S[1,]
    Z[1,]
4*7


    
model<-function()
    { #model
    # Priors
    phi ~ dunif(0,1)   		# Survival (constant)
    beta0 ~ dunif(-10,10) 	# Intercept in sigma estimate
    beta1 ~ dunif(-10,10)	# Coefficient for flow in sigma estimate
    tauv ~ dunif(0,40) 		# Normal dist spread for correlated S's, delete for uniform model
    tau <- 1/(tauv*tauv)  	# Normal dist spread for correlated S's, delete for uniform model
    for (t_ in 1:T)         # daily antenna baseline encounter rate
        {        
        lam0[t_] ~ dgamma(0.1, 0.1)  
        }
    ###################################################################
    # PROCESS MODEL
    ###################################################################
    for (t_ in 1:T)
        { #t
        # daily sigma, flow covariate
        log(sigma[t_]) <- beta0 + beta1 * flow[t_] 
        sigma2[t_] <- sigma[t_] * sigma[t_]  
        } #t
    # WEEKLY SURVIVAL
    for(i in 1:M)
        {
        for(ww in 1:(week[(first[i])]-1))
            {
            z[i,ww] <- 1  # Individual not in river, needed to follow node in JAGS
            } #t        
        z[i,week[first[i]]] ~ dbern(1) 
        for(www in (week[first[i]]+1):week[last[i]])
            {
            phi0[i,www]<- phi*z[i,www-1]
            z[i,www]~ dbern(phi0[i,www])            
            }
        for(wwww in (week[last[i]]+1):W)
            { 
            z[i,wwww] <- 0 
            } #t      
        }
    # DAILY MOVEMENT 
    for (i in 1:M)
        {    
        for(t_ in 1:(first[i]-1)) 
            { 
            S[i,t_] <- 0  # Individual not in river, needed to follow node in JAGS
            }       
        S[i,first[i]] ~ dunif(xl,xu)  
        for (t_ in (first[i]+1):last[i]) 
            { 
            S[i,t_] ~ dnorm(S[i,t_-1], tau);T(xl,xu) # truncated normal, correlated S 
            }
        # Periods after censor
        for(t_ in (last[i]+1):T) 
            { 
            S[i,t_] <- 0 
            }           
        }
    
    for (i in 1:M)
        { #m 
        for(j in 1:nantenna) 
            { #j
            for (t_ in first[i]:last[i]) 
                { #t           
                # Estimate activity center
                D2[i,j,t_] <- pow(S[i,t_]-antenna.loc[j], 2) 
                lam[i,j,t_] <- lam0[t_]*exp(-D2[i,j,t_]/(2*sigma2[t_])) # Lam0,sigma
                tmp[i,j,t_] <- z[i,week[t_]]*lam[i,j,t_]#*ON[j,first[i]] 
                y[i,j,t_] ~ dpois(tmp[i,j,t_]) 
                } #t_
            } # j         
        } # m
    }
  
		
###### INPUT DATA
dat<- list(y=y, 
    xl=x_lower,
    xu=x_upper,
    first=first,
    last=last,
    M=nfish, 
    T=T,
    flow=flow,
    week=week,
    W=max(week),
    nantenna=nant, 
    antenna.loc=antenna.loc)

x=1
z_ini[x,]
week[first[x]]
week[last[x]]
y[x,3,6]
first[x]
last[x]
###### INITIAL VALUES
z_ini<-lapply(1:nfish,function(x){
    xx<-matrix(NA,1,max(week))
    xx[,(week[first[x]]):week[last[x]]]<-1
    return(xx)})
z_ini<- as.matrix(do.call("rbind",z_ini))


inits <- function(){list(z=z_ini, 
    phi=runif(1,0,1), 
    lam0=runif(12*7,1,3),
    tauv=runif(1,0,30), 
    beta0=runif(1,-1,1),
    beta1=runif(1,-1,1)) }
parameters <- c("phi", #"lam0","sigma", 
    "beta0", "beta1", "tauv")
library(R2jags)
out <- jags(data=dat,
    inits=inits,
    parameters=parameters,	
    model = model,
	n.chains = 3,
	n.iter = 50,
	n.burnin = 15)
 #save(out,file="out.Rdata") 
