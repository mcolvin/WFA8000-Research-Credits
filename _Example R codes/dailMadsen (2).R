library(MASS)
setwd("C:/Users/mcolvin/Desktop/tmp/dnf")
library(R2WinBUGS)	
library(reshape2)
library(plyr)
library(unmarked)


# from help
	set.seed(3)
    M <- 50
    T <- 5
    lambda <- 4 # INITIAL ABUNDANCE
    gamma <- 1.5 # RECRUITMENT
    omega <- 0.8 # SURVIVAL REATE
    p <- 0.7 # DETECTION PROBABILITY
    y <- N <- matrix(NA, M, T)
    S <- G <- matrix(NA, M, T-1)
    N[,1] <- rpois(M, lambda) # SET INITIAL ABUNDANCE
    for(t in 1:(T-1)) {
            S[,t] <- rbinom(M, N[,t], omega)
            G[,t] <- rpois(M, gamma)
            N[,t+1] <- S[,t] + G[,t]
            }
    y[] <- rbinom(M*T, N, p)
     
     
     # Prepare data
    umf <- unmarkedFramePCO(y = y, numPrimary=T)
    summary(umf)
     
     
     # Fit model and backtransform
    (m1 <- pcountOpen(~1, ~1, ~1, ~1, umf, K=20)) # Typically, K should be higher
     
    (lam <- coef(backTransform(m1, "lambda"))) # or
    lam <- exp(coef(m1, type="lambda"))
    gam <- exp(coef(m1, type="gamma"))
    om <- plogis(coef(m1, type="omega"))
    p <- plogis(coef(m1, type="det"))
     
     ## Not run:
     
     # Finite sample inference. 
	 # Abundance at site i, year t
     re <- ranef(m1)
     devAskNewPage(TRUE)
     plot(re, layout=c(5,5), subset = site %in% 1:25 & year %in% 1:2,
          xlim=c(-1,15))
     devAskNewPage(FALSE)
     
     (N.hat1 <- colSums(bup(re)))
     CI <- apply(confint(re), c(2,3), sum)
     
     
     # Expected values of N[i,t]
     N.hat2 <- matrix(NA, M, T)
     N.hat2[,1] <- lam
     for(t in 2:T) {
         N.hat2[,t] <- om*N.hat2[,t-1] + gam
         }
     
     rbind(N=colSums(N), N.hat1=N.hat1, N.hat2=colSums(N.hat2))
     
     
     plot(1:T, N.hat1, ylim=c(0,600))
     points(1:T, colSums(N), col="blue", pch=16)
     arrows(1:T, CI[1,], 1:T, CI[2,], code=3, length=0.05, angle=90)
     ## End(Not run)
     



n<-10 #n= number of wetlands surveyed#
T<-4  #T= number of years surveyed#
J<-17 #J= number of repetitions in one year#


lambda<- 20 # INITIAL ABUNANDANCE
omega<-0.8 # survival
gamma<- 8.4# recruitment
Q<- 0.7# detection probability

# DAIL MADSEN MODEL
N<- matrix(0,nrow=n*T, ncol=J)
S<- matrix(0,nrow=n*T, ncol=J)
G<- matrix(0,nrow=n*T, ncol=J)
y<- array(0,c(n,T,J))
# loop across wetlands
N[,1] <- rpois(n*T,lambda)
for(t in 1:(J-1)) 
	{
	# Estimate survival
	S[,t] <- rbinom(n*T,N[,t],omega) 
	G[,t] <- rpois(n*T,gamma)
	N[,t+1]<- S[,t]+G[,t]
	}
matplot(t(N),type='l',main="True abundance")

# SIMULATE DETECTION
y<- N
for(i in 1:J) 
	{
	y[,i]<-rbinom(n*T, N[,i],Q)
	}
matplot(t(y),type='l')
# Prepare data
umf <- unmarkedFramePCO(y = y, numPrimary=J)
summary(umf)
# Fit model and backtransform
m1 <- pcountOpen(~1, ~1, ~1, ~1, umf, K=80) # Typically, K should be higher
lam <- exp(coef(m1, type="lambda"))
gam <- exp(coef(m1, type="gamma"))
om <- plogis(coef(m1, type="omega"))
p <- plogis(coef(m1, type="det"))



# LAMBDA AS A FUNCTION OF 
# AREA
area<- matrix(runif(n*T*J,10,100),ncol=J)
lambda<- 20 # INITIAL ABUNANDANCE
omega<-0.8 # survival

g0<-0
g1<-0.025
gamma<- matrix(0,nrow=n*T, ncol=J)
for(i in 1:J)
	{
	gamma[,i]<- exp(g0+area[,i]*g1) # recruitment
	}
Q<- 0.7# detection probability

# DAIL MADSEN MODEL
N<- matrix(0,nrow=n*T, ncol=J)
S<- matrix(0,nrow=n*T, ncol=J)
G<- matrix(0,nrow=n*T, ncol=J)
y<- array(0,c(n,T,J))
# loop across wetlands
N[,1] <- rpois(n*T,lambda)
for(t in 1:(J-1)) 
	{
	# Estimate survival
	S[,t] <- rbinom(n*T,N[,t],omega) 
	# Recruitment
	G[,t] <- rpois(n*T,gamma[,t])
	N[,t+1]<- S[,t]+G[,t]
	}
matplot(t(N),type='l',main="True abundance")

# SIMULATE DETECTION
y<- N
for(i in 1:J) 
	{
	y[,i]<-rbinom(n*T, N[,i],Q)
	}
matplot(t(y),type='l')
# Prepare data
yscovs<- list(area=as.matrix(area))
umf <- unmarkedFramePCO(y = y, 
	yearlySiteCovs=yscovs,
	numPrimary=J)
summary(umf)
# Fit model and backtransform
m1 <- pcountOpen(~1, # abundance
	~area, #recruitment
	~1, # survival
	~1, #detection
	umf, K=80) # Typically, K should be higher
coef(m1, type="lambda")
lam <- exp(coef(m1, type="lambda"))
gam <- exp(coef(m1, type="gamma"))
om <- plogis(coef(m1, type="omega"))
p <- plogis(coef(m1, type="det"))

lam
gam
om
p






# VERIFICATION
# LAMBDA AS A FUNCTION OF AREA
# ADDING OBSERVERS
n<-10 #n= number of wetlands surveyed#
T<-4  #T= number of years surveyed#
J<-17 #J= number of repetitions in one year#


area<- runif(n*T,10,100)
lambda<- exp(1+0.03*area+rnorm(n*T,0,0.2)) # INITIAL ABUNANDANCE
omega<-0.8 # survival
gamma<- 8.4# recruitment
Q<- 0.7# detection probability

# DAIL MADSEN MODEL
N<- matrix(0,nrow=n*T, ncol=J)
S<- matrix(0,nrow=n*T, ncol=J)
G<- matrix(0,nrow=n*T, ncol=J)
p<- matrix(0,nrow=n*T, ncol=J)

observers<- sample(c("a","b"), n*T*J,replace=TRUE)
p<- observers
p[p=="a"]<- runif(length(p[p=="a"]),0.5,0.8)
p[p=="b"]<- runif(length(p[p=="b"]),0.4,0.6)
p<- as.numeric(p)
observers<- matrix(observers,nrow=n*T, ncol=J)
p<- matrix(p,nrow=n*T, ncol=J)
oc<- list(observers=observers)

# loop across wetlands
N[,1] <- rpois(n*T,lambda)
for(t in 1:(J-1)) 
	{
	# Estimate survival
	S[,t] <- rbinom(n*T,N[,t],omega) 
	G[,t] <- rpois(n*T,gamma)
	N[,t+1]<- S[,t]+G[,t]
	}
matplot(t(N),type='l',main="True abundance")

# SIMULATE DETECTION
y<- N
for(i in 1:J) 
	{
	y[,i]<-rbinom(n*T, N[,i],p[,i])
	}
matplot(t(y),type='l')
# Prepare data
umf <- unmarkedFramePCO(y = y, 
	obsCovs=oc,
	siteCovs=data.frame(area=area),
	numPrimary=J)
summary(umf)
# Fit model and backtransform
m1 <- pcountOpen(~area, ~1, ~1, ~observers, umf, K=80) # Typically, K should be higher
coef(m1, type="lambda")
lam <- exp(coef(m1, type="lambda"))
gam <- exp(coef(m1, type="gamma"))
om <- plogis(coef(m1, type="omega"))
plogis(coef(m1, type="det")[1])
plogis(coef(m1, type="det")[1]+coef(m1, type="det")[2])

 
 
 