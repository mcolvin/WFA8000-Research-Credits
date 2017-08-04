## Simulation
## No covariates, constant time intervals between primary periods, and
## no secondary sampling periods
library(unmarked)
set.seed(3)
M <- 50  # NUMBER OF SAMPLING SITES
T <- 5 # NUMBER OF OCCASSIONS
lambda <- 4 # UNDERLYING DENSITY
gamma <- 1.5 # RECRUITMENT
omega <- 0.8 # APPARENT SURVIVAL
p <- 0.7
y <- N <- matrix(NA, M, T)
S <- G <- matrix(NA, M, T-1)
N[,1] <- rpois(M, lambda) # INITIAL ABUNDANCE
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
# Finite sample inference. Abundance at site i, year t
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
