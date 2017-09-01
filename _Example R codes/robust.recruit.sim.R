#SIMULATE DATA UNDER J-S SUPERPOPULATION MODEL AND RUNS JOLLY , POPAN, AND PRADEL ANALYSES
rm(list=ls())

#load required functions first
# Function to simulate capture-recapture data under the JS model
require(RMark)

pasty<-function(x) 
{
k<-ncol(x)
n<-nrow(x)
out<-array(dim=n)
for (i in 1:n)
{
out[i]<-paste(x[i,],collapse="")
}
return(out)
}
phi=0.8
p=0.3
f=0.3
N.init=100
n.primary=10
n.secondary=3


simul.robust.js <- function(phi=0.8, p=0.3, f=0.3, N.init=100,n.primary=10,n.secondary=3)
{
phi<-rep(phi,n.primary-1)
p<-rep(p,n.primary*n.secondary)

#getting p .ent values for simulations
#initial N
N<-array(dim=n.primary)
#survival rate

#B needs to be integer. get p.ent first...

B_<-array(dim=n.primary-1)
N[1]<-B_[1]<-N.init
for(i in 1:(n.primary-1))
{
B_[i+1]<-N[i]*f
N[i+1]<-N[i]*phi[i]+B_[i+1]

}
N.super<-round(sum(B_),0)
p.ent<-B_/N.super

B <- rmultinom(1, N.super, p.ent) 

   PHI <- matrix(rep(phi, (n.primary-1)*N.super), ncol = n.primary-1, nrow = N.super, byrow = T)
n.occasions<-n.primary*n.secondary

   P <- matrix(rep(p, n.occasions*N.super), ncol = n.occasions, nrow = N.super, byrow = T)
  

   CH.sur <- matrix(0, ncol = n.primary, nrow = N.super)
   CH.p<-CH.sur.aug <- matrix(0, ncol = n.occasions, nrow = N.super)


   # Define a vector with the occasion of entering the population


   ent.occ <- numeric()
   for (t in 1:n.primary){
      ent.occ <- c(ent.occ, rep(t, B[t]))
      }
   # Simulating survival
   for (i in 1:N.super){
      CH.sur[i, ent.occ[i]] <- 1   # Write 1 when ind. enters the pop.
      if (ent.occ[i] == n.primary) next
      for (t in (ent.occ[i]+1):n.primary){
         # Bernoulli trial: has individual survived occasion?
         sur <- rbinom(1, 1, PHI[i,t-1])
         ifelse (sur==1, CH.sur[i,t] <- 1, break)
         } #t
      } #i
   # Simulating capture
   for (i in 1:N.super){
      CH.p[i,] <- rbinom(n.occasions, 1, P[i,])
      } #i

#expanded survival history (over secondary occasions
for(i in 1:N.super)
{
x<-numeric(0)
for(j in 1:n.primary)
{
x<-append(x,rep(CH.sur[i,j],n.secondary))

}
CH.sur.aug[i,]<-x
}
   # Full capture-recapture matrix
   CH <- CH.sur.aug * CH.p
   
   # Remove individuals never captured
   cap.sum <- rowSums(CH)
   never <- which(cap.sum == 0)
   CH <- CH[-never,]
   #Nt <- colSums(CH.sur)    # Actual population size

#convert to capture history strings for RMark
robust.js.data<-data.frame(ch=pasty(CH))
secondary<-c(rep(0,n.secondary-1),1)
secondary.last<-rep(0,n.secondary-1)
intervals<-c(rep(secondary,n.primary-1),secondary.last)

  
robust.js.processed=process.data(robust.js.data,model="RDPdfClosed",time.intervals=intervals)
robust.js.ddl=make.design.data(robust.js.processed)

mod.0<-mark(robust.js.processed,robust.js.ddl)
N.hat<-mod.0$results$derived$estimate
f.hat<-mod.0$results$real$estimate[2]
phi.hat<-mod.0$results$real$estimate[1]
p.hat<-mod.0$results$real$estimate[3]

B.hat<-N.hat[1:(n.primary-1)]*f.hat

true=list(phi=phi[1],f=f, B=B[2:10,1], N=N,p=p[1])
estimates=list(phi=phi.hat,f=f.hat,B=B.hat,N=N.hat,p=p.hat)
results=list(estimates=estimates,true=true)
return(results)
   }

#execute 1 simulation run to return estimated parameters
results<-simul.robust.js(phi=0.8, p=0.3, f=0.3, N.init=100,n.primary=10,n.secondary=3)

results













