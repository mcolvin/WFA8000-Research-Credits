
### lets simulate some occupancy data 3 visits
### N = 500 sites
## specify known values
p = 0.25
psi = 0.5
N = 500

# we first simulate presence
pres =rbinom(N,1,psi)

# next we simulate detection (3 occasion)
hist = cbind(rbinom(N,pres,p),cbind(rbinom(N,pres,p),rbinom(N,pres,p)))

## create capture histories
## Freq is number of sites with capture history
capt=as.data.frame(table(hist[,1],hist[,2],hist[,3]))
##lets look
capt

## create a finction that returns the log likelihood given
## a capture history and candidate values of p and psi

occ<- function(capt,est.psi,est.p){
prob= c()
## probability of 000 capture history
## probability not present (or) plus probability present by never detected
prob[1] = (1-est.psi) + est.psi*(1-est.p)^3
## probability of 100 capture history
prob[2] = est.psi*est.p*(1-est.p)^2
## probability of 010 capture history
prob[3] = est.psi*est.p*(1-est.p)^2
## probability of 110 capture history
prob[4] = est.psi*est.p*est.p*(1-est.p)
## probability of 001 capture history
prob[5] = est.psi*est.p*(1-est.p)^2
## probability of 101 capture history
prob[6] = est.psi*est.p*est.p*(1-est.p)
## probability of 011 capture history
prob[7] = est.psi*est.p*est.p*(1-est.p)
## probability of 111 capture history
prob[8] = est.psi*est.p^3
# this is the multinomial likelihood it returns the 
# log likelihood for the capture history frequencies 
# given the probability of the capture frequency
LL = dmultinom(capt[,4], size = NULL, prob, log = TRUE)
return(LL)
}

## heres what the function returns
# go ahead and try test values for psi (2nd element) and p (third element)
occ(capt,0.65,0.1)

## create a wrapper for function
# note that this returns the log likelihood thus
# we want the values of psi and p that maximize the log likelihood
fitter<-function(parm){
  z<-occ(capt,parm[1],parm[2])
  return(z)
}

## one way to get the maximum likelihood estimates 
## let's create a matrix that contains all combinations of psi and p
## ranging from 0.01 to 0.99
large = as.matrix(merge(seq(0.01,0.99, by =0.01),seq(0.01,0.99, by =0.01)))
colnames(large) = c("est.psi","est.p")

## here we get the log likelihood values for these combinations
yy= c()
for(i in 1:length(large[,1])){
xx<-c(fitter(large[i,]),large[i,])
yy = rbind(yy,xx)
}
head(yy)
colnames(yy)[1]<- "llik" 
vals<- as.data.frame(yy) # NEED FOR CONTOUR PLOTTING

## lets plot the values to find the maximum likelihood estimates
require(reshape2)
# RESHAPE VALUES FOR PLOTTING USING CONTOUR
plotdat<-dcast(vals,est.p~est.psi, value.var="llik",mean) # CONVERTS OUTPUTS TO A 'GRID' OF LLIK VALUES 
z<- as.matrix(plotdat[,-1] )# GRID MATRIX OF LLIK VALUES FOR PLOTTING
p.est<- seq(0.01,0.99, by =0.01)
psi.est<- seq(0.01,0.99, by =0.01) 
contour(p.est, psi.est, z,  levels = seq(-2300, -15, by = 75), ylab="Psi", xlab="p")
# LETS FIND THE P AND PSI VALUES AT THE MAXIMUM FROM THE GRID SEARCH
maxid<- which.max(vals$llik)
points(vals[maxid,]$est.p, vals[maxid,]$est.psi, col="red", pch=19)# THERE IT IS

## these are the maximum likelihood estimates based on exhaustive search
yy[yy[,1]==min(yy[,1])] 




# Now we'll use the optim function 
# with the bounded quasi newton to find the MLE
# this is basically how MARK and PRESENCE do it
fit<- optim(
  par=c(.5,.5),
  fn= fitter,
  method="L-BFGS-B",
  control=list(fnscale=-1),
  lower=c(0.001,0.001), 
  upper=c(.999,.999))
# print out MLE
fit


fitterAndPlot<- function(parm){# a function that plots the trys
	points(parm[1], parm[2])
	z<-occ(capt,parm[1],parm[2])
	return(z)
	}
	
contour(p.est, psi.est, z,  levels = seq(-2300, -15, by = 75), ylab="Psi", xlab="p")
fit<-  optim(
  par=c(.5,.5),
  fn= fitter_plot,
  method="L-BFGS-B",
   control=list(fnscale=-1),
  lower=c(0.001,0.001), 
  upper=c(.999,.999))

