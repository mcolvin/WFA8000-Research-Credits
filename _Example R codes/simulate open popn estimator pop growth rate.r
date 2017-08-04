
# Simulation for evaluating tradeoffs for sample designs that utilize the Pradel open
# capture-recapture estimator NOTE estimator assumes random selection of sample units 
# The programs requires users to have RMark package installed. 
# To run, sumbit the entire program once to load it. Then specify input parameters at 
# the end of the script.
# Written by Jim Peterson OR Coop Unit May 30, 2014


eval.design<- function(stream.length,no.units,unit.size,true.N,primary,capt.prob,survival,lambda, runs){

require(RMark)

if(is.element("RMark",installed.packages()[,1]) == 0) {print("ERROR: Package RMark is not installed"); break}

prim = primary
cap.p = capt.prob
surv = survival
rep = lambda - survival

# stream.length<- 1000
# 
# no.units<- 5
# 
# unit.size<- 200
# 
# true.N<- 300
# 
# prim = 8
# cap.p = 0.2
# surv = 0.975
# rep <- 0.1
# runs = 100


prop.surv <- round(((unit.size*no.units)/area.length),3)

if(prop.surv > 1) {print("ERROR: Proportion of study area sampled exceeds 100%"); break}

if(prim < 3) {print("ERROR: Not enough primary sample periods to fit models"); break}


est.S= est.cap.p=est.f=cv.S=cv.cap.p=cv.f=est.lamb=cv.lamb=NULL

for(aa in 1:runs){

hist<-array(NA,c(true.N*10,prim))
            
N = rep(1,true.N)
for(i in 1:prim){
  pop.tm1 <- sum(N)
  if(i > 1) N <- c(rbinom(length(N),N,surv),rep(1,round(pop.tm1*rep)))
  
  hist[,i]<- c(rbinom(length(N),N,cap.p*prop.surv), rep(0,length(hist[,i])-length(N)))
  
}
biggie= subset(hist, rowSums(hist) > 0)

cap.hist=NULL
for(i in 1:length(biggie[,1])){
  xx<-paste(biggie[i,1:length(biggie[1,])],collapse="")
  cap.hist<-rbind(cap.hist,xx)
}
cap.hist<- as.data.frame(cap.hist)
names(cap.hist) = "ch"
cap.hist$ch=as.character(cap.hist$ch)

model.out=mark(data = cap.hist, groups = NULL, model = "Pradrec",
               time.intervals=NULL,threads=2, output = F, silent = T)


#str(model.out)               

est.S<- c(est.S,model.out$results$real$estimate[1])
est.cap.p<- c(est.cap.p,model.out$results$real$estimate[2])
est.f <- c(est.f,model.out$results$real$estimate[3])

est.lamb<-c(est.lamb,model.out$results$derived$estimate[1])


cv.S<-c(cv.S,model.out$results$real$se[1]/model.out$results$real$estimate[1])
cv.cap.p<- c(cv.cap.p,model.out$results$real$se[2]/model.out$results$real$estimate[2])
cv.f<- c(cv.f,model.out$results$real$se[3]/model.out$results$real$estimate[3])

cv.lamb<-c(cv.lamb,model.out$results$derived$se[1]/model.out$results$derived$estimate[1])

}

bias.S = est.S - surv
bias.cap.p = est.cap.p - (cap.p*prop.surv)
bias.rep = est.f - rep


print(paste("Estimated capture prob:",round(mean(est.cap.p),3), sep = " "))
print(paste("Estimated survival:",round(mean(est.S),3), sep = " "))
print(paste("Estimated reprod rate:",round(mean(est.f),3), sep = " "))
print(paste("Estimated pop growth rate (lambda):",round(mean(est.lamb),3), sep = " "))

print(paste("Precision of estimated capture prob:",round(mean(cv.cap.p),3), sep = " "))
print(paste("Precision of estimated survival:",round(mean(cv.S),3), sep = " "))
print(paste("Precision of estimated reprod rate:",round(mean(cv.f),3), sep = " "))
print(paste("Precision of estimated pop growth rate (lambda):",round(mean(cv.lamb),3), sep = " "))

print(paste("Bias of estimated capture prob:",round(mean(bias.cap.p),3), sep = " "))
print(paste("Bias of estimated survival:",round(mean(bias.S),3), sep = " "))
print(paste("Bias of estimated reprod rate:",round(mean(bias.rep),3), sep = " "))


}
  
## This is where you specify the sample design, such as size of the sample unit, number of primary 
## sample sample periods (i.e., this is probability number of years for your BT sampling), 
## capture probability, population size etc. The function will output the mean eatimate, bias,
## precision (expressed as a coefficient of variation), and the percent of runs that failed. 
## That is, the proportion of simulations where you generated useless data. This should be used as an indicator is 
## a sample design is likely to produce data that are not very useful. Note that the capture probability estimated
## with this design incluses an estimate of the capture probability and the probability of encountering 
## the fish with the sample design. If 100% of stream is sampled, estimated capture probability should equal 
## specified capture probability.


eval.design(stream.length = 1000, ## total length of stream
            no.units = 3,         ## number of sample units to sample    
            unit.size  = 100,     ## size (length) of each sample unit 
            true.N = 300,         ## true population size at beginning of study           
            primary = 4,          ## number of primary sample occasions
            capt.prob = 0.5,      ## probability of capturing a fish given it is in the sample unit
            survival = 0.9,       ## survival rate between primary periods (should be fairly high)
            lambda = 1.1,         ## population growth rate, 1 mean stable, no growth
            runs = 10)            ## number of simulations to run                     



