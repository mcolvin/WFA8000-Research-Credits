
################################################################################################
################################################################################################
## This function estimates the number of outmigrating salmonids using screwtrap 
## capture recapture data and the maximum likelihood estimator of Steinhorst, K., Y. Wu, B. Dennis, 
## and P. Kline 2004. Confidence intervals for fish out-migrant estimates using stratified trap 
## efficiency methods. Journal of Agricultural, Biological, and Environmental Statistics 9:284-299. 
## It also creates two types of confidence intervals profile likelihood and bootsrtap.
## Program Beta Version 1.0 by James T Peterson, Oregon Cooperative Fish and Wildlife Research Unit
## August 12, 2014
##
##
##    >>>>>>  NOTE THAT THE PROGRAM REQUIRES R PACKAGE optimx <<<<<<<
##
##################################################################################################


#### BEGINNING OF FUNCTION
MLE.trap.est<- function(data,time ="WEEK", catch="CATCH", mark="MRK",recap="RCP"){
  
  work<- na.omit(data)

  miss<- nrow(data)- nrow(work)
  if(miss > 0){cat(paste("\nNote",paste(miss," observations excluded due to missing data \n\n",sep = ""),sep = " "))}
  
  if(is.element("optimx",installed.packages()[,1]) == 0) {print("ERROR: Package optimx is not installed"); break}
  require(optimx)
  
  time = toupper(time); catch=toupper(catch); mark=toupper(mark); recap=toupper(recap);
  
  colnam<-toupper(names(work))
  
  cht<- work[,which(colnam == catch)]
  mrk<- work[,which(colnam == mark)]
  rcp<- work[,which(colnam == recap)]
  
  bad<-sum(ifelse(rcp > mrk,1,0))

  if(bad > 0) {print(paste("ERROR: Number recaptured is greater than marked for ", paste(bad," observation(s)", sep = ""),sep = "")); break}
  

stuff<-function(Est.N,p){

LL= log(dbinom(cht,round(Est.N),p)*dbinom(rcp,mrk,p)+ 0.00001)/(1-(1-p)^mrk)

return(sum(LL))
}
fitter<-function(parm){
  parm<-matrix(parm,ncol = 2)
  z<-stuff(parm[,1],parm[,2])
  return(z)
}
start.p<-rcp/mrk
init<-c(round(cht/start.p),start.p)
low<-c((cht+1),rep(0.001,length(cht)))
up <- c((round(cht/start.p)*5),rep(0.99,length(cht)))
  fit<- optimx(
    par=init,
    fn= fitter,
    method="L-BFGS-B",
    lower = low,
    upper = up,
    control=list(fnscale=-1, dowarn = F),
    hessian = T)
est.N<-as.numeric(fit[1:length(cht)])
est.p<-as.numeric(fit[(1+length(cht)):(2*length(cht))])

cl=NULL
 for(i in 1:length(est.N)){
   NN<- c(cht[i]:(5*est.N[i]))
   f<- 2*(dbinom(cht[i],est.N[i], est.p[i], log = T) - dbinom(cht[i],round(NN), est.p[i], log = T))
 cl<-rbind(cl,c(min(NN[which(f<qchisq(.95,1))]),max(NN[which(f<qchisq(.95,1))]))) 
}

dull<-array(NA,dim= c(length(cht),5000))
for(i in 1:length(cht)){
  # i = 1
  cal.sim<-rbinom(5000,mrk[i],est.p[i])
  cap.sim<-rbinom(5000,est.N[i],est.p[i])
  cap.sim<-cap.sim[cal.sim > 0]
  cal.sim<-cal.sim[cal.sim > 0]
  
  suc.N<- round(cap.sim/(cal.sim/mrk[i]))
  
  dull[i,] <- sample(suc.N, ncol(dull), replace = T)                 
}


popn <- colSums(dull)

cl2 <- round(t(apply(dull, 1, quantile, probs = c(0.025,0.975))))

tada<-cbind(as.data.frame(work[,which(colnam == time)]),round(est.p,3),est.N,cl,cl2)

colnames(tada) = c(time,"p.hat","N.hat","Prof.lower.95","Prof.upper.95","Boot.lower.95","Boot.upper.95")

comb<-as.data.frame(t(as.data.frame(c(round(mean(popn)),round(sd(popn),2),round(quantile(popn,c(0.025,0.975)))))))

colnames(comb)= c("Estimate","SD","Lower_95%","Upper_95%")
row.names(comb) <- NULL

output<- list(tada,comb)
names(output) = c("Time_specific_estimates","Combined_population_estimate")

return(output)

}
##### END OF FUNCTION

## FUNCTION INPUTS 
### data- name of dataframe containg catch data
### time - name of column containing time index,e.g., julian week
### catch - name of column containing number of fish caught during time interval
### mark- name of column containing number of marked fish released during time interval
### recap- name of column containing number of marked fish recaptured during time interval


## Example of usage using:
## Fish Creek steelhead data from Steinhorst et al. 2004
test<-as.data.frame(matrix(c(1, 15, 15, 10,2, 13, 13, 7, 3, 45, 45, 11,
                             4, 23, 23, 8, 5, 13, 13, 3), ncol = 4, byrow = T))
colnames(test)= c("WEEK","CATCH","MRK","RCP")
test

## here is where we execute the function
MLE.trap.est(data=test,time ="WEEK", catch="CATCH", mark="MRK",recap="RCP")


## Cathrine Creek Fall 2010 STS data
another.test<-as.data.frame(matrix(c(37, 8, 387, 134,38, 9, 387, 134,
39, 4, 387, 134, 40, 16, 387, 134,41, 62, 387, 134,42, 46, 387, 134,43, 272, 387, 134,
44, 72, 387, 134,45, 68, 387, 134,46, 146, 387, 134, 47, 35, 387, 134), ncol = 4, byrow = T))
colnames(another.test)= c("WEEK","STS", "STS_REL", "STS_REC")

## here is where we execute the function
MLE.trap.est(data = another.test, time ="WEEK", catch="STS", mark="STS_REL", recap="STS_REC")
