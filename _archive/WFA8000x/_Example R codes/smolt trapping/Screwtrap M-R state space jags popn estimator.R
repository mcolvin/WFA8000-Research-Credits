
################################################################################################
################################################################################################
## This function estimates the number of outmigrating salmonids using screwtrap 
## capture recapture data and the a state space model (Peterson unplblished)
## The program can use wither continious sample data or subsampled data
## Program Beta Version 1.0 by James T Peterson, Oregon Cooperative Fish and Wildlife Research Unit
## August 12, 2014
##
##      >>>>>> NOTE REQUIRES R PACKAGE R2JAGS 
##                  AND 
##      >>>>>> JAGS SOFTWARE INSTALLED (available: http://mcmc-jags.sourceforge.net/)
##
##################################################################################################

#### BEGINNING OF FUNCTION
StateSpace.trap.est<- function(data,time ="WEEK", catch="CATCH", mark="MRK",recap="RCP"){
  
#   ## Fish Creek steelhead data from Steinhorst et al. 2004
#   test<-as.data.frame(matrix(c(1, 15, 15, 10,2, 13, 13, 7, 3, 45, 45, 11,
#                                4, 23, 23, 8, 5, 13, 13, 3), ncol = 4, byrow = T))
#   colnames(test)= c("WEEK","CATCH","MRK","RCP")
#   
#   ## here is where we execute the function
#   data=test; time ="WEEK"; catch="CATCH"; mark="MRK"; recap="RCP"; 
  
  work<- na.omit(data)
  
  miss<- nrow(data)- nrow(work)
  if(miss > 0){cat(paste("\nNote",paste(miss," observations excluded due to missing data \n\n",sep = ""),sep = " "))}
  
  if(is.element("R2jags",installed.packages()[,1]) == 0) {print("ERROR: Package R2jags is not installed"); break}
  require(R2jags)
  
  time = toupper(time); catch=toupper(catch); mark=toupper(mark); recap=toupper(recap);
  
  colnam<-toupper(names(work))
  
  cht<- work[,which(colnam == catch)]
  mrk<- work[,which(colnam == mark)]
  rcp<- work[,which(colnam == recap)]
  samples = length(cht)
  
  bad<-sum(ifelse(rcp > mrk,1,0))
  if(bad > 0) {print(paste("ERROR: Number recaptured is greater than marked for ", paste(bad," observation(s)", sep = ""),sep = "")); break}

### THE JAGS MODEL 
jag.model<- function ()
{
  N.star <- round(inbet) + total;
  inbet<- 1000*n.cont;
  n.cont ~ dunif(0.001, 1000);
  
  for(s in 1:samp){
    catch[s]  ~ dbin(p[s],N.int[s]);
    
    N.int[s] ~ dbin(gamma[s],N.star);
    
    recap[s] ~ dbin(p[s], mark[s]);  
    
    logit(p[s])<-b0[s];
   
  }
  for (i in 1:samp){ b0[i] ~ dnorm(b.bar,b.tau);
                    alpha[i]<-1}
  
  b.bar ~ dnorm(0,0.37);
  b.tau <- pow(b.ss,-2);
  b.ss ~ dunif(0,6); 
  gamma[1:samp]~ddirch(alpha[1:samp])
  
}

params<- c("N.star", "N.int", "p", "gamma")


jdata<- list(samp=samples, mark=mrk, recap =rcp, catch =cht, total=sum(cht))

inits<-function(){list(b0=rep(0,samples), n.cont = 100)}


ZZ<-jags(data =jdata, inits=inits, parameters.to.save=params, model.file=jag.model, n.thin=1, n.chains=2, n.burnin=20000, n.iter=100000)


if(sum(ZZ$BUGSoutput$summary[,8])/length(ZZ$BUGSoutput$summary[,8])> 1.05) print("Model may not have converged. Consider increasing no. iterations.")


by.time<-round(ZZ$BUGSoutput$summary[1:length(cht),c(1,2,3,7)],2)
by.time[,c(1,3,4)]<-round(by.time[,c(1,3,4)])
by.time<-cbind(as.data.frame(work[,which(colnam == time)]),by.time)
pss<-round(ZZ$BUGSoutput$summary[(length(ZZ$BUGSoutput$summary[,1])-samples+1):length(ZZ$BUGSoutput$summary[,1]),c(1,2)],3)
by.time<-cbind(by.time,pss)
colnames(by.time) = c(time,"Estimate","SD","Lower_95%","Upper_95%","Est_cap_eff","Cap_eff_SD")
row.names(by.time) <- NULL

prop<-round(ZZ$BUGSoutput$summary[(3+samples):(2+samples*2),c(1,2,3,7)],3)
prop<-cbind(as.data.frame(work[,which(colnam == time)]),prop)
colnames(prop) = c(time,"Est.prop.leave","SD","Lower_95%","Upper_95%")
row.names(prop) <- NULL

comb<-round(ZZ$BUGSoutput$summary[(length(cht)+1),c(1,2,3,7)],2)
comb[c(1,3,4)]<-round(comb[c(1,3,4)])
names(comb)= c("Estimate","SD","Lower_95%","Upper_95%")

output<- list(by.time,prop,comb)
names(output) = c("Time_specific_estimates","Estimated proportion migrating","Combined_population_estimate")

return(output)
}
##### END OF FUNCTION

## FUNCTION INPUTS 
### data- name of dataframe containg catch data
### time - name of column containing time index, e.g., julian week
### catch - name of column containing number of fish caught during time interval
### mark- name of column containing number of marked fish released during time interval
### recap- name of column containing number of marked fish recaptured during time interval
### sub.sample - this is set to "TRUE" or "T" when the screwtrap was subsampled

## Example of usage using:
## Fish Creek steelhead data from Steinhorst et al. 2004
test<-as.data.frame(matrix(c(1, 15, 15, 10,2, 13, 13, 7, 3, 45, 45, 11,
                               4, 23, 23, 8, 5, 13, 13, 3), ncol = 4, byrow = T))
colnames(test)= c("WEEK","CATCH","MRK","RCP")
  
## here is where we execute the function
StateSpace.trap.est(data=test, time ="WEEK", catch="CATCH", mark="MRK", recap="RCP") 

## Another example data set Cathrine Creek Fall 2010 STS data
another.test<-as.data.frame(matrix(c(37, 8, 387, 134,38, 9, 387, 134,
                                     39, 4, 387, 134, 40, 16, 387, 134,41, 62, 387, 134,42, 46, 387, 134,43, 272, 387, 134,
                                     44, 72, 387, 134,45, 68, 387, 134,46, 146, 387, 134, 47, 35, 387, 134), ncol = 4, byrow = T))
colnames(another.test)= c("WEEK","STS", "STS_REL", "STS_REC")

## here is where we execute the function
StateSpace.trap.est(data = another.test, time ="WEEK", catch="STS", mark="STS_REL", recap="STS_REC")
