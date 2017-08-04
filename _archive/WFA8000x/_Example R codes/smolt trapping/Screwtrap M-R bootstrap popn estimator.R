
################################################################################################
################################################################################################
## This function estimates the number of outmigrating salmonids using screwtrap 
## capture recapture data and the bootstrap proceedure detailed in Thedinga J. F., M. L. Murphy, 
## S. W. Johnson, J. M Lorenz and K. V Koski. 1994. Determination of slamonid smolt yield with 
## rotary screw traps in the Situk River, Alaska, to predict effects of glacial flooding. North 
## American Journal of Fisheries Management. 14:837-851. 
## The program can use either continious sample data or subsampled data
## Program Beta Version 1.0 by James T Peterson, Oregon Cooperative Fish and Wildlife Research Unit
## August 12, 2014
##################################################################################################

#### BEGINNING OF FUNCTION
original.boot<- function(data,time ="WEEK", catch="CATCH", mark="MRK",recap="RCP",
                     sub.sample = F, P24 = NULL, PSS = NULL){

work<- na.omit(data)
miss<- nrow(data)- nrow(work)
if(miss > 0){cat(paste("\nNote",paste(miss," observations excluded due to missing data \n\n",sep = ""),sep = " "))}

time = toupper(time); catch=toupper(catch); mark=toupper(mark); recap=toupper(recap);
P24 = toupper(P24); PSS=toupper(PSS);

colnam<-toupper(names(work))

ch<- work[,which(colnam == catch)]
mrk<- work[,which(colnam == mark)]
rcp<- work[,which(colnam == recap)]

bad<-sum(ifelse(rcp > mrk,1,0))
if(bad > 0) {print(paste("ERROR: Number recaptured is greater than marked for ", paste(bad," observation(s)", sep = ""),sep = "")); break}
p= (rcp+1)/(mrk+1)
est.ch<-round(ch/p)
if(sub.sample == T){
PSS.no<- work[,which(colnam == PSS)]
P24.no<-work[,which(colnam == P24)]
PRO = (PSS.no+1)/(P24.no+1) 
bad<-sum(ifelse(PSS.no > P24.no,1,0))
if(bad > 0) {print(paste("ERROR: Number fish captured in subsample is greater than full sample for ", paste(bad," observation(s)", sep = ""),sep = "")); break}
}

mean.p<- mean.prop <- NULL
dull<-array(NA,dim= c(length(ch),15000))
for(i in 1:length(ch)){
 # i = 1
 cal.sim<-rbinom(15000,mrk[i],p[i])
 cap.sim<-rbinom(15000,est.ch[i],p[i])
 cap.sim<-cap.sim[cal.sim > 0]
 cal.sim<-cal.sim[cal.sim > 0]
 if(sub.sample == T){ pro.sim<-rbinom(15000,P24.no[i],PRO[i]); prop<-pro.sim/P24.no[i]} else prop <-1
 est.p<- (cal.sim/mrk[i])
 
 suc.N<- round(cap.sim/(est.p*prop))
 
 dull[i,] <- sample(suc.N, ncol(dull), replace = T)
 mean.p<- c(mean.p,round(mean(est.p),3))
 mean.prop <- c(mean.prop,round(mean(prop),3))
                   
}

popn <- colSums(dull)

meanz<-  round(apply(dull, 1, mean))
sdz<-  round(apply(dull, 1, sd),2)
CL <- round(t(apply(dull, 1, quantile, probs = c(0.025,0.975))))
CL[,1]<- ifelse(CL[,1]<ch,ch,CL[,1])

by.time<- cbind(meanz,sdz,CL)

by.time<- cbind(as.data.frame(work[,which(colnam == time)]),by.time)
by.time<- cbind(by.time,mean.p)

colnames(by.time) = c(time,"Estimate","SD","Lower_95%","Upper_95%","Est_cap_eff")

if(sub.sample == T){by.time<- cbind(by.time,mean.prop); colnames(by.time)[c(7)] = c("Est_prop")}

cl2<-round(quantile(popn,c(0.025,0.975)))
if(sum(ch)>cl2[1]) cl2[1]= sum(ch)

comb<-as.data.frame(t(as.data.frame(c(round(mean(popn)),round(sd(popn),2),cl2))))

colnames(comb)= c("Estimate","SD","Lower_95%","Upper_95%")
row.names(comb) <- NULL

output<- list(by.time,comb)
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
### sub.sample - this is set to "TRUE" or "T" when the screwtrap was subsampled
### P24 - the numnber of fish captured during the 24 hr monitoring period (only used when subsample = T)
### PSS - the numnber of fish captured in the subsample during the 24 hr monitoring period (only used when subsample = T)

## Example of usage using:
## Fish Creek steelhead data from Steinhorst et al. 2004
test<-as.data.frame(matrix(c(1, 15, 15, 10,2, 13, 13, 7, 3, 45, 45, 11,
                             4, 23, 23, 8, 5, 13, 13, 3), ncol = 4, byrow = T))
colnames(test)= c("WEEK","CATCH","MRK","RCP")

## here is where we execute the function
original.boot(data=test,time ="WEEK", catch="CATCH", mark="MRK",recap="RCP", sub.sample = F)

#####################################################################
### Another example dataset from
## Cathrine Creek Fall 2010 STS continuous data
another.test<-as.data.frame(matrix(c(37, 8, 387, 134,38, 9, 387, 134,
                                     39, 4, 387, 134, 40, 16, 387, 134,41, 62, 387, 134,42, 46, 387, 134,43, 272, 387, 134,
                                     44, 72, 387, 134,45, 68, 387, 134,46, 146, 387, 134, 47, 35, 387, 134), ncol = 4, byrow = T))
colnames(another.test)= c("WEEK","STS", "STS_REL", "STS_REC")

## here is where we execute the function
original.boot(data = another.test, time ="WEEK", catch="STS", mark="STS_REL", recap="STS_REC", sub.sample = F)


#####################################################################
### Last example dataset from
## Cathrine Creek spring 2011 STS subsample data
sub.data<-as.data.frame(matrix(c(13, 75, 190, 114, 151, 15, 14, 101, 174, 110, 151, 15,
                                 15, 12, 91, 61, 162, 12, 16, 4, 111, 73, 162, 12), ncol = 6, byrow = T))
colnames(sub.data)= c("WEEK", "NUM_COLL", "P24", "PSS", "REL", "REC")

## Run the function with the subset data
original.boot(data = sub.data, time ="WEEK", catch="NUM_COLL", mark="REL", recap="REC",
              sub.sample = T, P24 = "P24", PSS = "PSS")

