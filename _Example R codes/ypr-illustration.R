N<- 1000
maxAge=7
min_age_harvested=3
tr<- 4.5
dt<-0.01
t0=-1.02
k=0.39
linf=350
M_above<-0.6
F_above<-0.4
F_below<- 0
a=0.00000058# 
b=3.61
times<-  seq(1,maxAge,by=dt)
N<- rep(NA,length(times))
Y<- rep(NA,length(times))
H<- rep(NA,length(times))
N[1]<- 1000
Y[1]<- 0

ll1<-log(1-((203/linf)))/-k + t0
tr<-log(1-((254/linf)))/-k + t0


for(i in 2:length(times))
	{
	indx1<-ifelse(times[i]>=min_age_harvested &times[i]< tr,1,0)
	indx2<-ifelse(times[i]>=tr,1,0)
	harvested<-N[i-1]*(F_below*indx1+F_above*indx2)
	H[i]<- harvested
	mortality<- N[i-1]*M_above
	dN<- (mortality+harvested)*dt			
	Lt<-linf * (1 - exp(-k* (times[i]-t0)))
	## WEIGHT AT AGE
	Wt = a*Lt^b
	dY<- (Wt*harvested)*dt			
	N[i]<-N[i-1]-dN
	Y[i]<- Y[i-1]+dY
	}
	
	
presPlot()
plot(times,H,type="n",xlab="Age",
	ylim=c(0,180),ylab="Harvest amount",las=1,lwd=3)	
polygon(c(times,rev(times)),c(H,rep(0,length(times))),col="grey",border="grey")
abline(v=c(tr),lty=2)
abline(v=c(ll1),lty=2)





F_below<- 0.01

for(i in 2:length(times))
	{
	indx1<-ifelse(times[i]>=ll1 &times[i]< tr,1,0)
	indx2<-ifelse(times[i]>=tr,1,0)
	harvested<-N[i-1]*(F_below*indx1+F_above*indx2)
	H[i]<- harvested
	mortality<- N[i-1]*M_above
	dN<- (mortality+harvested)*dt			
	Lt<-linf * (1 - exp(-k* (times[i]-t0)))
	## WEIGHT AT AGE
	Wt = a*Lt^b
	dY<- (Wt*harvested)*dt			
	N[i]<-N[i-1]-dN
	Y[i]<- Y[i-1]+dY
	}
	

plot(times,H,type="n",xlab="Age",
	ylim=c(0,180),ylab="Harvest amount",las=1,lwd=3)	
polygon(c(times,rev(times)),c(H,rep(0,length(times))),col="grey",border="grey")
abline(v=c(tr),lty=2)
abline(v=c(ll1),lty=2)

F_below<- 0.05

for(i in 2:length(times))
	{
	indx1<-ifelse(times[i]>=ll1 &times[i]< tr,1,0)
	indx2<-ifelse(times[i]>=tr,1,0)
	harvested<-N[i-1]*(F_below*indx1+F_above*indx2)
	H[i]<- harvested
	mortality<- N[i-1]*M_above
	dN<- (mortality+harvested)*dt			
	Lt<-linf * (1 - exp(-k* (times[i]-t0)))
	## WEIGHT AT AGE
	Wt = a*Lt^b
	dY<- (Wt*harvested)*dt			
	N[i]<-N[i-1]-dN
	Y[i]<- Y[i-1]+dY
	}
	
plot(times,H,type="n",xlab="Age",
	ylim=c(0,180),ylab="Harvest amount",las=1,lwd=3)	
polygon(c(times,rev(times)),c(H,rep(0,length(times))),col="grey",border="grey")
abline(v=c(tr),lty=2)
abline(v=c(ll1),lty=2)

F_below<- 0.1

for(i in 2:length(times))
	{
	indx1<-ifelse(times[i]>=ll1 &times[i]< tr,1,0)
	indx2<-ifelse(times[i]>=tr,1,0)
	harvested<-N[i-1]*(F_below*indx1+F_above*indx2)
	H[i]<- harvested
	mortality<- N[i-1]*M_above
	dN<- (mortality+harvested)*dt			
	Lt<-linf * (1 - exp(-k* (times[i]-t0)))
	## WEIGHT AT AGE
	Wt = a*Lt^b
	dY<- (Wt*harvested)*dt			
	N[i]<-N[i-1]-dN
	Y[i]<- Y[i-1]+dY
	}
	
plot(times,H,type="n",xlab="Age",
	ylim=c(0,180),ylab="Harvest amount",las=1,lwd=3)	
polygon(c(times,rev(times)),c(H,rep(0,length(times))),col="grey",border="grey")
abline(v=c(tr),lty=2)
abline(v=c(ll1),lty=2)


F_below<- 0.2
for(i in 2:length(times))
	{
	indx1<-ifelse(times[i]>=ll1 &times[i]< tr,1,0)
	indx2<-ifelse(times[i]>=tr,1,0)
	harvested<-N[i-1]*(F_below*indx1+F_above*indx2)
	H[i]<- harvested
	mortality<- N[i-1]*M_above
	dN<- (mortality+harvested)*dt			
	Lt<-linf * (1 - exp(-k* (times[i]-t0)))
	## WEIGHT AT AGE
	Wt = a*Lt^b
	dY<- (Wt*harvested)*dt			
	N[i]<-N[i-1]-dN
	Y[i]<- Y[i-1]+dY
	}
	
plot(times,H,type="n",xlab="Age",
	ylim=c(0,180),ylab="Harvest amount",las=1,lwd=3)	
polygon(c(times,rev(times)),c(H,rep(0,length(times))),col="grey",border="grey")
abline(v=c(tr),lty=2)
abline(v=c(ll1),lty=2)


