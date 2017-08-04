

xx<-rbinom(10000,30,0.02)
prob<- length(which(xx>0))/10000
prob
1-pbinom(0,30,prob=0.02)

mort_risk<- function(n,p)
	{
	1-pbinom(0,30,prob=0.02)
	}

pbinom(1,30,prob=0.02,lower.tail=TRUE)