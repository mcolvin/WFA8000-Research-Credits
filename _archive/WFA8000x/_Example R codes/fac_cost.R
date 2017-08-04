

lifespan<- 30
turnover_rate_5<- 0.33
turnover_rate<- 0.05

yrs<-Z<- matrix(0,500,100)
yrs[c(1:20),1]<- sample(c(1:30),20)
Z[c(1:20),1]<-1
startup<- 250000
total<-rep(0,100)
for(i in 2:100)
	{
	leave<- sapply(1:500,function(x) {ifelse(yrs[x,i-1]>5, 
		rbinom(1,1,turnover_rate*Z[x,i-1]),
		rbinom(1,1,turnover_rate_5*Z[x,i-1]))})
	# RETIRE
	if(max(yrs[,i-1])>=30){leave[which(yrs[,i-1]>=30)]<-1}
	
	# UPDATE Z
	yrs[,i]<- (yrs[,i-1]+1)*Z[,i-1]*(1-leave)
	Z[,i]<- Z[,i-1]*(1-leave)
	# HIRE
	n_hire<-sum(leave)
	indx<-which(apply(Z,1,sum)==0)
	Z[indx[1:n_hire],i]<-1
	yrs[indx[1:n_hire],i]<-1
	total[i]<- n_hire*startup
	}

cost<- c(cost,sum(total))

plot(c(0.13,0.23,0.33),cost,type='b')