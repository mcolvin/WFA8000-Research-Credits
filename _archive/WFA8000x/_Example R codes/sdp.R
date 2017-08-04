
v<- c(0,0,0)
r<- c(0.5,1,1.5)
x<- matrix(c(0.25,0.5,0.25,0.25,0.3,0.45,0.15,0.3,0.55),nrow=3, ncol=3, byrow=TRUE)
r+ x%*%v

v<- c(0,0,0)
r<- c(5,10,15)*0.2
x<- matrix(c(0.6,0.3,0.1,0.3,0.5,0.2,0.35,0.4,0.25),nrow=3, ncol=3, byrow=TRUE)
r+ x%*%v

v<- c(0,0,0)
r<- c(5,10,15)*0.3
x<- matrix(c(0.8,0.2,0,0.65,0.3,0.05,0.35,0.55,0.1),nrow=3, ncol=3, byrow=TRUE)
v<-r+ x%*%v

v<- matrix(0,nrow=3,ncol=1)





r[,,1]<- c(5,10,15)*0.1
r[,,2]<- c(5,10,15)*0.2
r[,,3]<- c(5,10,15)*0.3
x<- array(0,dim=c(3,3,3))
x[,,1]<- matrix(c(0.2,0.5,0.3,0.2,0.3,0.5,0.1,0.3,0.6),nrow=3, ncol=3, byrow=TRUE)
x[,,3]<- matrix(c(0.7,0.3,0.0,0.6,0.3,0.1,0.3,0.5,0.2),nrow=3, ncol=3, byrow=TRUE)
x[,,2]<- matrix(c(0.5,0.3,0.2,0.2,0.5,0.3,0.2,0.4,0.4),nrow=3, ncol=3, byrow=TRUE)

y<- array(0,dim=c(3,3,3))
y[,,1]<- matrix(c(0.3,0.5,0.2,0.3,0.3,0.4,0.2,0.3,0.5),nrow=3, ncol=3, byrow=TRUE)
y[,,3]<- matrix(c(0.9,0.1,0,0.7,0.3,0,0.4,0.6,0),nrow=3, ncol=3, byrow=TRUE)
y[,,2]<- matrix(c(0.7,0.3,0,0.4,0.5,0.1,0.5,0.4,0.1),nrow=3, ncol=3, byrow=TRUE)




	r<- array(0,dim=c(3,1,3))
	r[,,1]<- c(5,10,15)*0.1
	r[,,2]<- c(5,10,15)*0.2
	r[,,3]<- c(5,10,15)*0.3
	w<- matrix(0.4,nrow=3,ncol=3)
	o<-matrix(0,nrow=3,ncol=3)
	e<-matrix(0,nrow=3,ncol=3)
	v<- matrix(0,nrow=3,ncol=1)
	p<- array(0,dim=c(3,3,3))	
	# Tf
	p[,,1]<- x[,,1]*w[,1] +  y[,,1]*(1-w[,1])
	p[,,2]<- x[,,2]*w[,2] +  y[,,2]*(1-w[,2])
	p[,,3]<- x[,,3]*w[,3] +  y[,,3]*(1-w[,3])	
	# CALCULATE AND SOLVE BELLMAN
	for(i in 1:3)
		{
		o[,i]<- r[,,i] + p[,,i] %*% v
		e[,i]<- p[,,i] %*% c(5,10,15)
		}
	# DETERMINE POLICY THAT MAXIMIZES UTILITY
	policy<- apply(o,1,which.max)
	# UPDATE V WITH NEW ACCRUED VALUES
	v<- apply(o,1,max)
	
for(iter in 1:50)
	{
	# Tf-1	
		p[,,1]<- x[,,1]*w[,1] +  y[,,1]*(1-w[,1])
		p[,,2]<- x[,,2]*w[,2] +  y[,,2]*(1-w[,2])
		p[,,3]<- x[,,3]*w[,3] +  y[,,3]*(1-w[,3])
			
		# CALCULATE AND SOLVE BELLMAN
		for(i in 1:3)
			{
			o[,i]<- r[,,i]+p[,,i]%*%v
			e[,i]<- p[,,i]%*% c(5,10,15)
			}
		# DETERMINE POLICY THAT MAXIMIZES UTILITY
		policy<- cbind(policy,apply(o,1,which.max))
		# UPDATE V WITH NEW ACCRUED VALUES
		v<- apply(o,1,max)
		print(policy)
		#print(v)
		for(stat in 1:3)
			{
			for(dec in 1:3)
				{
				fit1<- approxfun(c(5,10,15),x[stat,,dec])
				fit2<- approxfun(c(5,10,15),y[stat,,dec])
				w[stat,dec]<- (fit1(e[stat,dec])*w[stat,dec])/((fit1(e[stat,dec])*w[stat,dec])+
					(fit2(e[stat,dec])*(1-w[stat,dec])))
				}
			}
	}

	
	
	
	
	
######################################################
# TRY WITH WEIGHT BEING LINKED TO OPTIMAL DECISION
	r<- array(0,dim=c(3,1,3))
	r[,,1]<- c(5,10,15)*0.1
	r[,,2]<- c(5,10,15)*0.2
	r[,,3]<- c(5,10,15)*0.3
	w<- matrix(0.9,nrow=3,ncol=1)
	o<-matrix(0,nrow=3,ncol=3)
	e<-matrix(0,nrow=3,ncol=3)
	v<- matrix(0,nrow=3,ncol=1)
	p<- array(0,dim=c(3,3,3))
	
	
	# Tf
	p[,,1]<- x[,,1]*w[1] +  y[,,1]*(1-w[1])
	p[,,2]<- x[,,2]*w[2] +  y[,,2]*(1-w[2])
	p[,,3]<- x[,,3]*w[3] +  y[,,3]*(1-w[3])
	
	# CALCULATE AND SOLVE BELLMAN
	for(i in 1:3)
		{
		o[,i]<- r[,,i] + p[,,i] %*% v
		e[,i]<- p[,,i] %*% c(5,10,15)
		}
	# DETERMINE POLICY THAT MAXIMIZES UTILITY
	policy<- apply(o,1,which.max)
	# UPDATE V WITH NEW ACCRUED VALUES
	v<- apply(o,1,max)
	policy
	v

for(iter in 1:25)
	{
# Tf-1	
	p[,,1]<- x[,,1]*w[1] +  y[,,1]*(1-w[1])
	p[,,2]<- x[,,2]*w[2] +  y[,,2]*(1-w[2])
	p[,,3]<- x[,,3]*w[3] +  y[,,3]*(1-w[3])
		
	# CALCULATE AND SOLVE BELLMAN
	for(i in 1:3)
		{
		o[,i]<- r[,,i]+p[,,i]%*%v
		e[,i]<- p[,,i]%*%c(5,10,15)
		}
	# DETERMINE POLICY THAT MAXIMIZES UTILITY
	policy<- cbind(policy,apply(o,1,which.max))
	# UPDATE V WITH NEW ACCRUED VALUES
	v<- apply(o,1,max)
	print(policy)

	for(stat in 1:3)
		{
		fit1<- approxfun(c(5,10,15),x[stat,,policy[i,ncol(policy)]])
		fit2<- approxfun(c(5,10,15),x[stat,,policy[i,ncol(policy)]])
		w[stat]<- (fit1(e[stat,policy[i,ncol(policy)]])*w[stat])/((fit1(e[stat,policy[i,ncol(policy)]])*w[stat])+
			(fit2(e[stat,policy[i,ncol(policy)]])*(1-w[stat])))
		}

}





	
	