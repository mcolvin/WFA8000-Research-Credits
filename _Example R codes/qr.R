
# EXAMPLE OF QUANTILE REGRESSION

library(quantreg) 
data(engel) 
y<-cbind(engel[,2]) 
x<-cbind(rep(1,length(engel[,1])),engel[,1]) 
x1<-engel[,1]
bhat.ls<-solve(t(x)%*%x)%*%t(x)%*%y 

# QUANTILES 
quant=.9 
  
fr.1=function(B) 
	{ # objective function
	pred<- x%*%B 
	res=y-pred
	val=abs(ifelse(res>0,
		quant*res,# slope of quantile if residual is > 0
		(1-quant)*res))# # 1- slope of quantile if residual is < 0
	return(sum(val))
	} 


result=optim(bhat.ls,fr.1,method="BFGS") 
result$par 

rq(y~x1,tau=quant) 


y_hat<- x %*% result$par
res<- y-y_hat
val=abs(ifelse(res>0,
		quant*res,# slope of quantile if residual is > 0
		(1-quant)*res))# # 1- slope of quantile if residual is < 0
plot(res,val)






