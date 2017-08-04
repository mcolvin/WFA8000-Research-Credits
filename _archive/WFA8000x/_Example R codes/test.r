
M1<- function(x,val,h)
	{#Logistic
	yy<- x[val]+(0.4*x[val]*(600-x[val])/600)-x[val]*x[h]
	return(rpois(1,yy))
	}
M2<- function(x, val,h)
	{
	yy<-x[val]+ (0.4*x[val]*((600-x[val])/600))^1.2-x[val]*x[h]
	return(rpois(1,yy))
	}


vals<- expand.grid(xx=c(100,300,400),h=c(0.1,0.2,0.3))
vals<- vals[rep(c(1:9),10000),]
vals$y_m1<-apply(vals,1,M1,val="xx", h="h") 
vals$y_m1<- ifelse(vals$y_m1> 400, 400, vals$y_m1)
vals$y_m2<-apply(vals,1,M2,val="xx", h="h") 
vals$y_m2<- ifelse(vals$y_m2> 400, 400, vals$y_m2)
vals[vals>=400]<- 400
vals$bin1<- cut(vals$y_m1, breaks=c(0,100,300,400), labels=c(100,300,400))
vals$bin2<- cut(vals$y_m2, breaks=c(0,100,300,400), labels=c(100,300,400))

require(reshape2)
require(plyr)
vals$tmp<- 1


trans<- dcast(vals, xx+h~bin1, value.var='tmp', sum)
trans[,-c(1,2)]<-trans[,-c(1,2)]/apply(trans[,-c(1,2)],1,sum) 
P1<- array(0,dim=c(3,3,3))
P1[,,1]<-  as.matrix(trans[trans$h==0.1,-c(1,2)])
P1[,,2]<-  as.matrix(trans[trans$h==0.2,-c(1,2)])
P1[,,3]<-  as.matrix(trans[trans$h==0.3,-c(1,2)])

trans<- dcast(vals, xx+h~bin2, value.var='tmp', sum)
trans[,-c(1,2)]<-trans[,-c(1,2)]/apply(trans[,-c(1,2)],1,sum) 
P2<- array(0,dim=c(3,3,3))
P2[,,1]<-  as.matrix(trans[trans$h==0.1,-c(1,2)])
P2[,,2]<-  as.matrix(trans[trans$h==0.2,-c(1,2)])
P2[,,3]<-  as.matrix(trans[trans$h==0.3,-c(1,2)]) 

R<- outer(c(100,300,400),c(0.1,0.2,0.3), "*")

P<- P1*0.5 + P2*0.5


ini<-c(0,0,0)# RETURNS
out<- mdp_bellman_operator(P, R, 0.99999, ini)
for(i in 1:10){
out<- mdp_bellman_operator(P, R, 0.99999, out$V)
print(out)
}
mdp_policy_iteration(P, R, discount=.99999)

V <- mdp_eval_policy_iterative(P, R, discount=.99999 , c(2,3,3))

### TRY WITH UNCERT IN r
M1<- function(x,val,h,r)
	{#Logistic
	yy<- x[val]+(x[r]*x[val]*(600-x[val])/600)-x[val]*x[h]
	return(rpois(1,yy))
	}
M2<- function(x, val,h,r)
	{
	yy<-x[val]+ (x[r]*x[val]*((600-x[val])/600))^1.2-x[val]*x[h]
	return(rpois(1,yy))
	}

require(reshape2)
require(plyr)
vals<- expand.grid(xx=c(100,300,400),h=c(0.1,0.2,0.3))
vals<- vals[rep(c(1:9),10000),]
vals$r<- runif(nrow(vals), 0.2, 0.6)
vals$y_m1<-apply(vals,1,M1,val="xx", h="h",r='r') 
vals$y_m1<- ifelse(vals$y_m1> 400, 400, vals$y_m1)
vals$y_m2<-apply(vals,1,M2,val="xx", h="h",r='r') 
vals$y_m2<- ifelse(vals$y_m2> 400, 400, vals$y_m2)
vals[vals>=400]<- 400
vals$bin1<- cut(vals$y_m1, breaks=c(0,100,300,400), labels=c(100,300,400))
vals$bin2<- cut(vals$y_m2, breaks=c(0,100,300,400), labels=c(100,300,400))
vals$bin_r<- cut(vals$r, breaks=c(0.2,0.4,0.6), labels=c("0.2-0.4","0.4-0.6"))
vals$tmp<- 1



trans<- dcast(vals, xx+bin_r+h~bin1+bin_r, value.var='tmp', sum)
trans[,-c(1:3)]<-trans[,-c(1:3)]/apply(trans[,-c(1:3)],1,sum) 
P1<- array(0,dim=c(6,6,3))
P1[,,1]<-  as.matrix(trans[trans$h==0.1,-c(1:3)])
P1[,,2]<-  as.matrix(trans[trans$h==0.2,-c(1:3)])
P1[,,3]<-  as.matrix(trans[trans$h==0.3,-c(1:3)])

trans<- dcast(vals, xx+bin_r+h~bin2+bin_r, value.var='tmp', sum,drop=FALSE)
trans[,-c(1:3)]<-trans[,-c(1:3)]/apply(trans[,-c(1:3)],1,sum) 
P2<- array(0,dim=c(6,6,3))
P2[,,1]<-  as.matrix(trans[trans$h==0.1,-c(1:3)])
P2[,,2]<-  as.matrix(trans[trans$h==0.2,-c(1:3)])
P2[,,3]<-  as.matrix(trans[trans$h==0.3,-c(1:3)]) 

R<- outer(c(100,100,300,300,400,400),c(0.1,0.2,0.3), "*")



P<- P1*0.5 + P2*0.5



ini<-c(0,0,0,0,0,0)# RETURNS
out<- mdp_bellman_operator(P, R, 0.99999, ini)
out<- mdp_bellman_operator(P, R, 0.99999, out$V)
out<- mdp_bellman_operator(P, R, 0.99999, out$V)
out<- mdp_bellman_operator(P, R, 0.99999, out$V)
out<- mdp_bellman_operator(P, R, 0.99999, out$V)
out<- mdp_bellman_operator(P, R, 0.99999, out$V)
out<- mdp_bellman_operator(P, R, 0.99999, out$V)
out<- mdp_bellman_operator(P, R, 0.99999, out$V)
out<- mdp_bellman_operator(P, R, 0.99999, out$V)
out<- mdp_bellman_operator(P, R, 0.99999, out$V)
out<- mdp_bellman_operator(P, R, 0.99999, out$V)
out<- mdp_bellman_operator(P, R, 0.99999, out$V)
out<- mdp_bellman_operator(P, R, 0.99999, out$V)
out<- mdp_bellman_operator(P, R, 0.99999, out$V)
out<- mdp_bellman_operator(P, R, 0.99999, out$V)
out<- mdp_bellman_operator(P, R, 0.99999, out$V)
out<- mdp_bellman_operator(P, R, 0.99999, out$V)
out<- mdp_bellman_operator(P, R, 0.99999, out$V)
out<- mdp_bellman_operator(P, R, 0.99999, out$V)
out<- mdp_bellman_operator(P, R, 0.99999, out$V)
out<- mdp_bellman_operator(P, R, 0.99999, out$V)
out<- mdp_bellman_operator(P, R, 0.99999, out$V)
out<- mdp_bellman_operator(P, R, 0.99999, out$V)
out<- mdp_bellman_operator(P, R, 0.99999, out$V)
out<- mdp_bellman_operator(P, R, 0.99999, out$V)
out<- mdp_bellman_operator(P, R, 0.99999, out$V)
out<- mdp_bellman_operator(P, R, 0.99999, out$V)
out<- mdp_bellman_operator(P, R, 0.99999, out$V)
out<- mdp_bellman_operator(P, R, 0.99999, out$V)
out<- mdp_bellman_operator(P, R, 0.99999, out$V)
out<- mdp_bellman_operator(P, R, 0.99999, out$V)
out<- mdp_bellman_operator(P, R, 0.99999, out$V)
out<- mdp_bellman_operator(P, R, 0.99999, out$V)
out<- mdp_bellman_operator(P, R, 0.99999, out$V)
out<- mdp_bellman_operator(P, R, 0.99999, out$V)
out<- mdp_bellman_operator(P, R, 0.99999, out$V)
out<- mdp_bellman_operator(P, R, 0.99999, out$V)
out<- mdp_bellman_operator(P, R, 0.99999, out$V)
out<- mdp_bellman_operator(P, R, 0.99999, out$V)



mdp_policy_iteration(P=P, R=R, discount=.99999)

V <- mdp_eval_policy_iterative(P, R, discount=.99999 , c(2,3,3,2,3,3))


PR<- mdp_computePR(P, R)
 mdp_eval_policy_matrix(P, PR, discount=.99999,          policy=c(2,3,3,2,3,3))
