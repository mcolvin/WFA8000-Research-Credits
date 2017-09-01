

N<- 10000
S<- 0.4
maxAge<- 7

for(i in 2:maxAge){N<- c(N,rbinom(1,N[i-1],S))}
age<-c(1:maxAge)


t0<- 0
Linf<-30
k<- 0.5
a<-0.03	
b<- 3.1


out<-data.frame()
for(i in 1:100)
{
dat<-data.frame(a=rep(age,N))
dat$L<- rlnorm(length(dat$a),log(vbgf(Linf,t0,k,dat$a)),0.15)
dat$W<- rlnorm(length(dat$a),log(a*dat$L^b),0.1)
dat$lL<-log(dat$L)
dat$lW<-log(dat$W)
dat$L_bin<-as.factor(floor(dat$L))
indx<-sample(1:nrow(dat),5000,replace=FALSE)
samp<-dat[indx,]

samp<-samp[order(samp$L_bin),]
yy<-unique(samp$L_bin)
samp$order<-unlist(sapply(yy,function(x)
    {
    pp<-samp[samp$L_bin==x,]
    xx<-c(1:nrow(pp))
    xx<-sample(order,nrow(pp),replace=FALSE)
    return(xx)
    }))
 
sampW<- subset(samp,order<=5)   
top<-table(dat$L_bin)/nrow(dat)
bot<-table(sampW$L_bin)/nrow(sampW)
rw<- data.frame(L_bin=levels(dat$L_bin),
    rw= as.numeric(top/bot))
rw$rw<- ifelse(is.na(rw$rw),1, rw$rw)
rw$rw<- ifelse(rw$rw==Inf,0, rw$rw)
sampW<-merge(sampW,rw,by="L_bin")


lmu<-lm(lW~lL,sampW)   
lmw<-lm(lW~lL,sampW,weights=sampW$rw)   
app<-data.frame(a_u=exp(coef(lmu)[1]))
app$a_w<-exp(coef(lmw)[1])
app$b_u<-coef(lmu)[2]
app$b_w<-coef(lmw)[2]
app$a_u_se<- summary(lmu)$coefficients[1,2]
app$a_w_se<- summary(lmw)$coefficients[1,2]   
app$b_u_se<- summary(lmu)$coefficients[2,2]  
app$b_w_se<- summary(lmw)$coefficients[2,2]   
out<-rbind(out,app)
}


brks<- seq(0,0.05,by=0.001)
hist(out$a_u, breaks=brks, col=rgb(1,0,0,0.5),xlim=c(0,0.1),ylim=c(0,25))
hist(out$a_w, breaks=brks, col=rgb(0,0,1,0.5), add=T)
box()
abline(v=a)


brks<- seq(2.8,3.3,by=0.02)
hist(out$b_u, breaks=brks, col=rgb(1,0,0,0.5),xlim=c(2.75,3.25),ylim=c(0,25))
hist(out$b_w, breaks=brks, col=rgb(0,0,1,0.5), add=T)
box()
abline(v=b)

mean(out$a_u)
mean(out$a_w)
mean(out$b_u)
mean(out$b_w)



brks<- seq(0,0.1,by=0.005)
hist(out$b_u_se, breaks=brks, col=rgb(1,0,0,0.5),xlim=c(0,0.3),ylim=c(0,25))
hist(out$b_u_se, breaks=brks, col=rgb(0,0,1,0.5), add=T)
box()
abline(v=b)


brks<- seq(0,0.2,by=0.005)
hist(out$a_u_se, breaks=brks, col=rgb(1,0,0,0.5),xlim=c(0,0.3),ylim=c(0,25))
hist(out$a_u_se, breaks=brks, col=rgb(0,0,1,0.5), add=T)
box()
