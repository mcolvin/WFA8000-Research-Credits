


X<- c(1:10)
Ylo<- runif(10, 0,1)
Yhi<- runif(10,2,3)
trans_black<- rgb(0,0,0,alpha=40,maxColorValue=255)
plot(Yhi~X,type='n',ylim=c(0,3))
polygon(x=c(X,rev(X)),y=c(Ylo,rev(Yhi)),col=trans_black)
mn<-apply(cbind(Ylo,Yhi),1,mean)
points(mn~X,type='l',col="black") 
trans_red<- rgb(228,16,16,alpha=40,maxColorValue=255)

Ylo<- runif(10, 0,1)
Yhi<- runif(10,2,3)
polygon(x=c(X,rev(X)),y=c(Ylo,rev(Yhi)),col=trans_red)
mn<-apply(cbind(Ylo,Yhi),1,mean)
points(mn~X,type='l',col="red") 