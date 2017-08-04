
y_out<- c()
for(i in 1:2000)
	{	
	y<- rnorm(1,100,15)
	K<- runif(1, 300,400)
	r<- runif(1, 0.2,0.4)
	for(j in 2:10)
		{
		y<- c(y,(y[j-1]+r*y[j-1]*((K-y[j-1])/K)))
		}
	y_out<-cbind(y_out,y)
	}

par(mfrow=c(1,2))
matplot(y_out,type='l') 
	
matplot(y_out, type='n')
trans_grey<- rgb(120,120,120,alpha=10,maxColorValue=255)# make grey color with some transparency
for(i in 1:ncol(y_out)){points(y_out[,i],col=trans_grey,type='l')}




