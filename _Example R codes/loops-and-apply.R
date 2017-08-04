

# apply, sapply, for loop demo
n=100000
dat<- data.frame(x1=runif(n),x2=runif(n),x3=runif(n))
fun<- function(data,v1,v2)
	{
	data[v1]+data[v2]^3
	}
	
times<-list()
## for loop
times$out1<-system.time({
for(i in 1:n)
	{
	dat$t1[i]<- dat$x1[i]+dat$x2[i]^3
	}})


## sapply
times$out2<-system.time({
dat$t2<-sapply(1:n,function(x){dat$x1[x]+dat$x2[x]^3}) })


## apply
times$out3<-system.time({
dat$t3<- apply(dat,1,fun,v1="x1",v2="x2")})


## base
times$out4<-system.time({
dat$t4<- dat$x1+dat$x2^3})

times

c(out1, out2, out3, out4)


system.time({ #your function here })


#your function here
