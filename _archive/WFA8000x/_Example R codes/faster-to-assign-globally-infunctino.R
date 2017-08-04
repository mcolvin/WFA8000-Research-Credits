

# is is faster to assing values globally rather
# than copying withing a function
x<- list(x=runif(10),y=c(3,4,5))

yy<- function(inn)
	{
	x<-inn
	x$x<- c(1:7)
	return(x)
	}


yyy<- function(...)
	{
	x$x<<- c(1:7)
	}
yyyy<- function(...)
	{
	x$x<- c(1:7)
	return(x)
	}	
xx<-yyyy()

library(microbenchmark)
library(ggplot2)
mbm = microbenchmark::microbenchmark(
	base = x$x<- c(1:7),
	new = xx<-yy(x),
	newer = yyy(),
	newest = xx<-yyyy())
ggplot2::autoplot(mbm)
	
	
	exp(1)
	2.718282^1
	
mbm = microbenchmark::microbenchmark(
	base = exp(1),
	new = 2.718282^1)
ggplot2::autoplot(mbm)

