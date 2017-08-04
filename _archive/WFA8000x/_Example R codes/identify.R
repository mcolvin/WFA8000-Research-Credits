
?locator
locator(2)

z<- runif(100)
x<- 1:10
dat<- expand.grid(x=x,y=x)
dat$z<- z

plot(y~x,dat)

image(x,x,z)

outer(
identify(x=dat$x,y=dat$y, labels=dat$z)