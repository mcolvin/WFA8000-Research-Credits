# shape1 is successes
# shape 2 is failures

a=20
b=10

x<- rbeta(n=1000, shape1=a, shape2=b)
hist(x)
mean(x)
var(x)
a/(a+b)
estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}


estBetaParams(mean(x),var(x))
mu<- 0.66
var_mu<- 0.007
a <- ((1 - mu) / var_mu - 1 / mu) * mu ^ 2
b <- alpha * (1 / mu - 1)
x<- rbeta(n=1, shape1=a, shape2=b)



x<-c(0:10)
out<-dbinom(x,10,0.5)
plot(x,out);abline(v=5)