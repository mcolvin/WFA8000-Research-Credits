require(lme4)
require(nlme)

ngroups=50
int<- rnorm(ngroups,0.5,2)
b<- rnorm(ngroups,8,0.95)
dat<- data.frame(int = rep(int,30), b= rep(b, 30), group=rep(c(1:ngroups),30))
dat$group<- as.factor(dat$group)
dat$x<- runif(ngroups*30,10,50)
dat$obs<- dat$int+ dat$b*dat$x
dat$obs<- rnorm(ngroups*30,dat$obs,1)

xyplot(obs~x,dat,group=group)


fit<- lmer(obs~x + (1|group), dat)
fit<- lmer(obs~x + (x|group), dat)
summary(fit)
fit<- lme(obs~x, dat, random= ~ x|group, control=list(maxIter=100))
summary(fit)


fit<- lme(obs~x, dat, random= ~ 1+x|group)
summary(fit)


sqrt(sum(mean(int)-int)^2)))

res<- mean(int)-int
sum(res^2)

nclass = 20
x<- rnorm(nclass)*2
x<- rnorm(nclass)
hist(x)
hist(x*2)


?lmeControl
nclass = 20

# And thirty students per classroom

nstud = 30

# Let's imagine that we have a classroom effect that varies randomly and is uncorrelated with the student level effect.

class.effect = rnorm(nclass)*2

# Imagine also we have a unique outcome per student.

# We have nclass*nstud number of students in total.

student.effect = rnorm(nstud*nclass)*3

# Now we have all the data we need in order to generate our analysis.
data.set = data.frame(class.id = rep(1:nclass, each=nstud),
                  class.effect = rep(class.effect, each=nstud),
                  student.id = 1:(nstud*nclass),
                  student.effect = student.effect)

data.set$outcomes = data.set$class.effect + data.set$student.effect

head(data.set)
# Looking good.  Now let's load our HLM package.

require(nlme)

# For quick reference I used:
# https://txcdk.unt.edu/iralab/sites/default/files/HLM_R_Handout.pdf

lme(outcomes ~ 1, random = ~ 1 | class.id, data=data.set)

# We can see that lme is pretty good at estimating a standard deviation of the level 2 effect at around 2 and a level 3 effect around 3.

int<-c()
b<- c()
require(lme4)
for(i in 1:100)
{
# GLMER
ngroups=50
nobs=50
int<- rnorm(ngroups,-0.8,1.5)
x<- runif(ngroups*nobs,1,50)
b<- 0.085
dat<- data.frame(int=sort(rep(int,nobs)),id=sort(rep(c(1:ngroups),nobs)))
dat$x<-x
dat$b<- b
dat$y<- dat$int+dat$x*dat$b
dat$p<- exp(dat$y)/(1+exp(dat$y))

dat$psm<- rbinom(nrow(dat),15,dat$p)
dat$spn<- 15-dat$psm
dat$psm_rate<- dat$psm/15


fit<- glmer(cbind(psm,spn)~ x + (1 | id),family = binomial(link = "logit"), data = dat) 
int<-c(int,fixef(fit)[1])
b<-c(b,fixef(fit)[2])
}

hist(int,breaks=seq(-4,4,0.2))
abline(v=-0.8)
mean(int)

hist(b,breaks=seq(0,0.1,0.001))
abline(v=0.085)
mean(int)

dat$pred<- fitted(fit)
xyplot(p~x,dat,group=id)
plot(p~pred,dat,ylim=c(0,1),xlim=c(0,1))
abline(0,1)

summary(fit)
fixef(fit)
