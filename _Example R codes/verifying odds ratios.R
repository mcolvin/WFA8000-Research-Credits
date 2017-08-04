
n=10000
x<- runif(n, 0, 20)
b0<- -1
b1<- 0.123
y<- b0+b1*x
plot(x,y)
y<- exp(y)/(1+exp(y))
plot(x,y)
obs<- rbinom(n,1, y)
dat<- data.frame(x=x, obs=obs)
plot(obs~x,dat)

fit<- glm(obs~x,dat,family=binomial)
fit # the coefficents are the log odds
or<- exp(coef(fit))# convert to odds ratio
or[2] # says that for a 1 unit increase in x the it is 13% increase in the odds of success


## WHAT DOES THAT MEAN IN PRACTICE
x<- c(1:20)
p<- coef(fit)[1] + coef(fit)[2]*x # log odds 
plot(c(1:20),p) # log odds versus x
p<- exp(p)/(1+exp(p)) # convert to probability via logit function
plot(c(1:20),p) # bound between 0 and 1
# CALCULATE THE ODDS FOR EACH POINT FROM PROBABILITIES
odds<- p/(1-p)

# ODDS OF SUCCESS AT X = 5 IS
odds[5] # or 0.68 to one, not very good odds!
# ODDS OF SUCCESS AT X = 10 IS
odds[10] # or 1.2 to one, better more likley be successful

# ODDS OF SUCCESS AT X = 20 IS
odds[20] # or 4.3 to one, very likley be successful

# ODDS RATIO PROVIDES A WAY TO EVALUTE THE HOW A CHANGE WILL INCREASE OR 
# DECREASE YOUR ODDS OF SUCCESS
# THE ODDS FROM ABOVE
odds
# the ratio of the odds
odds[2]/odds[1]
odds[15]/odds[14]
odds[20]/odds[19]
# THE ABOVE ARE ALL THE SAME FOR A 1 UNIT INCREASE THE ODDS IS 1.13 TIMES THE PREVIOUS
# IF YOU EXPONATE THE COEFFICIENT FOR X FROM THE MODEL YOU GET THE SAME NUMBER!
exp(coef(fit))[2]

# SO IT IS A NICE WAY TO COMPARE THE EFFECT OF DIFFERENT PARAMETERS ON THE ODDS OF SUCCESS
# OR FAILURE.  