## Example from Conroy and Peterson 2013 SDP example in Appendix E
## Stochastic dynamic programming requires this library
library(MDPtoolbox)
# With a decision specific transition matrices matrix


#### weight of model 1
mod1.wt<- 0.5

Model1 <-Model2 <- array(0, c(3,3,3))

## decision 1 harvest = 0.1
Model1[,,1] <- matrix(c(0.2, 0.5, 0.3, 0.2,.3,0.5,0.1,0.3,0.6), 3, 3, byrow=TRUE)
Model2[,,1] <- matrix(c(0.3, 0.5, 0.2, 0.3,0.3,0.4,0.2,0.3,0.5), 3, 3, byrow=TRUE)
## model averaged transition matrix


## decision 1 harvest = 0.2
Model1[,,2]<- matrix(c(0.5,0.3,0.2,0.2,0.5,0.3,0.2,0.4,0.4), 3, 3, byrow=TRUE)
Model2[,,2]<- matrix(c(0.7,0.3,0.0,0.4,0.5,0.1,0.5,0.4,0.1), 3, 3, byrow=TRUE)


## decision 1 harvest = 0.3
Model1[,,3]<- matrix(c(0.7,0.3,0.0,0.6,0.3,0.1,0.3,0.5,0.2), 3, 3, byrow=TRUE)
Model2[,,3]<- matrix(c(0.9,0.1,0.0,0.7,0.3,0.0,0.4,0.6,0.0), 3, 3, byrow=TRUE)


##Reward matrix
R <- matrix(c(0.5,1.0,1.5,1,2,3,1.5,3.0,4.5), 3, 3, byrow=TRUE)




harv.decis<- function(cur.popn,dec,R,Model1,Model2,mod1.wt){
	# FUNCTION TO DETERMINE THE OPTIMAL HARVEST RATE?
	P <- array(0, c(3,3,3))
	ret = 0
	popn = cur.popn 
	mod.wt <- mod1.wt

	for(yr in 1:length(dec))
		{
		yr <- yr+1
		#### annual return
		yr.ret<- popn*R[,dec[yr]]

		## model averaging transition matrices
		P[,,dec[yr]] <- Model1[,,dec[yr]]*mod.wt + Model2[,,dec[yr]]*(1-mod.wt)
		 
		### predictions under alternative models
		### for each harvest rate
		prb1=t(Model1[,,dec[yr]]) %*% popn # wtf does this work?
		prb2=t(Model2[,,dec[yr]]) %*% popn # wtf does this work?
		 
		## predicted change to population 
		popn= t(P[,,dec[yr]]) %*% popn
		 
		est.pop = popn [which.max(popn)]

		## Bayes rule for updating weights
		mod.wt<- (dmultinom(x=est.pop, prob =prb1)*mod.wt)/(dmultinom(x=est.pop, prob =prb1)*mod.wt + 
			dmultinom(x=est.pop, prob =prb2)*(1-mod.wt))
		 
		# cumulative return 
		ret <- ret + sum(yr.ret)

		}
	return(ret)
}

### create matrix with every value of current population
sys.state<- matrix(c(1,0,0,0,1,0,0,0,1),byrow = T,ncol = 3)

library(rgenoud)
tada <- c()
#### create wrapper for evaluating over all values of cur.popn and information state
for(mod1.wt in seq(0.1, 0.9, by= 0.1))
	{
	#create variable to put optimal solutions
	opt<-c()
	for(j in 1:3)# system states
		{
		harv.optim<- function(parms)
			{
			stuff<-harv.decis(dec = parms,
				cur.popn = sys.state[j,],
				R = R,
				Model1 = Model1,
				Model2 = Model2,
				mod1.wt= 0.1)#mod1.wt)
			return(stuff)
			}
  
  #### initial value vector
  inits<- rep(1,20)
  
  ## get minimum and maximum values
  min.max <- cbind(rep(1,20),rep(3,20))
  
  fit<- genoud(
    fn = harv.optim, 
    nvars = 20, 
    max = TRUE, 
    pop.size = 2500,
    starting.values=inits,
    data.type.int = TRUE,
    Domains = min.max,
    boundary.enforcement = 2)
  
  ## extract optimal policy and cumulative return
  opt<-rbind(opt,c(j,as.numeric(fit$par[1])))
}

tada = rbind(tada,cbind(opt,mod1.wt))
}

colnames(tada)= c("popn.state","opt.decis","Model_wt")
tada
