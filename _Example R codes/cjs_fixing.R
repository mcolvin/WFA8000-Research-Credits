## FIXING PARAMETERS USING RMARK

require(RMark)


Nfish=700
S<-rep(0.6,7)
p<- rep(0.8,7)
p[7]<-1

# simulate some covariates 
# [note there is no effect of these covariates on anything...
# just to demo how to handle covariates in data processing]
sex=sample(c("male","female"),Nfish,replace=TRUE)
fl<- runif(Nfish, 100,400) # SIZE AT TAGGING


# SET UP MATRIX FOR SURVIVAL
Z<- matrix(0,nrow=Nfish,ncol=8)

# SIMULATE TRUE SURVIVAL
Z[,1]<-1 # marking occasion at smolt trap
for(i in 2:ncol(Z))
	{
	Z[,i]<- rbinom(Nfish,1,Z[,i-1]*S[1])# simulate true	survival
	}
ZZ<- Z
# SIMULATE DETECTION GIVEN FISH IS AVAILABLE (I.E., ALIVE) TO DETECT
for(i in 2:ncol(ZZ))
	{
	ZZ[,i]<- rbinom(Nfish,1,Z[,i]*p[1])# simulate detection, given fish survived to be detected
	}
## end simulation of known data


# make some capture histories
# and bundle up all the data
ch<- data.frame(ch=apply(ZZ,1,paste,collapse=""),sex=sex,fl=fl)
ch$ch<- as.character(ch$ch)# make sure ch is not a factor or Rmark=GRUMPY

# PROCESS THE DATA FOR MARK [SETS UP 
dat_proc<- process.data(ch,model="CJS", groups="sex")# this processes the data for use by Program Mark

# MAKE THE DESIGN DATA FILE
dd<-make.design.data(dat_proc) # this makes a design matrix, needed to fix parameters

# how to fix parameters
# need to get right row index from dd
p_fixed_indx<-c(as.numeric(row.names(dd$p[(dd$p$time ==8 ),]))) # fix detection at time '8' to be 1
# vector of values to fix
p_fixed_val<- rep(1,length(p_fixed_indx)) # fix time 8 to be 1 for detection probability

# run the cjs model fit by Program Mark via Rmark
# throw a sham covariate at Phi(sex) and p(fl)
fit<-mark(
	data=dat_proc,
	ddl=dd,
	model.parameters=list(
		Phi = list(formula="~ sex"),
		p = list(formula= "~ fl", fixed=list(index=p_fixed_indx, 
			value = p_fixed_val))),
	brief=TRUE) 
summary(fit, show.fixed=TRUE)

# FIT THE MODEL 
# Phi(.) and p(.)
fit<-mark(
	data= dat_proc,
	ddl=dd,
	model.parameters=list(
		Phi   = list(formula="~ 1"),
		p   = list(formula= "~1", fixed=list(index=p_fixed_indx, 
			value = p_fixed_val))),
	brief=TRUE) 
summary(fit, show.fixed=TRUE)
