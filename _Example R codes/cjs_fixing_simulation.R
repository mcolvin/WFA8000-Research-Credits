	
require(RMark)

Nfish<-25
Z<- matrix(0,nrow=Nfish,ncol=8)

S1<-rep(0.6,7)
S2<-rep(0.4,7)
p1<- rep(0.8,7)
p2<- rep(0.6,7)

Z[,1]<-1
for(i in 2:ncol(Z))
	{
	Z1[,i]<- rbinom(Nfish,1,Z[,i-1]*S1[i])# true
	Z2[,i]<- rbinom(Nfish,1,Z[,i-1]*S2[i])# true	
	}



ZZ1<- Z1
ZZ2<- Z2
for(i in 2:ncol(ZZ1))
	{
	ZZ1[,i]<- rbinom(Nfish,1,Z1[,i]*p1[i])# true
	ZZ2[,i]<- rbinom(Nfish,1,Z2[,i]*p2[i])# true	
	}
## end simulation of known data

	
# make some capture histories
ch<- data.frame(ch=apply(ZZ,1,paste,collapse=""),count=1)
inp<- aggregate(count~ch,ch,sum)
inp$ch<- as.character(inp$ch)
names(inp)[2]<- "freq"
# PROCESS THE DATA FOR MARK
unif_proc<- process.data(inp,model="CJS")

# MAKE DESIGN MATRIX
unif_ddl=make.design.data(unif_proc)
fit<-mark(unif_proc,unif_ddl,model.parameters=list(
			Phi = list(formula="~1"),
			p   = list(formula= "~1")),
			brief=TRUE) 
summary(fit, show.fixed=TRUE)

# end


Nfish<- 700
Z<- matrix(0,nrow=Nfish,ncol=8)

S<-rep(0.6,7)
p<- rep(0.8,7)
p[7]<-1

Z[,1]<-1
for(i in 2:ncol(Z))
	{
	Z[,i]<- rbinom(Nfish,1,Z[,i-1]*S[1])# true	
	}
ZZ<- Z
for(i in 2:ncol(ZZ))
	{
	ZZ[,i]<- rbinom(Nfish,1,Z[,i]*p[1])# true	
	}
## end simulation of known data

	
# make some capture histories
ch<- data.frame(ch=apply(ZZ,1,paste,collapse=""),count=1)
inp<- aggregate(count~ch,ch,sum)
inp$ch<- as.character(inp$ch)
names(inp)[2]<- "freq"
# PROCESS THE DATA FOR MARK
unif_proc<- process.data(inp,model="CJS")

# FIX P FOR YEARS AND STATES WHERE THERE IS NO POTENTIAL FOR RECAPTURE
p_fixed_indx_unif<-c(as.numeric(row.names(unif_ddl$p[(unif_ddl$p$time ==8 ),])))
p_fixed_val_unif<- rep(1,length(p_fixed_indx_unif)) # fix time 8 to be 0 for detection probability
fit<-mark(unif_proc,unif_ddl,model.parameters=list(
			Phi   = list(formula="~1"),
			p   = list(formula= "~1", fixed=list(index=p_fixed_indx_unif, value = p_fixed_val_unif))),
			brief=TRUE) 
summary(fit, show.fixed=TRUE)


# FIX P FOR YEARS AND STATES WHERE THERE IS NO POTENTIAL FOR RECAPTURE
p_fixed_indx_unif<-c(as.numeric(row.names(unif_ddl$p[(unif_ddl$p$time ==8 ),])))
p_fixed_val_unif<- rep( 0.9 ,length(p_fixed_indx_unif)) # fix time 8 to be 0 for detection probability
fit<-mark(unif_proc,unif_ddl,model.parameters=list(
			Phi   = list(formula="~1"),
			p   = list(formula= "~1", fixed=list(index=p_fixed_indx_unif, value = p_fixed_val_unif))),
			brief=TRUE) 
summary(fit, show.fixed=TRUE)
# end




## now with sex as a covariate
S<-rep(0.6,7)
p<- rep(0.8,7)
p[7]<-1

Z[,1]<-1
for(i in 2:ncol(Z))
	{
	Z[,i]<- rbinom(Nfish,1,Z[,i-1]*S[1])# true	
	}
ZZ<- Z
for(i in 2:ncol(ZZ))
	{
	ZZ[,i]<- rbinom(Nfish,1,Z[,i]*p[1])# true	
	}
## end simulation of known data
sex=sample(c("male","female"),Nfish,replace=TRUE)
	
# make some capture histories
ch<- data.frame(ch=apply(ZZ,1,paste,collapse=""),sex=sex)
ch$ch<- as.character(ch$ch)

# PROCESS THE DATA FOR MARK
unif_proc<- process.data(ch,model="CJS", groups="sex")
unif_ddl=make.design.data(unif_proc)
# FIX P FOR YEARS AND STATES WHERE THERE IS NO POTENTIAL FOR RECAPTURE
p_fixed_indx_unif<-c(as.numeric(row.names(unif_ddl$p[(unif_ddl$p$time ==8 ),])))
p_fixed_val_unif<- rep(1,length(p_fixed_indx_unif)) # fix time 8 to be 0 for detection probability
fit<-mark(unif_proc,unif_ddl,model.parameters=list(
			Phi   = list(formula="~ sex"),
			p   = list(formula= "~1", fixed=list(index=p_fixed_indx_unif, value = p_fixed_val_unif))),
			brief=TRUE) 
summary(fit, show.fixed=TRUE)

# no effect of sex
fit<-mark(unif_proc,unif_ddl,model.parameters=list(
			Phi   = list(formula="~ 1"),
			p   = list(formula= "~1", fixed=list(index=p_fixed_indx_unif, value = p_fixed_val_unif))),
			brief=TRUE) 
summary(fit, show.fixed=TRUE)






# with sex and fork length
Nfish=700
S<-rep(0.6,7)
p<- rep(0.8,7)
p[7]<-1

# simulate some covariates [note there is no effect of these covariates on anything...]
sex=sample(c("male","female"),Nfish,replace=TRUE)
fl<- runif(Nfish, 100,400) # SIZE AT TAGGING

Z[,1]<-1 # marking occasion at smolt trap
for(i in 2:ncol(Z))
	{
	Z[,i]<- rbinom(Nfish,1,Z[,i-1]*S[1])# simulate true	survival
	}
ZZ<- Z
for(i in 2:ncol(ZZ))
	{
	ZZ[,i]<- rbinom(Nfish,1,Z[,i]*p[1])# simulate detection, given fish survived to be detected
	}
## end simulation of known data


# make some capture histories
# bundle up all the data
ch<- data.frame(ch=apply(ZZ,1,paste,collapse=""),sex=sex,fl=fl)
ch$ch<- as.character(ch$ch)# make sure ch is not a factor

# PROCESS THE DATA FOR MARK
unif_proc<- process.data(ch,model="CJS", groups="sex")# this processes the data for use by Program Mark
unif_ddl<-make.design.data(unif_proc) # this makes a design matrix, needed to fix parameters

# how to fix parameters
p_fixed_indx_unif<-c(as.numeric(row.names(unif_ddl$p[(unif_ddl$p$time ==8 ),]))) # fix detection at time '8' to be 1
p_fixed_val_unif<- rep(1,length(p_fixed_indx_unif)) # fix time 8 to be 1 for detection probability

# run the cjs model fit by Program Mark
fit<-mark(unif_proc,unif_ddl,model.parameters=list(
			Phi   = list(formula="~ sex"),
			p   = list(formula= "~ fl", fixed=list(index=p_fixed_indx_unif, value = p_fixed_val_unif))),
			brief=TRUE) 
summary(fit, show.fixed=TRUE)

# no effect of sex
fit<-mark(unif_proc,unif_ddl,model.parameters=list(
			Phi   = list(formula="~ 1"),
			p   = list(formula= "~1", fixed=list(index=p_fixed_indx_unif, value = p_fixed_val_unif))),
			brief=TRUE) 
summary(fit, show.fixed=TRUE)
