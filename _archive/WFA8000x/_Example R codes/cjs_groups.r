
group_n<- c(30,40,50,33)
groups<- length(group_n)
release_occ<- c(1,2,3,4)
n_occ<- 8
inds<- sum(group_n)
# initialize true survival matrix Z
Z<- matrix(0,sum(group_n),n_occ)
releases<- cbind(c(1:inds),sort(rep(release_occ,group_n)))
Z[releases]<- 1

# covariate data 
group<- sort(rep(c(1:groups),group_n))
sex<- sample(c("m","f"),sum(group_n),prob=c(0.5,0.5),replace=TRUE)
dat<- data.frame(inds=c(1:inds), group=as.factor(group),sex=sex)

# MAKE UP SOME SURVIVAL PROBABILITY DATA 
S_dat<- dat
S_dat$T<- 1
for(i in 2:(n_occ-1))
	{
	dat$T<- i
	S_dat<- rbind(S_dat,dat)
	}
	
S_dat$T<- as.factor(S_dat$T)	
X_surv<- model.matrix(~group, S_dat)
S_parms<- c(0.1, 0.05,-0.05,0.08) # assumes constant S over time but varies by group, on logit scale
S_dat$S<- plogis(X_surv %*% S_parms)# actual survivals
library(reshape2)
S_matrix<- dcast(S_dat, inds~T,value.var="S",mean)[,-1]

# SIMULATE TRUE SURVIVAL STATE
for(i in 1:nrow(Z))
	{
	for(j in releases[i,2]:(n_occ-1))
		{
		Z[i,j+1]<- rbinom(1,1,Z[i,j]*S_matrix[i,j])
		}
	}
	
# MAKE UP SOME DETECTION PROBABILITY DATA 
p_dat<- dat
p_dat$T<- 1
for(i in 2:n_occ)
	{
	dat$T<- i
	p_dat<- rbind(p_dat,dat)
	}
p_dat$T<- as.factor(p_dat$T)	

X_p<- model.matrix(~group + T, p_dat)
p_parms<- c(0.1, # intercept assumes constant p varies by time and by group but no interaction, on logit scale
	0.05,-0.05,0.08, # group effect
	0.02,-0.02,0.04,0.08,-0.04,-0.06,-0.1) # time effect
p_dat$p<- plogis(X_p %*% p_parms)# actual survivals
p_matrix<- dcast(p_dat, inds~T,value.var="p",mean)[,-1]

D<- Z
# SIMULATE TRUE SURVIVAL STATE
for(i in 1:nrow(Z))
	{
	for(j in (releases[i,2]+1):n_occ)
		{
		D[i,j]<- rbinom(1,1,Z[i,j]*p_matrix[i,j])
		}
	}
	
ch<- data.frame(ch=apply(D,1, paste,collapse=""),group=groups)


unif_proc<- process.data(ch,model="CJS", groups="group")
unif_ddl=make.design.data(unif_proc)

fit<-mark(unif_proc,unif_ddl,model.parameters=list(
			S   = list(formula="~ group"),
			p   = list(formula= "~group+Time"),
			brief=TRUE) 
summary(fit, show.fixed=TRUE)


