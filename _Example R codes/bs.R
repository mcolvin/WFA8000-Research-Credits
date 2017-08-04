

deployments<- 8
# CATCHABILITY
q<- runif(deployments, 0.00005, 0.00015)
# EFFORT
f<- runif(deployments, 80,120)
# CAPTURE PROBABILITY
Pi<- q*f
P<- sum(Pi)

# CALCULATE CONDTITIONAL PROBABILITIES
condP<- Pi
for(i in 2:deployments)
    {
    condP[i]<- Pi[2]/prod(1-Pi[1:i])
    condP[3]<- Pi[3]/prod(1-Pi[1:2])
    }


# number of critters
N<- 1000 

N1<-c() # CATCHABILITY
N2<-c() # 
N3<-c() # INVIDUAL CAPTURE

for(i in 1:10000)
    {
    C_i<- rpois(deployments,N*q*f) # DEPLOYMENT CATCH
    N1<- c(N1,sum(C_i)) # BEND CATCH

    C_ind<- rbinom(N,1,P)    
    N2<- c(N2,sum(C_ind))
    
    }

for(i in 1:10000)
    {
    C_d<- matrix(0,N,deployments)
    C_d[,1]<- rbinom(N,1,Pi[1])
    for(i in 2:deployments)
        {
        C_d[,i]<- rbinom(N,1, (1-rowSums(C_d[,1:i]))*Pi[i])
        }
    N3<-c(N3,sum(C_d))
    }
# same
boxplot(cbind(N1,N2))


N<- 500000
p1<-0.2
p2<-0.5
A<-rbinom(N,1,p1)
B<-rbinom(N,1,p2*(1-A))
out<-data.frame(A=A,B=B)

xx<-table(apply(out,1,paste,collapse=""))
xx/N



# Pr A given No B: P(A|~B) 
0.2/(0.2+(0.8*0.5))

mean(out[which(out$B==0),]$A)
xx<-table(apply(out[which(out$B==0),],1,paste,collapse=""))
xx/N
xx/(sum(xx))
0.2/(0.4+0.2)


# Pr no A given No B: P(~A|~B) 
(0.5*p1)/(1-p2)
1-mean(out[which(out$B==0),]$A)



# Pr B given no A: Pr(B|~A)
(0.5*0.8)/((0.5*0.8)+(0.5*0.8))
p2
mean(out[which(out$A==0),]$B)

# Pr no B given no A: Pr(~B|~A)
(1-p2)
1-mean(out[which(out$A==0),]$B)

# Pr A given B: Pr(A|B)
mean(out[which(out$B==1),]$A)

# Pr A given B: Pr(B|A)
mean(out[which(out$A==1),]$B)

# Pr A and B  : Pr(A & B)
length(which(out$B==1 & out$A==1))/N

# Pr no A and no B: Pr(~A & ~B)
(1-p1)*(1-p2)
length(which(out$B==0 & out$A==0))/N






