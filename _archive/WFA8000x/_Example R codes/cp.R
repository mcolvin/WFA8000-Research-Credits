

deployments<- 8
# CATCHABILITY
q<- runif(deployments, 0.00005, 0.00015)
# EFFORT
f<- runif(deployments, 80,120)
# CAPTURE PROBABILITY
Pi<- q*f # DEPLOYMENT LEVEL
P<- sum(Pi) # OVERALL CAPTURE PROBABILITY

# CALCULATE CONDTITIONAL PROBABILITIES
# NEEDED TO GENERATE CATCH FOR EACH DEPLOYMENT
# CONDITIONAL THAT YOU CANNOT CATCH THE SAME 
# FISH ON MORE THAN 1 GEAR.
condP<- Pi
for(i in 2:deployments)
    {
    condP[i]<- Pi[i]/prod(1-condP[1:i])
    }


## SIMULATE CATCH PROCESS AS WOUDL BE IMPLENTMED
## FOR CAPTURE RECAPTURE   

# number of critters
N<- 1000 

N1<-c() # CATCHABILITY
N2<-c() # CAUGHT IN ALL DEPLOYMENTS
N3<-c() # DEPLOYMENT LEVEL CAPTURE OF INDIVUALS

for(i in 1:10000)
    {
    # CATCH FOR ALL DEPLOYMENTS
    C_i<- rpois(deployments,N*q*f) # DEPLOYMENT CATCH
    N1<- c(N1,sum(C_i)) # BEND CATCH
    # CATCH FOR ALL DEPLOYMENTS
    C_ind<- rbinom(N,1,P)    
    N2<- c(N2,sum(C_ind))
    
    }

# SIMULTE DEPLOYMENT LEVEL CATCH
for(i in 1:10000)
    {
    C_d<- matrix(0,N,deployments)
    C_d[,1]<- rbinom(N,1,condP[1])
    for(i in 2:deployments)
        {
        C_d[,i]<- rbinom(N,1, (1-rowSums(C_d[,1:i]))*condP[i])
        }
    N3<-c(N3,sum(C_d))
    }

    
# same
boxplot(cbind(N1,N2,N3))


