
survival<- 0.56
daily_survival<- 0.56^(1/365)
pop1<- 15000
marked1<- 200
days_between<- 30
pop2<- round(pop1*daily_survival^days_between,0)
marked1<- round(marked1*daily_survival^days_between,0)
marked2<-marked1+100

M<- 200+100

days_between<- 100
pop3<- round(pop1*daily_survival^days_between,0)
marked3<- round(marked2*daily_survival^days_between,0)
p<- 0.05

# NUMBER OF FISH CAPTURED
U<- round((pop3-marked3)*p,0)
m<- round((marked3)*p,0)
((M+1)*(U+m+1))/(m+1)





U<-12000
M<-300

u<- round((U*(daily_survival^190))*0.05)
m<- round((M*(daily_survival^190))*0.05)

(M+1)*(u+m+1)/(m+1)



The default is to use all ages in the age vector. 
This is only appropriate if the age and catch vectors contain only the ages and catches on the descending limb of the catch curve. 
Use ages2use to isolate only the catch and ages on the descending limb.

The Chapman-Robson method provides an estimate of the annual survival rate, with the annual mortality rate (A) determined by 1-S. 
The instantaneous mortality rate is often computed as -log(S). However, 
Hoenig et al. (1983) showed that this produced a biased (over)estimate of Z and provided a correction. 
The correction is applied by setting zmethod="Hoenigetal". 
Smith et al. (2012) showed that the Hoenig et al. method should be corrected for a variance inflation factor. 
This correction is applied by setting zmethod="Smithetal" (which is the default behavior). 
Choose zmethod="original" to use the original estimates for Z and it's SE as provided by Chapman and Robson.