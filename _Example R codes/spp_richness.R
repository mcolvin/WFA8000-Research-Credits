
n<- 5
p<- 0.5
spp1<- rbinom(n,1,0.4)
spp1<- rbinom(n,1,spp1*p)

spp2<- rbinom(n,1,0.4)
spp2<- rbinom(n,1,spp2*p)

spp3<- rbinom(n,1,0.6)
spp3<- rbinom(n,1,spp3*p)

spp4<- rbinom(n,1,0.8)
spp4<- rbinom(n,1,spp4*p)

spp5<- rbinom(n,1,0.9)
spp5<- rbinom(n,1,spp3*p)


x<-t(cbind(spp1, spp2, spp3, spp4, spp4, spp5))

x

ch<- apply(x,1, paste, collapse="")



