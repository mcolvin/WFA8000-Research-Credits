library(microbenchmark)
# MAPPLY VERSUS SAPPLY
# VECTOR FUNCION WITH INDICES FOR "WHICH"

funm<-function(a=0.0001,b=3,L,M,O)
    {
    W<- (a*L^b)*M*O
    }
funs<-function(x,a=0.0001,b=3)
    {
    W<- (a*L[,x]^b)*M[,x]*O[,x]
    }  
N=100000
R=50  
L<-matrix(runif(N*R,200,1200),nrow=N,ncol=R)
M<- matrix(round(runif(N*R),0),nrow=N,ncol=R)
O<- matrix(round(runif(N*R),0),nrow=N,ncol=R)

	indx<- lapply(1:R,
		function(x){which(O[,x]==1 & M[,x]==1)}) # ROW INDICES
	tmp<- unlist(lapply(1:R,
		function(x) rep(x,length(indx[[x]])))) # COLUMN INDICES
	indx<- cbind(unlist(indx),tmp)#row,column


msl<- microbenchmark(
    map=mapply(funm, a=0.0001, b=3, L,M,O),
    sap=sapply(1:R,funs,a=0.0001,b=3),
    lap=lapply(1:R,funs,a=0.0001,b=3),
    indx=funm(a=0.0001,b=3,L=L[indx],M=M[indx],O=O[indx]),
    t2= (0.0001*L^3)*M*O
    )
ggplot2::autoplot(msl)


# CAN YOU LAPPLY A MARKOV PROCESS?
## NO
XX<-matrix(0,nrow=10,ncol=50)
XX[,1]<-2
yy<-lapply(1:50,function(x){XX+2})
do.call("cbind",yy)

