
# We want to be able to assign fish to bend, potential to 
# inform from passive receivers
# and within segment, active telemetry
### movement
rkm<- c(1,2,3,4,5,6,7,8,10,13,15)

#PAIRWISE DISTANCE MATRIX
dis<- -1*abs(outer(rkm,rkm,"-")  )  
ud<-dis
ud[upper.tri(dis)]<- 1
ud[lower.tri(dis)]<- -1
# EFFECT OF DISTANCE
beta2<- 0  # upstream/downstream
beta1<-1.5 # effect of distance
beta0<- 1
nyears=10
nfish=100
bend<-matrix(NA,nrow=nfish,ncol=nyears) 
bend[,1]<-sample(1:length(rkm),nfish,replace=T) 

for(yr in 2:nyears)
    {
    z<-matrix(NA,nrow=(nfish),ncol=ncol(dis))
    p<-matrix(NA,nrow=(nfish),ncol=ncol(dis))
    for(i in 1:nfish)
        {
        z[i,]<- exp(beta0+
            beta1*c(dis[bend[i,yr-1],]))
        z[i,bend[i,yr-1]]<- 1
        p[i,]<-z[i,]/sum(z[i,])
        }

    bend[,yr]<- unlist(lapply(1:nrow(p),function(x)
        {
        sample(1:length(rkm),1,prob=p[x,])
        }))    
    
    }
    
bend[13,]   
plot(c(p[13,])~rkm,type='b')    
abline(v=9)
    
    
  














  
    
# river plot for sankey
library(riverplot)

nodes<- data.frame(ID=c(0,1:length(rkm)),
    x=c(1,rep(2,length(rkm))),
    labels=NA)
    
    
edges<- expand.grid(N1=0,
    N2=1:length(rkm))
edges$Value=c(p[1,])
nodes = data.frame(ID = unique(c(edges$N1, edges$N2)), 
    stringsAsFactors = FALSE)
nodes$x = c(1,rep(2,length(rkm)))
nodes$y = c(6,rkm)


 p <- makeRiver(nodes, edges)
   my_style <- default.style()
   my_style$textcol <- "grey20"
   my_style$srt = 0
par(family = family)
   plot(p, default_style = my_style, plot_area = 0.9)

library(RColorBrewer)
palette = paste0(brewer.pal(3, "Set1"), "60")
styles = lapply(nodes$y, function(n) {
   list(col = palette[1], lty = 0, textcol = "black")
})
names(styles) = nodes$ID
library(riverplot)
rp <- list(nodes = nodes, edges = edges, styles = styles)

class(rp) <- c(class(rp), "riverplot")
plot(rp, plot_area = 0.95, yscale=0.06)

 
 
 
r <- makeRiver( nodes, edges, node_xpos= c( 1,1,2 ),
node_labels= c( A= "Node A", B= "Node B", C= "Node C" ),
node_styles= list( A= list( col= "yellow" )) )

plot( r )    


# equivalent form:
nodes <- data.frame( ID= LETTERS[1:3],
x= c( 1, 2, 2 ),
col= c( "yellow", NA, NA ),
labels= c( "Node A", "Node B", "Node C" ),
stringsAsFactors= FALSE )
r <- makeRiver( nodes, edges )
plot( r )