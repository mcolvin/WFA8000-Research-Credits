

xx<- matrix(0,100,50)

lapply(2:50,function(x)
    {xx[,x]<<- xx[,x-1]+2})
    