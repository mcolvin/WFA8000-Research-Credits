


presPlot()
par(mar=c(4,5,1,1))
aa<- seq(0,100,1)
xx<- pnorm(aa,50,15)
plot(aa[-1],diff(xx), ylab="",
    xlab="Vegetation density",las=1,type='l',
        lwd=3)
mtext(side=2,"Density",outer=TRUE,line=0,cex=1.5)
xx<- rnorm(5000000,50,5)
plot(density(xx))


library(MASS)
mu <- c(0,0)                         # Mean
Sigma <- matrix(c(1, .5, .5, 1), 2)  # Covariance matrix
bivn <- mvrnorm(5000, mu = mu, Sigma = Sigma ) 
# Classic Bivariate Normal Diagram
library(ellipse)
rho <- cor(bivn)
y_on_x <- lm(bivn[,2] ~ bivn[,1])    # Regression Y ~ X
x_on_y <- lm(bivn[,1] ~ bivn[,2])    # Regression X ~ Y
plot_legend <- c("99% CI green", "95% CI red","90% CI blue",
                 "Y on X black", "X on Y brown")
 
plot(bivn, xlab = "X", ylab = "Y",
     col = "dark blue",
     main = "Bivariate Normal with Confidence Intervals")
lines(ellipse(rho), col="red")       # ellipse() from ellipse package
lines(ellipse(rho, level = .99), col="green")
lines(ellipse(rho, level = .90), col="blue")
abline(y_on_x)
abline(x_on_y, col="brown")
legend(3,1,legend=plot_legend,cex = .5, bty = "n")

# Three dimensional surface
# Basic perspective plot
# Calculate kernel density estimate
bivn.kde <- kde2d(bivn[,1], bivn[,2], n = 50)  
persp(bivn.kde, phi = 45, theta = 30, shade = .1, border = NA) # from base graphics package
 