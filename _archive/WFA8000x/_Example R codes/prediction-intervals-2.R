

<!-- how r does prediction interval se
http://stackoverflow.com/questions/38109501/how-does-predict-lm-compute-confidence-interval-and-prediction-interval

betas<- coef(fit)
mm<- model.matrix(as.formula("~habitat_area+group_size+habitat_area:group_size"),preddat)	
y_hat<- mm%*%betas	
V<-vcov(fit) # variance,covariance matrix
se<- sqrt(rowSums((mm %*% V) * mm))
preddat$uci <- y_hat + 2* sqrt(se^2 + summary(fit)$sigma^2) # variances add, ses do not
preddat$lci <- y_hat - 2* sqrt(se^2 + summary(fit)$sigma^2) # variances add, ses do not
-->
