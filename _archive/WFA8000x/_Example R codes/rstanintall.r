

Sys.getenv('PATH')
Sys.getenv('R_USER')
system('g++ -v')


install.packages("inline", lib=paclib)
install.packages("Rcpp", lib=paclib)

library(inline) 
library(Rcpp)
src <- ' 
  std::vector<std::string> s; 
  s.push_back("hello");
  s.push_back("world");
  return Rcpp::wrap(s);
'
hellofun <- cxxfunction(body = src, includes = '', plugin = 'Rcpp', verbose = FALSE)
cat(hellofun(), '\n') 

remove.packages('rstan') ### if older version of rstan is installed
## add current repository of rstan
options(repos = c(getOption("repos"), rstan = "http://wiki.stan.googlecode.com/git/R"))
install.packages('rstan', type = 'source',lib=paclib)

library(inline) 
library(Rcpp)
library(rstan) 

schools_code <- '
  data {
    int<lower=0> J; // number of schools 
    real y[J]; // estimated treatment effects
    real<lower=0> sigma[J]; // s.e. of effect estimates 
  }
  parameters {
    real mu; 
    real<lower=0> tau;
    real eta[J];
  }
  transformed parameters {
    real theta[J];
    for (j in 1:J)
      theta[j] <- mu + tau * eta[j];
  }
  model {
    eta ~ normal(0, 1);
    y ~ normal(theta, sigma);
  }
'

schools_dat <- list(J = 8, 
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))

fit <- stan(model_code = schools_code, data = schools_dat, 
            iter = 1000, chains = 4)
			
			
			
stanmodelcode <- '
	data {
		int<lower=0> N;
		real y[N];
		} 

parameters {
	real mu;
	real<lower=0> std;
		} 

model {
  mu ~ normal(0, 10);
  std ~ uniform(0, 1000);
  y ~ normal(mu, std); 
} 
'
model_name <- "normal1"; 

rr <- stan_model(model_code = stanmodelcode, model_name = model_name, 
                 verbose = TRUE) 

y <- rnorm(20) 
mean(y) 
sd(y)
dat <- list(N = 20, y = y) 
f <- sampling(rr, data = dat, init = c(0,1), iter = 2012, sample_file = 'norm1.csv')

sampling(rr, data = dat, iter = 2012, chains = 1,
         init = list(list(mu = 2)), seed = 3, thin = 1, 
         sample_file = 'norm1.csv')

post <- read.csv(file = 'norm1.csv', header = TRUE, skip = 19, comment = '#') 
colMeans(post)