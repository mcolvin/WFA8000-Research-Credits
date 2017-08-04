	require(coda)
	require(rlecuyer)
	require(snowfall)
	require(R2WinBUGS)
	require(reshape2)
	require(plyr)
	
	setwd("...")
	
	# A SHAM MODEL TO FIT
	# MAKE 10 COPIES OF THIS NAMED M_1.bug ... M_10.bug
	for(i in 1:10)
	{
	model<- function ()
		{ 
		for(i in 1:300)
			{
			Y[i]~dnorm(mu,precision)
			}
		# PRIORS
		mu~dnorm(0,0.0001)
		precision~dgamma(0.01,0.01) 
		}
	filename <- paste(getwd(),"/M_",i,".bug", sep="")
	write.model(model, filename)
	}
	
	ncpus=4 # number of cores to use
	fn<- dir() # read in the files in the working directory
	fn<- fn[grep(".bug",fn)] # grab the *.bug
	
	# this is key, need to make a matrix where the columns indexes
	# the core number.  This basically sets up which model is being
	# fit by which core.  The NAs filling in at the end will throw an 
	# error in the multithreading, but they occur after everything is done
	# so, no big deal.
	fn<- matrix(c(fn, rep(NA, length(fn) %% ncpus)),ncol=ncpus)
	
	
	# inits
	inits<- function()
		{
		list(mu=runif(1,0,30), precision=1/runif(1,0.01,20))		
		}
	dat<- list(Y= rnorm(300,5,10))
	# parms to monitor
	parms<- c("mu","precision")
	
	
	# a function to run the analysis
	# node = the core to run on 
	run_mc<- function(i,node)
			{
			filename<- paste(getwd(),fn[i,node],sep="/")# get the right *.bug model to fit
			model<- unlist(strsplit(fn[i,node],"\\."))[1] # extract the model name for saving the output
			out <- bugs(
				data=dat,
				inits=inits,
				parameters=parms, 
				model = filename,
				working.directory=outfile_mc[node],
				clearWD=TRUE,
				n.chains = 3,
				n.iter = 20000,
				n.burnin = 6000,
				debug=FALSE,
				bugs.directory=bugsdirr[node])
			save(out, file=paste(model,".Rdata",sep=""))# save the fitted model output as  *.Rdata file for loading once the anlaysis is done	
			}
	
	# this is key: a vector of 4 (equal to the number of cores) locations of winbugs
	bugsdirr<- c("C:/Users/colvinm/Documents/WinBUGS14 - 1","C:/Users/colvinm/Documents/WinBUGS14 - 2",
		"C:/Users/colvinm/Documents/WinBUGS14 - 3","C:/Users/colvinm/Documents/WinBUGS14 - 4")
	
	# this is key as well, there needs to be 4 (equal to the number of cores) folders to serve as working directories 
	# to prevent any overwriting
	outfile_mc<- c("C:/Users/colvinm/Desktop/winbugs_multicore/out1","C:/Users/colvinm/Desktop/winbugs_multicore/out2",
		"C:/Users/colvinm/Desktop/winbugs_multicore/out3","C:/Users/colvinm/Desktop/winbugs_multicore/out4")

		
	# here it is	
	# USING SNOWFALL
	sfInit(parallel=T, cpus=ncpus)
	sfExportAll()
	sfLibrary(R2WinBUGS)
	sfClusterSetupRNG()
	# RUN THE FUNCTION run_mc on 4 cores
	sfLapply(1:ncpus, function(jj)# jj indexes the cores
		{
		for(i in 1:nrow(fn)) # loop over the models allocated to each core in fn
			{
			run_mc(i = i, node=jj)
			}
		})
		sfStop()
		# NOTE THERE WILL BE AN ERROR WHEN THE FUNCTION HITS THE NAs IN fn
