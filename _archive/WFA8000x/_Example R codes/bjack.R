 
SimpleSim <- function(..., fun, pairwise=F) {
  # SimpleSim allows for the calling of a function varying 
  # multiple parameters entered as vectors. In pairwise form 
  # it acts much like apply. In non-paiwise form it makes a 
  # combination of each possible parameter mix
  # in a manner identical to block of nested loops.
 
  returner <- NULL
  L        <- list(...)
  # Construct a vector that holds the lengths of each object
  vlength <- unlist(lapply(L, length)) 
  npar    <- length(vlength)
  CL      <- lapply(L, "[", 1) # Current list is equal to the first element
 
 # Pairwise looping
 if (pairwise) {
  # If pairwise is selected than all elements greater than 1 must be equal.
  # Checks if all of the elements of a vector are equal
  if (!(function(x) all(x[1]==x))(vlength[vlength>1])) {
   print(unlist(lapply(L, length)))
   stop("Pairwise: all input vectors must be of equal length", call. =F)
  }
  for (i in 1:max(vlength)) { # Loop through calling the function
   CL[vlength>1]  <- lapply(L, "[", i)[vlength>1] # Current list
   returner <- rbind(returner,c(do.call(fun, CL),pars="", CL))
  }
 } # End Pairwise
 
 # Non-pairwise looping
 if (!pairwise) {
  ncomb <- prod(vlength) # Calculate the number of combinations
  print(paste(ncomb, "combinations to loop through"))
  comb <- matrix(NA, nrow=prod(vlength), ncol=npar+1)
  comb[,1] <- 1:prod(vlength) # Create an index value
  comb <- as.data.frame(comb) # Converto to data.frame
  colnames(comb) <- c("ID", names(CL))
  for (i in (npar:1)) { # Construct a matrix of parameter combinations
   comb[,i+1] <- L[[i]] # Replace one column with values
   comb<-comb[order(comb[,(i+1)]),] # Reorder rows
  }
  comb<-comb[order(comb[,1]),]
  for (i in 1:ncomb) {
   for (ii in 1:npar) CL[ii] <- comb[i,ii+1]
   returner <- rbind(returner,c(do.call(fun, CL),pars="", CL))
  }
 } # End Non-Pairwise
 
 return(returner)
 
} # END FUNCTION DEFINITION
 
# Let's first define a simple function for demonstration
minmax <- function(...) c(min=min(...),max=max(...))
 
# Pairwise acts similar to that of a multidimensional apply across columns 
SimpleSim(a=1:20,b=-1:-20,c=21:40, pairwise=T, fun="minmax")
# The first set of columns are those of returns from the function "fun" called.
# The second set divided by "par" are the parameters fed into the function.
SimpleSim(a=1:20,b=-1:-20,c=10, pairwise=T, fun="minmax")
 
# Non-pairwise creates combinations of parameter sets.
# This form is much more resource demanding.
SimpleSim(a=1:5,b=-1:-5,c=1:2, pairwise=F, fun="minmax")
 
# Let's try something a little more interesting.
 
# Let's simulate a game of black jack strategies assuming no card counting is possible.
blackjack <- function(points=18, points.h=NULL, points.ace=NULL, 
                      cards=10, cards.h=NULL, cards.ace=NULL,
                      sims=100, cutoff=10) {
  # This function simulates a blackjack table in which the player
  # has a strategy of standing (not asking for any more cards)
  # once he has either recieved a specific number of points or 
  # a specific number of cards.  This function repeates itself sims # of times.
  # This function allows for up to three different strategies to be played.
  # 1. If the dealer's hole card is less than the cuttoff
  # 2. If the dealer's hole card is greater than or equal to the cuttoff
  # 3. If the dealer's hole card is an ace
  # In order to use 3 level strategies input parameters as .h and .ace
 
  # It returns # of wins, # of losses, # of pushes (both player and dealer gets 21)
  # and the number of blackjacks.
 
  # This simulation assumes the number of decks used is large thus
  # the game is like drawing with replacement.
 
  if (is.null(points.h))   points.h   <- points
  if (is.null(points.ace)) points.ace <- points.h
  if (is.null(cards.h))    cards.h    <- cards
  if (is.null(cards.ace))  cards.ace  <- cards.h
 
  bdeck <- c(11,2:9,10,10,10,10) # 11 is the ace
 
  bdresult <- c(ppoints=NULL, pcards=NULL, dpoints=NULL, dcards=NULL)
 
  for (s in 1:sims) {
   dhand <- sample(bdeck,1) # First draw the deal's revealed card
   phand <- sample(bdeck,2, replace=T)
 
   # Specify target's based on dealer's card
   if (dhand<cutoff) {
     pcuttoff <- points
     ccuttoff <- cards
   }
   if (dhand>=cutoff) {
     pcuttoff <- points.h
     ccuttoff <- cards.h
   }
   if (dhand==11) {
     pcuttoff <- points.ace
     ccuttoff <- cards.ace
   }
 
   # player draws until getting above points or card count
   while ((sum(phand)<pcuttoff)&(length(phand)<ccuttoff)){
     phand <- c(phand, sample(bdeck,1))
       # If player goes over then player may change aces to 1s 
       if (sum(phand)>21) phand[phand==11] <- 1
   }
 
   # Dealer must always hit 17 so hand is predetermined
   while (sum(dhand)<17) {
     dhand <- c(dhand, sample(bdeck,1))
     # If dealer goes over then dearler may change aces to 1s
     if (sum(dhand)>21) dhand[dhand==11] <- 1
   }
   bdresult <- rbind(bdresult, 
        c(ppoints=sum(phand), pcards=length(phand), 
          dpoints=sum(dhand), dcards=length(dhand)))
  }
 
  # Calculate the times that the player wins, pushes (ties), and loses
  pbj <- (bdresult[,1]==21) & (bdresult[,2]==2)
  dbj <- (bdresult[,3]==21) & (bdresult[,4]==2)
  pwins <- ((bdresult[,1] >  bdresult[,3]) & (bdresult[,1] <  22)) | (pbj & !dbj)
  push  <- (bdresult[,1] == bdresult[,3]) | (pbj & dbj)
  dwins <- !(pwins | push)
 
  # Specify the return.
  c(odds=sum(pwins)/sum(dwins), 
    pwins=sum(pwins), 
    dwins=sum(dwins), 
    push=sum(push), 
    pcards=mean(bdresult[,2]), 
    dcards=mean(bdresult[,4]),
    pblackjack=sum(pbj),
    dblackjack=sum(dbj))
}
 
blackjack(points=18, sims=4000)
# We can see unsurprisingly, that the player is not doing well.
 
blackjack(points=18, points.h=19, sims=4000)
# We can see that by adopting a more aggressive strategy for when
# the dealer has a 10 point card or higher, we can do slightly better.
# But overall, the dealer is still winning about 3x more than us.
 
# We could search through different parameter combinations manually to
# find the best option.  Or we could use our new command SimpleSim!
 
MCresults <- SimpleSim(fun=blackjack, points=15:21, points.h=18:21, 
                       points.ace=18:21, cutoff=9:10, cards=10, sims=100)
 
# Let's now order our results from the most promising.
MCresults[order(-unlist(MCresults[,1])),]
 
# By the simulation it looks like we have as high as a 50% ratio of loses to wins.
# Which means for every win there are 2 loses.
# However, I don't trust it since we only drew 100 simulations.
# In addition, this is the best random draw from all 224 combinations which each
# have different probabilities.
 
# Let's do the same simulation but with 2000 draws per.
# This might take a little while.
MCresults <- SimpleSim(fun=blackjack, points=15:21, points.h=18:21, 
                       points.ace=18:21, cutoff=9:10, cards=10, sims=5000)
 
# Let's now order our results from the most promising.
MCresults[order(-unlist(MCresults[,1])),]
hist(unlist(MCresults[,1]), main="Across all combinations\nN(Win)/N(Loss)", 
     xlab = "Ratio", ylab = "Frequency")

# The best case scenario 38% win to loss ratio appears around were we started, 
# playing to hit 18 always and doing almost as well when the dealer is high
# (having a 10 or ace) then playing for 19.
 
# Overall, the odds are not in our favor.  For every win we expect 1/.38 (2.63) loses.