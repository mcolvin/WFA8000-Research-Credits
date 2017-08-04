
# http://www.statalgo.com/

# solve fib by recursion
fib1 <- function(n) {
  print(paste("fib1(", n, ")", sep=""))
  if(n == 0) return(0)
  else if(n == 1) return(1)
  else {
    return(fib1(n-1) + fib1(n-2))
  }
}
# by dynamics programming (
fib2 <- function(n) {
  fibs <- c(0, 1)
  if(n > 1) {
    for (i in 3:n) {
      fibs[i] = fibs[i - 1] + fibs[i - 2]
    }
  }
  return(fibs[n])
}


fib1(20)
fib2(20)
# knapsack problem

knapsack <- function(w, v, W) {
	A <- matrix(rep(0, (W + 1) * (length(w) + 1)), ncol=W+1)
	for (j in 1:length(w)) 
		{
		for (Y in 1:W) 
			{
			if (w[j] > Y) {A[j+1, Y+1] = A[j, Y+1]}else
			 {A[j+1, Y+1] = max( A[j, Y+1], v[j] + A[j, Y- w[j]+1])}
			}
		}

  return(A)
}

optimal.weights <- function(w, W, A) {

  amount = rep(0, length(w))
  a = A[nrow(A), ncol(A)]
  j = length(w)
  Y = W
   
  while(a > 0) 
	{
    while(A[j+1,Y+1] == a) 
		{
		j = j - 1
		}
    j = j + 1
    amount[j] = 1
    Y = Y - w[j]
    j = j - 1;
    a = A[j+1,Y+1];
	}

  return(amount)
}


w = c(1, 1, 1, 1, 2, 2, 3)
v = c(1, 1, 2, 3, 1, 3, 5)
W = 7

A <- knapsack(w, v, W)

best.value <- A[nrow(A), ncol(A)]
weights <- optimal.weights(w, W, A)