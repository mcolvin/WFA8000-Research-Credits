library(adagio)


planks_we_have <- rep(96,100)
planks_we_want <- round(c(rep(15.5,16),rep(12.5,8),rep(14,8),rep(57,4))+0.5,0)
planks_we_want_actual<- c(rep(15.5,16),rep(12.5,8),rep(14,8),rep(57,4))
solution <- mknapsack(planks_we_want, planks_we_want + 1, planks_we_have)
solution$ksack
n_2x4x8<-max(solution$ksack)


x<-data.frame(cutt=planks_we_want,
	cuts=planks_we_want_actual,
	board=solution$ksack)
x[order(x$board),]

# 2x6x8
planks_we_have <- rep(96,100)
planks_we_want <- round(rep(76,8),0)
solution <- mknapsack(planks_we_want, planks_we_want + 1, planks_we_have)
solution$ksack
n_2x6x8<-max(solution$ksack)


cost<-n_2x4x8*3.12 + n_2x6x8*5.68
cost


planks_we_have <- rep(96,100)
planks_we_want <- round(c(rep(15.5,16),rep(12.5,8),rep(14,8),rep(57,4),rep(76,16))+0.5,0)
solution <- mknapsack(planks_we_want, planks_we_want + 1, planks_we_have)
solution$ksack
n_2x4x8<-max(solution$ksack)
cost<-n_2x4x8*3.12 
cost