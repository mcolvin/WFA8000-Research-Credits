library(adagio)
# mknapsack calling signature is: mknapsack(values, weights, capacities)
planks_we_want <- c(19, 19, 19, 19, 79, 79, 79, 103, 103, 103, 135, 135, 135, 135, 160)
planks_we_have <- c(120, 137, 220, 420, 480)
solution <- mknapsack(planks_we_want, planks_we_want + 1, planks_we_have)
# Above I added +1 cm  to each length to compensate for the loss when sawing. solution$ksack

# Now pretty printing what to cut so that we don't make mistakes...
assignment <- data.frame(
  cut_this = planks_we_have[solution$ksack],
  into_this = planks_we_want)
t(assignment[order(assignment[,1]), ])