moving.3averager <- function(x){
  result <- matrix()
  for (i in 1:(length(x) - 2)) result <- rbind(result, mean(x[i:(i + 2)]))
  result[1] <- sum(x[1], x[2]) / 3
  result <- rbind(result, sum(x[length(x) - 1], x[length(x)]) / 3)
  return(result)
}