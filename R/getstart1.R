get.start1 <- function(sig, window = 55, quant = 0.9, squared = 3){
  sig <- as.numeric(sig)^squared
  sig <- (sig - min(sig)) / max(sig - min(sig))
  diff <- matrix()
  for (i in 1:(length(sig) - window)){
    data <- sig[i:(i + window - 1)]
    
    wind.median <- median(data)
    wind.mean <- mean(data)
    diff <- c(diff, wind.mean - wind.median)
  }
  diff <- diff[-1]
  diff[diff < 0] <- 0
  diff[diff < quantile(diff, quant)] <- 0
  diff[diff > 0] <- 1
  diff <- data.frame(diff=diff)
  x_pos <- grep(1, diff$diff)[1] + (window + 1) - 1
  
  return(x_pos)
}