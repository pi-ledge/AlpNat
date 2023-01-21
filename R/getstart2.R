get.start2 <- function(sig, window = 55, threshold = 2, squared = 3){
  sig <- as.numeric(sig)^squared
  sig <- (sig - min(sig)) / max(sig - min(sig))
  i <- 1
  data <- sig[i:(i + window - 1)]
  while (sig[i + window] < (threshold * max(data))){
    i <- i + 1
    data <- sig[i:(i + window - 1)]
  }
  return(i + window - 1)
}