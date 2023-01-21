get.end <- function(sig, start){
  fh.norm <- mean(sig[1:start])
  
  test <- sig > fh.norm
  test <- grep(TRUE, test)
  ende_oben_o <- test[length(test)]
}