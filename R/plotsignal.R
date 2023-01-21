plot.signal <- function(signal, start, end, xlim=NULL, lcol='red', pcol='black', vcol='black', main=NULL, xlab=NULL, ylab=NULL){
  
  if (is.null(xlim)) xlim <- c(start, end)
  
  plot(signal, type='l', xlim = xlim, col = lcol, main=main, xlab=xlab, ylab=ylab)
  points(c(start, end), c(signal[start], signal[end]), col = pcol)
  abline(v=c(c(start, end)), col = vcol)
}