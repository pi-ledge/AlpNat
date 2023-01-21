plt.3signals <- function(signal1, signal2, signal3, name1='', name2='', name3='', col1='red', col2='green', col3='blue', xlim=NULL, main='maximale Fließhöhe', xlab='ID', ylab='Amplitude'){
  plot(c(1, length(signal1)), c(min(signal1, signal2, signal3), max(signal1, signal2, signal3)), col="white", xlab = xlab, ylab = ylab, main = main, xlim=xlim)
  lines(signal1, type = "l", col = col1, )
  lines(signal2, type = "l", col = col2)
  lines(signal3, type = "l", col = col3)
  legend("topright", c(name1, name2, name3), col = c(col1, col2, col3), lty = 1 , cex = 0.5)
}
