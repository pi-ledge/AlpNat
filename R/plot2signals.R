plt.2signals <- function(signal1, signal2, name1='', name2=''){
  plot(c(1, length(signal1)), c(min(signal1, signal2), max(signal1, signal2)), col="white", xlab = "timestep", ylab = "amplitude", main = "flow height\nmax")
  lines(signal1, type = "l", col = "red", )
  lines(signal2, type = "l", col = "blue")
  legend("topright", c("unten", "oben"), col = c("red", "blue"), lty = 1 , cex = 0.8)
}