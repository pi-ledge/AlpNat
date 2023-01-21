plt.3signals <- function(signal1, signal2, signal3, name1='', name2='', name3='', xlim=NULL){
  plot(c(1, length(signal1)), c(min(signal1, signal2, signal3), max(signal1, signal2, signal3)), col="white", xlab = "timestep", ylab = "amplitude", main = "flow height\nmax", xlim=xlim)
  lines(signal1, type = "l", col = "red", )
  lines(signal2, type = "l", col = "blue")
  lines(signal3, type = "l", col = "green")
  legend("topright", c("unten", "oben_o", "oben_u"), col = c("red", "blue", 'green'), lty = 1 , cex = 0.5)
}