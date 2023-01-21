stagecurve <- function(signal, start, end, plot=F, xlim=NULL, lcol='red', regr.col='black', main=NULL, xlab=NULL, ylab=NULL){
  ganglinie <- data.frame(x=start:end)
  ganglinie$ln_x <- log(ganglinie$x)
  ganglinie$RA_max <- signal[start:end] / 100
  
  ganglinie <- ganglinie[-1,]
  
  reg <- lm(RA_max ~ ln_x, data = ganglinie)
  k <- reg$coefficients[2]
  d <- reg$coefficients[1]
  
  ganglinie$trend <- k * log(ganglinie$x) + d
  
  RA_max.anfang <- ganglinie$RA_max[1]
  RA_max.ende <- ganglinie$RA_max[nrow(ganglinie)]
  
  RA_trend_ln.anfang <- ganglinie$trend[1]
  RA_trend_ln.ende <- ganglinie$trend[nrow(ganglinie)]
  
  ganglinie$anpassung <- (RA_max.anfang - RA_max.ende) * (ganglinie$trend - RA_trend_ln.ende) / (RA_trend_ln.anfang - RA_trend_ln.ende) + RA_max.ende
  RA_anpassung.anfang <- ganglinie$anpassung[1]
  RA_anpassung.ende <- ganglinie$anpassung[nrow(ganglinie)]
  
  ganglinie$skaliert <- ganglinie$RA_max - ganglinie$anpassung
  ganglinie$skaliert[(ganglinie$RA_max - ganglinie$anpassung) < 0] <- 0
  
  if (plot){
    plot(ganglinie$RA_max, type = 'l', xlim = xlim, col = lcol, main=main, xlab=xlab, ylab=ylab)
    lines(ganglinie$trend, type = 'l', col = regr.col, lwd = 2)
    lines(ganglinie$anpassung, type = 'l', col = scales::alpha(regr.col, 0.3), lwd = 2)
  }
  
  return(ganglinie)
}