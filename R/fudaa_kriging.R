fudaa_kriging <- function(path, y.transect = 3, x.points = 10, y.points = 10, clustertype = 'FORK', ncores <- parallelly::availableCores() - 1){

  folders <- list.files(path, full.names = T)
  
  library(foreach)

  files <- list.files(folders[1], full.names = T, pattern = '.dat$')
  for (i in 2:length(folders)) files <- c(files, list.files(folders[i], full.names = T, pattern = '.dat$'))

  UseCores <- parallelly::availableCores() - 1
  cl <- parallel::makeCluster(UseCores, type = clustertype)
  doParallel::registerDoParallel(cl)
  
  
  results <- foreach::foreach(i2=seq_along(files)) %dopar% {
    
    cat(i2)
    data <- read.csv2(files[i2], sep = '', header = F)[1:4]
    for (i1 in seq(ncol(data))) data[,i1] <- as.numeric(data[,i1])
    colnames(data) <- c('x', 'y', 'vx', 'vy')
    data$velo <- round(sqrt(data$vx^2 + data$vy^2),3)
    
    size.x <- c(min(data$x), (max(data$x) - min(data$x)))
    size.y <- c(min(data$y), (max(data$y) - min(data$y)))
    range.x <- size.x[2] / (x.points - 1)
    range.y <- size.y[2] / (y.points - 1)
    
    seq_x <- seq(size.x[1], size.x[2] + size.x[1], by=range.x)
    seq_y <- seq(size.y[1], size.y[2] + size.y[1], by=range.y)
    
    meuse_grid <- expand.grid(x=seq_x, y=seq_y)
    
    min(meuse_grid$x)
    max(meuse_grid$x)
    min(meuse_grid$y)
    max(meuse_grid$y)
    
    gitter <- sp::SpatialPointsDataFrame(meuse_grid, data=data.frame(vals=rep(NA, nrow(meuse_grid))))
    punkte <- sp::SpatialPointsDataFrame(data[,1:2], data=data.frame(vals=data$vy))
    transect <- unique(meuse_grid[,2])[y.transect]
    
    if (all(data$vx == 0)){
      pred <- data.frame(var1.pred=rep(0, nrow(data)))
      
    }else{
      krig <- automap::autoKrige(vals ~ 1,input_data = punkte,new_data=gitter)
      pred <- data.frame(krig$krige_output)
      
    }
    
    pred <- matrix(pred$var1.pred, ncol = y.points, byrow = F)
    mean.velocity <- round(mean(pred[,y.transect], na.rm = T), 3)
    
    mean.velocity
  }
  
  parallel::stopCluster(cl)
  return(results)
}
