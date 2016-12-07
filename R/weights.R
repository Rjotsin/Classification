# WEIGHTS FUNCTIONS
# input: probability data frame, probability data frame, probability data frame, test data frame, start, end, increment
# output: list of LL, x, y, z

wts <- function(df1, df2, df3, train.df, strt, end, inc){
  
  temp <- train.df[train.df$obs %in% df1$obs,]
  temp <- temp[order(temp$obs),]
  
  df1 <- df1[order(df1$obs),]
  df2 <- df2[order(df2$obs),]
  df3 <- df3[order(df3$obs),]
  
  df1$obs <- NULL
  df2$obs <- NULL
  df3$obs <- NULL
  
  x <- c()
  y <- c()
  z <- c()
  p <- c()
  
  for(i in seq(strt, end, by=inc)){
    for(j in seq(strt, end, by=inc)){
      for(m in seq(strt, end, by=inc)){
        x <- c(x,i)
        y <- c(y,j)
        z <- c(z,m)
        k <- ((i*df1)+(j*df2)+(m*df3))/(i+j+m)
        l <- MultiLogLoss(k,temp$interesting) 
        p <- c(p,l)
      }
    }
  }

  min_index <- which.min(p)
  new_x <- x[min_index]
  new_y <- y[min_index]
  new_z <- z[min_index]
  
  return(c(min(p),new_x,new_y,new_z))
  
}

weights1 <- wts(rf_results[[1]][[1]], gbm_results[[1]][[1]], mn_results[[1]][[1]], original_train_data, 0.1, 1.1, 0.1)
weights2 <- wts(rf_results[[1]][[2]], gbm_results[[1]][[2]], mn_results[[1]][[2]], original_train_data, 0.1, 1.1, 0.1)
weights3 <- wts(rf_results[[1]][[3]], gbm_results[[1]][[3]], mn_results[[1]][[3]], original_train_data, 0.1, 1.1, 0.1)
weights4 <- wts(rf_results[[1]][[4]], gbm_results[[1]][[4]], mn_results[[1]][[4]], original_train_data, 0.1, 1.1, 0.1)
