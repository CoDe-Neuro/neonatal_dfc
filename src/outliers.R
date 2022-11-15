outliers <- function(vec, scale = 1.5) {
  
  vec > scale * IQR(vec) + quantile(vec)[4]
  
}


outlierLevel <- function(vec, lvl = c(0.15, 1.50, 15.00)){
  
  as.numeric(outliers(vec, scale = lvl[1])) + 
    as.numeric(outliers(vec, scale = lvl[2])) + 
    as.numeric(outliers(vec, scale = lvl[3]))
  
}

neighbours <- function(vec, 
                       span = 1, 
                       lvl = c(0.15, 1.50, 15.00)){
  
  outs <- as.numeric(outliers(vec, scale = lvl[2]))
  nbs <- numeric(length(vec))
  
  span_vec <- 1:span
  
  idx <- as.vector(outer(which(outs != 0), span_vec, FUN = "+"))
  
  nbs[idx] <- 1
  
  nbs[which(outs != 0)] <- 2
  nbs <- nbs[1:2280]
  nbs
  
}