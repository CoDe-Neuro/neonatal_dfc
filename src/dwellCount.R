dwellCount <- function(x){
  
  f <- c(which(diff(x) != 0),length(x))
  
  y2 <- c(f[1],diff(f))
  y1 <- x[f]
  
  y <- list("cluster" = y1, 
            "dwell" = y2)
  
  return(y)
}