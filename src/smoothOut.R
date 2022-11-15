smoothOut <- function(input){
  
  outliers <- 2*as.numeric(input)
  
  bef <- which(outliers == 2) - 1
  aft <- which(outliers == 2) + 1
  
  outliers[bef[outliers[bef] == 0]] <- 1
  outliers[aft[outliers[aft] == 0]] <- 1
  
  new_out <- outliers
}