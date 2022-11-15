doEuclid <- function(df,no){
  
  df <- df[,no]
  d <- matrix(0,dim(df)[1])
  d[1] = NA
  for(i in seq(2,dim(df)[1])){
    
    d[i] <- dist(rbind(aa[i-1,],aa[i,]))
    
  }
  
  return(d)
}