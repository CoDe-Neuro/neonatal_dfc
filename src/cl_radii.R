cl_radii <- function(ss_x, ss_y, zz_x, zz_y, rr = 1){
  
  radius_x = rr*cos(2*pi/50*(0:(50-1)))
  radius_y = rr*sin(2*pi/50*(0:(50-1)))
  
  zz_dx = radius_x + zz_x
  zz_dy = radius_y + zz_y
  
  dists <- c()
  
  euclid <- function(v1, v2) sqrt(sum((v1 - v2) ^ 2))
  
  for (i in seq(length(zz_dx))){
    
    dists[i] <- euclid(c(ss_x,ss_y), c(zz_dx[i],zz_dy[i]))
    
  }
  
  return(list('dx' = zz_dx[which(dists == min(dists))[1]], 
              'dy' = zz_dy[which(dists == min(dists))[1]]))
  
}