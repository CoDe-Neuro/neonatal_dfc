whichTwin <- function(tbl){
  
  tbl$twin[which(tbl$motparam_fd_outlier_count == min(tbl$motparam_fd_outlier_count))]
}

whichSes <- function(tbl){
  
  tbl <- tbl %>% 
    dplyr::mutate(sunnivaed = FALSE)
  
  tbl$sunnivaed <- tbl$sunnivaed[which(tbl$motparam_fd_outlier_count == min(tbl$motparam_fd_outlier_count))] <- TRUE
  tbl
  
}