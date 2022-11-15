patmotJSON <- function(path){
  
  require(rjson)
  
  json <- fromJSON(file = path)
  
  motparam_fd <- c(NA,json$motparam_fd)[1:2300]
  
  motparam_fd_outlier <- (motparam_fd > (1.5 * IQR(motparam_fd, 
                                                   na.rm = TRUE) + quantile(motparam_fd, 
                                                                            na.rm = TRUE)[4]))
  raw_dvars <- c(NA,json$raw_dvars)[1:2300]
  raw_dvars_outlier <- (raw_dvars > (1.5 * IQR(raw_dvars, 
                                               na.rm = TRUE) + quantile(raw_dvars, 
                                                                        na.rm = TRUE)[4]))
  sub <- json$subid
  ses <- json$sesid
  
  ttime = 1:2300
  
  out <- list("motparam_fd" = motparam_fd, 
              "motparam_fd_outlier" = motparam_fd_outlier, 
              "sub" = sub, 
              "ses" = ses, 
              "raw_dvars" = raw_dvars, 
              "raw_dvars_outlier" = raw_dvars_outlier, 
              "ttime" = ttime
  )
  
  return(out)
}

get_ses <- function(path){
  
  out <- dir(path = path, 
             full.names = FALSE)
  
  return(out)
}

whichTwin <- function(tbl){
  
  tbl$twin[which(tbl$motparam_fd_outlier_count == min(tbl$motparam_fd_outlier_count))]
}