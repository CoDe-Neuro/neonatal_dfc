library(rjson)
library(dplyr)
library(tidyr)
library(tibble)
library(feather)
library(R.matlab)

source("patmotJSON.R")
source("outliers.R")

mat <- R.matlab::readMat("../data/2020_04_MASTER_connectomes90_AllFCrerun2_withSCifAvailable_shard4_584subjs.mat")
pma <- unlist(mat$pma)
sub <- unlist(mat$sub)
ses <- unlist(mat$ses)
rm(mat)

sub <- sub[pma >= 37]
ses <- ses[pma >= 37]

filt_dup <- rep(TRUE, length(sub))
filt_dup[410] <- FALSE

sub <- sub[filt_dup]
ses <- ses[filt_dup]


for (idx in 1:length(sub)){
  
  path <- paste("../data/FCqc/", 
                sub[idx], 
                "_", 
                ses[idx], 
                "_qc.json", 
                
                sep = "")
  
  json = patmotJSON(path)
  
  mot_df <- tibble::tibble("motparam_fd" = json$motparam_fd, 
                           "raw_dvars" = json$raw_dvars)
  
  mot_df <- mot_df %>% 
    dplyr::mutate(outliers = outliers(raw_dvars)) %>% 
    dplyr::mutate(nbours = neighbours(raw_dvars)) %>% 
    dplyr::mutate(anom = ifelse(nbours == 0, FALSE, TRUE)) %>% 
    dplyr::mutate(nbours = dplyr::recode(nbours, 
                           `0` = "Regular", 
                           `1` = "Neighbour", 
                           `2` = "Outlier"))
  
  feather::write_feather(mot_df, 
                path = paste("../data/FCqc/anom_", 
                             idx, 
                             ".feather",
                             sep = ""))
  
}
