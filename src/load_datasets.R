# LOADING LEIDA DATA

df <- readr::read_csv(file = './data/df_LEiDa.csv') %>% 
  dplyr::select(-...1) %>% 
  dplyr::mutate(ttime = ttime + 1) %>% 
  dplyr::mutate(patTags = stringr::str_replace_all(patTags, "b'sub-", "'"), 
                patTags = stringr::str_remove_all(patTags, "'"), 
                ses = stringr::str_replace_all(ses, "b'ses-", "'"), 
                ses = stringr::str_remove_all(ses, "'")) %>% 
  dplyr::rename(sub = patTags)
  

# LOADING PCA DATA

df_c <- readr::read_csv(file = './data/df_clus_LEiDa_dim.csv') %>% 
  dplyr::select(-...1) %>% 
  dplyr::mutate(ttime = ttime + 1) %>% 
  dplyr::mutate(sub = stringr::str_replace_all(sub, "b'sub-", "'"), 
                sub = stringr::str_remove_all(sub, "'"), 
                ses = stringr::str_replace_all(ses, "b'ses-", "'"), 
                ses = stringr::str_remove_all(ses, "'")) %>% 
  dplyr::mutate(sub = stringr::str_remove_all(sub, "sub-"), 
                ses = stringr::str_remove_all(ses, "ses-")) %>% 
  dplyr::select(ttime, !!params$clusTag, sub, ses) %>% 
  dplyr::rename(cluster = !!params$clusTag)

# MERGING DATASETS

df <- merge(df,df_c)

# LOADING MOTION AND SUB INFO

source(file = 'src/samp_clin.R')

# MERGING DATAFRAMES

dfm <- merge(df, data_samp)

# ADDING SOME CLINICAL DATA

dfm <- dfm %>% 
  dplyr::mutate(pt = (ga < 37)) %>% 
  dplyr::mutate(pt = as.character(pt)) %>% 
  dplyr::mutate(pt = recode(pt, 
                            "FALSE" = "Term", 
                            "TRUE" = "Preterm"))


# CLEANING MEMORY

rm(df, df_c, fd)


bayley <- readr::read_csv(file = 'data/bayley_data.csv') %>%
  dplyr::filter(redcap_event_name == '18_month_assessmen_arm_1') %>%
  dplyr::select(-c(redcap_event_name,
                   redcap_event_name,
                   redcap_repeat_instrument,
                   redcap_repeat_instance)) %>%
  dplyr::rename(sub = participationid)

dfm <- dfm %>%
  left_join(bayley, by = c('sub'))

rm(bayley)


# KOP data

kura <- read.csv(file = 'data/df_kura.csv')

kura <- kura %>% 
  dplyr::select(-X) %>% 
  dplyr::mutate(ttime = ttime + 1) %>% 
  dplyr::rename(sub = patTags) %>% 
  dplyr::select(ttime, sub, ses, ensembleSync, ensembleMetastab, ensembleEntropy)


# REMOVING SUBJECT AND SESSION PREFIXES

kura <- kura %>% 
  tidyr::separate(ses, c(NA,"ses"), sep = "ses-") %>% 
  tidyr::separate(sub, c(NA,"sub"), sep = "sub-")

dfm <- merge(dfm, kura)

rm(kura)

dfm <- dfm %>% 
  dplyr::mutate(sex = as.character(sex)) %>% 
  dplyr::mutate(sex = recode(sex, 
                             '1' = 'Male', 
                             '2' = 'Female'))

# LOADING POLYGENIC SCORES

pgrs <- readr::read_csv(file = 'data/final_pgrs.csv')
pgrs <- merge(dfm, pgrs)
