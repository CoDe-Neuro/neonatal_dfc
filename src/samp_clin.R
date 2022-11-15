files <- data.frame(dir(path = "data/fmri_release3_qc", 
                        full.names = TRUE))
names(files) <- c("path")


fd <- files %>% 
  dplyr::mutate(json = map(path, patmotJSON)) %>% 
  tidyr::unnest_wider(json) %>% 
  dplyr::select(path, 
                motparam_fd, 
                motparam_fd_outlier, 
                raw_dvars, 
                raw_dvars_outlier, 
                sub, 
                ses, 
                ttime) %>% 
  tidyr::unnest(c(motparam_fd, 
                  motparam_fd_outlier, 
                  raw_dvars, 
                  raw_dvars_outlier, 
                  ttime)) %>% 
  dplyr::group_by(sub, ses, path) %>% 
  dplyr::summarise(motparam_fd_outlier_count = sum(unlist(motparam_fd_outlier), 
                                                   na.rm = TRUE), 
                   raw_dvars_outlier_count = sum(unlist(raw_dvars_outlier), 
                                                 na.rm = TRUE), 
                   mean_dvars = mean(unlist(raw_dvars), 
                                     na.rm = TRUE), 
                   mean_fd = mean(unlist(motparam_fd), 
                                  na.rm = TRUE)) %>% 
  dplyr::ungroup()

qcVols <- tibble::tibble(readr::read_csv(file = 'data/full_subs.csv', col_names = c('sub')), 
                         readr::read_csv(file = 'data/full_ses.csv', col_names = c('ses')), 
                         readr::read_csv(file = 'data/lowVol_subs.csv', col_names = c('lowVol')), 
                         readr::read_csv(file = 'data/volBOLD.csv', col_names = c('volBOLD'))) %>% 
  dplyr::mutate(sub = stringr::str_remove_all(sub, "sub-"), 
                ses = stringr::str_remove_all(ses, "ses-"))

data_tab <- readr::read_tsv(file = 'https://raw.githubusercontent.com/BioMedIA/dHCP-release-notes/master/supplementary_files/combined.tsv') %>% 
  dplyr::select(participant_id, 
                session_id, 
                scan_age, 
                birth_age, 
                sex, 
                birth_weight, 
                radiology_score) %>% 
  dplyr::rename('sub' = 'participant_id', 
                'ses' = 'session_id', 
                'pma' = 'scan_age', 
                'ga' = 'birth_age') %>% 
  dplyr::mutate(ses = as.character(ses)) %>% 
  dplyr::inner_join(fd) %>% 
  dplyr::inner_join(qcVols) %>% 
  dplyr::mutate(id = stringr::str_c(sub, ses, sep = "_"))

## Sunniva-ing the data

# 1 - Remove poor quality images


filt_data <- data_tab %>% 
  dplyr::mutate(sunnivaed = if_else(radiology_score < 4, 
                                    true = TRUE, 
                                    false = FALSE))

# 2 - Deal with twins


twin_select <- data_tab %>% 
  dplyr::filter(radiology_score < 4) %>% 
  dplyr::mutate(subs = str_replace(sub, 
                                   c('XX'), 
                                   c('.XX.'))) %>% 
  dplyr::mutate(subs = str_replace(subs, 
                                   c('AN'), 
                                   c('.AN.'))) %>% 
  dplyr::mutate(subs = str_replace(subs, 
                                   c('BN'), 
                                   c('.BN.'))) %>% 
  dplyr::mutate(subs = str_replace(subs, 
                                   c('CN'), 
                                   c('.CN.'))) %>% 
  tidyr::separate(subs, into = c('prefix', 'twin', 'suffix')) %>% 
  dplyr::filter(twin %in% c('AN', 'BN', 'CN')) %>% 
  dplyr::group_by(prefix, suffix) %>% 
  tidyr::nest() %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(choice = purrr::map(data, ~whichTwin(.x))) %>% 
  tidyr::unnest(choice) %>% 
  dplyr::mutate(sub = paste0(prefix, choice, suffix)) %>% 
  dplyr::select(sub)

twinNeg <- data_tab %>% 
  dplyr::filter(radiology_score < 4) %>% 
  dplyr::mutate(subs = str_replace(sub, 
                                   c('XX'), 
                                   c('.XX.'))) %>% 
  dplyr::mutate(subs = str_replace(subs, 
                                   c('AN'), 
                                   c('.AN.'))) %>% 
  dplyr::mutate(subs = str_replace(subs, 
                                   c('BN'), 
                                   c('.BN.'))) %>% 
  dplyr::mutate(subs = str_replace(subs, 
                                   c('CN'), 
                                   c('.CN.'))) %>% 
  tidyr::separate(subs, into = c('prefix', 'twin', 'suffix')) %>% 
  dplyr::filter(twin %in% c('AN', 'BN', 'CN')) %>% 
  dplyr::select(-c(prefix, twin, suffix)) %>% 
  dplyr::mutate(sunnivaed = if_else(sub %in% twin_select$sub, TRUE, FALSE)) %>% 
  filter(sunnivaed == 'FALSE')

filt_data <- filt_data %>% 
  dplyr::mutate(sunnivaed = if_else(sub %in% twinNeg$sub, 
                                    true = FALSE, 
                                    false = sunnivaed))

# 3 - Remove excessive motion outliers and small volumes


filt_data <- filt_data %>% 
  dplyr::mutate(sunnivaed = if_else(motparam_fd_outlier_count/23 > 10, 
                                    true = FALSE, 
                                    false = sunnivaed)) %>% 
  dplyr::mutate(sunnivaed = if_else(lowVol > 0, 
                                    true = FALSE, 
                                    false = sunnivaed)) %>% 
  dplyr::mutate(sunnivaed = if_else(pma < 37, 
                                    true = FALSE, 
                                    false = sunnivaed))


# 4 - Remove duplicates and filter sunnivaed values


data_samp <- filt_data %>% 
  dplyr::filter(sunnivaed == 'TRUE') %>% 
  dplyr::group_by(sub) %>% 
  dplyr::filter(motparam_fd_outlier_count == min(motparam_fd_outlier_count)) %>% 
  dplyr::select(-sunnivaed)
