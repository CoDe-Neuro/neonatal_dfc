nest_fo <- function(tbl, vars, foVar){
  
  require(dplyr)
  require(tidyr)
  require(janitor)
  
  vars_ <- dplyr::syms(vars)
  
  nested_fo <- tbl %>% 
    dplyr::rename('clus' = all_of(foVar)) %>% 
    dplyr::group_by(!!!vars_) %>% 
    dplyr::summarise(clusVec = list(clus)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(fo = purrr::map(clusVec, 
                                  janitor::tabyl)) %>% 
    tidyr::unnest(fo) %>% 
    dplyr::rename('cluster' = `.x[[i]]`, 
                  'perc' = 'percent') %>% 
    dplyr::mutate(cluster = as.character(cluster)) %>% 
    dplyr::select(-c(n, 
                     clusVec)) %>% 
    dplyr::group_by(cluster) %>% 
    tidyr::nest() 
  
  return(nested_fo)
  
}

nest_dwell <- function(tbl, vars, foVar, sortBy){
  
  vars_ <- dplyr::syms(vars)
  vars__ <- dplyr::syms(append(vars, 'cluster'))
  srt_ <- dplyr::syms(sortBy)
  
  tbl %>% 
    dplyr::rename('clus' = all_of(foVar)) %>% 
    dplyr::group_by(!!!vars_) %>% 
    dplyr::arrange(!!!srt_) %>%
    dplyr::summarise(clus = list(clus)) %>% 
    dplyr::mutate(cluster = purrr::map(clus, dwellCount)) %>% 
    tidyr::unnest_wider(cluster) %>% 
    dplyr::select(-clus) %>% 
    tidyr::unnest(c(cluster, dwell)) %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(dwell != 1) %>% 
    dplyr::group_by(!!!vars__) %>% 
    dplyr::summarise(mean_dwell = mean(dwell)) %>% 
    dplyr::mutate(cluster = as.character(cluster)) %>% 
    dplyr::group_by(cluster) %>% 
    tidyr::nest()
  
}

clusters_markov <- function(tbl, 
                            vars, 
                            cVar, 
                            sortBy, 
                            groupBy, 
                            remIntra = FALSE){
  
  srt_ <- dplyr::syms(sortBy)
  grp_ <- dplyr::syms(groupBy)
  vars_ <- dplyr::syms(append(vars, 
                              c('source', 
                              'target', 
                              'tag')))
  vars__ <- dplyr::syms(append(vars, 
                               'source'))
  
  summ_ <- dplyr::syms(append(vars[vars != groupBy], 
                              'tag'))
  
  cluster_mk <- tbl %>% 
    dplyr::rename('clus' = all_of(cVar)) %>% 
    dplyr::group_by(!!!grp_) %>% 
    dplyr::arrange(!!!srt_) %>%
    dplyr::mutate(source = lag(clus, n = 1), 
                  target = clus, 
                  tag = paste0(source, '_', target)) %>%
    dplyr::ungroup() %>% 
    dplyr::filter(source != 'NA')
  
  nClus <- max(cluster_mk$clus)
  inward <- character(length(nClus))
  for (i in 0:nClus){inward[i + 1] <- glue::glue('{i}_{i}')}
  
  if (remIntra) {
    
    cluster_mk <- cluster_mk %>% 
    dplyr::filter(!tag %in% inward)
    
  }
  
  cluster_mk %>% 
    dplyr::group_by(!!!vars_) %>% 
    dplyr::summarise(n = table(tag)) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(!!!vars__) %>% 
    dplyr::mutate(tot = sum(n)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(nCount = n/tot) %>% 
    dplyr::group_by(tag, source, target) %>% 
    tidyr::nest()
  
}
