get_newp <- function(perm_stat, tref){
  
  p.perm <- length(perm_stat[abs(perm_stat) >= abs(tref)])/length(perm_stat)
  return(p.perm)
}

grouped_perm_glm <- function(tbl, formla, var_to_perm, permNum = 1000, seed = 42){
  
  require(modelr)
  require(dplyr)
  
  set.seed(seed)
  
  perms <- modelr::permute(tbl, permNum, all_of(var_to_perm))
  models <- map(perms$perm, 
                ~ glm(formla, 
                      data = .))
  tdy_idx <- map_df(models, broom::tidy, .id = "id")
  
  mod <- tidy(glm(formla, 
                  data = tbl))
  
  tdy_stats <- tdy_idx %>% 
    group_by(term) %>% 
    summarise(perm_stat = list(statistic)) %>% 
    ungroup()
  
  tdy_stats <- dplyr::inner_join(tdy_stats, mod)
  
  final <- tdy_stats %>% 
    dplyr::mutate(p.perm = map2(perm_stat, statistic, get_newp)) %>% 
    dplyr::mutate(p.perm = unlist(p.perm)) %>% 
    dplyr::select(term, estimate, statistic, p.value, p.perm)
  
  return(final)
}

grouped_perm_glmm <- function(tbl,
                              formla,
                              var_to_perm,
                              permNum = 1000,
                              seed = 42){
  
  require(modelr)
  require(dplyr)
  require(broom.mixed)
  require(lme4)
  
  set.seed(seed)
  
  perms <- modelr::permute(tbl,
                           permNum,
                           all_of(var_to_perm))
  
  models <- map(perms$perm,
                ~ lmer(formla,
                       data = .))
  
  tdy_idx <- map_df(models,
                    broom.mixed::tidy,
                    .id = "id")
  
  mod <- broom.mixed::tidy(lmer(formla,
                                data = tbl))
  
  tdy_stats <- tdy_idx %>%
    dplyr::filter(effect == 'fixed') %>%
    group_by(term) %>%
    summarise(perm_stat = list(statistic)) %>%
    ungroup()
  
  tdy_stats <- dplyr::inner_join(tdy_stats,
                                 mod)
  
  final <- tdy_stats %>%
    dplyr::mutate(p.perm = map2(perm_stat,
                                statistic,
                                get_newp)) %>%
    dplyr::mutate(p.perm = unlist(p.perm)) %>%
    dplyr::select(term,
                  estimate,
                  statistic,
                  p.perm)
  
  return(final)
}