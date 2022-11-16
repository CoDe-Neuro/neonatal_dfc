## ---- kop_calc --------

kop <- dfm %>% 
  dplyr::group_by(sub, 
                  pma, 
                  ga, 
                  pt, 
                  sex, 
                  ensembleMetastab, 
                  ensembleEntropy, 
                  motparam_fd_outlier_count) %>% 
  dplyr::summarise(meanSync = mean(ensembleSync)) %>% 
  dplyr::ungroup()

meanSync_glm <- grouped_perm_glm(
  kop, 
  formla = meanSync ~ pt + sex + pma + motparam_fd_outlier_count, 
  var_to_perm = 'meanSync', 
  permNum = params$nPerms, 
  seed = params$seed) %>% 
  dplyr::mutate(p.adj = as.character(signif(p.perm, 3))) %>% 
  dplyr::mutate(p.adj = glue::glue('p = {p.adj}')) %>% 
  dplyr::mutate(p.adj = dplyr::recode(p.adj, 'p = 0'='p < 0.001'))

metastab_glm <- grouped_perm_glm(
  kop, 
  formla = ensembleMetastab ~ pt + sex + pma + motparam_fd_outlier_count, 
  var_to_perm = 'ensembleMetastab', 
  permNum = params$nPerms, 
  seed = params$seed) %>% 
  dplyr::mutate(p.adj = as.character(signif(p.perm, 3))) %>% 
  dplyr::mutate(p.adj = glue::glue('p = {p.adj}')) %>% 
  dplyr::mutate(p.adj = dplyr::recode(p.adj, 'p = 0'='p < 0.001'))

entropy_glm <- grouped_perm_glm(
  kop, 
  formla = ensembleEntropy ~ pt + sex + pma + motparam_fd_outlier_count, 
  var_to_perm = 'ensembleEntropy', 
  permNum = params$nPerms, 
  seed = params$seed) %>% 
  dplyr::mutate(p.adj = as.character(signif(p.perm, 3))) %>% 
  dplyr::mutate(p.adj = glue::glue('p = {p.adj}')) %>% 
  dplyr::mutate(p.adj = dplyr::recode(p.adj, 'p = 0'='p < 0.001'))

chd <- dfm %>% 
  dplyr::group_by(sub, pma, ga, pt, ensembleMetastab) %>% 
  dplyr::summarise(meanSync = mean(ensembleSync)) %>% 
  dplyr::ungroup() %>% 
  rstatix::cohens_d(meanSync ~ pt) %>% 
  dplyr::mutate(D.adj = signif(effsize, 3))

chd_kop <- cbind(chd, p.adj = meanSync_glm$p.adj[4])

chd <- dfm %>% 
  dplyr::group_by(sub, pma, ga, pt, ensembleMetastab) %>% 
  dplyr::summarise(meanSync = mean(ensembleSync)) %>% 
  dplyr::ungroup() %>% 
  rstatix::cohens_d(ensembleMetastab ~ pt) %>% 
  dplyr::mutate(D.adj = signif(effsize, 3))

chd_metastab <- cbind(chd, p.adj = metastab_glm$p.adj[4])

chd <- dfm %>% 
  dplyr::group_by(sub, pma, ga, pt, ensembleEntropy) %>% 
  dplyr::summarise() %>% 
  dplyr::ungroup() %>% 
  rstatix::cohens_d(ensembleEntropy ~ pt) %>% 
  dplyr::mutate(D.adj = signif(effsize, 3))

chd_entr <- cbind(chd, p.adj = entropy_glm$p.adj[4])

## ---- kopfig --------

chd <- dfm %>% 
  dplyr::group_by(sub, pma, ga, pt, ensembleMetastab) %>% 
  dplyr::summarise(meanSync = mean(ensembleSync)) %>% 
  dplyr::ungroup() %>% 
  rstatix::cohens_d(meanSync ~ pt) %>% 
  dplyr::mutate(D.adj = signif(effsize, 3))

chd <- cbind(chd, p.adj = meanSync_glm$p.adj[4])


kop_scatter <- dfm %>% 
  dplyr::filter(pma >= 37) %>% 
  dplyr::group_by(sub, pma, ga, pt, ensembleMetastab) %>% 
  dplyr::summarise(meanSync = mean(ensembleSync)) %>% 
  
  ggplot2::ggplot(aes(x = pma, 
                      y = meanSync)) + 
  geom_point(aes(fill = pt), 
             size = 3, 
             pch = 21, 
             colour = 'white') + 
  xlab('PMA (in weeks - at scan)') + 
  ylab('Mean KOP') + 
  scale_y_continuous(labels = scales::percent, 
                     limits = c(0.25, 0.8)) + 
  scale_fill_manual(values = mypal2,
                    name = element_blank()) + 
  theme(legend.position = "none")

kop_vp <- dfm %>% 
  dplyr::group_by(sub, pma, ga, pt, ensembleMetastab) %>% 
  dplyr::summarise(meanSync = mean(ensembleSync)) %>% 
  
  ggplot2::ggplot(aes(x = pt, 
                      y = meanSync)) + 
  geom_boxplot(aes(color = pt, 
                   fill = pt), 
               width = 0.5, 
               alpha = 0.3) + 
  ggforce::geom_sina(aes(fill = pt, 
                         colour = pt), 
                size = 2, 
                pch = 21, 
                alpha = 0.4, 
                maxwidth = .5) + 
  stat_pvalue_manual(chd, 
                     label = "D = {D.adj} | {p.adj}", 
                     xmin = "group1", 
                     xmax = "group2", 
                     y.position = 0.8) + 
  xlab(element_blank()) + 
  ylab('Mean synchronisation') + 
  ylim(0.25, 0.8) +
  scale_fill_manual(values = mypal2, 
                    name = element_blank()) + 
  scale_colour_manual(values = mypal2, 
                      name = element_blank()) + 
  theme(legend.position = "none", 
        text = element_text(size = 24)) 

kop_comp <- cowplot::plot_grid(kop_scatter, 
                               kop_vp, 
                               rel_widths = c(2, 1))

chd <- dfm %>% 
  dplyr::group_by(sub, pma, ga, pt, ensembleMetastab) %>% 
  dplyr::summarise(meanSync = mean(ensembleSync)) %>% 
  dplyr::ungroup() %>% 
  rstatix::cohens_d(ensembleMetastab ~ pt) %>% 
  dplyr::mutate(D.adj = signif(effsize, 3))

chd <- cbind(chd, p.adj = metastab_glm$p.adj[4])

metaStb_scatter <- dfm %>% 
  dplyr::group_by(sub, pma, ga, pt, ensembleMetastab) %>% 
  dplyr::summarise(meanSync = mean(ensembleSync), 
                   meanMetastab = mean(ensembleMetastab)) %>% 
  
  ggplot2::ggplot(aes(x = pma, 
                      y = meanMetastab)) + 
  geom_point(aes(fill = pt), 
             size = 3, 
             pch = 21, 
             colour = 'white') + 
  xlab('PMA (in weeks - at scan)') + 
  ylab('Metastability') + 
  ylim(0.13, 0.26) + 
  scale_fill_manual(values = mypal2,
                    name = element_blank()) + 
  theme(legend.position = "none")

metaStb_vp <- dfm %>% 
  dplyr::group_by(sub, pma, ga, pt, ensembleMetastab) %>% 
  dplyr::summarise(meanSync = mean(ensembleSync), 
                   meanMetastab = mean(ensembleMetastab)) %>% 
  
  ggplot2::ggplot(aes(x = pt, 
                      y = meanMetastab)) + 
  geom_boxplot(aes(color = pt, 
                   fill = pt), 
               width = 0.5, 
               alpha = 0.3) + 
  ggforce::geom_sina(aes(fill = pt, 
                         colour = pt), 
                     size = 2, 
                     pch = 21, 
                     alpha = 0.4, 
                     maxwidth = .5) + 
  stat_pvalue_manual(chd, 
                     label = "D = {D.adj} | {p.adj}", 
                     xmin = "group1", 
                     xmax = "group2", 
                     y.position = 0.26) + 
  xlab(element_blank()) + 
  ylab('Metastability') + 
  ylim(0.13, 0.26) + 
  scale_fill_manual(values = mypal2, 
                    name = element_blank()) + 
  scale_colour_manual(values = mypal2, 
                      name = element_blank()) + 
  theme(legend.position = "none", 
        text = element_text(size = 24)) 

metaStb_comp <- cowplot::plot_grid(metaStb_scatter, 
                                   metaStb_vp, 
                                   rel_widths = c(2, 1))

chd <- dfm %>% 
  dplyr::group_by(sub, pma, ga, pt, ensembleEntropy) %>% 
  dplyr::summarise() %>% 
  dplyr::ungroup() %>% 
  rstatix::cohens_d(ensembleEntropy ~ pt) %>% 
  dplyr::mutate(D.adj = signif(effsize, 3))

chd <- cbind(chd, p.adj = entropy_glm$p.adj[4])

entr_scatter <- dfm %>% 
  dplyr::group_by(sub, pma, ga, pt, ensembleEntropy) %>% 
  dplyr::summarise() %>% 
  
  ggplot2::ggplot(aes(x = pma, 
                      y = ensembleEntropy)) + 
  geom_point(aes(fill = pt), 
             size = 3, 
             pch = 21, 
             colour = 'white') + 
  xlab('PMA (in weeks - at scan)') + 
  ylab('Entropy') + 
  ylim(6.9, 7.8) + 
  scale_fill_manual(values = mypal2,
                    name = element_blank()) + 
  theme(legend.position = "none")

entr_vp <- dfm %>% 
  dplyr::group_by(sub, pma, ga, pt, ensembleEntropy) %>% 
  dplyr::summarise() %>% 
  
  ggplot2::ggplot(aes(x = pt, 
                      y = ensembleEntropy)) + 
  geom_boxplot(aes(color = pt, 
                   fill = pt), 
               width = 0.5, 
               alpha = 0.3) + 
  ggforce::geom_sina(aes(fill = pt, 
                         colour = pt), 
                     size = 2, 
                     pch = 21, 
                     alpha = 0.4, 
                     maxwidth = .5) + 
  stat_pvalue_manual(chd, 
                     label = "D = {D.adj} | {p.adj}", 
                     xmin = "group1", 
                     xmax = "group2", 
                     y.position = 7.8) + 
  xlab(element_blank()) + 
  ylab('Entropy') + 
  ylim(6.9, 7.8) + 
  scale_fill_manual(values = mypal2, 
                    name = element_blank()) + 
  scale_colour_manual(values = mypal2, 
                    name = element_blank()) + 
  theme(legend.position = "none", 
        text = element_text(size = 24)) 

entr_comp <- cowplot::plot_grid(entr_scatter, 
                                entr_vp, 
                                rel_widths = c(2, 1))

kop_met <- cowplot::plot_grid(kop_vp, 
                              metaStb_vp, 
                              align = 'v',
                              nrow = 1, 
                              labels = c('a','b'))


cowplot::plot_grid(kop_met, kop_qchat_fig, ncol = 1, labels = c('','c'), rel_heights = c(1,2/3))


