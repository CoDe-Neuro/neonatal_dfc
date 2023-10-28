## ---- clusNum --------
py <- readr::read_csv(file = 'data/ch_score_LEiDa_dim.csv', 
                      col_names = FALSE)

py2 <- readr::read_csv(file = 'data/db_score_LEiDa_dim.csv', 
                       col_names = FALSE)

clusSc <- as.data.frame(cbind(2:20, 
                              py$X1, 
                              py2$X1))
names(clusSc) <- c("clus", 
                   "Calinski-Harabasz", 
                   "Davies-Bouldin")

clusSc %>% 
  tidyr::gather(`Calinski-Harabasz`, 
                `Davies-Bouldin`, 
                key = "Index", 
                value = "measure") %>% 
  
  ggplot2::ggplot(aes(x = clus, 
                      y = measure)) + 
  geom_line(size = 1, 
            colour = "#4DBBD5FF") + 
  geom_vline(xintercept = 6, 
             colour = "#DC0000FF", 
             size = 1, 
             linetype = 'dashed') + 
  geom_point(size = 2, 
             colour = "#4DBBD5FF") + 
  facet_wrap(~Index, 
             scales = "free_y") + 
  xlab("# of clusters") + 
  ylab("Measure")


## ---- plotBrains --------
raw_centr <- as.matrix(readr::read_csv(file = paste0('data/centroids_LEiDa_',params$clusNum,'.csv'), 
                                       col_names = FALSE))

for (i in seq(1, params$clusNum)) {
  
  raw_centr[i,] <- raw_centr[i,]/max(abs(raw_centr[i,]))
  
}


ROI <- readr::read_csv('data/all_tags.csv')


centr <- tibble(idx = 1:90, 
                ROI = ROI$name, 
                as_tibble(t(raw_centr)))

centr <- centr %>% 
  tidyr::gather(-c(ROI, idx), 
                key = ctr, 
                value = value) %>% 
  dplyr::mutate(ROI = stringr::str_replace_all(ROI, "_", " "))


negativity <- centr %>% 
  dplyr::group_by(ctr) %>% 
  dplyr::summarise(tot = sum(value)) %>% 
  dplyr::arrange(tot) %>% 
  dplyr::mutate(col = dplyr::row_number()) %>% 
  dplyr::mutate(col_rt = labRetag) %>% 
  dplyr::mutate(lab = glue::glue('State {col_rt}')) %>% 
  dplyr::mutate(n = ctr) %>% 
  tidyr::separate(n, into = c(NA, 'n'), sep = 'V') %>% 
  dplyr::mutate(cluster = as.numeric(n)) %>% 
  dplyr::mutate(n = col)

centr_p <- centr %>%
  dplyr::mutate(across(ctr, factor, levels = negativity$ctr, labels = negativity$lab)) %>% 
  ggplot2::ggplot(aes(x = reorder(ROI, idx), 
                      y = value)) + 
  geom_bar(aes(fill = value), 
           stat = 'identity') + 
  coord_flip() + 
  scale_fill_gradientn(colours = scales3, 
                       limits = c(-1,1),
                       name = 'LEiDA',
                       guide = 'none') + 
  ylab(element_blank()) + 
  xlab(element_blank()) + 
  ylim(-1,1) + 
  theme_bw(base_size = 24, 
           base_family = "Helvetica") +
  facet_wrap(~ctr, 
             nrow = 1) 

negativity$lab_0 <- factor(negativity$lab, levels = c('State Glb.A', 'State Glb.B', 'State Glb.C', 'State Occ.', 'State SM', 'State FP'))

ctr_plot_r <- negativity %>% 
  dplyr::mutate(x = 0, 
                y = 0, 
                right = glue::glue('figs/centr_k{params$clusNum}_{n}_r_plot.png'), 
                left = glue::glue('figs/centr_k{params$clusNum}_{n}_l_plot.png')) %>% 
  
  ggplot2::ggplot(aes(x = x, y = y)) + 
  geom_image(aes(image = right), size = 1) + 
  facet_wrap(~lab_0, nrow = 1) + 
  theme_bw(base_size = 24, 
           base_family = "Helvetica") +
  theme(axis.title = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text = element_blank(), 
        panel.grid = element_blank())


ctr_plot_l <- negativity %>% 
  dplyr::mutate(x = 0, 
                y = 0, 
                right = glue::glue('figs/centr_k{params$clusNum}_{n}_r_plot.png'), 
                left = glue::glue('figs/centr_k{params$clusNum}_{n}_l_plot.png')) %>% 
  
  ggplot2::ggplot(aes(x = x, y = y)) + 
  geom_image(aes(image = left), size = 1) + 
  facet_wrap(~lab_0, nrow = 1) + 
  theme_bw(base_size = 24, 
        base_family = "Helvetica") +
  theme(axis.title = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text = element_blank(), 
        panel.grid = element_blank())

legend <- cowplot::get_legend(centr_p + theme(legend.position = "bottom") + 
  guides(fill = guide_colourbar(barwidth = 30, 
                                  barheight = 3)))



bigFig <- cowplot::plot_grid(centr_p, 
                             ctr_plot_r,
                             ctr_plot_l,
                             ncol = 1, 
                             rel_heights = c(22,4,4)/30, 
                             align = 'v', 
                             labels = c('a','b','c'), 
                             label_size = 32)

cowplot::plot_grid(bigFig, 
                   legend, 
                   rel_heights = c(1,0.05), 
                   ncol = 1)


## ---- kopStates --------
meanSync_state_glm <- dfm %>% 
  dplyr::select(sub,
                ses,
                cluster,
                pma,
                ga,
                sex,
                motparam_fd_outlier_count,
                pt,
                ensembleSync,
                ensembleMetastab,
                ensembleEntropy) %>% 
  dplyr::mutate(cluster = as.character(cluster)) %>% 
  dplyr::group_by(cluster, sub, pt, ga, sex, pma, motparam_fd_outlier_count) %>% 
  dplyr::summarise(syncMed = mean(ensembleSync),
                   metastabMed = sd(ensembleSync)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(cluster) %>% 
  tidyr::nest() %>% 
  dplyr::mutate(mk = list(map_df(data, 
                                 ~grouped_perm_glm(
                                   .x, 
                                   formla = syncMed ~ pt + sex + pma + motparam_fd_outlier_count, 
                                   var_to_perm = 'syncMed', 
                                   permNum = params$nPerms, 
                                   seed = params$seed)))) %>% 
  tidyr::unnest_wider(mk) %>% 
  tidyr::unnest(-c(data)) %>% 
  dplyr::group_by(term) %>% 
  dplyr::mutate(cor_p = stats::p.adjust(p.perm, 
                                        method = 'fdr', 
                                        n = length(p.perm))) %>% 
  dplyr::filter(term == 'ptPreterm') %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(p.adj = as.character(signif(p.perm, 3)), 
                s.adj = as.character(signif(statistic, 3))) %>% 
  dplyr::mutate(p.adj = glue::glue('p = {p.adj}'), 
                s.adj = glue::glue('t = {s.adj}')) %>% 
  dplyr::mutate(p.adj = dplyr::recode(p.adj, 'p = 0'='p < 0.001')) %>% 
  dplyr::filter(cor_p < 0.05) %>% 
  dplyr::mutate(group1 = 'Preterm', 
                group2 = 'Term', 
                .y. = 'metastabMed') %>% 
  dplyr::select(-c(term, data)) %>% 
  dplyr::mutate(lab = glue::glue('{s.adj} - {p.adj}'))

  

kop_plot <- dfm %>% 
  dplyr::select(sub,
                ses,
                cluster,
                pma,
                ga,
                sex,
                motparam_fd_outlier_count,
                pt,
                ensembleSync,
                ensembleMetastab,
                ensembleEntropy) %>% 
  dplyr::mutate(cluster = as.character(cluster)) %>% 
  dplyr::group_by(cluster, sub, pt) %>% 
  dplyr::summarise(syncMed = mean(ensembleSync),
                   metastabMed = mean(ensembleMetastab)) %>% 
  dplyr::ungroup() %>% 
  
  ggplot2::ggplot(aes(x = pt, 
                      y = syncMed)) + 
  geom_beeswarm(aes(fill = pt), 
                   size = 2, 
                   pch = 21, 
                   colour = 'white') + 
  geom_boxplot(aes(colour = pt), 
               width = 0.1, 
               outlier.shape = NA, 
               position = position_dodge(width = 0.8)) + 
  facet_wrap(~cluster, 
             labeller = labeller(cluster = labels), 
             nrow = 1) + 
  scale_fill_manual(values = scales[c(2,10)], 
                    name = 'Prematurity') +
  scale_colour_manual(values = c('#000000', '#000000'), 
                      guide = 'none') + 
  theme_bw() + 
  theme(strip.text.x = element_markdown(color = "black", 
                                       size = 11)) + 
  xlab(element_blank()) + 
  ylab('Mean KOP') + 
  theme(legend.position = 'none')

metastab_state_glm <- dfm %>% 
  dplyr::select(sub,
                ses,
                cluster,
                pma,
                ga,
                sex,
                motparam_fd_outlier_count,
                pt,
                ensembleSync,
                ensembleMetastab,
                ensembleEntropy) %>% 
  dplyr::mutate(cluster = as.character(cluster)) %>% 
  dplyr::group_by(cluster, sub, pt, ga, sex, pma, motparam_fd_outlier_count) %>% 
  dplyr::summarise(syncMed = mean(ensembleSync),
                   metastabMed = sd(ensembleSync)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(cluster) %>% 
  tidyr::nest() %>% 
  dplyr::mutate(mk = list(map_df(
    data, 
    ~grouped_perm_glm(
      .x, 
      formla = metastabMed ~ pt + sex + pma + motparam_fd_outlier_count, 
      var_to_perm = 'metastabMed', 
      permNum = params$nPerms, 
      seed = params$seed
      )))) %>% 
  tidyr::unnest_wider(mk) %>% 
  tidyr::unnest(-c(data)) %>% 
  dplyr::group_by(term) %>% 
  dplyr::mutate(cor_p = stats::p.adjust(p.perm, 
                                        method = 'fdr', 
                                        n = length(p.perm))) %>% 
  dplyr::filter(term == 'ptPreterm') %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(p.adj = as.character(signif(p.perm, 3)), 
                s.adj = as.character(signif(statistic, 3))) %>% 
  dplyr::mutate(p.adj = glue::glue('p = {p.adj}'), 
                s.adj = glue::glue('t = {s.adj}')) %>% 
  dplyr::mutate(p.adj = dplyr::recode(p.adj, 'p = 0'='p < 0.001')) %>% 
  dplyr::filter(cor_p < 0.05) %>% 
  dplyr::mutate(group1 = 'Preterm', 
                group2 = 'Term', 
                .y. = 'metastabMed') %>% 
  dplyr::select(-c(term, data)) %>% 
  dplyr::mutate(lab = glue::glue('{s.adj} - {p.adj}'))

metastab_plot <- dfm %>% 
  dplyr::select(sub,
                ses,
                cluster,
                pma,
                ga,
                sex,
                motparam_fd_outlier_count,
                pt,
                ensembleSync,
                ensembleMetastab,
                ensembleEntropy) %>% 
  dplyr::mutate(cluster = as.character(cluster)) %>% 
  dplyr::group_by(cluster, sub, pt) %>% 
  dplyr::summarise(syncMed = mean(ensembleSync),
                   metastabMed = sd(ensembleSync)) %>% 
  dplyr::ungroup() %>% 
  
  
  ggplot2::ggplot(aes(x = pt, 
                      y = metastabMed)) + 
  geom_beeswarm(aes(fill = pt), 
                size = 2, 
                pch = 21, 
                colour = 'white') + 
  geom_boxplot(aes(colour = pt), 
               width = 0.1, 
               outlier.shape = NA, 
               position = position_dodge(width = 0.8)) + 
  facet_wrap(~cluster, 
             labeller = labeller(cluster = labels), 
             nrow = 1) + 
  scale_fill_manual(values = scales[c(2,10)], 
                    name = 'Prematurity') +
  scale_colour_manual(values = c('#000000', '#000000'), 
                      guide = 'none') + 
  theme_bw() + 
  theme(strip.text.x = element_markdown(color = "black", 
                                        size = 11)) + 
  stat_pvalue_manual(metastab_state_glm, 
                     label = "lab", 
                     xmin = "group1", 
                     xmax = "group2", 
                     y.position = 0.16) +
  xlab(element_blank()) + 
  ylab('Mean Metastability') + 
  theme(legend.position = 'none')


fo_state_glm <- dfm %>% 
  dplyr::select(ttime, 
                sub, 
                pma, 
                cluster, 
                pt, 
                sex, 
                motparam_fd_outlier_count) %>% 
  stateR::nest_fo(., 
          vars = c('sub', 
                   'pma', 
                   'pt', 
                   'sex', 
                   'motparam_fd_outlier_count'), 
          foVar = 'cluster') %>% 
  dplyr::mutate(cc = list(map_df(data, 
                                 ~grouped_perm_glm(.x, 
                                                   formla = perc ~ 
                                                     pt + 
                                                     sex + 
                                                     pma + 
                                                     motparam_fd_outlier_count, 
                                                   var_to_perm = 'perc', 
                                                   permNum = params$nPerms, 
                                                   seed = params$seed)))) %>% 
  tidyr::unnest_wider(cc) %>% 
  tidyr::unnest(-c(cluster, data)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(term) %>% 
  dplyr::mutate(cor_p = stats::p.adjust(p.perm, 
                                        method = 'fdr', 
                                        n = length(p.perm))) %>% 
  dplyr::filter(term == 'ptPreterm') %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(p.adj = as.character(signif(p.perm, 3)), 
                s.adj = as.character(signif(statistic, 3))) %>% 
  dplyr::mutate(p.adj = glue::glue('p = {p.adj}'), 
                s.adj = glue::glue('t = {s.adj}')) %>% 
  dplyr::mutate(p.adj = dplyr::recode(p.adj, 'p = 0'='p < 0.001')) %>% 
  dplyr::filter(cor_p < 0.05) %>% 
  dplyr::mutate(group1 = 'Preterm', 
                group2 = 'Term', 
                .y. = 'fo') %>% 
  dplyr::select(-c(term, data)) %>% 
  dplyr::mutate(lab = glue::glue('{s.adj} - {p.adj}'))

fo_plot <- dfm %>% 
  dplyr::select(ttime, 
                sub, 
                pma, 
                cluster, 
                pt, 
                sex, 
                motparam_fd_outlier_count) %>% 
  stateR::nest_fo(., 
          vars = c('sub', 
                   'pma', 
                   'pt', 
                   'sex', 
                   'motparam_fd_outlier_count'), 
          foVar = 'cluster') %>% 
  tidyr::unnest(-c(cluster)) %>% 
  
  
  ggplot2::ggplot(aes(x = pt, 
                      y = perc)) + 
  geom_beeswarm(aes(fill = pt), 
                size = 2, 
                pch = 21, 
                colour = 'white') + 
  geom_boxplot(aes(colour = pt), 
               width = 0.1, 
               outlier.shape = NA, 
               position = position_dodge(width = 0.8)) + 
  facet_wrap(~cluster, 
             labeller = labeller(cluster = labels), 
             nrow = 1) + 
  scale_fill_manual(values = scales[c(2,10)], 
                    name = 'Prematurity') +
  scale_colour_manual(values = c('#000000', '#000000'), 
                      guide = 'none') + 
  theme_bw() + 
  theme(strip.text.x = element_markdown(color = "black", 
                                        size = 11)) + 
  stat_pvalue_manual(fo_state_glm, 
                     label = "lab", 
                     xmin = "group1", 
                     xmax = "group2", 
                     y.position = 0.55) +
  xlab(element_blank()) + 
  ylab('Mean Fractional Occupancy') + 
  ylim(c(0,0.6)) + 
  theme(legend.position = 'none')


dt_state_glm <- dfm %>% 
  dplyr::select(ttime, 
                sub, 
                pma, 
                cluster, 
                pt, 
                sex, 
                motparam_fd_outlier_count) %>% 
  stateR::nest_dwell(., vars = c('sub', 
                         'pma', 
                         'pt', 
                         'sex', 
                         'motparam_fd_outlier_count'), 
             foVar = 'cluster', 
             sortBy = c('sub','ttime')) %>% 
  dplyr::mutate(dw = list(map_df(data, 
                                 ~grouped_perm_glm(.x, 
                                                   formla = mean_dwell ~
                                                     pt + 
                                                     sex + 
                                                     pma + 
                                                     motparam_fd_outlier_count, 
                                                   var_to_perm = 'mean_dwell', 
                                                   permNum = params$nPerms, 
                                                   seed = params$seed)))) %>% 
  tidyr::unnest_wider(dw) %>% 
  tidyr::unnest(-c(cluster, data)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(term) %>% 
  dplyr::mutate(cor_p = stats::p.adjust(p.perm, 
                                        method = 'fdr', 
                                        n = length(p.perm))) %>% 
  dplyr::filter(term == 'ptPreterm') %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(p.adj = as.character(signif(p.perm, 3)), 
                s.adj = as.character(signif(statistic, 3))) %>% 
  dplyr::mutate(p.adj = glue::glue('p = {p.adj}'), 
                s.adj = glue::glue('t = {s.adj}')) %>% 
  dplyr::mutate(p.adj = dplyr::recode(p.adj, 'p = 0'='p < 0.001')) %>% 
  dplyr::filter(cor_p < 0.05) %>% 
  dplyr::mutate(group1 = 'Preterm', 
                group2 = 'Term', 
                .y. = 'fo') %>% 
  dplyr::select(-c(term, data)) %>% 
  dplyr::mutate(lab = glue::glue('{s.adj} - {p.adj}'))


dt_plot <- dfm %>% 
  dplyr::select(ttime, 
                sub, 
                pma, 
                cluster, 
                pt, 
                sex, 
                motparam_fd_outlier_count) %>% 
  stateR::nest_dwell(., vars = c('sub', 
                         'pma', 
                         'pt', 
                         'sex', 
                         'motparam_fd_outlier_count'), 
             foVar = 'cluster', 
             sortBy = c('sub','ttime')) %>% 
  tidyr::unnest(-c(cluster)) %>% 
  
  ggplot2::ggplot(aes(x = pt, 
                      y = mean_dwell)) + 
  geom_beeswarm(aes(fill = pt), 
                size = 2, 
                pch = 21, 
                colour = 'white') + 
  geom_boxplot(aes(colour = pt), 
               width = 0.1, 
               outlier.shape = NA, 
               position = position_dodge(width = 0.8)) + 
  facet_wrap(~cluster, 
             labeller = labeller(cluster = labels), 
             nrow = 1) + 
  scale_fill_manual(values = scales[c(2,10)], 
                    name = 'Prematurity') +
  scale_colour_manual(values = c('#000000', '#000000'), 
                      guide = 'none') + 
  theme_bw() + 
  theme(strip.text.x = element_markdown(color = "black", 
                                        size = 11)) + 
  stat_pvalue_manual(dt_state_glm, 
                     label = "lab", 
                     xmin = "group1", 
                     xmax = "group2", 
                     y.position = 27) +
  xlab(element_blank()) + 
  ylab('Mean Dwelling Time') + 
  ylim(c(0,30)) + 
  theme(legend.position = 'none')


leg <- cowplot::get_legend(kop_plot + theme(legend.position = 'bottom'))


cowplot::plot_grid(kop_plot, 
                   metastab_plot, 
                   fo_plot, 
                   dt_plot, 
                   leg,
                   ncol = 1, 
                   rel_heights = c(1, 1, 1, 1, 0.1))

## ---- transitions --------
allTransitions <- dfm %>% 
  dplyr::select(ttime, 
                sub, 
                pma, 
                cluster, 
                pt, 
                sex, 
                motparam_fd_outlier_count) %>% 
  dplyr::filter(pt == 'Term') %>% 
  stateR::clusters_markov(., 
                  vars = c('sub', 
                           'pma', 
                           'sex', 
                           'pt',
                           'motparam_fd_outlier_count'), 
                  cVar = c('cluster'), 
                  sortBy = c('sub', 'ttime'), 
                  groupBy = c('sub'), 
                  remIntra = FALSE) %>% 
  dplyr::mutate(mk = list(map_df(data, 
                                 ~grouped_perm_glm(.x, 
                                                   formla = nCount ~ sex + pma + motparam_fd_outlier_count, 
                                                   var_to_perm = 'nCount', 
                                                   permNum = params$nPerms, 
                                                   seed = params$seed)))) %>% 
  tidyr::unnest_wider(mk) %>% 
  tidyr::unnest(-c(tag, data)) %>%  
  dplyr::group_by(term) %>% 
  dplyr::mutate(cor_p = stats::p.adjust(p.perm, 
                                        method = 'fdr', 
                                        n = length(p.perm))) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(cor_p = ifelse(cor_p >= 0.05, 
                               NA, 
                               cor_p))


intra <- allTransitions %>% 
  tidyr::unnest(data) %>% 
  dplyr::group_by(source, target, tag) %>% 
  dplyr::summarise(nCount = mean(nCount)) %>% 
  dplyr::ungroup() %>% 
  
  ggplot2::ggplot(aes(x = source, 
                      y = ordered(target, 
                                  levels = rev(sort(unique(target)))), 
                      fill = as.numeric(nCount))) + 
  geom_tile() + 
  geom_shadowtext(aes(label = label_percent(accuracy = 0.01)(nCount)), 
                  colour = I("white")) + 
  scale_fill_gradientn(name = 'Pct.',
                       colours = rev(scales[1:5]), 
                       label = label_percent(accuracy = 0.01),
                       trans = "log10", 
                       na.value = "transparent") + 
  scale_x_discrete(limits = c(1, 2, 3, 4), 
                   labels = labels) + 
  scale_y_discrete(labels = labels, 
                   limits = rev(c('1', '2', '3', '4'))) + 
  xlab('Source state') + 
  ylab('Target state')  + 
  theme_minimal() + 
  theme(axis.text.y = element_markdown(color = "black", 
                                       size = 11),
        axis.text.x = element_markdown(color = "black", 
                                       size = 11),
        legend.position = 'none')


allTransitions$statistic[is.na(allTransitions$cor_p)] <- NA

allTransitions <- allTransitions %>% 
  dplyr::filter(term != '(Intercept)') %>% 
  dplyr::mutate(stat = format(round(statistic, 
                                    digits = 3), 
                              nsmall = 3)) %>% 
  dplyr::mutate(p = format(round(cor_p, 
                                 digits = 3), 
                           nsmall = 3)) %>% 
  dplyr::mutate(term = recode(term, 
                              'pma' = 'PMA', 
                              'motparam_fd_outlier_count' = 'Motion', 
                              'ptPreterm' = 'Preterm-born', 
                              'sexmale' = 'Sex')) %>% 
  dplyr::mutate(s = stat)

allTransitions$p[is.na(allTransitions$cor_p)] <- ''
allTransitions$s[is.na(allTransitions$cor_p)] <- ''

allTransitions$p[allTransitions$p == '0.000'] <- '<0.001'


allTransTermPMA_Net_8 <- allTransitions %>% 
  dplyr::filter(term == 'PMA') %>% 
  plot_brainNet(., 
                k = params$clusNum, 
                scale_fct = 20, 
                ttl = element_blank(), 
                sttl = 'Transitions influenced by PMA', 
                pal = scales, 
                lbl = labRetag) + 
  theme(legend.position = 'none')


allTransitionsCA <- dfm %>% 
  dplyr::select(ttime, 
                sub, 
                pma, 
                ga, 
                cluster, 
                pt, 
                sex, 
                motparam_fd_outlier_count) %>% 
  dplyr::mutate(ca = 7*(pma - ga)) %>% 
  dplyr::filter(pt == 'Term') %>% 
  stateR::clusters_markov(., 
                  vars = c('sub', 
                           'pma', 
                           'ca',
                           'pt', 
                           'sex', 
                           'motparam_fd_outlier_count'), 
                  cVar = c('cluster'), 
                  sortBy = c('sub', 'ttime'), 
                  groupBy = c('sub'), 
                  remIntra = FALSE) %>% 
  dplyr::mutate(mk = list(map_df(data, 
                                 ~grouped_perm_glm(.x, 
                                                   formla = nCount ~ sex + ca + pma + motparam_fd_outlier_count, 
                                                   var_to_perm = 'nCount', 
                                                   permNum = params$nPerms, 
                                                   seed = params$seed)))) %>% 
  tidyr::unnest_wider(mk) %>% 
  tidyr::unnest(-c(tag, data)) %>%  
  dplyr::group_by(term) %>% 
  dplyr::mutate(cor_p = stats::p.adjust(p.perm, 
                                        method = 'fdr', 
                                        n = length(p.perm))) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(cor_p = ifelse(cor_p >= 0.05, 
                               NA, 
                               p.perm))



allTransitionsCA$statistic[is.na(allTransitionsCA$cor_p)] <- NA

allTransitionsCA <- allTransitionsCA %>% 
  dplyr::filter(term != '(Intercept)') %>% 
  dplyr::mutate(stat = format(round(statistic, 
                                    digits = 3), 
                              nsmall = 3)) %>% 
  dplyr::mutate(p = format(round(cor_p, 
                                 digits = 3), 
                           nsmall = 3)) %>% 
  dplyr::mutate(term = recode(term, 
                              'pma' = 'PMA', 
                              'motparam_fd_outlier_count' = 'Motion', 
                              'ca' = 'Postnatal days',
                              'sexmale' = 'Sex')) %>% 
  dplyr::mutate(s = stat)

models_ca <- allTransitionsCA

allTransitionsCA$p[is.na(allTransitionsCA$cor_p)] <- ''
allTransitionsCA$s[is.na(allTransitionsCA$cor_p)] <- ''

allTransitionsCA$p[allTransitionsCA$p == '0.000'] <- '<0.001'


allTransPMAPMA_Net <- allTransitionsCA %>% 
  dplyr::filter(term == 'PMA') %>% 
  plot_brainNet(., 
                k = params$clusNum, 
                scale_fct = 20, 
                ttl = element_blank(), 
                sttl = 'Transitions influenced by PMA* at scan', 
                pal = met.brewer("Archambault", 11)[1], 
                lbl = labRetag)  + 
  theme(legend.position = 'none')


allTransPMACA_Net <- allTransitionsCA %>% 
  dplyr::filter(term == 'Postnatal days') %>% 
  plot_brainNet(., 
                k = params$clusNum, 
                scale_fct = 20, 
                ttl = element_blank(), 
                sttl = 'Transitions influenced by PND* at scan', 
                pal = met.brewer("Archambault", 11)[8], 
                lbl = labRetag)  + 
  theme(legend.position = 'none')



allTransitionsPT <- dfm %>% 
  dplyr::select(ttime, 
                sub, 
                pma, 
                cluster, 
                pt, 
                sex, 
                motparam_fd_outlier_count) %>% 
  dplyr::mutate(pt = as.factor(pt)) %>% 
  dplyr::mutate(pt = relevel(pt, ref = 'Term')) %>% 
  stateR::clusters_markov(., 
                  vars = c('sub', 
                           'pma', 
                           'pt', 
                           'sex', 
                           'motparam_fd_outlier_count'), 
                  cVar = c('cluster'), 
                  sortBy = c('sub', 'ttime'), 
                  groupBy = c('sub'), 
                  remIntra = FALSE) %>% 
  dplyr::mutate(mk = list(map_df(data, 
                                 ~grouped_perm_glm(.x, 
                                                   formla = nCount ~ pt + sex + pma + motparam_fd_outlier_count, 
                                                   var_to_perm = 'nCount', 
                                                   permNum = params$nPerms, 
                                                   seed = params$seed)))) %>% 
  tidyr::unnest_wider(mk) %>% 
  tidyr::unnest(-c(tag, data)) %>%  
  dplyr::group_by(term) %>% 
  dplyr::mutate(cor_p = stats::p.adjust(p.perm, 
                                        method = 'fdr', 
                                        n = length(p.perm))) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(cor_p = ifelse(cor_p >= 0.05, 
                               NA, 
                               p.perm))


allTransitionsPT$statistic[is.na(allTransitionsPT$cor_p)] <- NA

allTransitionsPT <- allTransitionsPT %>% 
  dplyr::filter(term != '(Intercept)') %>% 
  dplyr::mutate(stat = format(round(statistic, 
                                    digits = 3), 
                              nsmall = 3)) %>% 
  dplyr::mutate(p = format(round(cor_p, 
                                 digits = 3), 
                           nsmall = 3)) %>% 
  dplyr::mutate(term = recode(term, 
                              'pma' = 'PMA', 
                              'motparam_fd_outlier_count' = 'Motion', 
                              'ptPreterm' = 'Preterm-born', 
                              'sexmale' = 'Sex')) %>% 
  dplyr::mutate(s = stat)

allTransitionsPT$p[is.na(allTransitionsPT$cor_p)] <- ''
allTransitionsPT$s[is.na(allTransitionsPT$cor_p)] <- ''

allTransitionsPT$p[allTransitionsPT$p == '0.000'] <- '<0.001'

models_pt <- allTransitionsPT


allTransTerm_Net <- allTransitionsPT %>% 
  dplyr::filter(term == 'Preterm-born') %>% 
  plot_brainNet(., 
                k = params$clusNum, 
                scale_fct = 20, 
                ttl = element_blank(), 
                sttl = 'Transitions influenced by prematurity (Preterm-born**)', 
                pal = met.brewer("Archambault", 11)[6], 
                lbl = labRetag)  + 
  theme(legend.position = 'none')



allTransitionsGA <- dfm %>% 
  dplyr::select(ttime, 
                sub, 
                pt, 
                pma, 
                cluster, 
                ga, 
                sex, 
                motparam_fd_outlier_count) %>% 
  stateR::clusters_markov(., 
                  vars = c('sub', 
                           'pma', 
                           'ga', 
                           'pt', 
                           'sex', 
                           'motparam_fd_outlier_count'), 
                  cVar = c('cluster'), 
                  sortBy = c('sub', 'ttime'), 
                  groupBy = c('sub'), 
                  remIntra = FALSE) %>% 
  dplyr::mutate(mk = list(map_df(data, 
                                 ~grouped_perm_glm(.x, 
                                                   formla = nCount ~ ga + sex + pma + motparam_fd_outlier_count, 
                                                   var_to_perm = 'nCount', 
                                                   permNum = params$nPerms, 
                                                   seed = params$seed)))) %>% 
  tidyr::unnest_wider(mk) %>% 
  tidyr::unnest(-c(tag, data)) %>%  
  dplyr::group_by(term) %>% 
  dplyr::mutate(cor_p = stats::p.adjust(p.perm, 
                                        method = 'fdr', 
                                        n = length(p.perm))) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(cor_p = ifelse(cor_p >= 0.05, 
                               NA, 
                               p.perm))

allTransitionsGA$statistic[is.na(allTransitionsGA$cor_p)] <- NA

allTransitionsGA <- allTransitionsGA %>% 
  dplyr::filter(term != '(Intercept)') %>% 
  dplyr::mutate(stat = format(round(statistic, 
                                    digits = 3), 
                              nsmall = 3)) %>% 
  dplyr::mutate(p = format(round(cor_p, 
                                 digits = 3), 
                           nsmall = 3)) %>% 
  dplyr::mutate(term = recode(term, 
                              'pma' = 'PMA', 
                              'motparam_fd_outlier_count' = 'Motion', 
                              'ga' = 'GA', 
                              'sexmale' = 'Sex')) %>% 
  dplyr::mutate(s = stat)

allTransitionsGA$p[is.na(allTransitionsGA$cor_p)] <- ''
allTransitionsGA$s[is.na(allTransitionsGA$cor_p)] <- ''

allTransitionsGA$p[allTransitionsGA$p == '0.000'] <- '<0.001'

models_ga <- allTransitionsGA

allTransGA_Net <- allTransitionsGA %>% 
  dplyr::filter(term == 'GA') %>% 
  plot_brainNet(., 
                k = params$clusNum, 
                scale_fct = 20, 
                ttl = element_blank(), 
                sttl = 'Transitions influenced by GA***', 
                pal = met.brewer("Archambault", 11)[6], 
                lbl = labRetag)  + 
  theme(legend.position = 'none')

legend <- cowplot::get_legend(
  allTransGA_Net + 
    theme(legend.position = "bottom") + 
    guides(colour = guide_colourbar(barwidth = 15, 
                                    barheight = 1.5))
)

legend2 <- cowplot::get_legend(
  centr_p + 
    theme_bw(base_size = 14, 
             base_family = "Helvetica") + 
    theme(legend.position = "bottom") + 
    guides(fill = guide_colourbar(barwidth = 15, 
                                  barheight = 1.5, 
                                  title = 'LEiDA'))
)

big_leg <- cowplot::plot_grid(legend, legend2, rel_widths = c(1, 0.5), ncol = 2)


pre_plot_intra_term <- cowplot::plot_grid(allTransPMAPMA_Net, 
                                     allTransPMACA_Net, 
                                     ncol = 2, 
                                     labels = c('e','f'))

pre_plot_intra <- cowplot::plot_grid(allTransTerm_Net, 
                                     allTransGA_Net, 
                                     ncol = 2, 
                                     labels = c('c','d'))

## ---- state_glms --------
meanSync_state_glm <- dfm %>% 
  dplyr::select(sub,
                ses,
                cluster,
                pma,
                ga,
                sex,
                motparam_fd_outlier_count,
                pt,
                ensembleSync,
                ensembleMetastab,
                ensembleEntropy) %>% 
  dplyr::mutate(pt = as.factor(pt)) %>% 
  dplyr::mutate(pt = relevel(pt, ref = 'Term')) %>% 
  dplyr::mutate(cluster = as.character(cluster)) %>% 
  dplyr::group_by(cluster, sub, pt, ga, sex, pma, motparam_fd_outlier_count) %>% 
  dplyr::summarise(syncMed = mean(ensembleSync),
                   metastabMed = sd(ensembleSync)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(cluster) %>% 
  tidyr::nest() %>% 
  dplyr::mutate(mk = list(map_df(data, 
                                 ~grouped_perm_glm(
                                   .x, 
                                   formla = syncMed ~ pt + sex + pma + motparam_fd_outlier_count, 
                                   var_to_perm = 'syncMed', 
                                   permNum = params$nPerms, 
                                   seed = params$seed)))) %>% 
  tidyr::unnest_wider(mk) %>% 
  tidyr::unnest(-c(data)) %>% 
  dplyr::mutate(var = 'meanKOP') %>% 
  dplyr::filter(term == 'ptPreterm')

meanSync_state_glm_ga <- dfm %>% 
  dplyr::select(sub,
                ses,
                cluster,
                pma,
                ga,
                sex,
                motparam_fd_outlier_count,
                pt,
                ensembleSync,
                ensembleMetastab,
                ensembleEntropy) %>% 
  dplyr::mutate(cluster = as.character(cluster)) %>% 
  dplyr::group_by(cluster, sub, pt, ga, sex, pma, motparam_fd_outlier_count) %>% 
  dplyr::summarise(syncMed = mean(ensembleSync),
                   metastabMed = sd(ensembleSync)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(cluster) %>% 
  tidyr::nest() %>% 
  dplyr::mutate(mk = list(map_df(data, 
                                 ~grouped_perm_glm(
                                   .x, 
                                   formla = syncMed ~ ga + sex + pma + motparam_fd_outlier_count, 
                                   var_to_perm = 'syncMed', 
                                   permNum = params$nPerms, 
                                   seed = params$seed)))) %>% 
  tidyr::unnest_wider(mk) %>% 
  tidyr::unnest(-c(data)) %>% 
  dplyr::mutate(var = 'meanKOP') %>% 
  dplyr::filter(term == 'ga')


meanSync_state_glm_term <- dfm %>% 
  dplyr::select(sub,
                ses,
                cluster,
                pma,
                ga,
                sex,
                motparam_fd_outlier_count,
                pt,
                ensembleSync,
                ensembleMetastab,
                ensembleEntropy) %>% 
  dplyr::mutate(pt = as.factor(pt)) %>% 
  dplyr::mutate(cluster = as.character(cluster)) %>% 
  dplyr::group_by(cluster, sub, pt, ga, sex, pma, motparam_fd_outlier_count) %>% 
  dplyr::summarise(syncMed = mean(ensembleSync),
                   metastabMed = sd(ensembleSync)) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(pt == 'Term') %>% 
  dplyr::mutate(ca = 7*(pma - ga)) %>% 
  dplyr::group_by(cluster) %>% 
  tidyr::nest() %>% 
  dplyr::mutate(mk = list(map_df(data, 
                                 ~grouped_perm_glm(
                                   .x, 
                                   formla = syncMed ~ sex + pma + ca + motparam_fd_outlier_count, 
                                   var_to_perm = 'syncMed', 
                                   permNum = params$nPerms, 
                                   seed = params$seed)))) %>% 
  tidyr::unnest_wider(mk) %>% 
  tidyr::unnest(-c(data)) %>% 
  dplyr::mutate(var = 'meanKOP') %>% 
  dplyr::filter(term %in% c('ca', 'pma'))


metastab_state_glm <- dfm %>% 
  dplyr::select(sub,
                ses,
                cluster,
                pma,
                ga,
                sex,
                motparam_fd_outlier_count,
                pt,
                ensembleSync,
                ensembleMetastab,
                ensembleEntropy) %>% 
  dplyr::mutate(pt = as.factor(pt)) %>% 
  dplyr::mutate(pt = relevel(pt, ref = 'Term')) %>% 
  dplyr::mutate(cluster = as.character(cluster)) %>% 
  dplyr::group_by(cluster, sub, pt, ga, sex, pma, motparam_fd_outlier_count) %>% 
  dplyr::summarise(syncMed = mean(ensembleSync),
                   metastabMed = sd(ensembleSync)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(cluster) %>% 
  tidyr::nest() %>% 
  dplyr::mutate(mk = list(map_df(
    data, 
    ~grouped_perm_glm(
      .x, 
      formla = metastabMed ~ pt + sex + pma + motparam_fd_outlier_count, 
      var_to_perm = 'metastabMed', 
      permNum = params$nPerms, 
      seed = params$seed
    )))) %>% 
  tidyr::unnest_wider(mk) %>% 
  tidyr::unnest(-c(data)) %>% 
  dplyr::mutate(var = 'metastab') %>% 
  dplyr::filter(term == 'ptPreterm')

metastab_state_glm_ga <- dfm %>% 
  dplyr::select(sub,
                ses,
                cluster,
                pma,
                ga,
                sex,
                motparam_fd_outlier_count,
                pt,
                ensembleSync,
                ensembleMetastab,
                ensembleEntropy) %>% 
  dplyr::mutate(cluster = as.character(cluster)) %>% 
  dplyr::group_by(cluster, sub, pt, ga, sex, pma, motparam_fd_outlier_count) %>% 
  dplyr::summarise(syncMed = mean(ensembleSync),
                   metastabMed = sd(ensembleSync)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(cluster) %>% 
  tidyr::nest() %>% 
  dplyr::mutate(mk = list(map_df(
    data, 
    ~grouped_perm_glm(
      .x, 
      formla = metastabMed ~ ga + sex + pma + motparam_fd_outlier_count, 
      var_to_perm = 'metastabMed', 
      permNum = params$nPerms, 
      seed = params$seed
    )))) %>% 
  tidyr::unnest_wider(mk) %>% 
  tidyr::unnest(-c(data)) %>% 
  dplyr::mutate(var = 'metastab') %>% 
  dplyr::filter(term == 'ga')



metastab_state_glm_term <- dfm %>% 
  dplyr::select(sub,
                ses,
                cluster,
                pma,
                ga,
                sex,
                motparam_fd_outlier_count,
                pt,
                ensembleSync,
                ensembleMetastab,
                ensembleEntropy) %>% 
  dplyr::mutate(pt = as.factor(pt)) %>% 
  dplyr::mutate(cluster = as.character(cluster)) %>% 
  dplyr::group_by(cluster, sub, pt, ga, sex, pma, motparam_fd_outlier_count) %>% 
  dplyr::summarise(syncMed = mean(ensembleSync),
                   metastabMed = sd(ensembleSync)) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(pt == 'Term') %>% 
  dplyr::mutate(ca = 7*(pma - ga)) %>% 
  dplyr::group_by(cluster) %>% 
  tidyr::nest() %>% 
  dplyr::mutate(mk = list(map_df(data, 
                                 ~grouped_perm_glm(
                                   .x, 
                                   formla = metastabMed ~ sex + pma + ca + motparam_fd_outlier_count, 
                                   var_to_perm = 'syncMed', 
                                   permNum = params$nPerms, 
                                   seed = params$seed)))) %>% 
  tidyr::unnest_wider(mk) %>% 
  tidyr::unnest(-c(data)) %>% 
  dplyr::mutate(var = 'metastab') %>% 
  dplyr::filter(term %in% c('ca', 'pma'))

fo_state_glm <- dfm %>% 
  dplyr::mutate(pt = as.factor(pt)) %>% 
  dplyr::mutate(pt = relevel(pt, ref = 'Term')) %>% 
  dplyr::select(ttime,
                sub, 
                pma, 
                cluster,
                pt,
                sex,
                motparam_fd_outlier_count) %>% 
  stateR::nest_fo(., 
          vars = c('sub', 
                   'pma', 
                   'pt', 
                   'sex', 
                   'motparam_fd_outlier_count'), 
          foVar = 'cluster') %>% 
  dplyr::mutate(cc = list(map_df(data, 
                                 ~grouped_perm_glm(.x, 
                                                   formla = perc ~ 
                                                     pt + 
                                                     sex + 
                                                     pma + 
                                                     motparam_fd_outlier_count, 
                                                   var_to_perm = 'perc', 
                                                   permNum = params$nPerms, 
                                                   seed = params$seed)))) %>% 
  tidyr::unnest_wider(cc) %>% 
  tidyr::unnest(-c(cluster, data)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(var = 'fo') %>% 
  dplyr::filter(term == 'ptPreterm')

fo_state_glm_ga <- dfm %>% 
  dplyr::select(ttime, 
                sub, 
                pma, 
                cluster, 
                ga, 
                sex, 
                motparam_fd_outlier_count) %>% 
  stateR::nest_fo(., 
          vars = c('sub', 
                   'pma', 
                   'ga', 
                   'sex', 
                   'motparam_fd_outlier_count'), 
          foVar = 'cluster') %>% 
  dplyr::mutate(cc = list(map_df(data, 
                                 ~grouped_perm_glm(.x, 
                                                   formla = perc ~ 
                                                     ga + 
                                                     sex + 
                                                     pma + 
                                                     motparam_fd_outlier_count, 
                                                   var_to_perm = 'perc', 
                                                   permNum = params$nPerms, 
                                                   seed = params$seed)))) %>% 
  tidyr::unnest_wider(cc) %>% 
  tidyr::unnest(-c(cluster, data)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(var = 'fo') %>% 
  dplyr::filter(term == 'ga')

fo_state_glm_term <- dfm %>% 
  dplyr::mutate(pt = as.factor(pt)) %>% 
  dplyr::select(ttime, 
                sub, 
                pma, 
                ga, 
                cluster, 
                pt, 
                sex, 
                motparam_fd_outlier_count) %>% 
  dplyr::filter(pt == 'Term') %>% 
  dplyr::mutate(ca = 7*(pma - ga)) %>% 
  stateR::nest_fo(., 
          vars = c('sub', 
                   'pma', 
                   'ca', 
                   'sex', 
                   'motparam_fd_outlier_count'), 
          foVar = 'cluster') %>% 
  dplyr::mutate(cc = list(map_df(data, 
                                 ~grouped_perm_glm(.x, 
                                                   formla = perc ~ 
                                                     sex + 
                                                     pma + 
                                                     ca + 
                                                     motparam_fd_outlier_count, 
                                                   var_to_perm = 'perc', 
                                                   permNum = params$nPerms, 
                                                   seed = params$seed)))) %>% 
  tidyr::unnest_wider(cc) %>% 
  tidyr::unnest(-c(cluster, data)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(var = 'fo') %>% 
  dplyr::filter(term %in% c('ca', 'pma'))


dt_state_glm <- dfm %>% 
  dplyr::mutate(pt = as.factor(pt)) %>% 
  dplyr::mutate(pt = relevel(pt, ref = 'Term')) %>% 
  dplyr::select(ttime, 
                sub, 
                pma, 
                cluster, 
                pt, 
                sex, 
                motparam_fd_outlier_count) %>% 
  stateR::nest_dwell(., vars = c('sub', 
                         'pma', 
                         'pt', 
                         'sex', 
                         'motparam_fd_outlier_count'), 
             foVar = 'cluster', 
             sortBy = c('sub','ttime')) %>% 
  dplyr::mutate(dw = list(map_df(data, 
                                 ~grouped_perm_glm(.x, 
                                                   formla = mean_dwell ~
                                                     pt + 
                                                     sex + 
                                                     pma + 
                                                     motparam_fd_outlier_count, 
                                                   var_to_perm = 'mean_dwell', 
                                                   permNum = params$nPerms, 
                                                   seed = params$seed)))) %>% 
  tidyr::unnest_wider(dw) %>% 
  tidyr::unnest(-c(cluster, data)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(var = 'dt') %>% 
  dplyr::filter(term == 'ptPreterm')

dt_state_glm_ga <- dfm %>% 
  dplyr::select(ttime, 
                sub, 
                pma, 
                cluster, 
                ga, 
                sex, 
                motparam_fd_outlier_count) %>% 
  stateR::nest_dwell(., vars = c('sub', 
                         'pma', 
                         'ga', 
                         'sex', 
                         'motparam_fd_outlier_count'), 
             foVar = 'cluster', 
             sortBy = c('sub','ttime')) %>% 
  dplyr::mutate(dw = list(map_df(data, 
                                 ~grouped_perm_glm(.x, 
                                                   formla = mean_dwell ~
                                                     ga + 
                                                     sex + 
                                                     pma + 
                                                     motparam_fd_outlier_count, 
                                                   var_to_perm = 'mean_dwell', 
                                                   permNum = params$nPerms, 
                                                   seed = params$seed)))) %>% 
  tidyr::unnest_wider(dw) %>% 
  tidyr::unnest(-c(cluster, data)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(var = 'dt') %>% 
  dplyr::filter(term == 'ga')

dt_state_glm_term <- dfm %>% 
  dplyr::mutate(pt = as.factor(pt)) %>% 
  dplyr::select(ttime, 
                sub, 
                pma, 
                cluster, 
                ga, 
                pt, 
                sex, 
                motparam_fd_outlier_count) %>% 
  dplyr::filter(pt == 'Term') %>% 
  dplyr::mutate(ca = 7*(pma - ga)) %>% 
  stateR::nest_dwell(., vars = c('sub', 
                         'pma', 
                         'ca', 
                         'sex', 
                         'motparam_fd_outlier_count'), 
             foVar = 'cluster', 
             sortBy = c('sub','ttime')) %>% 
  dplyr::mutate(dw = list(map_df(data, 
                                 ~grouped_perm_glm(.x, 
                                                   formla = mean_dwell ~
                                                     sex + 
                                                     pma + 
                                                     ca + 
                                                     motparam_fd_outlier_count, 
                                                   var_to_perm = 'mean_dwell', 
                                                   permNum = params$nPerms, 
                                                   seed = params$seed)))) %>% 
  tidyr::unnest_wider(dw) %>% 
  tidyr::unnest(-c(cluster, data)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(var = 'dt') %>% 
  dplyr::filter(term %in% c('ca', 'pma'))

state_glm_term <- rbind(meanSync_state_glm_term,
                        metastab_state_glm_term,
                        fo_state_glm_term, 
                        dt_state_glm_term)

state_glm <- rbind(meanSync_state_glm,
                   meanSync_state_glm_ga,
                   metastab_state_glm, 
                   metastab_state_glm_ga, 
                   fo_state_glm, 
                   fo_state_glm_ga, 
                   dt_state_glm, 
                   dt_state_glm_ga)


models_term <- state_glm_term %>% 
  dplyr::group_by(term, var) %>% 
  dplyr::mutate(pp.cor = p.adjust(p.perm, 
                                  method = 'fdr', 
                                  n = length(p.perm))) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(filt.t = if_else(pp.cor < 0.05, statistic, NA_real_), 
                filt.p = if_else(pp.cor < 0.05, p.perm, NA_real_), 
                pp.for = glue::glue('p = {filt.p}'), 
                pp.for = recode(pp.for, 
                                'p = 0' = 'p < 0.001'), 
                pp.for = if_else(pp.for == 'p = NA', NA_character_, pp.for)) %>% 
  dplyr::select(c(cluster, var, filt.t, p.perm, filt.p, pp.for, term)) %>% 
  dplyr::mutate(term = dplyr::recode(term, 
                                     'ca' = 'PND*', 
                                     'pma' = 'PMA*')) %>% 
  dplyr::mutate(var = recode(var, 'dt' = 'Dwelling times', 
                             'meanKOP' = 'Mean KOP', 
                             'fo' = 'Fractional Occupancy', 
                             'metastab' = 'Metastability'))

models <- state_glm %>% 
  dplyr::group_by(term, var) %>% 
  dplyr::mutate(pp.cor = p.adjust(p.perm, 
                                  method = 'fdr', 
                                  n = length(p.perm))) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(filt.t = if_else(pp.cor < 0.05, statistic, NA_real_), 
                filt.p = if_else(pp.cor < 0.05, p.perm, NA_real_), 
                pp.for = glue::glue('p = {filt.p}'), 
                pp.for = recode(pp.for, 
                                'p = 0' = 'p < 0.001'), 
                pp.for = if_else(pp.for == 'p = NA', NA_character_, pp.for)) %>% 
  dplyr::select(c(cluster, var, filt.t, p.perm, filt.p, pp.for, term)) %>% 
  dplyr::mutate(term = dplyr::recode(term,  
                                     'ptPreterm' = 'Born Preterm**', 
                                     'ga' = 'GA***')) %>% 
  dplyr::mutate(var = recode(var, 'dt' = 'Dwelling times', 
                             'meanKOP' = 'Mean KOP', 
                             'fo' = 'Fractional Occupancy', 
                             'metastab' = 'Metastability'))
  
models_term <- models_term %>% 
  dplyr::mutate(cluster = as.factor(cluster)) 

models <- models %>% 
  dplyr::mutate(cluster = as.factor(cluster)) 

models_term_ <- models_term
models_ <- models

models_term_pma <- models_term %>% 
  dplyr::mutate(for.t = as.character(format(round(filt.t, 2), nsmall = 2))) %>% 
  dplyr::mutate(for.t = dplyr::if_else(is.na(filt.t), NA_character_, for.t)) %>% 
  dplyr::filter(term == 'PMA*') %>% 
  dplyr::mutate(var = dplyr::recode(var, 'Mean KOP' = 'Mean synchronisation')) %>%
  
  ggplot2::ggplot(aes(x = var, 
                      y = cluster)) + 
  geom_point(aes(size = abs(filt.t)), 
             pch = 21, 
             stroke = 2, 
             colour = met.brewer("Archambault", 11)[1]) + 
  geom_text(aes(label = for.t), 
                  colour = I("black")) + 
  scale_colour_gradientn(colours = scales, 
                         name = 't-value', 
                         limits = c(-6, 6)) + 
  scale_size(range = c(10, 20), 
             limits = c(-6, 6), 
             guide = NULL) + 
  theme_minimal() + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + 
  scale_y_discrete(labels = labels, 
                   limits = rev(levels(models_term$cluster))) + 
  xlab(NULL) + 
  ylab('State') + 
  labs(title = 'PMA* at scan') + 
  theme(axis.text.y = element_markdown(color = "black", 
                                       size = 11), 
        legend.position = 'none')


models_term_pnd <- models_term %>% 
  dplyr::mutate(for.t = as.character(format(round(filt.t, 2), nsmall = 2))) %>% 
  dplyr::mutate(for.t = dplyr::if_else(is.na(filt.t), NA_character_, for.t)) %>% 
  dplyr::filter(term == 'PND*') %>% 
  dplyr::mutate(var = dplyr::recode(var, 'Mean KOP' = 'Mean synchronisation')) %>%
  
  ggplot2::ggplot(aes(x = var, 
                      y = cluster)) + 
  geom_point(aes(size = abs(filt.t)), 
             pch = 21, 
             stroke = 2, 
             colour = met.brewer("Archambault", 11)[8]) + 
  geom_text(aes(label = for.t), 
            colour = I("black")) + 
  scale_colour_gradientn(colours = scales, 
                         name = 't-value', 
                         limits = c(-6, 6)) + 
  scale_size(range = c(10, 20), 
             limits = c(-6, 6), 
             guide = NULL) + 
  theme_minimal() + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + 
  scale_y_discrete(labels = labels, 
                   limits = rev(levels(models_term$cluster))) + 
  xlab(NULL) + 
  ylab('State') + 
  labs(title = 'PND* at scan') + 
  theme(axis.text.y = element_markdown(color = "black", 
                                       size = 11), 
        legend.position = 'none')


models_pt_ <- models %>% 
  dplyr::mutate(for.t = as.character(format(round(filt.t, 2), nsmall = 2))) %>% 
  dplyr::mutate(for.t = dplyr::if_else(is.na(filt.t), NA_character_, for.t)) %>% 
  dplyr::filter(term == 'Born Preterm**') %>% 
  dplyr::mutate(var = dplyr::recode(var, 'Mean KOP' = 'Mean synchronisation')) %>%
  
  
  ggplot2::ggplot(aes(x = var, 
                      y = cluster)) + 
  geom_point(aes(size = abs(filt.t)), 
             pch = 21, 
             stroke = 2, 
             colour = met.brewer("Archambault", 11)[6]) + 
  geom_text(aes(label = for.t), 
            colour = I("black")) + 
  scale_colour_gradientn(colours = scales, 
                         name = 't-value', 
                         limits = c(-6, 6)) + 
  scale_size(range = c(10, 20), 
             limits = c(-6, 6), 
             guide = NULL) + 
  theme_minimal() + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + 
  scale_y_discrete(labels = labels, 
                   limits = rev(levels(models$cluster))) + 
  xlab(NULL) + 
  ylab('State') + 
  labs(title = bquote('Preterm-birth' ^`††`)) + 
  theme(axis.text.y = element_markdown(color = "black", 
                                       size = 11), 
        legend.position = 'none')

models_ga_ <- models %>% 
  dplyr::mutate(for.t = as.character(format(round(filt.t, 2), nsmall = 2))) %>% 
  dplyr::mutate(for.t = dplyr::if_else(is.na(filt.t), NA_character_, for.t)) %>% 
  dplyr::filter(term == 'GA***') %>% 
  dplyr::mutate(var = dplyr::recode(var, 'Mean KOP' = 'Mean synchronisation')) %>% 
  
  ggplot2::ggplot(aes(x = var, 
                      y = cluster)) + 
  geom_point(aes(size = abs(filt.t)), 
             pch = 21, 
             stroke = 2, 
             colour = met.brewer("Archambault", 11)[6]) + 
  geom_text(aes(label = for.t), 
            colour = I("black")) + 
  scale_colour_gradientn(colours = scales, 
                         name = 't-value', 
                         limits = c(-6, 6)) + 
  scale_size(range = c(10, 20), 
             limits = c(-6, 6), 
             guide = NULL) + 
  theme_minimal() + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + 
  scale_y_discrete(labels = labels, 
                   limits = rev(levels(models$cluster))) + 
  xlab(NULL) + 
  ylab('State') + 
  labs(title = 'GA*** at birth' ) + 
  theme(axis.text.y = element_markdown(color = "black", 
                                       size = 11), 
        legend.position = 'none')


legend <- cowplot::get_legend(
  models_term_pma + 
    theme(legend.position = "bottom") + 
    guides(colour = guide_colourbar(barwidth = 15, 
                                    barheight = 1.5))
)

## ---- modelsTerm --------
models_term_plots <- cowplot::plot_grid(models_term_pma, 
                                        models_term_pnd, 
                                        labels = c('c', 'd'), 
                                        nrow = 1)


cowplot::plot_grid(mTrans, 
                   models_term_plots, 
                   pre_plot_intra_term, 
                   ncol = 1, 
                   rel_heights = c(1, 1, 1), 
                   rel_widths = c(1, 1, 1))

## ---- modelsAll --------
models_plots <- cowplot::plot_grid(models_pt_, 
                                   models_ga_, 
                                   labels = c('a', 'b'), 
                                   nrow = 1)



stats_trans <- cowplot::plot_grid(models_pt_, 
                                  allTransTerm_Net, 
                                  labels = c('e', 'f'), 
                                  ncol = 1)



stats_bplot <- cowplot::plot_grid(vplots, 
                                  stats_trans, 
                                  nrow = 1, 
                                  rel_widths = c(1, 0.6), 
                                  align = c('none'))


stats_bplot

## ---- stateRecode --------
dfm <- dfm %>% 
  dplyr::rename('cluster_o' = 'cluster') %>% 
  dplyr::mutate(cluster_o = as.character(cluster_o)) %>% 
  dplyr::mutate(cluster = recode(cluster_o, 
                               '0' = '1', 
                               '1' = '2', 
                               '3' = '3', 
                               '5' = '4', 
                               '4' = '5', 
                               '2' = '6')) %>% 
  dplyr::select(-cluster_o) %>% 
  dplyr::mutate(cluster = as.numeric(cluster))


## ---- meanTrans --------
get_topn <- function(tbl, n){
  
  ttbl <- tbl %>% 
    dplyr::arrange(desc(nCount)) %>% 
    slice_head(n = 3) %>% 
    dplyr::arrange(nCount) %>% 
    slice_head(n = 2)
  
  return(ttbl)
  
}

k = params$clusNum
scale_fct = 20

id = 1:(k)
theta = 2*pi/k*id

states <- tibble::tibble(id, theta, lbl = labRetag) %>% 
  dplyr::mutate(x = scale_fct*cos(theta), 
                y = scale_fct*sin(theta)) %>% 
  dplyr::mutate(image = glue::glue('figs/centr_k{k}_{id}_r_plot.png'))

main_net <- allTransitions %>% 
  tidyr::unnest(data) %>% 
  dplyr::group_by(pt, 
                  source, 
                  target, 
                  tag) %>% 
  dplyr::summarise(nCount = mean(nCount)) %>% 
  dplyr::ungroup() %>% 
  filter(pt == 'Term') %>% 
  mutate(theta_s = 2*pi/k*source,
         source_x = scale_fct*cos(theta_s),
         source_y = scale_fct*sin(theta_s),
         theta_t = 2*pi/k*target,
         target_x = scale_fct*cos(theta_t),
         target_y = scale_fct*sin(theta_t)) %>% 
  dplyr::mutate(rd = pmap(list(source_x, 
                               source_y, 
                               target_x, 
                               target_y, 
                               rr = 0.3*scale_fct), 
                          cl_radii)) %>% 
  tidyr::unnest_wider(rd) %>% 
  dplyr::rename('zz_dx' = 'dx', 
                'zz_dy' = 'dy') %>% 
  dplyr::mutate(rd = pmap(list(target_x, 
                               target_y, 
                               source_x, 
                               source_y, 
                               rr = 0.3*scale_fct), 
                          cl_radii)) %>% 
  tidyr::unnest_wider(rd) %>% 
  dplyr::rename('ss_dx' = 'dx', 
                'ss_dy' = 'dy') %>% 
  dplyr::group_by(source) %>% 
  tidyr::nest() %>% 
  mutate(data_n = list(map_df(data, ~get_topn(.x)))) %>% 
  select(-data) %>% 
  tidyr::unnest(cols = c(data_n))


fig_arr <- ggplot2::ggplot(data = states, 
                           aes(x = x, 
                               y = y)) + 
  geom_image(aes(image = image), 
             size = 0.15) + 
  geom_text(aes(x = 1.6*x, 
                y = 1.6*y, 
                label = lbl), 
            size = 6.5, 
            colour = 'grey50') + 
  xlim(scale_fct*c(-1.65,1.65)) + 
  ylim(scale_fct*c(-1.65,1.65)) + 
  theme_net() + 
  geom_curve(data = main_net, 
             aes(x = ss_dx, 
                 y = ss_dy, 
                 xend = zz_dx, 
                 yend = zz_dy, 
                 size = nCount, 
                 color = nCount), 
             arrow = arrow(length = unit(0.2, "cm"), 
                           type="closed"), 
             curvature = - 0.2) + 
  geom_label_repel(data = main_net, 
                   aes(x = zz_dx - 0.2*zz_dx, 
                       y = zz_dy - 0.2*zz_dy, 
                       label = paste0(as.character(format(round(nCount*100, 2), nsmall = 2)), '%'), 
                       colour = as.numeric(nCount)), 
                   size = 3) + 
  scale_size_continuous(range = c(0.5, 1.5),  
                        labels = scales::percent, 
                        limits = c(0.01, 0.05), 
                        breaks = c(0.01, 0.03, 0.05),
                        name = 'Transition probability') + 
  scale_colour_gradient(low = 'white',
                        high = 'black',
                        limits = c(0.0001,1), 
                        labels = scales::percent, 
                        trans = "log10", 
                        name = element_blank()) + 
  guides(size=guide_legend(override.aes = list(arrow = NULL)))



intra <- allTransitions %>% 
  tidyr::unnest(data) %>% 
  dplyr::group_by(source, target, tag) %>% 
  dplyr::summarise(nCount = mean(nCount)) %>% 
  dplyr::ungroup() %>% 
  
  ggplot2::ggplot(aes(x = source, 
                      y = ordered(target, 
                                  levels = rev(sort(unique(target)))), 
                      fill = as.numeric(nCount))) + 
  geom_tile() + 
  geom_shadowtext(aes(label = label_percent(accuracy = 0.01)(nCount)), 
                  colour = I("white")) + 
  scale_fill_gradient(low = 'white',
                      high = met.brewer("Archambault", 11)[3],
                      limits = c(0.0001,0.05), 
                      labels = scales::percent, 
                      trans = "log10", 
                      name = element_blank()) + 
  scale_x_discrete(limits = seq(1,params$clusNum), 
                   labels = labels) + 
  scale_y_discrete(labels = labels, 
                   limits = rev(as.character(seq(1,params$clusNum)))) + 
  xlab('Source state') + 
  ylab('Target state')  + 
  theme_minimal() + 
  theme(axis.text.y = element_markdown(color = "black", 
                                       size = 11),
        axis.text.x = element_markdown(color = "black", 
                                       size = 11),
        legend.position = 'none')



mTrans <- cowplot::plot_grid(intra, fig_arr, nrow = 1, labels = c('a','b'))

## ---- measuresStates --------
kop_term <- dfm %>% 
  dplyr::select(sub,
                ses,
                cluster,
                pma,
                ga,
                sex,
                motparam_fd_outlier_count,
                pt,
                ensembleSync,
                ensembleMetastab,
                ensembleEntropy) %>% 
  dplyr::mutate(cluster = as.character(cluster)) %>% 
  dplyr::group_by(cluster, sub, pt, ga, sex, pma, motparam_fd_outlier_count) %>% 
  dplyr::summarise(syncMed = mean(ensembleSync),
                   metastabMed = sd(ensembleSync)) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(pt == 'Term')


fo_term <- dfm %>% 
  dplyr::select(ttime, 
                sub, 
                pma, 
                cluster, 
                pt, 
                sex, 
                motparam_fd_outlier_count) %>% 
  stateR::nest_fo(., 
          vars = c('sub', 
                   'pma', 
                   'pt', 
                   'sex', 
                   'motparam_fd_outlier_count'), 
          foVar = 'cluster') %>% 
  tidyr::unnest(data) %>% 
  dplyr::filter(pt == 'Term')

dt_term <- dfm %>% 
  dplyr::select(ttime, 
                sub, 
                pma, 
                cluster, 
                pt, 
                sex, 
                motparam_fd_outlier_count) %>% 
  stateR::nest_dwell(., vars = c('sub', 
                         'pma', 
                         'pt', 
                         'sex', 
                         'motparam_fd_outlier_count'), 
             foVar = 'cluster', 
             sortBy = c('sub','ttime')) %>% 
  tidyr::unnest(data) %>% 
  dplyr::filter(pt == 'Term')

kop_term_long <- kop_term %>% 
  dplyr::inner_join(fo_term) %>% 
  dplyr::inner_join(dt_term) %>% 
  tidyr::pivot_longer(cols = c(syncMed, metastabMed, perc, mean_dwell), 
                      names_to = 'measure', 
                      values_to = 'value') %>% 
  dplyr::mutate(measure = dplyr::recode(measure, 
                                        'mean_dwell' = 'Mean dwelling time', 
                                        'syncMed' = 'Mean KOP', 
                                        'perc' = 'Mean fractional occupancy', 
                                        'metastabMed' = 'Mean metastability'))

lmm <- kop_term_long %>% 
  dplyr::filter(measure == 'Mean KOP') %>% 
  dplyr::mutate(state = as.factor(cluster)) %>% 
  dplyr::mutate(state = relevel(state, ref = 1)) %>% 
  lmerTest::lmer(., 
                 formula = value ~ state + (1 | sub )) 


tb <- summary(lmm)$coefficients[-1,] %>% 
  tibble::as_tibble() %>%
  rstatix::add_significance(p.col = 'Pr(>|t|)', 
                            output.col = 'sig', 
                            cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                            symbols = c("***", "**", "*", "ns")) %>% 
  dplyr::mutate(idx = seq(2, params$clusNum), 
                t = paste0('t = ', 
                           as.character(format(round(`t value`, 
                                                     digits = 3), 
                                               nsmall = 3)), 
                           sig)) %>% 
  dplyr::mutate(t = if_else(sig == 'ns', 'ns', t)) %>% 
  dplyr::inner_join(kop_term_long %>% 
                      dplyr::group_by(cluster, measure) %>% 
                      dplyr::summarise(max = max(value)) %>% 
                      dplyr::filter(measure == 'Mean KOP') %>% 
                      dplyr::select(-measure) %>% 
                      dplyr::mutate(idx = as.numeric(cluster)))

print('Mean KOP')  
anova(lmm)



phoc <- difflsmeans(lmm, test.effs = "state") %>% 
  cbind(.) %>% 
  dplyr::mutate(p.adj = stats::p.adjust(`Pr(>|t|)`, 
                                        method = 'fdr',
                                        n = length(`Pr(>|t|)`))) %>%
  tidyr::separate(col = levels, 
                  into = c("statea", "stateb"), 
                  sep = " - ")

phoc <- phoc %>%
  tibble::as_tibble() %>% 
  dplyr::bind_rows(phoc %>% 
          tibble::as_tibble() %>% 
                      dplyr::rename('stateb' = 'statea', 
                                    'statea' = 'stateb'))
  
phoc_kop <- phoc %>% 
  dplyr::mutate(t = paste0(as.character(format(round(abs(`t value`), 
                                                     digits = 1), 
                                               nsmall = 1)))) %>% 
  dplyr::mutate(t = dplyr::if_else(p.adj >= 0.05, NA_character_, t), 
                `t value` = dplyr::if_else(p.adj >= 0.05, NA_real_, `t value`)) %>% 
  ggplot2::ggplot(aes(x = statea, 
                      y = forcats::fct_relevel(stateb, 
                                               levels = rev(as.character(seq(params$clusNum)))))) + 
  geom_point(aes(size = abs(`t value`), 
                 colour = abs(`t value`))) + 
  geom_shadowtext(aes(label = t), 
                  colour = I("white")) + 
  scale_colour_gradientn(colours = sq1, 
                         name = 't-value', 
                         limits = c(2, 220), 
                         trans = 'log10') + 
  scale_size(range = c(6, 15), 
             limits = c(2,220), 
             guide = NULL, 
             trans = 'log10') + 
  theme_minimal() + 
  #scale_x_discrete(guide = guide_axis(n.dodge = 2)) + 
  scale_x_discrete(labels = labels, 
                   limits = rev(levels(phoc$statea))) + 
  scale_y_discrete(labels = labels, 
                   limits = rev(levels(phoc$statea))) + 
  xlab('State') + 
  ylab('State') + 
  labs(title = 'Mean synchronisation') + 
  theme(axis.text.y = element_markdown(color = "black", 
                                       size = 11), 
        axis.text.x = element_markdown(color = "black", 
                                       size = 11), 
        legend.position = 'none')


kop_term_long_kop <- kop_term_long %>% 
  dplyr::filter(measure == 'Mean KOP') %>% 
  ggplot2::ggplot(aes(x = cluster, 
                      y = value)) + 
  geom_quasirandom(aes(fill = pt), 
                size = 2, 
                pch = 21, 
                colour = 'white') + 
  geom_boxplot(aes(colour = pt), 
               width = 0.15, 
               outlier.shape = NA, 
               position = position_dodge(width = 0.8)) + 
  #geom_text(data = tb, aes(x = idx, y = max + 0.15*max, label = t)) + 
  scale_fill_manual(values = mypal2, 
                    name = 'Prematurity') +
  scale_colour_manual(values = c('#000000', '#000000'), 
                      guide = 'none') + 
  theme_bw() + 
  scale_x_discrete(name = 'State', 
                   labels = labels) + 
  ylab('Mean synchronisation') + 
  theme(axis.text.x = element_markdown(color = "black", 
                                       size = 11), 
        legend.position = 'none')

lmm <- kop_term_long %>% 
  dplyr::filter(measure == 'Mean fractional occupancy') %>% 
  dplyr::mutate(state = as.factor(cluster)) %>% 
  dplyr::mutate(state = relevel(state, ref = 1)) %>% 
  lmerTest::lmer(., 
                 formula = value ~ state + (1 | sub )) 


tb <- summary(lmm)$coefficients[-1,] %>% 
  tibble::as_tibble() %>%
  rstatix::add_significance(p.col = 'Pr(>|t|)', 
                            output.col = 'sig', 
                            cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                            symbols = c("***", "**", "*", "ns")) %>% 
  dplyr::mutate(idx = seq(2,params$clusNum), 
                t = paste0('t = ', 
                           as.character(format(round(`t value`, 
                                                     digits = 3), 
                                               nsmall = 3)), 
                           sig)) %>% 
  dplyr::mutate(t = if_else(sig == 'ns', 'ns', t)) %>% 
  dplyr::inner_join(kop_term_long %>% 
                      dplyr::group_by(cluster, measure) %>% 
                      dplyr::summarise(max = max(value)) %>% 
                      dplyr::filter(measure == 'Mean fractional occupancy') %>% 
                      dplyr::select(-measure) %>% 
                      dplyr::mutate(idx = as.numeric(cluster)))

print('Mean fractional occupancy')
anova(lmm)

phoc <- difflsmeans(lmm, test.effs = "state") %>% 
  cbind(.) %>% 
  dplyr::mutate(p.adj = stats::p.adjust(`Pr(>|t|)`, 
                                        method = 'fdr',
                                        n = length(`Pr(>|t|)`))) %>%
  tidyr::separate(col = levels, 
                  into = c("statea", "stateb"), 
                  sep = " - ")

phoc <- phoc %>%
  tibble::as_tibble() %>% 
  dplyr::bind_rows(phoc %>% 
                     tibble::as_tibble() %>% 
                     dplyr::rename('stateb' = 'statea', 
                                   'statea' = 'stateb'))

phoc_fo <- phoc %>% 
  dplyr::mutate(t = paste0(as.character(format(round(abs(`t value`), 
                                                     digits = 1), 
                                               nsmall = 1)))) %>% 
  dplyr::mutate(t = dplyr::if_else(p.adj >= 0.05, NA_character_, t), 
                `t value` = dplyr::if_else(p.adj >= 0.05, NA_real_, `t value`)) %>% 
  ggplot2::ggplot(aes(x = statea, 
                      y = forcats::fct_relevel(stateb, 
                                               levels = rev(as.character(seq(params$clusNum)))))) + 
  geom_point(aes(size = abs(`t value`), 
                 colour = abs(`t value`))) + 
  geom_shadowtext(aes(label = t), 
                  colour = I("white")) + 
  scale_colour_gradientn(colours = sq1, 
                         name = 't-value', 
                         limits = c(2, 220), 
                         trans = 'log10') + 
  scale_size(range = c(6, 15), 
             limits = c(2,220), 
             guide = NULL, 
             trans = 'log10') + 
  theme_minimal() + 
  #scale_x_discrete(guide = guide_axis(n.dodge = 2)) + 
  scale_x_discrete(labels = labels, 
                   limits = rev(levels(phoc$statea))) + 
  scale_y_discrete(labels = labels, 
                   limits = rev(levels(phoc$statea))) + 
  xlab('State') + 
  ylab('State') + 
  labs(title = 'Fractional occupancy') + 
  theme(axis.text.y = element_markdown(color = "black", 
                                       size = 11), 
        axis.text.x = element_markdown(color = "black", 
                                       size = 11), 
        legend.position = 'none')

kop_term_long_perc <- kop_term_long %>% 
  dplyr::filter(measure == 'Mean fractional occupancy') %>% 
  ggplot2::ggplot(aes(x = cluster, 
                      y = value)) + 
  geom_quasirandom(aes(fill = pt), 
                   size = 2, 
                   pch = 21, 
                   colour = 'white') + 
  geom_boxplot(aes(colour = pt), 
               width = 0.15, 
               outlier.shape = NA, 
               position = position_dodge(width = 0.8)) + 
  #geom_text(data = tb, aes(x = idx, y = max + 0.15*max, label = t)) + 
  scale_fill_manual(values = mypal2, 
                    name = 'Prematurity') +
  scale_colour_manual(values = c('#000000', '#000000'), 
                      guide = 'none') + 
  theme_bw() + 
  scale_x_discrete(name = 'State', 
                   labels = labels) + 
  scale_y_continuous(labels = scales::percent) + 
  ylab('Mean fractional occupancy') + 
  theme(axis.text.x = element_markdown(color = "black", 
                                       size = 11), 
        legend.position = 'none')

lmm <- kop_term_long %>% 
  dplyr::filter(measure == 'Mean dwelling time') %>% 
  dplyr::mutate(state = as.factor(cluster)) %>% 
  dplyr::mutate(state = relevel(state, ref = 1)) %>% 
  lmerTest::lmer(., 
                 formula = value ~ state + (1 | sub )) 


tb <- summary(lmm)$coefficients[-1,] %>% 
  tibble::as_tibble() %>%
  rstatix::add_significance(p.col = 'Pr(>|t|)', 
                            output.col = 'sig', 
                            cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                            symbols = c("***", "**", "*", "ns")) %>% 
  dplyr::mutate(idx = seq(2, params$clusNum), 
                t = paste0('t = ', 
                           as.character(format(round(`t value`, 
                                                     digits = 3), 
                                               nsmall = 3)), 
                           sig)) %>% 
  dplyr::mutate(t = if_else(sig == 'ns', 'ns', t)) %>% 
  dplyr::inner_join(kop_term_long %>% 
                      dplyr::group_by(cluster, measure) %>% 
                      dplyr::summarise(max = max(value)) %>% 
                      dplyr::filter(measure == 'Mean dwelling time') %>% 
                      dplyr::select(-measure) %>% 
                      dplyr::mutate(idx = as.numeric(cluster)))

print('Mean dwelling time')
anova(lmm)

phoc <- difflsmeans(lmm, test.effs = "state") %>% 
  cbind(.) %>% 
  dplyr::mutate(p.adj = stats::p.adjust(`Pr(>|t|)`, 
                                        method = 'fdr',
                                        n = length(`Pr(>|t|)`))) %>%
  tidyr::separate(col = levels, 
                  into = c("statea", "stateb"), 
                  sep = " - ")

phoc <- phoc %>%
  tibble::as_tibble() %>% 
  dplyr::bind_rows(phoc %>% 
                     tibble::as_tibble() %>% 
                     dplyr::rename('stateb' = 'statea', 
                                   'statea' = 'stateb'))

phoc_dt <- phoc %>% 
  dplyr::mutate(t = paste0(as.character(format(round(abs(`t value`), 
                                                     digits = 1), 
                                               nsmall = 1)))) %>% 
  dplyr::mutate(t = dplyr::if_else(p.adj >= 0.05, NA_character_, t), 
                `t value` = dplyr::if_else(p.adj >= 0.05, NA_real_, `t value`)) %>% 
  ggplot2::ggplot(aes(x = statea, 
                      y = forcats::fct_relevel(stateb, 
                                               levels = rev(as.character(seq(params$clusNum)))))) + 
  geom_point(aes(size = abs(`t value`), 
                 colour = abs(`t value`))) + 
  geom_shadowtext(aes(label = t), 
                  colour = I("white")) + 
  scale_colour_gradientn(colours = sq1, 
                         name = 't-value', 
                         limits = c(2, 220), 
                         trans = 'log10') + 
  scale_size(range = c(6, 15), 
             limits = c(2,220), 
             guide = NULL, 
             trans = 'log10') + 
  theme_minimal() + 
  #scale_x_discrete(guide = guide_axis(n.dodge = 2)) + 
  scale_x_discrete(labels = labels, 
                   limits = rev(levels(phoc$statea))) + 
  scale_y_discrete(labels = labels, 
                   limits = rev(levels(phoc$statea))) + 
  xlab('State') + 
  ylab('State') + 
  labs(title = 'Dwelling time') + 
  theme(axis.text.y = element_markdown(color = "black", 
                                       size = 11), 
        axis.text.x = element_markdown(color = "black", 
                                       size = 11), 
        legend.position = 'none')

kop_term_long_dwell <- kop_term_long %>% 
  dplyr::filter(measure == 'Mean dwelling time') %>% 
  ggplot2::ggplot(aes(x = cluster, 
                      y = value)) + 
  geom_quasirandom(aes(fill = pt), 
                   size = 2, 
                   pch = 21, 
                   colour = 'white') + 
  geom_boxplot(aes(colour = pt), 
               width = 0.15, 
               outlier.shape = NA, 
               position = position_dodge(width = 0.8)) + 
  #geom_text(data = tb, aes(x = idx, y = max + 0.15*max, label = t)) + 
  scale_fill_manual(values = mypal2, 
                    name = 'Prematurity') +
  scale_colour_manual(values = c('#000000', '#000000'), 
                      guide = 'none') + 
  theme_bw() + 
  scale_x_discrete(name = 'State', 
                   labels = labels) + 
  ylab('Mean dwelling time (TR)') + 
  theme(axis.text.x = element_markdown(color = "black", 
                                       size = 11), 
        legend.position = 'none')

lmm <- kop_term_long %>% 
  dplyr::filter(measure == 'Mean metastability') %>% 
  dplyr::mutate(state = as.factor(cluster)) %>% 
  dplyr::mutate(state = relevel(state, ref = 1)) %>% 
  lmerTest::lmer(., 
                 formula = value ~ state + (1 | sub )) 


tb <- summary(lmm)$coefficients[-1,] %>% 
  tibble::as_tibble() %>%
  rstatix::add_significance(p.col = 'Pr(>|t|)', 
                            output.col = 'sig', 
                            cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                            symbols = c("***", "**", "*", "ns")) %>% 
  dplyr::mutate(idx = seq(2,params$clusNum), 
                t = paste0('t = ', 
                           as.character(format(round(`t value`, 
                                                     digits = 3), 
                                               nsmall = 3)), 
                           sig)) %>% 
  dplyr::mutate(t = if_else(`Pr(>|t|)` >= 0.5, 'ns', t)) %>% 
  dplyr::inner_join(kop_term_long %>% 
                      dplyr::group_by(cluster, measure) %>% 
                      dplyr::summarise(max = max(value)) %>% 
                      dplyr::filter(measure == 'Mean metastability') %>% 
                      dplyr::select(-measure) %>% 
                      dplyr::mutate(idx = as.numeric(cluster)))

print('Mean metastability')  
anova(lmm)

phoc <- difflsmeans(lmm, test.effs = "state") %>% 
  cbind(.) %>% 
  dplyr::mutate(p.adj = stats::p.adjust(`Pr(>|t|)`, 
                                        method = 'fdr',
                                        n = length(`Pr(>|t|)`))) %>%
  tidyr::separate(col = levels, 
                  into = c("statea", "stateb"), 
                  sep = " - ")

phoc <- phoc %>%
  tibble::as_tibble() %>% 
  dplyr::bind_rows(phoc %>% 
                     tibble::as_tibble() %>% 
                     dplyr::rename('stateb' = 'statea', 
                                   'statea' = 'stateb'))

phoc_mstab <- phoc %>% 
  dplyr::mutate(t = paste0(as.character(format(round(abs(`t value`), 
                                                     digits = 1), 
                                               nsmall = 1)))) %>% 
  dplyr::mutate(t = dplyr::if_else(p.adj >= 0.05, NA_character_, t), 
                `t value` = dplyr::if_else(p.adj >= 0.05, NA_real_, `t value`)) %>% 
  ggplot2::ggplot(aes(x = statea, 
                      y = forcats::fct_relevel(stateb, 
                                               levels = rev(as.character(seq(params$clusNum)))))) + 
  geom_point(aes(size = abs(`t value`), 
                 colour = abs(`t value`))) + 
  geom_shadowtext(aes(label = t), 
                  colour = I("white")) + 
  scale_colour_gradientn(colours = sq1, 
                         name = 't-value', 
                         limits = c(2, 220), 
                         trans = 'log10') + 
  scale_size(range = c(6, 15), 
             limits = c(2,220), 
             guide = NULL, 
             trans = 'log10') + 
  theme_minimal() + 
  #scale_x_discrete(guide = guide_axis(n.dodge = 2)) + 
  scale_x_discrete(labels = labels, 
                   limits = rev(levels(phoc$statea))) + 
  scale_y_discrete(labels = labels, 
                   limits = rev(levels(phoc$statea))) + 
  xlab('State') + 
  ylab('State') + 
  labs(title = 'Metastability') + 
  theme(axis.text.y = element_markdown(color = "black", 
                                       size = 11), 
        axis.text.x = element_markdown(color = "black", 
                                       size = 11), 
        legend.position = 'none')

kop_term_long_mstab <- kop_term_long %>% 
  dplyr::filter(measure == 'Mean metastability') %>% 
  ggplot2::ggplot(aes(x = cluster, 
                      y = value)) + 
  geom_quasirandom(aes(fill = pt), 
                   size = 2, 
                   pch = 21, 
                   colour = 'white') + 
  geom_boxplot(aes(colour = pt), 
               width = 0.15, 
               outlier.shape = NA, 
               position = position_dodge(width = 0.8)) + 
  #geom_text(data = tb, aes(x = idx, y = max + 0.15*max, label = t)) + 
  scale_fill_manual(values = mypal2, 
                    name = 'Prematurity') +
  scale_colour_manual(values = c('#000000', '#000000'), 
                      guide = 'none') + 
  theme_bw() + 
  scale_x_discrete(name = 'State', 
                   labels = labels) + 
  ylab('Mean metastability') + 
  theme(axis.text.x = element_markdown(color = "black", 
                                       size = 11), 
        legend.position = 'none')



cowplot::plot_grid(kop_term_long_kop, 
                   kop_term_long_mstab, 
                   kop_term_long_perc, 
                   kop_term_long_dwell, 
                   ncol = 1, 
                   labels = c('a', 'b', 'c', 'd'))
## ---- measuresStatesPT --------
kop_ <- dfm %>% 
  dplyr::select(sub,
                ses,
                cluster,
                pma,
                ga,
                sex,
                motparam_fd_outlier_count,
                pt,
                ensembleSync,
                ensembleMetastab,
                ensembleEntropy) %>% 
  dplyr::mutate(cluster = as.character(cluster)) %>% 
  dplyr::group_by(cluster, sub, pt, ga, sex, pma, motparam_fd_outlier_count) %>% 
  dplyr::summarise(syncMed = mean(ensembleSync),
                   metastabMed = sd(ensembleSync)) %>% 
  dplyr::ungroup() 

fo_ <- dfm %>% 
  dplyr::select(ttime, 
                sub, 
                pma, 
                cluster, 
                pt, 
                sex, 
                motparam_fd_outlier_count) %>% 
  stateR::nest_fo(., 
                  vars = c('sub', 
                           'pma', 
                           'pt', 
                           'sex', 
                           'motparam_fd_outlier_count'), 
                  foVar = 'cluster') %>% 
  tidyr::unnest(data)

dt_ <- dfm %>% 
  dplyr::select(ttime, 
                sub, 
                pma, 
                cluster, 
                pt, 
                sex, 
                motparam_fd_outlier_count) %>% 
  stateR::nest_dwell(., vars = c('sub', 
                                 'pma', 
                                 'pt', 
                                 'sex', 
                                 'motparam_fd_outlier_count'), 
                     foVar = 'cluster', 
                     sortBy = c('sub','ttime')) %>% 
  tidyr::unnest(data)


kop_long <- kop_ %>% 
  dplyr::inner_join(fo_) %>% 
  dplyr::inner_join(dt_) %>% 
  tidyr::pivot_longer(cols = c(syncMed, metastabMed, perc, mean_dwell), 
                      names_to = 'measure', 
                      values_to = 'value') %>% 
  dplyr::mutate(measure = dplyr::recode(measure, 
                                        'mean_dwell' = 'Mean dwelling time', 
                                        'syncMed' = 'Mean KOP', 
                                        'perc' = 'Mean fractional occupancy', 
                                        'metastabMed' = 'Mean metastability'))


kop_long_kop_temp <- kop_long %>% 
  dplyr::filter(measure == 'Mean KOP') %>% 
  dplyr::group_by(cluster) %>% 
  dplyr::summarise(max = max(value)) %>% 
  dplyr::ungroup()

kop_long_kop <- kop_long %>% 
  dplyr::filter(measure == 'Mean KOP') %>% 
  ggplot2::ggplot(aes(x = cluster, 
                      y = as.numeric(value))) + 
  geom_quasirandom(aes(fill = pt), 
                   size = 2, 
                   pch = 21, 
                   colour = 'white', 
                   dodge.width = 0.8) + 
  geom_boxplot(aes(colour = pt), 
               width = 0.15, 
               outlier.shape = NA, 
               position = position_dodge(width = 0.8)) + 
  #geom_text(data = tb, aes(x = idx, y = max + 0.15*max, label = t)) + 
  ggpubr::stat_pvalue_manual(models %>% 
                       dplyr::filter(var == 'Mean KOP' & term == 'Born Preterm**') %>% 
                       dplyr::inner_join(kop_long_kop_temp) %>% 
                       dplyr::mutate(group1 = cluster, 
                                     group2 = cluster, 
                                     y.position = max + 0.15*max, 
                                     xmin = c(0,1,2,3,4,5)+0.8, 
                                     xmax = c(0,1,2,3,4,5)+1.2) %>% 
                       rstatix::add_significance("filt.p", 
                                                 cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                                                 symbols = c("***", "**", "*", "ns")) %>% 
                       dplyr::filter(!is.na(filt.p)),
                     label = "filt.p.signif") + 
  scale_fill_manual(values = mypal2, 
                    name = 'Prematurity') +
  scale_colour_manual(values = c('#000000', '#000000'), 
                      guide = 'none') + 
  theme_bw() + 
  scale_x_discrete(name = element_blank(), 
                   labels = element_blank()) + 
  ylab('Mean synchronisation') + 
  theme(axis.text.x = element_markdown(color = "black", 
                                       size = 11), 
        legend.position = 'none')


kop_long_perc_temp <- kop_long %>% 
  dplyr::filter(measure == 'Mean fractional occupancy') %>% 
  dplyr::group_by(cluster) %>% 
  dplyr::summarise(max = max(value)) %>% 
  dplyr::ungroup()

kop_long_perc <- kop_long %>% 
  dplyr::filter(measure == 'Mean fractional occupancy') %>% 
  ggplot2::ggplot(aes(x = cluster, 
                      y = as.numeric(value))) + 
  geom_quasirandom(aes(fill = pt), 
                   size = 2, 
                   pch = 21, 
                   colour = 'white', 
                   dodge.width = 0.8) + 
  geom_boxplot(aes(colour = pt), 
               width = 0.15, 
               outlier.shape = NA, 
               position = position_dodge(width = 0.8)) + 
  #geom_text(data = tb, aes(x = idx, y = max + 0.15*max, label = t)) + 
  ggpubr::stat_pvalue_manual(models %>% 
                               dplyr::filter(var == 'Fractional Occupancy' & term == 'Born Preterm**') %>% 
                               dplyr::inner_join(kop_long_perc_temp) %>% 
                               dplyr::mutate(group1 = cluster, 
                                             group2 = cluster, 
                                             y.position = max + 0.15*max, 
                                             xmin = c(0,1,2,3,4,5)+0.8, 
                                             xmax = c(0,1,2,3,4,5)+1.2) %>% 
                               rstatix::add_significance("filt.p", 
                                                         cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                                                         symbols = c("***", "**", "*", "ns")) %>% 
                               dplyr::filter(!is.na(filt.p)),
                             label = "filt.p.signif") + 
  scale_fill_manual(values = mypal2, 
                    name = 'Prematurity') +
  scale_colour_manual(values = c('#000000', '#000000'), 
                      guide = 'none') + 
  theme_bw() + 
  scale_x_discrete(name = element_blank(), 
                   labels = element_blank()) + 
  scale_y_continuous(labels = scales::percent) + 
  ylab('Mean FO') + 
  theme(axis.text.x = element_markdown(color = "black", 
                                       size = 11), 
        legend.position = 'none')

kop_long_dwell_temp <- kop_long %>% 
  dplyr::filter(measure == 'Mean dwelling time') %>% 
  dplyr::group_by(cluster) %>% 
  dplyr::summarise(max = max(value)) %>% 
  dplyr::ungroup()

kop_long_dwell <- kop_long %>% 
  dplyr::filter(measure == 'Mean dwelling time') %>% 
  ggplot2::ggplot(aes(x = cluster, 
                      y = value*TR)) + 
  geom_quasirandom(aes(fill = pt), 
                   size = 2, 
                   pch = 21, 
                   colour = 'white', 
                   dodge.width = 0.8) + 
  geom_boxplot(aes(colour = pt), 
               width = 0.15, 
               outlier.shape = NA, 
               position = position_dodge(width = 0.8)) + 
  #geom_text(data = tb, aes(x = idx, y = max + 0.15*max, label = t)) + 
  ggpubr::stat_pvalue_manual(models %>% 
                               dplyr::filter(var == 'Dwelling times' & term == 'Born Preterm**') %>% 
                               dplyr::inner_join(kop_long_dwell_temp) %>% 
                               dplyr::mutate(group1 = cluster, 
                                             group2 = cluster, 
                                             y.position = max*TR + 0.15*max*TR, 
                                             xmin = c(0,1,2,3,4,5)+0.8, 
                                             xmax = c(0,1,2,3,4,5)+1.2) %>% 
                               rstatix::add_significance("filt.p", 
                                                         cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                                                         symbols = c("***", "**", "*", "ns")) %>% 
                               dplyr::filter(!is.na(filt.p)),
                             label = "filt.p.signif") + 
  scale_fill_manual(values = mypal2, 
                    name = 'Prematurity') +
  scale_colour_manual(values = c('#000000', '#000000'), 
                      guide = 'none') + 
  theme_bw() + 
  
  scale_x_discrete(name = element_blank(), 
                   labels = element_blank()) + 
  ylab('Mean DT (s)') + 
  guides(fill = guide_legend(override.aes = list(size=4))) + 
  theme(axis.text.x = element_markdown(color = "black", 
                                       size = 11), 
        legend.position = c(0.92,0.88), 
        legend.background = element_blank(), 
        legend.title = element_blank())


kop_long_mstab_temp <- kop_long %>% 
  dplyr::filter(measure == 'Mean metastability') %>% 
  dplyr::group_by(cluster) %>% 
  dplyr::summarise(max = max(value)) %>% 
  dplyr::ungroup()

kop_long_mstab <- kop_long %>% 
  dplyr::filter(measure == 'Mean metastability') %>% 
  ggplot2::ggplot(aes(x = cluster, 
                      y = value)) + 
  geom_quasirandom(aes(fill = pt), 
                   size = 2, 
                   pch = 21, 
                   colour = 'white', 
                   dodge.width = 0.8) + 
  geom_boxplot(aes(colour = pt), 
               width = 0.15, 
               outlier.shape = NA, 
               position = position_dodge(width = 0.8)) + 
  #geom_text(data = tb, aes(x = idx, y = max + 0.15*max, label = t)) + 
  ggpubr::stat_pvalue_manual(models %>% 
                               dplyr::filter(var == 'Metastability' & term == 'Born Preterm**') %>% 
                               dplyr::inner_join(kop_long_mstab_temp) %>% 
                               dplyr::mutate(group1 = cluster, 
                                             group2 = cluster, 
                                             y.position = max + 0.15*max, 
                                             xmin = c(0,1,2,3,4,5)+0.8, 
                                             xmax = c(0,1,2,3,4,5)+1.2) %>% 
                               rstatix::add_significance("filt.p", 
                                                         cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                                                         symbols = c("***", "**", "*", "ns")) %>% 
                               dplyr::filter(!is.na(filt.p)),
                             label = "filt.p.signif") + 
  scale_fill_manual(values = mypal2, 
                    name = 'Prematurity') +
  scale_colour_manual(values = c('#000000', '#000000'), 
                      guide = 'none') + 
  theme_bw() + 
  scale_x_discrete(name = 'State', 
                   labels = labels) + 
  ylab('Mean metastab.') + 
  theme(axis.text.x = element_markdown(color = "black", 
                                       size = 11), 
        legend.position = 'none')


vplots <- cowplot::plot_grid(kop_long_dwell, 
                             kop_long_perc, 
                             kop_long_kop, 
                             kop_long_mstab, 
                             align = c('v'),
                             ncol = 1, 
                             labels = c('a', 'b', 'c', 'd'), 
                             rel_heights = c(1, 1, 1, 1.3))
## ---- phocs ----

sq_phoc <- cowplot::plot_grid(phoc_kop, 
                              phoc_mstab, 
                              phoc_fo, 
                              phoc_dt, 
                              ncol = 2, 
                              labels = c('a', 'b', 'c', 'd'))

leg_phoc <- cowplot::get_legend(phoc_kop +  
                                  theme(legend.position = "bottom") + 
                                  guides(colour = guide_colourbar(barwidth = 15, 
                                                                  barheight = 1.5)))

cowplot::plot_grid(sq_phoc, 
                   leg_phoc, 
                   ncol = 1, 
                   rel_heights = c(1,0.05))

## ---- bayley ----

imd_tab <- readxl::read_xlsx(path = 'data/imd_pat.xlsx')
names(imd_tab) <- c('sub', 'age_m', 'age_d', 'cor_age_m', 'cor_age_d', 'imd')

imd_tab$imd[imd_tab$sub == 'CC00551XX07'] <- 9612
imd_tab$imd[imd_tab$sub == 'CC00956XX16'] <- 27951

dfm$cog_comp[dfm$cog_comp < 0] <- NA
dfm$motor_comp[dfm$motor_comp < 0] <- NA
dfm$language_comp[dfm$language_comp < 0] <- NA
dfm$qchat_total[dfm$qchat_total < 0] <- NA

bl_ttests_fo_cog <- dfm %>% 
  dplyr::select(sub, 
                pma, 
                ga, 
                cluster, 
                pt, 
                sex, 
                motparam_fd_outlier_count, 
                cog_comp, 
                language_comp, 
                motor_comp, 
                qchat_total) %>%
  dplyr::left_join(imd_tab) %>% 
  dplyr::mutate(age = age_m + (1/30*age_d), 
                cor_age = cor_age_m + (1/30*cor_age_d)) %>% 
  nest_fo(., 
          vars = c('sub', 
                   'pma', 
                   'sex', 
                   'ga',
                   'motparam_fd_outlier_count', 
                   'cog_comp', 
                   'language_comp', 
                   'motor_comp', 
                   'qchat_total',
                   'cor_age',
                   'imd'), 
          foVar = 'cluster') %>% 
  dplyr::mutate(cc = list(map_df(data, 
                                 ~grouped_perm_glm(.x, 
                                                   formla = perc ~ 
                                                     pma + 
                                                     sex + 
                                                     ga + 
                                                     motparam_fd_outlier_count +
                                                     cog_comp + 
                                                     cor_age + 
                                                     imd, 
                                                   var_to_perm = 'perc', 
                                                   permNum = params$nPerms, 
                                                   seed = params$seed)))) %>% 
  tidyr::unnest_wider(cc) %>% 
  tidyr::unnest(-c(cluster, data)) %>% 
  dplyr::group_by(term) %>% 
  dplyr::mutate(pp.cor = p.adjust(p.perm, 
                                  method = 'fdr', 
                                  n = length(p.perm))) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(filt.t = if_else(pp.cor < 0.05, statistic, NA_real_)) %>% 
  dplyr::select(c(cluster, term, filt.t, pp.cor)) %>% 
  dplyr::mutate(term = dplyr::recode(term, 
                                     '(Intercept)' = 'Intercept', 
                                     'cog_comp' = 'Cog. Comp.', 
                                     'language_comp' = 'Lang. Comp.', 
                                     'motor_comp' = 'Mot. Comp', 
                                     'pma' = 'PMA', 
                                     'imd' = 'IMD', 
                                     'cor_age' = 'Corr. Age',
                                     'qchat_total' = 'q-Chat', 
                                     'motparam_fd_outlier_count' = 'M. outliers', 
                                     'sexmale' = 'Sex', 
                                     'ptTerm' = 'Prematurity')) %>% 
  dplyr::filter(term %in% c('Cog. Comp.'))


bl_ttests_fo_language <- dfm %>% 
  dplyr::select(sub, 
                pma, 
                ga, 
                cluster, 
                pt, 
                sex, 
                motparam_fd_outlier_count, 
                cog_comp, 
                language_comp, 
                motor_comp, 
                qchat_total) %>%
  dplyr::left_join(imd_tab) %>% 
  dplyr::mutate(age = age_m + (1/30*age_d), 
                cor_age = cor_age_m + (1/30*cor_age_d)) %>% 
  nest_fo(., 
          vars = c('sub', 
                   'pma', 
                   'sex', 
                   'ga',
                   'motparam_fd_outlier_count', 
                   'cog_comp', 
                   'language_comp', 
                   'motor_comp', 
                   'qchat_total',
                   'cor_age',
                   'imd'), 
          foVar = 'cluster') %>% 
  dplyr::mutate(cc = list(map_df(data, 
                                 ~grouped_perm_glm(.x, 
                                                   formla = perc ~ 
                                                     pma + 
                                                     sex + 
                                                     ga + 
                                                     motparam_fd_outlier_count +
                                                     language_comp + 
                                                     cor_age + 
                                                     imd, 
                                                   var_to_perm = 'perc', 
                                                   permNum = params$nPerms, 
                                                   seed = params$seed)))) %>% 
  tidyr::unnest_wider(cc) %>% 
  tidyr::unnest(-c(cluster, data)) %>% 
  dplyr::group_by(term) %>% 
  dplyr::mutate(pp.cor = p.adjust(p.perm, 
                                  method = 'fdr', 
                                  n = length(p.perm))) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(filt.t = if_else(pp.cor < 0.05, statistic, NA_real_)) %>% 
  dplyr::select(c(cluster, term, filt.t, pp.cor)) %>% 
  dplyr::mutate(term = dplyr::recode(term, 
                                     '(Intercept)' = 'Intercept', 
                                     'cog_comp' = 'Cog. Comp.', 
                                     'language_comp' = 'Lang. Comp.', 
                                     'motor_comp' = 'Mot. Comp', 
                                     'pma' = 'PMA', 
                                     'imd' = 'IMD', 
                                     'cor_age' = 'Corr. Age',
                                     'qchat_total' = 'q-Chat', 
                                     'motparam_fd_outlier_count' = 'M. outliers', 
                                     'sexmale' = 'Sex', 
                                     'ptTerm' = 'Prematurity')) %>% 
  dplyr::filter(term %in% c('Lang. Comp.'))


bl_ttests_fo_motor <- dfm %>% 
  dplyr::select(sub, 
                pma, 
                ga, 
                cluster, 
                pt, 
                sex, 
                motparam_fd_outlier_count, 
                cog_comp, 
                language_comp, 
                motor_comp, 
                qchat_total) %>%
  dplyr::left_join(imd_tab) %>% 
  dplyr::mutate(age = age_m + (1/30*age_d), 
                cor_age = cor_age_m + (1/30*cor_age_d)) %>% 
  nest_fo(., 
          vars = c('sub', 
                   'pma', 
                   'sex', 
                   'ga',
                   'motparam_fd_outlier_count', 
                   'cog_comp', 
                   'language_comp', 
                   'motor_comp', 
                   'qchat_total',
                   'cor_age',
                   'imd'), 
          foVar = 'cluster') %>% 
  dplyr::mutate(cc = list(map_df(data, 
                                 ~grouped_perm_glm(.x, 
                                                   formla = perc ~ 
                                                     pma + 
                                                     sex + 
                                                     ga + 
                                                     motparam_fd_outlier_count +
                                                     motor_comp + 
                                                     cor_age + 
                                                     imd, 
                                                   var_to_perm = 'perc', 
                                                   permNum = params$nPerms, 
                                                   seed = params$seed)))) %>% 
  tidyr::unnest_wider(cc) %>% 
  tidyr::unnest(-c(cluster, data)) %>% 
  dplyr::group_by(term) %>% 
  dplyr::mutate(pp.cor = p.adjust(p.perm, 
                                  method = 'fdr', 
                                  n = length(p.perm))) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(filt.t = if_else(pp.cor < 0.05, statistic, NA_real_)) %>% 
  dplyr::select(c(cluster, term, filt.t, pp.cor)) %>% 
  dplyr::mutate(term = dplyr::recode(term, 
                                     '(Intercept)' = 'Intercept', 
                                     'cog_comp' = 'Cog. Comp.', 
                                     'language_comp' = 'Lang. Comp.', 
                                     'motor_comp' = 'Mot. Comp.', 
                                     'pma' = 'PMA', 
                                     'imd' = 'IMD', 
                                     'cor_age' = 'Corr. Age',
                                     'qchat_total' = 'q-Chat', 
                                     'motparam_fd_outlier_count' = 'M. outliers', 
                                     'sexmale' = 'Sex', 
                                     'ptTerm' = 'Prematurity')) %>% 
  dplyr::filter(term %in% c('Mot. Comp.'))

bl_ttests_fo_qchat <- dfm %>% 
  dplyr::select(sub, 
                pma, 
                ga, 
                cluster, 
                pt, 
                sex, 
                motparam_fd_outlier_count, 
                cog_comp, 
                language_comp, 
                motor_comp, 
                qchat_total) %>%
  dplyr::left_join(imd_tab) %>% 
  dplyr::mutate(age = age_m + (1/30*age_d), 
                cor_age = cor_age_m + (1/30*cor_age_d)) %>% 
  nest_fo(., 
          vars = c('sub', 
                   'pma', 
                   'sex', 
                   'ga',
                   'motparam_fd_outlier_count', 
                   'cog_comp', 
                   'language_comp', 
                   'motor_comp', 
                   'qchat_total',
                   'cor_age',
                   'imd'), 
          foVar = 'cluster') %>% 
  dplyr::mutate(cc = list(map_df(data, 
                                 ~grouped_perm_glm(.x, 
                                                   formla = perc ~ 
                                                     pma + 
                                                     ga + 
                                                     sex + 
                                                     motparam_fd_outlier_count +
                                                     qchat_total + 
                                                     cor_age + 
                                                     imd, 
                                                   var_to_perm = 'perc', 
                                                   permNum = params$nPerms, 
                                                   seed = params$seed)))) %>% 
  tidyr::unnest_wider(cc) %>% 
  tidyr::unnest(-c(cluster, data)) %>% 
  dplyr::group_by(term) %>% 
  dplyr::mutate(pp.cor = p.adjust(p.perm, 
                                  method = 'fdr', 
                                  n = length(p.perm))) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(filt.t = if_else(pp.cor < 0.05, statistic, NA_real_)) %>% 
  dplyr::select(c(cluster, term, filt.t, pp.cor, p.perm)) %>% 
  dplyr::mutate(term = dplyr::recode(term, 
                                     '(Intercept)' = 'Intercept', 
                                     'cog_comp' = 'Cog. Comp.', 
                                     'language_comp' = 'Lang. Comp.', 
                                     'motor_comp' = 'Mot. Comp', 
                                     'pma' = 'PMA', 
                                     'imd' = 'IMD', 
                                     'cor_age' = 'Corr. Age',
                                     'qchat_total' = 'Q-CHAT', 
                                     'motparam_fd_outlier_count' = 'M. outliers', 
                                     'sexmale' = 'Sex', 
                                     'ptTerm' = 'Prematurity')) %>% 
  dplyr::filter(term %in% c('Q-CHAT'))


bl_ttests_fo <- dplyr::bind_rows(bl_ttests_fo_cog, 
                                 bl_ttests_fo_language, 
                                 bl_ttests_fo_motor, 
                                 bl_ttests_fo_qchat) %>% 
  plot_bl(., 
          pal = scales, 
          ttl = element_blank(), 
          sttl = 'Fractional occupancy')




bl_ttests_kop_cog <- dfm %>% 
  dplyr::select(ttime, 
                sub, 
                pma, 
                ga, 
                cluster, 
                pt, 
                sex, 
                motparam_fd_outlier_count, 
                cog_comp, 
                language_comp, 
                motor_comp, 
                qchat_total, 
                ensembleSync, 
                ensembleMetastab) %>%
  dplyr::left_join(imd_tab) %>% 
  dplyr::mutate(age = age_m + (1/30*age_d), 
                cor_age = cor_age_m + (1/30*cor_age_d)) %>% 
  dplyr::mutate(cluster = as.character(cluster)) %>% 
  dplyr::group_by(cluster, 
                  sub, 
                  pt, 
                  ga, 
                  sex, 
                  pma, 
                  motparam_fd_outlier_count, 
                  cog_comp, 
                  language_comp, 
                  motor_comp, 
                  qchat_total, 
                  imd, 
                  cor_age) %>% 
  dplyr::summarise(syncMed = mean(ensembleSync),
                   metastabMed = sd(ensembleSync)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(cluster) %>% 
  tidyr::nest() %>% 
  dplyr::mutate(dw = list(map_df(data, 
                                 ~grouped_perm_glm(.x, 
                                                   formla = syncMed ~
                                                     sex + 
                                                     pma + 
                                                     ga + 
                                                     motparam_fd_outlier_count + 
                                                     cor_age + 
                                                     cog_comp + 
                                                     imd, 
                                                   var_to_perm = 'syncMed', 
                                                   permNum = params$nPerms, 
                                                   seed = params$seed)))) %>% 
  tidyr::unnest_wider(dw) %>% 
  tidyr::unnest(-c(cluster, data)) %>% 
  dplyr::group_by(term) %>% 
  dplyr::mutate(pp.cor = p.adjust(p.perm, 
                                  method = 'fdr', 
                                  n = length(p.perm))) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(filt.t = if_else(pp.cor < 0.05, statistic, NA_real_)) %>% 
  dplyr::select(c(cluster, term, filt.t, pp.cor, p.perm)) %>% 
  dplyr::mutate(term = dplyr::recode(term, 
                                     '(Intercept)' = 'Intercept', 
                                     'cog_comp' = 'Cog. Comp.*', 
                                     'language_comp' = 'Lang. Comp.', 
                                     'motor_comp' = 'Mot. Comp.', 
                                     'pma' = 'PMA', 
                                     'imd' = 'IMD', 
                                     'cor_age' = 'Corr. Age',
                                     'qchat_total' = 'Q-CHAT', 
                                     'motparam_fd_outlier_count' = 'M. outliers', 
                                     'sexmale' = 'Sex')) %>% 
  dplyr::filter(term %in% c('Cog. Comp.*'))

bl_ttests_kop_language <- dfm %>% 
  dplyr::select(ttime, 
                sub, 
                pma, 
                ga, 
                cluster, 
                pt, 
                sex, 
                motparam_fd_outlier_count, 
                cog_comp, 
                language_comp, 
                motor_comp, 
                qchat_total, 
                ensembleSync, 
                ensembleMetastab) %>%
  dplyr::left_join(imd_tab) %>% 
  dplyr::mutate(age = age_m + (1/30*age_d), 
                cor_age = cor_age_m + (1/30*cor_age_d)) %>% 
  dplyr::mutate(cluster = as.character(cluster)) %>% 
  dplyr::group_by(cluster, 
                  sub, 
                  pt, 
                  ga, 
                  sex, 
                  pma, 
                  motparam_fd_outlier_count, 
                  cog_comp, 
                  language_comp, 
                  motor_comp, 
                  qchat_total, 
                  imd, 
                  cor_age) %>% 
  dplyr::summarise(syncMed = mean(ensembleSync),
                   metastabMed = sd(ensembleSync)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(cluster) %>% 
  tidyr::nest() %>% 
  dplyr::mutate(dw = list(map_df(data, 
                                 ~grouped_perm_glm(.x, 
                                                   formla = syncMed ~
                                                     sex + 
                                                     pma + 
                                                     ga + 
                                                     motparam_fd_outlier_count + 
                                                     cor_age + 
                                                     language_comp + 
                                                     imd, 
                                                   var_to_perm = 'syncMed', 
                                                   permNum = params$nPerms, 
                                                   seed = params$seed)))) %>% 
  tidyr::unnest_wider(dw) %>% 
  tidyr::unnest(-c(cluster, data)) %>% 
  dplyr::group_by(term) %>% 
  dplyr::mutate(pp.cor = p.adjust(p.perm, 
                                  method = 'fdr', 
                                  n = length(p.perm))) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(filt.t = if_else(pp.cor < 0.05, statistic, NA_real_)) %>% 
  dplyr::select(c(cluster, term, filt.t, pp.cor)) %>% 
  dplyr::mutate(term = dplyr::recode(term, 
                                     '(Intercept)' = 'Intercept', 
                                     'cog_comp' = 'Cog. Comp.', 
                                     'language_comp' = 'Lang. Comp.**', 
                                     'motor_comp' = 'Mot. Comp.', 
                                     'pma' = 'PMA', 
                                     'imd' = 'IMD', 
                                     'cor_age' = 'Corr. Age',
                                     'qchat_total' = 'Q-CHAT', 
                                     'motparam_fd_outlier_count' = 'M. outliers', 
                                     'sexmale' = 'Sex')) %>% 
  dplyr::filter(term %in% c('Lang. Comp.**'))


bl_ttests_kop_motor <- dfm %>% 
  dplyr::select(ttime, 
                sub, 
                pma, 
                ga, 
                cluster, 
                pt, 
                sex, 
                motparam_fd_outlier_count, 
                cog_comp, 
                language_comp, 
                motor_comp, 
                qchat_total, 
                ensembleSync, 
                ensembleMetastab) %>%
  dplyr::left_join(imd_tab) %>% 
  dplyr::mutate(age = age_m + (1/30*age_d), 
                cor_age = cor_age_m + (1/30*cor_age_d)) %>% 
  dplyr::mutate(cluster = as.character(cluster)) %>% 
  dplyr::group_by(cluster, 
                  sub, 
                  pt, 
                  ga, 
                  sex, 
                  pma, 
                  motparam_fd_outlier_count, 
                  cog_comp, 
                  language_comp, 
                  motor_comp, 
                  qchat_total, 
                  imd, 
                  cor_age) %>% 
  dplyr::summarise(syncMed = mean(ensembleSync),
                   metastabMed = sd(ensembleSync)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(cluster) %>% 
  tidyr::nest() %>% 
  dplyr::mutate(dw = list(map_df(data, 
                                 ~grouped_perm_glm(.x, 
                                                   formla = syncMed ~
                                                     sex + 
                                                     pma + 
                                                     ga + 
                                                     motparam_fd_outlier_count + 
                                                     cor_age + 
                                                     motor_comp + 
                                                     imd, 
                                                   var_to_perm = 'syncMed', 
                                                   permNum = params$nPerms, 
                                                   seed = params$seed)))) %>% 
  tidyr::unnest_wider(dw) %>% 
  tidyr::unnest(-c(cluster, data)) %>% 
  dplyr::group_by(term) %>% 
  dplyr::mutate(pp.cor = p.adjust(p.perm, 
                                  method = 'fdr', 
                                  n = length(p.perm))) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(filt.t = if_else(pp.cor < 0.05, statistic, NA_real_)) %>% 
  dplyr::select(c(cluster, term, filt.t, pp.cor, p.perm)) %>% 
  dplyr::mutate(term = dplyr::recode(term, 
                                     '(Intercept)' = 'Intercept', 
                                     'cog_comp' = 'Cog. Comp.', 
                                     'language_comp' = 'Lang. Comp.', 
                                     'motor_comp' = 'Mot. Comp.***', 
                                     'pma' = 'PMA', 
                                     'imd' = 'IMD', 
                                     'cor_age' = 'Corr. Age',
                                     'qchat_total' = 'Q-CHAT', 
                                     'motparam_fd_outlier_count' = 'M. outliers', 
                                     'sexmale' = 'Sex')) %>% 
  dplyr::filter(term %in% c('Mot. Comp.***'))


bl_ttests_kop_qchat <- dfm %>% 
  dplyr::select(ttime, 
                sub, 
                pma, 
                ga, 
                cluster, 
                pt, 
                sex, 
                motparam_fd_outlier_count, 
                cog_comp, 
                language_comp, 
                motor_comp, 
                qchat_total, 
                ensembleSync, 
                ensembleMetastab) %>%
  dplyr::left_join(imd_tab) %>% 
  dplyr::mutate(age = age_m + (1/30*age_d), 
                cor_age = cor_age_m + (1/30*cor_age_d)) %>% 
  dplyr::mutate(cluster = as.character(cluster)) %>% 
  dplyr::group_by(cluster, 
                  sub, 
                  pt, 
                  ga, 
                  sex, 
                  pma, 
                  motparam_fd_outlier_count, 
                  cog_comp, 
                  language_comp, 
                  motor_comp, 
                  qchat_total, 
                  imd, 
                  cor_age) %>% 
  dplyr::summarise(syncMed = mean(ensembleSync),
                   metastabMed = sd(ensembleSync)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(cluster) %>% 
  tidyr::nest() %>% 
  dplyr::mutate(dw = list(map_df(data, 
                                 ~grouped_perm_glm(.x, 
                                                   formla = syncMed ~
                                                     sex + 
                                                     pma + 
                                                     ga + 
                                                     motparam_fd_outlier_count + 
                                                     cor_age + 
                                                     qchat_total + 
                                                     imd, 
                                                   var_to_perm = 'syncMed', 
                                                   permNum = params$nPerms, 
                                                   seed = params$seed)))) %>% 
  tidyr::unnest_wider(dw) %>% 
  tidyr::unnest(-c(cluster, data)) %>% 
  dplyr::group_by(term) %>% 
  dplyr::mutate(pp.cor = p.adjust(p.perm, 
                                  method = 'fdr', 
                                  n = length(p.perm))) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(filt.t = if_else(pp.cor < 0.05, statistic, NA_real_)) %>% 
  dplyr::select(c(cluster, term, filt.t, pp.cor)) %>% 
  dplyr::mutate(term = dplyr::recode(term, 
                                     '(Intercept)' = 'Intercept', 
                                     'cog_comp' = 'Cog. Comp.*', 
                                     'language_comp' = 'Lang. Comp.', 
                                     'motor_comp' = 'Mot. Comp.', 
                                     'pma' = 'PMA', 
                                     'imd' = 'IMD', 
                                     'cor_age' = 'Corr. Age',
                                     'qchat_total' = 'Q-CHAT****', 
                                     'motparam_fd_outlier_count' = 'M. outliers', 
                                     'sexmale' = 'Sex')) %>% 
  dplyr::filter(term %in% c('Q-CHAT****'))


bl_ttests_kop <- dplyr::bind_rows(bl_ttests_kop_cog, 
                                  bl_ttests_kop_language, 
                                  bl_ttests_kop_motor, 
                                  bl_ttests_kop_qchat) %>% 
  plot_bl(., 
          pal = scales, 
          ttl = element_blank(), 
          sttl = 'Mean synchronisation')


cowplot::plot_grid(bl_ttests_kop, 
                   bl_ttests_fo, 
                   labels = c('a', 'b'), 
                   label_size = 24)


