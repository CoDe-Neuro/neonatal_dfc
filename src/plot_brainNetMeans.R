require(ggplot2)
require(tidyr)
require(dplyr)
require(RColorBrewer)
require(ggExtra)
require(stringr)
require(ggimage)
require(ggpubr)
require(geomnet)
require(ggrepel)


plot_brainNetMeans <- function(allTransitions, 
                          k, 
                          scale_fct, 
                          ttl, 
                          sttl, 
                          llim = -6, 
                          hlim = 6, 
                          pal = 'PuOr'){
  
  scales2 <- rev(RColorBrewer::brewer.pal(n = 11,
                                          name = pal))
  
  id = 0:(k - 1)
  theta = 2*pi/k*id
  
  states <- tibble::tibble(id, theta) %>% 
    dplyr::mutate(x = scale_fct*cos(theta), 
                  y = scale_fct*sin(theta)) %>% 
    dplyr::mutate(image = glue::glue('figs/centr_k{k}_{id + 1}_plot.png'))
  
  inward <- character(length(id))
  
  for (i in id){inward[i + 1] <- glue::glue('{i}_{i}')}
  
  aa <- allTransitions %>% 
    tidyr::unnest(data) %>% 
    dplyr::group_by(pt, 
                    source, 
                    target, 
                    tag, 
                    p.perm, 
                    cor_p, 
                    stat, 
                    p, 
                    s, 
                    term) %>% 
    dplyr::summarise(nCount = median(nCount)) %>% 
    dplyr::ungroup() %>% 
    filter(!tag %in% inward) %>% 
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
                                 rr = 0.22*scale_fct), 
                            cl_radii)) %>% 
    tidyr::unnest_wider(rd) %>% 
    dplyr::rename('ss_dx' = 'dx', 
                  'ss_dy' = 'dy')
  
  bb <- allTransitions %>% 
    tidyr::unnest(data) %>% 
    dplyr::group_by(pt, 
                    source, 
                    target, 
                    tag, 
                    p.perm, 
                    cor_p, 
                    stat, 
                    p, 
                    s, 
                    term) %>% 
    dplyr::summarise(nCount = median(nCount)) %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(tag %in% inward) %>% 
    dplyr::mutate(pt = as.character(pt)) %>% 
    dplyr::filter(pt == 'Term') %>% 
    mutate(theta_s = 2*pi/k*source,
           source_x = scale_fct*cos(theta_s),
           source_y = scale_fct*sin(theta_s),
           theta_t = 2*pi/k*target,
           target_x = scale_fct*cos(theta_t),
           target_y = scale_fct*sin(theta_t), 
           theta_1 = 2*pi/k*source, 
           theta_2 = theta_1 + pi/4) %>% 
    dplyr::mutate(ss_dx = source_x + 0.4*scale_fct*cos(theta_1), 
                  ss_dy = source_y + 0.4*scale_fct*sin(theta_1), 
                  zz_dx = source_x + 0.4*scale_fct*cos(theta_2), 
                  zz_dy = source_y + 0.4*scale_fct*sin(theta_2)) %>% 
    dplyr::select(-c(theta_1, 
                     theta_2))
  
  
  cc <- bind_rows(aa, bb)
  
  dd <- ggplot2::ggplot(data = states, 
                        aes(x = x, 
                            y = y)) + 
    geom_image(aes(image = image), 
               size = 0.15) + 
    geom_text(aes(x = 1.6*x, 
                  y = 1.6*y, 
                  label = glue::glue('#{id}')), 
              size = 6.5, 
              colour = 'grey50') + 
    xlim(scale_fct*c(-1.65,1.65)) + 
    ylim(scale_fct*c(-1.65,1.65)) + 
    theme_net()
  
  dd <- dd + geom_curve(data = cc, 
                        aes(x = ss_dx, 
                            y = ss_dy, 
                            xend = zz_dx, 
                            yend = zz_dy, , 
                            size = nCount), 
                        arrow = arrow(length = unit(0.2, "cm"), 
                                      type="closed")) + 
    scale_size_continuous(range = c(0.1,1.5), 
                          breaks = c(0.01, 0.05, 0.1, 0.25, 0.75, 0.90), 
                          labels = scales::percent, 
                          name = 'Transition probability') + 
    guides(size=guide_legend(override.aes = list(arrow = NULL))) + 
    labs(title = ttl, 
         subtitle = sttl)
  
  return(dd)
  
}