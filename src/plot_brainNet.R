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


plot_brainNet <- function(allTransitions, 
                          k, 
                          scale_fct, 
                          ttl, 
                          sttl, 
                          llim = -6, 
                          hlim = 6, 
                          pal, 
                          lbl){

  
  id = 1:(k)
  theta = 2*pi/k*id
  
  states <- tibble::tibble(id, theta, lbl) %>% 
    dplyr::mutate(x = scale_fct*cos(theta), 
                  y = scale_fct*sin(theta)) %>% 
    dplyr::mutate(image = glue::glue('figs/centr_k{k}_{id}_r_plot.png'))
  
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
  
  sig_ps <- cc %>% 
    dplyr::filter(!is.na(cor_p)) %>% 
    dplyr::mutate(p = glue::glue('t = {stat}')) %>% 
    dplyr::mutate(p = recode(p, `p = <0.001` = 'p < 0.001'))
  
  dd <- ggplot2::ggplot(data = states, 
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
    theme_net()
  
  dd <- dd + geom_curve(data = cc %>% 
                          dplyr::filter(!tag %in% c('0_0', '1_1', '2_2', '3_3', '4_4', '5_5', '6_6')) %>% 
                          dplyr::mutate(term = dplyr::if_else(is.na(cor_p), 
                                                              NA_character_, 
                                                              term)), 
                        aes(x = ss_dx, 
                            y = ss_dy, 
                            xend = zz_dx, 
                            yend = zz_dy, 
                            size = nCount, 
                            colour = term),
                        size = 2, 
                        arrow = arrow(length = unit(0.3, "cm"), 
                                      type="closed"), 
                        curvature = - 0.2) + 
    
    
    geom_curve(data = cc %>% 
                 dplyr::filter(tag %in% c('0_0', '1_1', '2_2', '3_3', '4_4', '5_5', '6_6')) %>% 
                 dplyr::mutate(term = dplyr::if_else(is.na(cor_p), 
                                                     NA_character_, 
                                                     term)), 
               aes(x = ss_dx, 
                   y = ss_dy, 
                   xend = zz_dx, 
                   yend = zz_dy, 
                   size = nCount, 
                   colour = term),
               size = 2, 
               arrow = arrow(length = unit(0.4, "cm"), 
                             type="closed")) + 
    
    scale_color_manual(values=c(pal), na.value = NA) + 
    
    geom_label_repel(data = sig_ps, 
                     aes(x = zz_dx - 0.2*zz_dx, 
                         y = zz_dy - 0.2*zz_dy, 
                         label = p, 
                         colour = term)) + 
    
    labs(title = ttl, 
         subtitle = sttl) + 
    theme(legend.position = 'none')
  
  return(dd)
  
}