plot_corr <- function(df, pal = 'RdBu'){
  
  df <- df %>% 
    dplyr::mutate(cluster = as.factor(cluster))
  
  df %>% 
    
    ggplot2::ggplot(aes(x = term, 
                        y = as.factor(cluster))) + 
    geom_point(aes(size = abs(filt.t), 
                   colour = filt.t)) + 
    geom_shadowtext(aes(label = for.t), 
                    colour = I("white")) + 
    scale_colour_gradientn(colours = rev(brewer.pal(n=11, name=pal)), 
                           name = 't-value', 
                           limits = c(-6, 6)) + 
    scale_size(range = c(4, 15), 
               guide = NULL) + 
    theme_minimal() + 
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) + 
    scale_y_discrete(name = NULL, 
                     labels = labels, 
                     limits = rev(levels(df$cluster))) + 
    xlab(NULL) + 
    theme(axis.text.y = element_markdown(color = "black", 
                                         size = 11), 
          legend.position = 'none')
  
}