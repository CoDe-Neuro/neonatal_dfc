plot_bl <- function(df, pal, ttl, sttl){
  
  df %>% 
    dplyr::mutate(for.t = as.character(signif(filt.t, 3))) %>% 
    
    ggplot2::ggplot(aes(x = term, 
                        y = as.factor(cluster))) + 
    geom_point(aes(size = abs(filt.t)), 
               pch = 21, 
               stroke = 2, 
               colour = met.brewer("Archambault", 11)[8]) + 
    geom_point(aes(size = abs(filt.t)), 
               pch = 21, 
               stroke = 2) + 
    geom_text(aes(label = for.t), 
              colour = I("black")) + 
    scale_size(range = c(10, 20), 
               limits = c(-6, 6),
               guide = NULL) + 
    theme_minimal() + 
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) + 
    scale_y_discrete(name = NULL, 
                     labels = labels, 
                     limits = rev(levels(as.factor(df$cluster)))) + 
    xlab(NULL) + 
    theme(axis.text.y = element_markdown(color = "black", 
                                         size = 11), 
          legend.position = 'none') + 
    labs(title = ttl, 
         subtitle = sttl)
  
  
}