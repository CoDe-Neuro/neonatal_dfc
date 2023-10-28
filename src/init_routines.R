## ---- libs --------
library(ggplot2)
library(tidyr)
library(dplyr)
library(cowplot)
library(purrr)
library(RColorBrewer)
library(knitr)
library(kableExtra)
library(janitor)
library(ggExtra)
library(stringr)
library(rjson)
library(broom)
library(plotly)
library(ggsci)
library(tidymodels)
library(shadowtext)
library(effsize)
library(modelr)
library(ggimage)
library(ggpubr)
library(patchwork)
library(modelsummary)
library(ggbeeswarm)
library(ggrepel)
library(ggtext)
library(MetBrewer)
library(lmerTest)
library(forcats)
library(stateR)
library(gt)
#library(DiagrammeR)
library(ggpmisc)
library(rstatix)

source("src/patmotJSON.R")
source("src/smoothOut.R")
source("src/outliers.R")
source("src/grouped_perm.R")
source('src/pop_sel.R')
source('src/cl_radii.R')
source('src/plot_brainNet.R')
source('src/plot_corr.R')
source('src/plot_bl.R')

## ---- ggset --------
# Define gg theme

theme_set(theme_bw() + 
            theme_bw(base_size = 14, 
                     base_family = "Helvetica") + 
            theme(panel.border = element_rect(size = 1.5)))

# Define palettes 

mypal <- pal_npg("nrc", alpha = 1)(9)
mypal3 <- mypal[c(8,2,4)]
mypal2 <- c(met.brewer("Archambault", 11)[3], met.brewer("Archambault", 11)[8])

scales <- colorRampPalette(colors = c(met.brewer("Ingres", 11)[2], 
                                      "#FFFFFF", 
                                      met.brewer("Ingres", 11)[10]))(13)

scales2 <- rev(RColorBrewer::brewer.pal(n = 11,
                                        name = 'PuOr'))

scales3 <- colorRampPalette(colors = c(met.brewer("Archambault", 11)[3], 
                                       "#FFFFFF", 
                                       met.brewer("Archambault", 11)[8]))(13)

sq1 <- colorRampPalette(c("#FFFFFF", met.brewer("Archambault", 11)[1]))(11)[2:11]

labels <- c(
  `1` = "<img src='figs/centr_k6_1_r_plot.png'
    width='25' align='middle' /><br>Glb.A", 
  `2` = "<img src='figs/centr_k6_2_r_plot.png'
    width='25' align='middle' /><br>Glb.B", 
  `3` = "<img src='figs/centr_k6_3_r_plot.png'
    width='25' align='middle' /><br>Glb.C", 
  `4` = "<img src='figs/centr_k6_4_r_plot.png'
    width='25' align='middle' /><br>Occ.", 
  `5` = "<img src='figs/centr_k6_5_r_plot.png'
    width='25' align='middle' /><br>SM", 
  `6` = "<img src='figs/centr_k6_6_r_plot.png'
    width='25' align='middle' /><br>FP")

labRetag <- c('Glb.A', 'Glb.B', 'Glb.C', 'Occ.', 'SM', 'FP')

TR <- 0.392

## ---- load --------
source(file = 'src/load_datasets.R')

dfm <- dfm %>% 
  tibble::as_tibble() %>% 
  dplyr::mutate(pt = as.factor(pt)) %>% 
  dplyr::mutate(pt = relevel(pt, ref = 'Term'))

pgrs <- pgrs %>% 
  tibble::as_tibble() %>% 
  dplyr::mutate(pt = as.factor(pt)) %>% 
  dplyr::mutate(pt = relevel(pt, ref = 'Term'))


## ---- descStat --------
descStat <- dfm %>% 
  dplyr::group_by(sub, pma, ga, pt) %>% 
  dplyr::summarise(pma = mean(pma), 
                   ga = mean(ga)) %>% 
  dplyr::ungroup() %>% 
  ggplot2::ggplot(aes(x = ga, y = pma)) + 
  geom_point(aes(fill = pt), 
             size = 4, 
             pch = 21, 
             colour = 'white') + 
  geom_rug(aes(color = pt)) + 
  scale_fill_manual(values = c(mypal2, 
                               mypal2)) + 
  scale_colour_manual(values = c(mypal2, 
                                 mypal2)) + 
  xlab('GA at birth (weeks)') + 
  ylab('PMA at scan (weeks)') + 
  theme(legend.position = c(0.15, 0.15), 
        legend.background = element_blank(), 
        legend.title = element_blank())


dens1 <- dfm %>% 
  dplyr::group_by(sub, pma, ga, pt) %>% 
  dplyr::summarise(pma = mean(pma), 
                   ga = mean(ga)) %>% 
  dplyr::ungroup() %>% 
  
  ggplot2::ggplot(aes(x = ga, 
                      fill = pt, 
                      colour = pt)) + 
  geom_density(alpha = 0.4) + 
  theme_void() + 
  theme(legend.position = "none") + 
  scale_fill_manual(values = c(mypal2, 
                               mypal2)) + 
  scale_colour_manual(values = c(mypal2, 
                                 mypal2))

dens2 <- dfm %>% 
  dplyr::group_by(sub, pma, ga, pt) %>% 
  dplyr::summarise(pma = mean(pma), 
                   ga = mean(ga)) %>% 
  dplyr::ungroup() %>% 
  
  ggplot2::ggplot(aes(x = pma, 
                      fill = pt, 
                      colour = pt)) + 
  geom_density(alpha = 0.4) + 
  theme_void() + 
  theme(legend.position = "none") + 
  coord_flip() + 
  scale_fill_manual(values = c(mypal2, 
                               mypal2)) + 
  scale_colour_manual(values = c(mypal2, 
                                 mypal2))

dens1 + 
  plot_spacer() + 
  descStat + 
  dens2 + 
  plot_layout(ncol = 2, 
              nrow = 2, 
              widths = c(8, 1), 
              heights = c(1, 8))
