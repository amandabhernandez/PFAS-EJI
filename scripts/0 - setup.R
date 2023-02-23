### AUTHOR: AHz
### WRITTEN IN: R version 4.2.2
### Purpose: set up dependencies for PFAS-EJI code



################################################################################
#  LOAD PACKAGES  ####
################################################################################

library(pacman)
p_load(tidyverse, readxl, janitor, sf, broom, flextable, tmap)

################################################################################
#  SET WD  ####
################################################################################

setwd(here::here())

################################################################################
#  OPTIONS  ####
################################################################################

theme_set(theme_bw(base_size= 22))

################################################################################
#  FUNCTIONS  ####
################################################################################

sina_boxplot <- function(ggobj){
    ggobj + 
    ggforce::geom_sina(alpha = 0.5, color = "lightgrey", show.legend = TRUE) +
    geom_boxplot(width = 0.1, guides = FALSE, outlier.shape = NA, size = 1, color = "#3a3838") 
}


density_plot <- function(ggobj){
  ggobj + 
    geom_histogram(aes(y = after_stat(density)),
                   color = "black",
                   fill = "white",
                   bins = 30) +
    geom_density(fill = "red", alpha = 0.25)
}