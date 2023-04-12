### AUTHOR: AHz
### WRITTEN IN: R version 4.2.2
### Purpose: set up dependencies for PFAS-EJI code



################################################################################
#  LOAD PACKAGES  ####
################################################################################

library(pacman)
p_load(tidyverse, readxl, janitor, sf, broom, flextable, tmap, lubridate)

################################################################################
#  SET WD  ####
################################################################################

setwd(here::here())

################################################################################
#  OPTIONS  ####
################################################################################

theme_set(theme_bw())

################################################################################
#  FUNCTIONS  ####
################################################################################

sina_boxplot <- function(ggobj){
    ggobj + 
    ggforce::geom_sina(alpha = 0.5, color = "lightgrey", show.legend = TRUE) +
    geom_boxplot(width = 0.1, guides = FALSE, outlier.shape = NA, 
                 size = 1, color = "#3a3838", alpha = 0.5) 
}


density_plot <- function(ggobj, bin_num = 30){
  ggobj + 
    geom_histogram(aes(y = after_stat(density)),
                   color = "black",
                   fill = "white",
                   bins = bin_num) +
    geom_density(fill = "red", alpha = 0.25)
}


tbl_theme <- function(dat, gt = FALSE, caption = "", footer = ""){
  if(gt == FALSE) {
    dat_flex <- dat %>% 
      as_flextable(show_coltype = FALSE)
  }
  else {
    dat_flex <- dat %>% 
      as_flex_table(show_coltype = FALSE) 
  }
  
  dat_flex %>% 
    set_caption(caption = caption) %>% 
    add_footer_lines(footer) %>% 
    color(part = "footer", color = "#666666") %>% 
    set_table_properties(layout = "autofit", width = 1)
}


################################################################################
#  CODEBOOK  ####
################################################################################



eji_codebook <- read_csv("data/raw/EJI/eji_codebook/files/output/eji_codebook.csv") 

eji_codebook_revised <- eji_codebook %>% 
  mutate(var_desc = case_when(str_detect(variable_name, "(?<=_).*") ~ str_extract(variable_name, "(?<=_).*"),
                              TRUE ~ variable_name),
         var_metric = str_extract(variable_name, "^.{3}(?=_)"))
