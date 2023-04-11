### AUTHOR: AHz
### WRITTEN IN: R version 4.2.2
### PURPOSE: figures for slides/meeting with gary



################################################################################
# 0. LOAD DEPENDENCIES  ####
################################################################################

source("scripts/0 - setup.R")
load("data/processed/all_ppps_1a_output.RData")
load("data/processed/eji_spatial_1b_output.RData")
load("data/processed/all_ppps_WOspatial_1c_output.RData")
load("data/processed/all_ppps_spatial_1c_output.RData")
load("data/processed/newEJI_wpfasrank_1e_output.RData")

options(scipen = 999)



################################################################################
# PFAS METHODS SUMMARY -- SATX ####
################################################################################

pfas_sa <- ppps_salvatore_tract %>%
  filter(countyfp == "029" & StateAbbr == "TX")

ppps_sa <- eji_census_tracts_transformed %>%
  filter(county == "Bexar County" & state == "TX") 

ppps_sa_select <- ppps_sa %>%
  filter(geoid == "48029980100")

sa_check <- ppps_perc %>% 
  filter(countyfp == "029" & StateAbbr == "TX")


load("data/processed/step1_ppps_1mi_buffer.RData")

ppps_1mi_buffer_sa <- ppps_1mi_buffer %>%
  filter(countyfp == "029" & StateAbbr == "TX")

sa_geoids <- unique(sa_check$geoid)

sa_comb_buffers <- do.call(rbind, buffers[sa_geoids]) %>%
  data.frame() %>% 
  pivot_longer(names_to = "geoid", values_to = "geometry", everything()) %>% 
  sf::st_as_sf()




save(pfas_sa, ppps_sa, ppps_sa_select, sa_check, 
     ppps_1mi_buffer_sa, sa_comb_buffers, file = "data/processed/sa_files.RData")

tm_shape(sa_check) + 
  tm_polygons(alpha = 0.8) + 
  tm_shape(sa_buffers) +
  tm_polygons(alpha=0.8, col = "turquoise") 





ppps_sa <- eji_census_tracts_transformed %>%
  filter(county == "Bexar County" & state == "TX") 

ppps_sa_select <- ppps_sa %>% 
  filter(geoid == "48029980100")

pfas_sa <- ppps_salvatore_tract %>%
  filter(countyfp == "029" & StateAbbr == "TX")

ppps_1mi_buffer_sa <- pfas_sa %>%
  st_buffer(1609.34)

ppps_buffer_overlap_sa <- ppps_1mi_buffer_sa %>% 
  st_union() %>% 
  st_make_valid()

buffers_sa <- st_intersection(ppps_buffer_overlap_sa, ppps_sa_select)


buffers_sa <- list()
perc_calc_sa <- list()
for(i in unique(ppps_sa$geoid)){
  
  ppps_sa_select2 <- ppps_sa %>% 
    filter(geoid == i)
  
  buffers_sa[[i]] <- suppressMessages(st_intersection(ppps_buffer_overlap_sa, ppps_sa_select2))
  
  perc_calc_sa[[i]] <- st_area(buffers_sa[[i]])/st_area(ppps_sa_select2)
  
}

buffers_diff_sa <- buffers_sa[lapply(buffers_sa, length) > 0] %>% 
  bind_rows() %>% 
  pivot_longer(names_to = "geoid", values_to = "geometry", everything()) %>% 
  st_as_sf()

perc_tract_pfas_sa <- perc_calc_sa[lapply(perc_calc_sa, length) > 0] %>% 
  bind_rows() %>% 
  pivot_longer(names_to = "geoid", values_to = "perc_area_pfas", everything()) 

ppps_sa_perc <- ppps_sa %>% 
  left_join(perc_tract_pfas_sa, by = "geoid")  %>% 
  mutate(perc_area_pfas = units::drop_units(perc_area_pfas))



# ppps_sa_tm <- tm_shape(ppps_sa) +
#   tm_polygons(alpha = 0.8) + 
#   tm_shape(pfas_sa) +
#   tm_dots()
# 
# 
# tmap_save(ppps_sa_tm, "output/figures/PFAS sources in SA.html")

################################################################################
# SUMMARIZE PFAS VARS ####
################################################################################

perc_area_pfas_density <- ggplot(percentilerank_pfas, aes(x =  perc_area_pfas_0)) %>% 
  density_plot() + 
  #ggtitle("Distribution of perc_area_pfas for all census tracts in EJI") + 
  xlab("% of census tract within a 1-mile\nbuffer of a PFAS source") + 
  theme_bw(base_size = 22)

perc_rank_pfas_density <- ggplot(percentilerank_pfas, aes(x =  percent_rank_pfas)) %>% 
  density_plot() + 
  #ggtitle("Distribution of percent_rank_pfas for all census tracts in EJI")+ 
  xlab("Percentile rank of proportion of census tract\nwithin 1-mile buffer of a PFAS source") + 
  theme_bw(base_size = 22)

cowplot::plot_grid(perc_area_pfas_density, perc_rank_pfas_density)