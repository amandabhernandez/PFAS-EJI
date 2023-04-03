### AUTHOR: AHz
### WRITTEN IN: R version 4.2.2
### PURPOSE: 



################################################################################
# 0. LOAD DEPENDENCIES  ####
################################################################################

source("scripts/0 - setup.R")
load("data/processed/all_ppps_1a_output.RData")
load("data/processed/eji_spatial_1b_output.RData")

tmap_mode("view")

################################################################################
# 1. JOIN PFAS DATA TO A CENSUS STRACT AND EJI INFO   ####
################################################################################


st_crs(eji_spatial_tidy)$Name
st_crs(all_ppps_salvatore)$Name
st_crs(all_ppps_eh249)$Name
message(st_crs(eji_spatial_tidy)$Name == st_crs(all_ppps_salvatore)$Name)
message(st_crs(all_ppps_salvatore)$Name == st_crs(all_ppps_eh249)$Name)


eji_census_tracts_transformed <- st_transform(eji_spatial_tidy, 
                                          st_crs(all_ppps_salvatore))

# find the overlap between a site and a census tract to assign a geoid to
# each facility
ppps_salvatore_tract <- all_ppps_salvatore %>% 
  st_join(eji_census_tracts_transformed, join = st_intersects)

ppps_eh249_tract <- all_ppps_eh249 %>% 
  st_join(eji_census_tracts_transformed, join = st_intersects)


#save as RData file so it can be opened in next script 
save(eji_census_tracts_transformed, ppps_salvatore_tract, ppps_eh249_tract,
     file = "data/processed/all_ppps_spatial_1c_output.RData")

# load("data/processed/all_ppps_spatial_1c_output.RData")

################################################################################
# 2. ADD 1 MILE BUFFER   ####
################################################################################

ppps_1mi_buffer <- ppps_salvatore_tract %>%
  st_buffer(1609.34)

beepr::beep(10)

#save as RData file so it can be opened in next script 
save(ppps_1mi_buffer,
     file = "data/processed/step1_ppps_1mi_buffer.RData")

# load("data/processed/step1_ppps_1mi_buffer.RData")

# tm_shape(ppps_1mi_buffer$buffer) +
#   tm_polygons(alpha=0.8, col = "red") +
#   tm_shape(ppps_1mi_buffer$geometry) +
#   tm_dots(alpha=0.8)




################################################################################
# 3. COMBINE BUFFERS   ####
################################################################################

ppps_buffer_overlap <- ppps_1mi_buffer %>% 
  st_union() 

beepr::beep(10)

#save as RData file so it can be opened in next script 
save(ppps_buffer_overlap,
     file = "data/processed/step3_ppps_buffer_overlap.RData")

ppps_buffer_overlap <- ppps_buffer_overlap %>% 
  st_make_valid()

beepr::beep(10)


load("data/processed/step3_ppps_buffer_overlap.RData")

################################################################################
# 4. CALC PERC OVERLAY   ####
################################################################################

load("data/processed/buffering_2023-04-02 18:55:26.RData")

start_time <- lubridate::now()
#buffers <- list()
#perc_calc <- list()
### ppps_1mi_buffer_list <- list()
### ppps_buffer_overlap_list <- list()
#geoids_completed <- list()
start_time_batch <- lubridate::now()
end_time_batch <- ""
# perc_tract_pfas <- data.frame(geoid = c(""), geometry = c(""))

# batch_cutpoints <- c(eji_census_tracts_transformed$geoid[c(seq(0, length(
#   unique(eji_census_tracts_transformed$geoid)), 1000))])

# n_completed <- 0
# batches_to_complete <- floor((length(unique(eji_census_tracts_transformed$geoid))-length(geoids_completed))/1000)

sf_use_s2(FALSE)

# geoids_completed <- names(perc_calc)
# basically, the loop only worked for census tracts where there was a point source... 
# so that's all still valid, but now we need to loop through the other census tracts
# for simplicity/efficiency, I'm going to just skip the geoids that have actually been done. 
# load("data/processed/buffering_FIXmaybe.RData")
geoids_completed <- names(perc_calc)
  
for (i in unique(eji_census_tracts_transformed$geoid[which(!eji_census_tracts_transformed$geoid %in% geoids_completed)])) {

  tract_select <- eji_census_tracts_transformed %>%
      filter(geoid == i)
    
    buffers[[i]] <-
      suppressMessages(st_intersection(ppps_buffer_overlap, tract_select))
    
    perc_calc[[i]] <- st_area(buffers[[i]]) / st_area(tract_select)
    
    geoids_completed <- append(geoids_completed, i)
    
    n_completed <- n_completed + 1
  
  #}
  
  if (n_completed %% 1000 == 0) {
    end_time_batch <- now()

    print(paste0(
      "batch ",
      n_completed/1000,
      " of ",
      batches_to_complete,
      " complete at ", Sys.time() ," in ", 
      as.duration(interval(start_time_batch, end_time_batch)), 
      " approximately ", 
      as.duration(interval(start_time_batch, end_time_batch))*(batches_to_complete-n_completed/1000),
      " remaining"
    ))
    beepr::beep(sound = 10)
    start_time_batch <- now()
  }
  
}


save(geoids_completed, tract_select, buffers, perc_calc,
     file = paste0("data/processed/buffering_",Sys.time(),".RData"))


perc_tract_pfas <- perc_calc[lapply(perc_calc, length) > 0] %>% 
  bind_rows() %>% 
  pivot_longer(names_to = "geoid", values_to = "perc_area_pfas", everything()) 

ppps_perc <- eji_census_tracts_transformed %>% 
  left_join(perc_tract_pfas, by = "geoid") %>% 
  mutate(perc_area_pfas = units::drop_units(perc_area_pfas),
         perc_area_pfas_0 = case_when(is.na(perc_area_pfas) ~ 0,
                                      TRUE ~ perc_area_pfas))

summary(ppps_perc$perc_area_pfas_0)


### run some checks: 


#this geoid overlaps with a ppps source but doesn't have it's own, so in theory 
#it should have > 0 perc_area_pfas
check_tract <- "48029161501"

sa_check <- ppps_perc %>% 
  filter(countyfp == "029" & StateAbbr == "TX")

sa_geoids <- unique(sa_check$geoid)

sa_buffers <- do.call(rbind, buffers[sa_geoids]) %>%
  data.frame() %>% 
  pivot_longer(names_to = "geoid", values_to = "geometry", everything()) %>% 
  sf::st_as_sf()

tm_shape(sa_check) + 
  tm_polygons(alpha = 0.8) + 
  tm_shape(sa_buffers) +
  tm_polygons(alpha=0.8, col = "turquoise") 







#### 
# load("data/processed/ppps_w_buffer_perc_tract_intersecting.RData")
# 
# 
# 
# this is what I was doing, but I failed to consider that looping through the 
# census tracts at the first step would mean that we lose info about census tracts
# that don't have a source but are within 1 mile of a source... so I have to do that
# outside of the for loop

start_time <- lubridate::now()
buffers <- list()
perc_calc <- list()
ppps_1mi_buffer_list <- list()
ppps_buffer_overlap_list <- list()
geoids_completed <- list()
start_time_batch <- now()
end_time_batch <- ""
# perc_tract_pfas <- data.frame(geoid = c(""), geometry = c(""))

batch_cutpoints <- c(eji_census_tracts_transformed$geoid[c(seq(0, length(
  unique(eji_census_tracts_transformed$geoid)), 1000))])




for (i in unique(eji_census_tracts_transformed$geoid[which(!eji_census_tracts_transformed$geoid %in% geoids_completed)])) {
  
  geoids_completed <- append(geoids_completed, i)
  
  ppps_1mi_buffer_list[[i]] <- ppps_salvatore_tract %>%
    filter(geoid == i) 
  
  if(!is_empty(ppps_1mi_buffer_list[[i]]$geometry)){
  
    ppps_buffer_overlap_list[[i]] <- ppps_1mi_buffer_list[[i]] %>% 
      st_union() %>% 
      st_make_valid()
    
    tract_select <- eji_census_tracts_transformed %>%
      filter(geoid == i)
    
    buffers[[i]] <-
      st_intersection(ppps_buffer_overlap_list[[i]], tract_select)
    
    perc_calc[[i]] <- st_area(buffers[[i]]) / st_area(tract_select)
    
  
  }
  
  if (i %in% batch_cutpoints) {
    end_time_batch <- now()
    print(paste0(
      "batch ",
      which(batch_cutpoints %in% i),
      " of ",
      length(batch_cutpoints),
      " complete in ", 
      as.duration(interval(start_time_batch, end_time_batch)), 
      " approximately ", 
      as.duration(interval(start_time_batch, end_time_batch))*(length(batch_cutpoints)-which(batch_cutpoints %in% i)),
      " remaining"
    ))
    beepr::beep(sound = 10)
    start_time_batch <- now()
  }
  

}


save(geoids_completed, ppps_1mi_buffer_list, ppps_buffer_overlap_list, 
     tract_select, buffers, perc_calc,
     file = "data/processed/buffering.RData")


# manually fix a tract that throws an error -- go back and figure out the 
# sf_use_s2 on/off situation -- this seemed helpful: https://r-spatial.org/book/07-Introsf.html#ellipsoidal-coordinates

# tm_shape(tract_select) + 
#   tm_polygons() + 
#   tm_shape(ppps_buffer_overlap[[error_tract]]) +
#   tm_polygons(alpha = 0.8, col = "purple") + 
#   tm_shape(st_intersection(ppps_buffer_overlap[[error_tract]], tract_select)) + 
#   tm_polygons()

error_tract <- "36087011602"
#error: 36087011602
#Error in wk_handle.wk_wkb(wkb, s2_geography_writer(oriented = oriented,  : 
#Loop 0 is not valid: Edge 342 is degenerate (duplicate vertex)

sf_use_s2(FALSE)

ppps_1mi_buffer_list[[error_tract]] <- ppps_salvatore_tract %>%
  filter(geoid == error_tract) %>% 
  st_buffer(1609.34)

ppps_buffer_overlap_list[[error_tract]] <- ppps_1mi_buffer_list[[error_tract]] %>% 
  st_union() %>% 
  st_make_valid()

tract_select <- eji_census_tracts_transformed %>%
  filter(geoid == error_tract)

buffers[[error_tract]] <-
  st_intersection(ppps_buffer_overlap_list[[error_tract]], tract_select)

perc_calc[[error_tract]] <- st_area(buffers[[error_tract]]) / st_area(tract_select)

sf_use_s2(TRUE)



perc_tract_pfas <- perc_calc[lapply(perc_calc, length) > 0] %>% 
  bind_rows() %>% 
  pivot_longer(names_to = "geoid", values_to = "perc_area_pfas", everything()) 

ppps_perc <- eji_census_tracts_transformed %>% 
  left_join(perc_tract_pfas, by = "geoid") %>% 
  mutate(perc_area_pfas = units::drop_units(perc_area_pfas),
         perc_area_pfas_0 = case_when(is.na(perc_area_pfas) ~ 0,
                                    TRUE ~ perc_area_pfas))

# ggplot(ppps_perc, aes(fill = perc_area_pfas)) + 
#   geom_sf() +
#   scale_fill_distiller(palette = "PuRd", direction= 1)
# 
# beepr::beep(10)


# tm_shape(eji_census_tracts_transformed) + 
#   tm_polygons() + 
#   tm_shape(ppps_sa_perc) + 
#   tm_polygons("perc_area_pfas", palette = "PuRd")



save(perc_tract_pfas, ppps_perc, 
     file = "data/processed/ppps_w_buffer_perc_tract_intersecting_FIXmaybe.RData")

################################################################################
# 5. LOOK AT PERC OVERLAP   ####
################################################################################

gtsummary::tbl_summary(ppps_perc %>% 
                         st_drop_geometry(), 
                       include = "perc_area_pfas_0",
                       type = list(everything() ~ "continuous2"),
                           statistic = list(
                             everything() ~ c("{mean} ({var})",
                                              "{median} ({p25}, {p75})",
                                              "{p80}",
                                              "{p85}",
                                              "{p90}",
                                              "{p95}",
                                              "{p99}")
                           ))

summary(ppps_perc$perc_area_pfas_0)




ggplot(ppps_perc, aes(x = perc_area_pfas)) + 
  geom_histogram(aes(y = after_stat(density)),
                 color = "black",
                 fill = "white",
                 bins = 30) +
  geom_density(fill = "red", alpha = 0.25) + 
  ggtitle("Distribution of perc_area_pfas for census tracts with a PFAS point source")

ggplot(ppps_perc, aes(x = perc_area_pfas_0)) + 
  geom_histogram(aes(y = after_stat(density)),
                 color = "black",
                 fill = "white",
                 bins = 30) +
  geom_density(fill = "red", alpha = 0.25) + 
  ggtitle("Distribution of perc_area_pfas for all census tracts in EJI")


ppps_perc %>% 
  filter(!is.na(StateAbbr)) %>% 
  ggplot(aes(fill = perc_area_pfas)) +
  geom_sf() +
  scale_fill_distiller(palette = "PuRd", direction= 1)

beepr::beep(10)

ggsave("output/figures/perc_area_pfas_map.png", height = 20, width = 30)



tm_shape(ppps_perc) +
  tm_polygons("perc_area_pfas", palette = "PuRd")


################################################################################
# 6. DROP SPATIAL DATA FOR ANALYSIS   ####
################################################################################

# now we can drop spatial data because we just need the geoids

ppps_salvatore_tract_df <- st_drop_geometry(ppps_salvatore_tract) %>% 
  distinct() %>% 
  select(dataset:geoid)

ppps_eh249_tract_df <- st_drop_geometry(ppps_eh249_tract) %>% 
  distinct() %>% 
  select(dataset:geoid)


eji_tracts_df <- st_drop_geometry(eji_spatial_tidy)  %>% 
  distinct()


ppps_perc_df <- st_drop_geometry(ppps_perc)  %>% 
  distinct()

save(ppps_salvatore_tract_df, ppps_eh249_tract_df, 
     eji_tracts_df, ppps_perc_df,
     file = "data/processed/all_ppps_WOspatial_1c_output.RData")






############################# GARBAGE <3 ######################################
#
#
#
#### ALL THE SA TEST STUFF
# # group_map(census_tract) %>% st_difference


test_tract <- "48029980100"

# sf_use_s2(FALSE)
# sf_use_s2(TRUE)

ppps_sa <- eji_census_tracts_transformed %>%
  filter(county == "Bexar County" & state == "TX") 

# ppps_1mi_buffer_sa <- ppps_salvatore_tract %>%
#   filter(city == "SAN ANTONIO" & state.x == "TX") %>% 
#   mutate(buffer = st_buffer(geometry, 1609.34))

pfas_sa <- ppps_salvatore_tract %>%
  filter(city == "SAN ANTONIO" & state.x == "TX") 

ppps_1mi_buffer_sa <- pfas_sa %>%
  st_buffer(1609.34)

tm_shape(ppps_1mi_buffer_sa$buffer) +
  tm_polygons(alpha=0.8, col = "red") +
  tm_shape(ppps_1mi_buffer_sa$geometry) +
  tm_dots(alpha=0.8)

ppps_buffer_overlap_sa <- ppps_1mi_buffer_sa %>% 
  st_union() %>% 
  st_make_valid()

tm_shape(ppps_sa) + 
  tm_polygons() + 
  tm_shape(ppps_buffer_overlap_sa) +
  tm_polygons(alpha=0.8, col = "turquoise") +
  tm_shape(ppps_1mi_buffer_sa$geometry) +
  tm_dots(alpha=0.8)

# st_area(ppps_buffer_overlap$overlap)


buffers_sa <- list()
perc_calc_sa <- list()
batch_cutpoints_sa <- c(unique(ppps_sa$geoid)[c(seq(0, length(
  unique(ppps_sa$geoid)), 100))])

for(i in unique(ppps_sa$geoid)){
  
  ppps_sa_select <- ppps_sa %>% 
    filter(geoid == i)
  
  buffers_sa[[i]] <- st_intersection(ppps_buffer_overlap_sa, ppps_sa_select)
  
  perc_calc_sa[[i]] <- st_area(buffers_sa[[i]])/st_area(ppps_sa_select)
  
  if(i %in% batch_cutpoints_sa){
    print(paste0("batch ", which(batch_cutpoints_sa %in% i)," of ", length(batch_cutpoints_sa), " complete"))
    beepr::beep(sound = 10)
  }
  
}

buffers_diff_sa <- buffers_sa[lapply(buffers_sa, length) > 0] %>% 
  bind_rows() %>% 
  pivot_longer(names_to = "geoid", values_to = "geometry", everything()) %>% 
  st_as_sf()

tm_shape(ppps_sa) + 
  tm_polygons() + 
  tm_shape(buffers_diff_sa$geometry) + 
  tm_polygons(alpha=0.8, col = "turquoise", border.col = "red") +
  tm_shape(ppps_1mi_buffer_sa$geometry) +
  tm_dots(alpha=0.8)


perc_tract_pfas_sa <- perc_calc_sa[lapply(perc_calc_sa, length) > 0] %>% 
  bind_rows() %>% 
  pivot_longer(names_to = "geoid", values_to = "perc_area_pfas", everything()) 

ppps_sa_perc <- ppps_sa %>% 
  left_join(perc_tract_pfas_sa, by = "geoid") %>% 
  mutate(perc_area_pfas = units::drop_units(perc_area_pfas))


ggplot(ppps_sa_perc, aes(fill = perc_area_pfas)) + 
  geom_sf() +
  scale_fill_distiller(palette = "PuRd", direction= 1)



tm_shape(ppps_sa_perc) + 
  tm_polygons("perc_area_pfas", palette = "PuRd") + 
  tm_shape(buffers_diff_sa$geometry) + 
  tm_polygons(border.col = "turquoise", alpha = 0) +
  tm_shape(ppps_1mi_buffer_sa$geometry) +
  tm_dots()






#
#
#
## ppps_1mi_buffer_sa <- ppps_salvatore_tract %>%
#   filter(city == "SAN ANTONIO" & state.x == "TX") %>% 
#   st_buffer(1609.34)




#  #group_by(geoid) %>% 
#st_union(by_feature = TRUE) %>% 
#
##st_union
# ppps_buffer_overlap <- ppps_1mi_buffer_sa %>% 
#   group_by(geoid) %>% 
#   summarize(overlap = st_combine(.$buffer)) %>% 
#   st_make_valid()
#   # st_union(by_feature = TRUE)
#   # mutate(union_buffer = st_union(buffer))
#   

# buffer_overlap_tracts <- buffer_overlap_tracts %>% g
#   filter(state == "TX" & county == "Bexar County") 

# tm_shape(buffer_overlap_tracts$geometry) +
#   tm_polygons(alpha=0.8) + 
# tm_shape(ppps_buffer_overlap) +
#   tm_polygons(alpha=0.8, col = "pink")
#   
#   
tract_polygon <- buffer_overlap_tracts %>% 
  group_by(geoid) %>% 
  mutate(tract_p = st_difference(ppps_buffer_overlap))


tm_shape(buffer_overlap_tracts) + 
  tm_polygons(alpha = 0.8) + 
  tm_shape(tract_polygon$geometry) +
  tm_polygons(alpha=0.8, col = "turquoise") 
# tm_shape(ppps_buffer_overlap) +
# tm_polygons(alpha=0.8, col = "pink")

