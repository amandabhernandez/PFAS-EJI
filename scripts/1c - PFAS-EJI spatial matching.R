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
save(eji_spatial_tidy, ppps_salvatore_tract, ppps_eh249_tract,
     file = "data/processed/all_ppps_spatial_1c_output.RData")


################################################################################
# 2. ADD 1 MILE BUFFER   ####
################################################################################


test_tract <- "48029980100"

# sf_use_s2(FALSE)
# sf_use_s2(TRUE)

ppps_sa <- eji_census_tracts_transformed %>%
  filter(county == "Bexar County" & state == "TX") 

# ppps_1mi_buffer_sa <- ppps_salvatore_tract %>%
#   filter(city == "SAN ANTONIO" & state.x == "TX") %>% 
#   mutate(buffer = st_buffer(geometry, 1609.34))

ppps_1mi_buffer_sa <- ppps_salvatore_tract %>%
    filter(city == "SAN ANTONIO" & state.x == "TX") %>%
    st_buffer(1609.34)

tm_shape(ppps_1mi_buffer_sa$buffer) +
  tm_polygons(alpha=0.8, col = "red") +
  tm_shape(ppps_1mi_buffer_sa$geometry) +
  tm_dots(alpha=0.8)


#### okay, now do it for the whole country (rip)

ppps_1mi_buffer <- ppps_salvatore_tract %>%
  st_buffer(1609.34)

beepr::beep(10)

# tm_shape(ppps_1mi_buffer$buffer) +
#   tm_polygons(alpha=0.8, col = "red") +
#   tm_shape(ppps_1mi_buffer$geometry) +
#   tm_dots(alpha=0.8)




################################################################################
# 3. COMBINE BUFFERS   ####
################################################################################


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

#### okay, now do it for the whole country (rip)

ppps_buffer_overlap <- ppps_1mi_buffer %>% 
  st_union() %>% 
  st_make_valid()

beepr::beep(10)


################################################################################
# 4. CALC PERC OVERLAY   ####
################################################################################

# group_map(census_tract) %>% st_difference

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

#### okay, now do it for the whole country (rip)

buffers <- list()
perc_calc <- list()
batch_cutpoints <- c(eji_census_tracts_transformed$geoid[c(seq(0, length(
  unique(eji_census_tracts_transformed$geoid)
), 1000))])

for(i in unique(eji_census_tracts_transformed$geoid)){
  
  #if(!i %in% c(perc_tract_pfas$geoid)){
    
    ppps_select <- eji_census_tracts_transformed %>% 
      filter(geoid == i)
    
    buffers[[i]] <- st_intersection(ppps_buffer_overlap, ppps_select)
    
    perc_calc[[i]] <- st_area(buffers[[i]])/st_area(ppps_select)
  #}
  
  if(i %in% batch_cutpoints){
    print(paste0("batch ", which(batch_cutpoints %in% i)," of ", length(batch_cutpoints), " complete"))
    #beepr::beep(sound = 10)
  }
}

beepr::beep(sound = 10)

perc_tract_pfas <- perc_calc[lapply(perc_calc, length) > 0] %>% 
  bind_rows() %>% 
  pivot_longer(names_to = "geoid", values_to = "perc_area_pfas", everything()) 

ppps_perc <- eji_census_tracts_transformed %>% 
  left_join(perc_tract_pfas, by = "geoid") %>% 
  mutate(perc_area_pfas = units::drop_units(perc_area_pfas))

ggplot(ppps_perc, aes(fill = perc_area_pfas)) + 
  geom_sf() +
  scale_fill_distiller(palette = "PuRd", direction= 1)

beepr::beep(10)


# tm_shape(eji_census_tracts_transformed) + 
#   tm_polygons() + 
#   tm_shape(ppps_sa_perc) + 
#   tm_polygons("perc_area_pfas", palette = "PuRd")
# 



save(ppps_perc, 
     file = "data/processed/ppps_w_buffer_perc_tract_intersecting.RData")

################################################################################
# X. DROP SPATIAL DATA FOR ANALYSIS   ####
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

save(ppps_salvatore_tract_df, ppps_eh249_tract_df, eji_tracts_df, 
     file = "data/processed/all_ppps_WOspatial_1c_output.RData")






############################# GARBAGE <3 ######################################
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
