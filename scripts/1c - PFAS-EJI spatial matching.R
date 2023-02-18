### AUTHOR: AHz
### WRITTEN IN: R version 4.2.2
### PURPOSE: 



################################################################################
# 0. LOAD DEPENDENCIES  ####
################################################################################

source("scripts/0 - setup.R")
load("data/processed/all_ppps_1a_output.RData")
load("data/processed/eji_spatial_1b_output.RData")

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
  sf::st_join(eji_census_tracts_transformed, join = st_intersects)

ppps_eh249_tract <- all_ppps_eh249 %>% 
  sf::st_join(eji_census_tracts_transformed, join = st_intersects)


#save as RData file so it can be opened in next script 
save(eji_spatial_tidy, ppps_salvatore_tract, ppps_eh249_tract,
     file = "data/processed/all_ppps_spatial_1c_output.RData")


################################################################################
# 2. DROP SPATIAL DATA FOR ANALYSIS   ####
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



