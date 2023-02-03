### AUTHOR: AHz
### WRITTEN IN: R version 4.2.2
### PURPOSE: 



################################################################################
# 0. LOAD DEPENDENCIES  ####
################################################################################

source("0 - setup.R")
load("data/processed/all_ppps_1a_output.RData")

################################################################################
# 1. JOIN PFAS DATA TO A CENSUS STRACT AND EJI INFO   ####
################################################################################


st_crs(eji_spatial_tidy)
st_crs(all_ppps_salvatore)
census_tracts_transformed<-st_transform(eji_spatial_tidy,st_crs(all_ppps_salvatore))


ppps_salvatore_tract <- all_ppps_salvatore %>% 
  sf::st_join(census_tracts_transformed, join = st_intersects)

ppps_eh249_tract <- all_ppps_eh249 %>% 
  sf::st_join(census_tracts_transformed, join = st_intersects)


#save as RData file so it can be opened in next script 
save(eji_spatial_tidy, ppps_salvatore_tract, ppps_eh249_tract,
     file = "census_eji_all_ppps_spatial.RData")


################################################################################
# 2. DROP SPATIAL DATA FOR ANALYSIS   ####
################################################################################


ppps_salvatore_tract_df <- st_drop_geometry(ppps_salvatore_tract) %>% 
  distinct() %>% 
  select(dataset:geoid)

ppps_eh249_tract_df <- st_drop_geometry(ppps_eh249_tract) %>% 
  distinct() %>% 
  select(dataset:geoid)


eji_tracts_df <- st_drop_geometry(eji_spatial_tidy)  %>% 
  distinct()

save(ppps_salvatore_tract_df, ppps_eh249_tract_df, eji_tracts_df, 
     file = "census_eji_all_ppps_wo_spatial.RData")




################################################################################
# 6. SUMMARIZE MERGES    ####
################################################################################



table(all_ppps$dataset, useNA = "ifany")


table(sites_census_tract$dataset)

st_drop_geometry(sites_census_tract) %>% 
  distinct() %>% 
  select(dataset:geoid)

table1 <- st_drop_geometry(all_ppps) %>% 
  distinct() %>% 
  group_by(dataset) %>% 
  count() %>% 
  mutate(count = "n") %>% 
  bind_rows(st_drop_geometry(sites_census_tract) %>% 
              distinct() %>% 
              group_by(dataset) %>% 
              count() %>% 
              mutate(count = "n_inEJI")) %>% 
  bind_rows(st_drop_geometry(all_ppps) %>% 
              distinct() %>% 
              group_by(.) %>% 
              count() %>% 
              mutate(dataset = "all ppps",
                     count = "n") %>% 
              bind_rows(st_drop_geometry(sites_census_tract) %>% 
                          distinct() %>% 
                          group_by(.) %>% 
                          count()%>% 
                          mutate(dataset = "all ppps",
                                 count = "n_inEJI"))) %>% 
  pivot_wider(names_from = count, values_from = n) %>% 
  mutate(perc_dif = (n_inEJI/n)) 


write_csv(table1, "summary of ppps merges.csv")




