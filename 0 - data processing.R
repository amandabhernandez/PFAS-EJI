### AUTHOR: AHz
### WRITTEN IN: R version 4.2.1
### Purpose: Load and process presumptive pfas data 


################################################################################
#  0. SET UP  ####
################################################################################


library(pacman)
p_load(tidyverse)
p_load(janitor)
p_load(readxl)
p_load(tmap)

options (stringsAsFactors = FALSE)

source_file_loc <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(source_file_loc)



################################################################################
#  1. LOAD SOME DATA!!!  ####
################################################################################


keepcols <- c("loc_name", "city", "county", "state", "geometry")

#for easy merging and cleaning later, add uniform state/county info 
add_dc <- data.frame("stateabb" = "DC",
                     "fullstate" = "District of Columbia",
                     "fullstate_lower" = "district of columbia")

stateabbnamekey <- as.data.frame(matrix(ncol = 3, nrow = 50))
colnames(stateabbnamekey) <- c("stateabb", "fullstate", "fullstate_lower")
stateabbnamekey <- stateabbnamekey %>% 
  mutate(stateabb = state.abb,
         fullstate = state.name,
         fullstate_lower = tolower(state.name))  %>% 
  bind_rows(add_dc)



################################################################################
#  1a. EPA STEWARDSHIP PROGRAM ####
################################################################################

#PFAS POINT SOURCE FILES FROM UCMR/EJ PROJECT (thx Jahred)

epastewardship <- read_excel("../../../../UCMR3/Data/PFAS point source data/Data/EPA 2010.2015 PFOA Stewardship Program sites.xlsx")

# Reshaping, processing, and renaming columns for point source data by counties
epastewardship_county <- epastewardship %>% 
  mutate(geography = paste0(tolower(County), " county, ", tolower(State))) %>% 
  separate(Coordinates, sep = ", ", into = c("lat", "long"))


epastewardship_county_sf <- epastewardship_county %>%
  sf::st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  mutate(loc_name = as.character(Company)) %>% 
  clean_names() %>% 
  select(any_of(keepcols))


################################################################################
#  1b. AIRPORTS ####
################################################################################


# PFAS POINT SOURCE FILES FROM UCMR/EJ PROJECT (also thx Jahred)
airports <- read_excel("../../../../UCMR3/Data/PFAS point source data/Data/Part 139_cert_airports.xlsx")

airports_county <- airports %>%
  left_join(stateabbnamekey, by = c("CountyState" = "stateabb")) %>%
  mutate(geography = paste0(tolower(County), " county, ", tolower(fullstate)))

airports_sf <- airports_county %>%
  mutate(lat = as.numeric(str_extract(ARPLatitudeS, "\\d*.\\d*"))/3600,
         long = as.numeric(str_extract(ARPLongitudeS, "\\d*.\\d*"))/-3600) %>%
  sf::st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  rename(loc_name = FacilityName) %>%
  clean_names() %>%
  select(any_of(keepcols))



################################################################################
#  1c. FIRE TRAINING FACILITIES ####
################################################################################
fire <- read_xlsx("PFAS Point Source Data/ECHO_Fire_Training_Industry3_0_0.xlsx") 

fire_sf <- fire %>% 
  separate(Location, c("lat", "long"), ", ") %>% 
  filter(!is.na(long)) %>% 
  sf::st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  rename(loc_name = Facility) %>% 
  clean_names() %>% 
  select(any_of(keepcols))

################################################################################
#  1d.  FEDERAL AGENCY LOCATIONS WITH KNOWN OR SUSPECTED PFAS DETECTIONS   ####
################################################################################

fed_agencies <- read_xlsx("PFAS Point Source Data/Federal_Agency_Locations_with_Known_or_Suspected_PFAS_Detections_02-28-2022.xlsx",
                          sheet = 2)

fed_agencies_sf <- fed_agencies %>% 
  filter(!is.na(as.numeric(Latitude)))%>% 
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  rename(loc_name = `Site Name`) %>% 
  clean_names() %>% 
  select(any_of(keepcols))




################################################################################
# 1e. SUPERFUND  ####
################################################################################

superfund <- read_xlsx("PFAS Point Source Data/Superfund_Sites_with_PFAS_Detections_03-04-2022_0.xlsx",
                       sheet = 2)

superfund_sf <- superfund %>% 
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  rename(loc_name = `Site Name`) %>% 
  clean_names() %>% 
  select(any_of(keepcols))





################################################################################
# 1f. FACILITIES  ####
################################################################################

facilities <- read_xlsx("PFAS Point Source Data/Facilities_in_industries_that_may_be_handling_PFAS_01-03-2022.xlsx",
                        sheet = 2)

facilities_sf <- facilities %>% 
  mutate(frs_id = str_extract(`ECHO Facility Report`, "(?<=fid=)\\d*")) %>% 
  filter(!Industry %in% c("Airports", "Fire Training", "National Defense")) %>% 
  filter(!is.na(Latitude)) %>% 
  filter(TRI_FLAG == "N") %>% 
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  rename(loc_name = Facility) %>% 
  clean_names() %>% 
  select(any_of(keepcols)) 


check_facilities <- facilities_sf %>% 
  filter(!state %in% stateabbnamekey$stateabb)


################################################################################
# 1g. SPILLS  ####
################################################################################

spills <- read_xlsx("PFAS Point Source Data/Initial_Calls_Reported_to_NRC_Indicating_AFFF_Usage_02-23-2022.xlsx",
                    sheet = 2)

spills_sf <- spills %>% 
  mutate(across(c(Latitude, Longitude), as.numeric),
         across(SEQNOS, as.character)) %>% 
  filter(!is.na(Latitude)) %>% 
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  rename(loc_name = SEQNOS) %>% 
  clean_names() %>% 
  select(any_of(keepcols))


################################################################################
# 1h. PRODUCTION  ####
################################################################################

production <- read_xlsx("PFAS Point Source Data/PFAS_Production_Data_02-28-2022_0.xlsx", sheet = 2)

table(production$Identifier %in% c(facilities_id$frs_id))

production_sf <- production %>% 
  mutate(across(c(Latitude, Longitude), as.numeric)) %>% 
  filter(!is.na(Latitude)) %>% 
  filter(!Identifier %in% facilities_id$frs_id) %>% 
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  rename(loc_name = `Facility Name`) %>% 
  clean_names() %>% 
  select(any_of(keepcols))


################################################################################
# 2. JOIN TOGETHER ####
################################################################################

all_ppps <- bind_rows(lst(epastewardship_county_sf, 
                          fire_sf, airports_sf,
                          fed_agencies_sf, superfund_sf, 
                          facilities_sf, spills_sf, production_sf), .id = "dataset") 

nrow(all_ppps)

################################################################################
# 3. LOAD EJI  ####
################################################################################

### load csv from CDC site
eji_data_fromCDC <- read_csv("eji/DataRecords.csv")


################################################################################
# 4. GET CENSUS TRACT INFO  ####
################################################################################

p_load(tigris)

# state_list <- c("MA")
state_list <- unique(stateabbnamekey$stateabb)

census_list <- list()

for(i in state_list){
  census_list[[i]] <- tracts(state = i)
}

census_data_bind <-  rbind_tigris(census_list)


################################################################################
# 5. BIND CENSUS TRACT AND EJI SPATIAL DATA  ####
################################################################################


census_tracts <- rbind_tigris(census_list) %>% 
  janitor::clean_names() 

census_tracts_eji <- census_tracts %>% 
  left_join(eji_data_fromCDC %>% select(1:geoid, contains("EJI")))


################################################################################
# 5. JOIN PFAS DATA TO A CENSUS STRACT AND EJI INFO   ####
################################################################################


st_crs(census_tracts)
st_crs(all_ppps)
census_tracts_transformed<-st_transform(census_tracts_eji,st_crs(all_ppps))


sites_census_tract <- all_ppps %>% 
  sf::st_join(census_tracts_eji, join = st_intersects)


# tm_shape(census_tracts) +
#   tm_polygons("RPL_EJI", palette="PuBu", alpha = 0.5) + 
#   tm_shape(sites_census_tract) + 
#   tm_dots("dataset", palette = "plasma", size = 0.05) + 
#   tm_legend(outside = TRUE)


#save as RData file so it can be opened in next script 
save(census_tracts_eji, sites_census_tract, 
     file = "census_eji_all_ppps.RData")

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



# 
# ggplot(table1 %>% 
#          pivot_longer(names_to = "count", values_to = "n", n:n_inEJI),
#        aes(x = dataset, y = n, fill = count)) + 
#   geom_bar(position = "stack", stat = "identity")
# 
# 
# ggplot(table1 %>% 
#          mutate(perc_dif = (n_inEJI/n)) %>% 
#          mutate( dataset = factor(dataset, levels = c("all ppps", "airports_sf", "epastewardship_county_sf",
#                                                       "facilities_sf", "fed_agencies_sf", "fire_sf", "production_sf",
#                                                       "spills_sf", "superfund_sf"),
#                                   labels = c("All Potential\nPFAS Point Sources", "14 CFR Part\n139 Airports",
#                                              "EPA Stewardship Program\nParticipating Facility",
#                                              "Facilities in Industries\nthat May be Handling PFAS",
#                                              "Federal Agency Locations with\nKnown or Suspected\nPFAS Detections",
#                                              "Fire Training Sites", "Facilities that Manufacture\nor Import PFAS",
#                                              "Known PFAS\nSpills/Release Incidents", "Superfund Sites\nwith PFAS Detections")))) + 
#   geom_bar(aes(x = dataset, y = perc_dif), stat = "identity", fill = "#165751") + 
#   scale_y_continuous(labels = scales::percent, limits = c(0,1)) + 
#   xlab("Data source") + 
#   ylab("Percent of facilities successfully matched to census tract") + 
#   # geom_bar(aes(x = dataset, y = n), stat = "identity", fill = "green") + 
#   # geom_bar(aes(x = dataset, y = n_inEJI),  stat = "identity") + 
#   theme_bw(base_size = 22) + 
#   theme(panel.grid.major.y = element_blank(),
#         panel.grid.major.x = element_line(color = "snow2"),
#         strip.text = element_text(color = "black", face = "bold", size = 18),
#         text = element_text(family = "Arial")
#   ) 
# 
# 
# ggsave("ppps_match_recovery.png", width = 25, height = 10, units = "in")
