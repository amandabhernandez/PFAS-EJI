### AUTHOR: AHz, LRS, JL 
### LAST EDIT: 
### WRITTEN IN: R version 4.2.1
### Purpose: process all demo info 
### Depends on: UCMR loading and processing.R (for FIPS only)

library(pacman)
p_load(tidyverse)
p_load(janitor)
p_load(readxl)
p_load(tmap)

options (stringsAsFactors = FALSE)

source_file_loc <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(source_file_loc)

################################################################################
#  1. GET STATES TO INCLUDE ####
################################################################################
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
#  2. LOAD SOME DATA!!!  ####
################################################################################


keepcols <- c("loc_name", "city", "county", "state", "geometry")

################################################################################
#  2a. EPA STEWARDSHIP PROGRAM ####
################################################################################

#PFAS POINT SOURCE FILES FROM UCMR/EJ PROJECT (thx Jahred)

epastewardship <- read_excel("../../../../UCMR3/Data/PFAS point source data/Data/EPA 2010.2015 PFOA Stewardship Program sites.xlsx")

# Reshaping, processing, and renaming columns for point source data by counties
epastewardship_county <- epastewardship %>% 
  # count(County, State) %>% 
  mutate(geography = paste0(tolower(County), " county, ", tolower(State))) %>% 
  separate(Coordinates, sep = ", ", into = c("lat", "long"))
  #rename(n_epastewardship = n)

epastewardship_county_sf <- epastewardship_county %>%
  sf::st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  mutate(loc_name = as.character(Company)) %>% 
  clean_names() %>% 
  select(any_of(keepcols))

sf::st_crs(epastewardship_county_sf)$Name


################################################################################
#  2b. AIRPORTS ####
################################################################################

 
# PFAS POINT SOURCE FILES FROM UCMR/EJ PROJECT (also thx Jahred)
airports <- read_excel("../../../../UCMR3/Data/PFAS point source data/Data/Part 139_cert_airports.xlsx")

airports_county <- airports %>%
  # add full state name and fix several geography names for ease of merging
  left_join(stateabbnamekey, by = c("CountyState" = "stateabb")) %>%
  mutate(geography = paste0(tolower(County), " county, ", tolower(fullstate)))

airports_sf <- airports_county %>%
  mutate(lat = as.numeric(str_extract(ARPLatitudeS, "\\d*.\\d*"))/3600,
         long = as.numeric(str_extract(ARPLongitudeS, "\\d*.\\d*"))/-3600) %>%
  sf::st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  rename(loc_name = FacilityName) %>%
  clean_names() %>%
  select(any_of(keepcols))


# 
# sf::st_crs(airports_sf)$Name

################################################################################
#  2c. FIRE TRAINING ACADEMY ####
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
#  2d.  FEDERAL AGENCY LOCATIONS WITH KNOWN OR SUSPECTED PFAS DETECTIONS   ####
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
# 2e. SUPERFUND  ####
################################################################################

superfund <- read_xlsx("PFAS Point Source Data/Superfund_Sites_with_PFAS_Detections_03-04-2022_0.xlsx",
                       sheet = 2)

superfund_sf <- superfund %>% 
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  rename(loc_name = `Site Name`) %>% 
  clean_names() %>% 
  select(any_of(keepcols))



################################################################################
# 2f. FACILITIES  ####
################################################################################

facilities <- read_xlsx("PFAS Point Source Data/Facilities_in_industries_that_may_be_handling_PFAS_01-03-2022.xlsx",
                        sheet = 2)

facilities_sf <- facilities %>% 
  filter(!is.na(Latitude)) %>% 
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  rename(loc_name = Facility) %>% 
  clean_names() %>% 
  select(any_of(keepcols))

check_facilities <- facilities_sf %>% 
  filter(!state %in% stateabbnamekey$stateabb)

# tm_shape(check_facilities) +
#   #tm_raster(style = "cont", palette = "viridis") +
#   #tm_legend(outside = TRUE) + 
#   # tm_shape(ca_pts)+
#   tm_dots(size=0.05,alpha=0.8) 


################################################################################
# 3. JOIN TOGETHER ####
################################################################################

all_ppps <- bind_rows(lst(epastewardship_county_sf, 
                          fire_sf, airports_sf,
                          fed_agencies_sf, superfund_sf, 
                          #facilities_sf
                          ), .id = "dataset")



################################################################################
# 4. MAKE SOME MAPS ####
################################################################################


tmap_mode("view")

tm_shape(epastewardship_county_sf) + 
  tm_dots(size=0.05,alpha=0.8, col = "#6667AB") + 
  tm_shape(fire_sf) + 
  tm_dots(size=0.05,alpha=0.8, col = "#8A4C5D") +
  tm_shape(airports_sf) + 
  tm_dots(size=0.05,alpha=0.8, col = "#86A094") + 
  tm_shape(fed_agencies_sf) + 
  tm_dots(size=0.05,alpha=0.8, col = "#CCB87E") + 
  tm_shape(superfund_sf) + 
  tm_dots(size=0.05,alpha=0.8, col = "#D4937F") #+ 
  # tm_legend(outside = TRUE) 


# tm_shape(all_ppps) +
#   #tm_raster(style = "cont", palette = "viridis") +
#   tm_legend(outside = TRUE) + 
#   # tm_shape(ca_pts)+
#   tm_dots(size=0.05,alpha=0.8) 



################################################################################
# EJI :( ####
################################################################################

### load csv from CDC site
eji_data_fromCDC <- read_csv("eji/DataRecords.csv")


###  GEOJSONR::
# p_load(geojsonR)
# 
# eji_geojson <- geojsonR::FROM_GeoJson("eji/DataRecords.geojson")


### JSONLITE:: 
p_load(jsonlite)

eji_geojson <- jsonlite::read_json("eji/DataRecords.geojson")

cr <- head(eji_geojson$features) %>% 
  enframe() %>% 
  unnest_wider(value) 
# now what? 


bind_rows(head(eji_geojson))
#nope

# eji_geojson_df <- as.data.frame(eji_geojson)


### GEOJSONIO (R graph gallery)
p_load(geojsonio)

# the r graph gallery told me this would work I can't believe they would play me like this 
eji_geojson_io <- geojsonio::geojson_read("eji/DataRecords.geojson")

eji_fortified <- broom::tidy(eji_geojson_io)
#nope

# ggplot() +
#   geom_polygon(data = eji_fortified, aes( x = long, y = lat, group = group), fill="#69b3a2", color="white") +
#   theme_void() +
#   coord_map()


### SHP FILE APPROACH

eji <- sf::st_read("eji/eji.shp")

eji_plot <- head(eji, 20) %>% 
  select(1:location, contains("eji"), geometry) 

st_make_valid(eji_plot$geometry)


tm_shape(eji_plot) +   
  tm_polygons()



### CENSUS TRACT APPROACH
p_load(tigris)

census_list <- list()

for(i in stateabbnamekey$stateabb){
  census_list[[i]] <- tracts(state = i)
}

census_tracts <- rbind_tigris(census_list)

tm_shape(census_tracts) +
  tm_polygons()





################################################################################
# <3 GARBAGE <3 <3 <3 <3 ####
################################################################################

# AHz: come back to this and do some setdiff/merge checks 

# 
# # WWTPfacilities <- read_csv("../../../UCMR3/Data/PFAS point source data/Data/WWTP facility_details.csv")
# 
# MFTA <- read_excel("../../../UCMR3/Data/PFAS point source data/Data/all MFTA_county.xlsx")
# 
#

# WWTPfacilities_county <- WWTPfacilities %>% 
#   mutate(`Existing Total Flow (Mgal/d)` = case_when(is.na(`Existing Total Flow (Mgal/d)`) ~ 0, 
#                                                     TRUE ~ `Existing Total Flow (Mgal/d)`)) %>% 
#   # add full state name and fix several geography names for ease of merging
#   left_join(stateabbnamekey, by = c("State" = "stateabb")) %>% 
#   mutate(geography = case_when(State!= "LA" ~ paste0(tolower(`County Name`), " county, ",
#                                                      tolower(fullstate)),
#                                State == "LA" ~ paste0(tolower(`County Name`), " parish, ",
#                                                       tolower(fullstate)))) %>% 
#   group_by(geography) %>% 
#   summarize(n_WWTP = length(unique(`CWNS Number`)),
#             WWTP_totalflow_mgd = sum(`Existing Total Flow (Mgal/d)`)) %>% 
#   correct_counties() 
# 
# WWTPfacilities_merge <- WWTPfacilities_county %>% 
#   select(geography, WWTP_totalflow_mgd, n_WWTP)
# 
# 
# MFTA_county <- MFTA %>% 
#   mutate(geography = case_when(`State/Territory`!="LOUISIANA" ~ paste0(tolower(COUNTY_NAME), " county, ",
#                                                                        tolower(`State/Territory`)),
#                                `State/Territory`=="LOUISIANA" ~ paste0(tolower(COUNTY_NAME), " parish, ",
#                                                                        tolower(`State/Territory`)))) %>% 
#   correct_counties() %>% 
#   mutate(geography = gsub("NA county, new hampshire", "rockingham county, new hampshire", geography))
# 
# 
# 
# 
# 
# MFTA_merge <- MFTA_county %>% 
#   group_by(geography) %>% 
#   #drop repeated Stanly/Badin national guard base
#   summarize(n_MFTA = length(unique(`Installation name`))) 
# 
# 
# # note: it looks like non-US-states (Guam, PR etc will be dropped)
# 
# # check matches
# setdiff(epastewardship_county$geography, county14all$geography)
# # none
# setdiff(airports_merge$geography, county14all$geography)
# # NA county, NA is in PR  (6)
# setdiff(WWTPfacilities_county$geography, county14all$geography)
# #all "NA" state (48)
# setdiff(MFTA_merge$geography, county14all$geography)
# #all PR/Guam/Marshall Islands (6)


# 
# p_load(choroplethr)
# p_load(choroplethrMaps)
# 


# 
# get_tract_demographics(state_name = "Massachusetts")
#pp <- TractChoropleth$new



# eji <- sf::read_sf("eji/eji.shp")

# eji <- read_csv("eji/eji.csv")
# 
# eji_plot <- st_as_sf(eji, crs=4269)
# 
# ca_pts<-st_transform(eji,st_crs(ndvi_ca0519))
# 
# tmap_options(check.and.fix = TRUE) 
# 
# st_is_valid(eji_plot)
# 
# tm_shape(eji_plot) +
#   tm_polygons() +
#   tm_legend(outside = TRUE)

