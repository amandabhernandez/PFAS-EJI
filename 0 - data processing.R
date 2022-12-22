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
p_load(sf)

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

naics_1 <- data.frame(naics = c( "313320 Fabric Coating Mills bcdefghik 380",
                                 "325510 Paint and Coating Manufacturing abcdefhik 2100",
                                 "322220 Paper Bag and Coated and Treated Paper Manufacturing bcdefghi 0",
                                 "313210 Broadwoven Fabric Mills bcdefhk 484",
                                 "322121 Paper (except Newsprint) Mills bcdefhk 610",
                                 "332813 Electroplating, Plating, Polishing, Anodizing, and Coloring bcdefhi 5642",
                                 "324110 Petroleum Refineries abcdehk 594",
                                 "325612 Polish and Other Sanitation Good Manufacturing abdefhk 673",
                                 "334413 Semiconductor and Related Device Manufacturing bcdefi 1552",
                                 "326113 Unlaminated Plastics Film and Sheet (except Packaging) Manufacturing bdefhi 597",
                                 "332812 Metal Coating, Engraving (except Jewelry and Silverware), and Allied Services to Manufacturers bcefgi 3587",
                                 "333318 Other Commercial and Service Industry Machinery Manufacturing bdefgh 0",
                                 "334419 Other Electronic Component Manufacturing bcdefg 1663",
                                 "562212 Solid Waste Landfill cdefjk 4765",
                                 "325199 All Other Basic Organic Chemical Manufacturing abdefi 1847",
                                 "323111 Commercial Printing (except Screen and Books) bcdhk 4226",
                                 "313110 Fiber, Yarn, and Thread Mills bcefh 0",
                                 "314110 Carpet and Rug Mills bdhik 189",
                                 "316110 Leather and Hide Tanning and Finishing bdefg 276",
                                 "325211 Plastics Material and Resin Manufacturing bdgik 2141",
                                 "324191 Petroleum Lubricating Oil and Grease Manufacturing abcdh 919",
                                 "325998 All Other Miscellaneous Chemical Product and Preparation Manufacturing aefgk 2598",
                                 "562211 Hazardous Waste Treatment and Disposal acefj 1357",
                                 "562213 Solid Waste Combustors and Incinerators acefj 407",
                                 "313310 Textile and Fabric Finishing Mills bcdh 0",
                                 "322219 Other Paperboard Container Manufacturing bcdh 0",
                                 "323120 Support Activities for Printing bcdh 0",
                                 "313220 Narrow Fabric Mills and Schiffli Machine Embroidery cefk 0",
                                 "313230 Nonwoven Fabric Mills bchk 239",
                                 "322130 Paperboard Mills cefk 258")
)


naics_2 <- data.frame(naics = c("332999 All Other Miscellaneous Fabricated Metal Product Manufacturing bdgh 4799",
                                "424690 Other Chemical and Allied Products Merchant Wholesalers bdhj 1698",
                                "314910 Textile Bag and Canvas Mills befi 0",
                                "326112 Plastics Packaging Film and Sheet (including Laminated) Manufacturing defi 350",
                                "335999 All Other Miscellaneous Electrical Equipment and Component Manufacturing befg 877",
                                "562112 Hazardous Waste Collection cefj 2561",
                                "562219 Other Nonhazardous Waste Treatment and Disposal cefj 744",
                                "325611 Soap and Other Detergent Manufacturing abeh 1012")
)

naics_salvatore <- bind_rows(naics_1, naics_2) %>% 
  mutate(naics_code = str_extract(naics, "\\d*"),
         industry_title = str_extract(naics, "(?<= ).*(?= \\d)"),
         n_facilities = str_extract(naics, "\\d*$")) %>% 
  select(-naics)

sum(as.numeric(naics_salvatore$n_facilities))

facilities <- read_xlsx("PFAS Point Source Data/Facilities_in_industries_that_may_be_handling_PFAS_01-03-2022.xlsx",
                        sheet = 2)

facilities_naics <- facilities %>% 
  mutate(frs_id = str_extract(`ECHO Facility Report`, "(?<=fid=)\\d*"))  %>% 
  select(frs_id, contains("NAICS")) %>% 
  separate_rows(CAA_NAICS, sep = " ") %>% 
  separate_rows(CWA_NAICS, sep = " ") %>% 
  separate_rows(RCRA_NAICS, sep = " ") %>% 
  filter(CAA_NAICS %in% c(naics_salvatore$naics_code) | 
           CWA_NAICS %in% c(naics_salvatore$naics_code) |
           RCRA_NAICS %in% c(naics_salvatore$naics_code))

facilities_naics %>% 
  pivot_longer(names_to = "reporting_req", values_to = "naics_code", CAA_NAICS:RCRA_NAICS) %>% 
  group_by(naics_code) %>% 
  summarize(n_frsid = length(unique(frs_id))) %>% 
  filter(naics_code %in% c(naics_salvatore$naics_code)) %>% 
  full_join(naics_salvatore) %>% 
  mutate(diff = abs(as.numeric(n_facilities)-n_frsid)) %>%
  View()


facilities_eh249 <- facilities %>% 
  mutate(frs_id = str_extract(`ECHO Facility Report`, "(?<=fid=)\\d*")) %>% 
  filter(!Industry %in% c("Airports", "Fire Training", "National Defense")) %>% 
  filter(!is.na(Latitude)) %>% 
  filter(TRI_FLAG == "N")

facilities_salvatore <- facilities %>% 
  mutate(frs_id = str_extract(`ECHO Facility Report`, "(?<=fid=)\\d*")) %>% 
  filter(!Industry %in% c("Airports", "Fire Training", "National Defense")) %>% 
  filter(!is.na(Latitude)) %>% 
  filter(frs_id %in% facilities_naics$frs_id) %>% 
  filter(TRI_FLAG == "N")


facilities_sf_eh249 <- facilities_eh249 %>% 
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  rename(loc_name = Facility) %>% 
  clean_names() %>% 
  select(any_of(keepcols)) 


facilities_sf_salvatore <- facilities_salvatore %>% 
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  rename(loc_name = Facility) %>% 
  clean_names() %>% 
  select(any_of(keepcols)) 



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

table(production$Identifier %in% c(facilities_salvatore$frs_id))

production_sf <- production %>% 
  mutate(across(c(Latitude, Longitude), as.numeric)) %>% 
  filter(!is.na(Latitude)) %>% 
  filter(!Identifier %in% facilities_salvatore$frs_id) %>% 
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  rename(loc_name = `Facility Name`) %>% 
  clean_names() %>% 
  select(any_of(keepcols))

################################################################################
# 1i. WWTP  ####
################################################################################


# WWTPfacilities <- read_csv("PFAS Point Source Data/WWTP facility_details.csv")
WWTPfacilities <- read_csv("PFAS Point Source Data/CWNSLoads09212017.csv") 

wwtp_sf <- WWTPfacilities %>% 
  filter(MAJOR_MINOR == "Major") %>% 
  filter(!is.na(LAT_updated)) %>% 
  sf::st_as_sf(coords = c("LONG_updated", "LAT_updated"))

################################################################################
# 2. JOIN TOGETHER ####
################################################################################

all_ppps_eh249 <- bind_rows(lst(epastewardship_county_sf, 
                          fire_sf, airports_sf,
                          fed_agencies_sf, superfund_sf,
                          facilities_sf_eh249, spills_sf, 
                          production_sf, wwtp_sf), .id = "dataset") 


all_ppps_salvatore <- bind_rows(lst(epastewardship_county_sf, 
                                fire_sf, airports_sf,
                                fed_agencies_sf, superfund_sf, 
                                facilities_sf_salvatore, spills_sf, 
                                production_sf, wwtp_sf), .id = "dataset") 

# nrow(all_ppps_eh249)

################################################################################
# 3. LOAD EJI  ####
################################################################################

### load csv from CDC site
eji_data_fromCDC <- read_csv("eji/DataRecords.csv") %>% 
  filter(!is.na(RPL_EJI))

# table(eji_data_fromCDC$StateAbbr, is.na(eji_data_fromCDC$RPL_EJI), useNA = "ifany")
# tabyl(eji_data_fromCDC, StateAbbr, statefp) %>% View
# 
# eji_data_fromCDC %>% 
#   filter(is.na(StateAbbr)) %>% 
#   View()


################################################################################
# 4. GET CENSUS TRACT INFO  ####
################################################################################

p_load(tigris)

state_list <- unique(stateabbnamekey$stateabb)


#census_data_bind <-  rbind_tigris(census_list)


fips <- tigris::fips_codes


p_load(tidycensus)
Sys.getenv("CENSUS_API_KEY")
census_api_key("f276739b31b176620b81010095385ce4e5608b5f")


tidycensus_list_5yr_2019 <- list()

for(i in state_list){
  tidycensus_list_5yr_2019[[i]] <- tidycensus::get_acs(
    geography = "tract",
    variables = "B19013_001",
    state = i,
    year = 2019, 
    geometry = TRUE
  )
}


 


################################################################################
# 5. BIND CENSUS TRACT AND EJI SPATIAL DATA  ####
################################################################################



## 5yr estimate tracts

tidy_tracts <- bind_rows(tidycensus_list_5yr_2019) %>%   
  janitor::clean_names() %>% 
  select(-variable, -estimate, -moe)

eji_spatial_tidy <- tidy_tracts %>% 
  left_join(eji_data_fromCDC %>% select(1:geoid, contains("EJI"))) %>% 
  left_join(fips, by = c( "statefp" = "state_code", "countyfp" = "county_code" )) 
#73128 (full)
#73128 (left)

# stopifnot()

eji_spatial_tidy %>%
  filter(is.na(name)) %>% 
  View()


eji_spatial_tidy %>%
  filter(is.na(RPL_EJI)) %>% 
  View()



table(is.na(eji_spatial_tidy$RPL_EJI))
#1379 where EJI is empty, but exists in census data, going to keep so we know that
#ppps are matching somewhere

#get stuff that's missing -- look at specific case



################################################################################
# 5. JOIN PFAS DATA TO A CENSUS STRACT AND EJI INFO   ####
################################################################################


st_crs(eji_spatial_tidy)
st_crs(all_ppps_salvatore)
census_tracts_transformed<-st_transform(eji_spatial_tidy,st_crs(all_ppps_salvatore))


ppps_salvatore_tract <- all_ppps_salvatore %>% 
  sf::st_join(census_tracts_transformed, join = st_intersects)

ppps_eh249_tract <- all_ppps_eh249 %>% 
  sf::st_join(census_tracts_transformed, join = st_intersects)




# tm_shape(census_tracts) +
#   tm_polygons("RPL_EJI", palette="PuBu", alpha = 0.5) + 
#   tm_shape(sites_census_tract) + 
#   tm_dots("dataset", palette = "plasma", size = 0.05) + 
#   tm_legend(outside = TRUE)


#save as RData file so it can be opened in next script 
save(eji_spatial_tidy, ppps_salvatore_tract, ppps_eh249_tract, eji_data_fromCDC,
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



################################################################################
#<3 GARBAGE <3 <3    ####
################################################################################



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


# 
# census_list <- list()
# 
# for(i in state_list){
#   census_list[[i]] <- tracts(state = i, year = "2021")
# }
# 
# census_list_2020 <- list()
# 
# for(i in state_list){
#   census_list_2020[[i]] <- tracts(state = i, year = "2020")
# }
# 
# 
# census_list_2019 <- list()
# 
# for(i in state_list){
#   census_list_2019[[i]] <- tracts(state = i, year = "2019")
# }
# 
# 
# 
# census_list_2018 <- list()
# 
# for(i in state_list){
#   census_list_2018[[i]] <- tracts(state = i, year = "2018")
# }
# 

## 2021
census_tracts <- rbind_tigris(census_list) %>% 
  janitor::clean_names() 

eji_spatial <- census_tracts %>% 
  full_join(eji_data_fromCDC %>% select(1:geoid, contains("EJI"))) %>% 
  left_join(fips, by = c( "statefp" = "state_code", "countyfp" = "county_code" )) 
#84476
#97066


## 2020
census_tracts_2020 <- rbind_tigris(census_list_2020) %>% 
  janitor::clean_names() 

eji_spatial_2020 <- census_tracts_2020 %>% 
  full_join(eji_data_fromCDC %>% select(1:geoid, contains("EJI"))) %>% 
  left_join(fips, by = c( "statefp" = "state_code", "countyfp" = "county_code" )) 

length(unique(eji_spatial_2020$geoid[which(is.na(eji_spatial_2020$RPL_EJI))]))



## 2019 is best match so far -- makes sense since the ACS data is 2014-2019 5 yr estimates
census_tracts_2019 <- rbind_tigris(census_list_2019) %>% 
  janitor::clean_names() 

eji_spatial_2019 <- census_tracts_2019 %>% 
  full_join(eji_data_fromCDC %>% select(1:geoid, contains("EJI"))) %>% 
  left_join(fips, by = c( "statefp" = "state_code", "countyfp" = "county_code" )) 

length(unique(eji_spatial_2019$geoid[which(is.na(eji_spatial_2019$RPL_EJI))]))
#2416

## 2018
census_tracts_2018 <- rbind_tigris(census_list_2018) %>% 
  janitor::clean_names() 

eji_spatial_2018 <- census_tracts_2018 %>% 
  full_join(eji_data_fromCDC %>% select(1:geoid, contains("EJI"))) %>% 
  left_join(fips, by = c( "statefp" = "state_code", "countyfp" = "county_code" )) 

length(unique(eji_spatial_2018$geoid[which(is.na(eji_spatial_2018$RPL_EJI))]))
#2416


cr <- eji_spatial %>%
  filter(!is.na(namelsad)) %>% 
  # filter(is.na(RPL_EJI))  %>% 
  # filter(statefp == "01" & countyfp == "001") %>% 
  mutate(flag = case_when(is.na(RPL_EJI) ~ "!"))

library(tmap)

tm_shape(cr) + 
  tm_polygons("flag")