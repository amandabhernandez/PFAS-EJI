### AUTHOR: AHz
### WRITTEN IN: R version 4.2.2
### PURPOSE: Load and process all PFAS Point Source data



################################################################################
# 0. LOAD DEPENDENCIES  ####
################################################################################

source("scripts/0 - setup.R")

keepcols <- c("loc_name", "city", "county", "state", "geometry")

################################################################################
#  1a. EPA STEWARDSHIP PROGRAM ####
################################################################################

#PFAS POINT SOURCE FILES FROM UCMR/EJ PROJECT (thx Jahred)

epastewardship <- read_excel("data/raw/PFAS Point Source Data/EPA 2010.2015 PFOA Stewardship Program sites.xlsx")

epastewardship_county_sf <- epastewardship %>%
  separate(Coordinates, sep = ", ", into = c("lat", "long")) %>%
  sf::st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  mutate(loc_name = paste0(tolower(County), ", ", tolower(State))) %>%
  clean_names() %>%
  select(any_of(keepcols))


################################################################################
#  1b. AIRPORTS ####
################################################################################

airports_epa <- read_excel("data/raw/PFAS Point Source Data/Airports_Data_FAA2_0_0.xlsx")

#!#! check out if these are different!! 
## PFAS POINT SOURCE FILES FROM UCMR/EJ PROJECT (also thx Jahred)
## airports <- read_excel("data/raw/PFAS Point Source Data/Part 139_cert_airports.xlsx") %>% 
#   mutate(LocationID = str_extract(pattern = "(?<=').*", LocationID))
# setdiff(airports$LocationID, airports_epa$AirportID)
# setdiff(airports_epa$AirportID, airports$LocationID)


airports_sf <- airports_epa %>%
  mutate(lat = as.numeric(str_extract(ARPLatitudeS, "\\d*.\\d*"))/3600,
         long = as.numeric(str_extract(ARPLongitudeS, "\\d*.\\d*"))/-3600) %>%
  sf::st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  rename(loc_name = AirportName) %>%
  clean_names() %>%
  select(any_of(keepcols))



################################################################################
#  1c. FIRE TRAINING FACILITIES ####
################################################################################
fire <- read_xlsx("data/raw/PFAS Point Source Data/ECHO_Fire_Training_Industry3_0_0.xlsx") 

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

fed_agencies <- read_xlsx("data/raw/PFAS Point Source Data/Federal_Agency_Locations_with_Known_or_Suspected_PFAS_Detections_02-28-2022.xlsx",
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

superfund <- read_xlsx("data/raw/PFAS Point Source Data/Superfund_Sites_with_PFAS_Detections_03-04-2022_0.xlsx",
                       sheet = 2)

superfund_sf <- superfund %>% 
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  rename(loc_name = `Site Name`) %>% 
  clean_names() %>% 
  select(any_of(keepcols))


################################################################################
# 1f. FACILITIES  ####
################################################################################

naics_salvatore <- data.frame(naics = c( "313320 Fabric Coating Mills bcdefghik 380",
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
                                 "322130 Paperboard Mills cefk 258",
                                 "332999 All Other Miscellaneous Fabricated Metal Product Manufacturing bdgh 4799",
                                "424690 Other Chemical and Allied Products Merchant Wholesalers bdhj 1698",
                                "314910 Textile Bag and Canvas Mills befi 0",
                                "326112 Plastics Packaging Film and Sheet (including Laminated) Manufacturing defi 350",
                                "335999 All Other Miscellaneous Electrical Equipment and Component Manufacturing befg 877",
                                "562112 Hazardous Waste Collection cefj 2561",
                                "562219 Other Nonhazardous Waste Treatment and Disposal cefj 744",
                                "325611 Soap and Other Detergent Manufacturing abeh 1012")
)

naics_salvatore <- naics_salvatore %>% 
  mutate(naics_code = str_extract(naics, "\\d*"),
         industry_title = str_extract(naics, "(?<= ).*(?= \\d)"),
         n_facilities = str_extract(naics, "\\d*$")) %>% 
  select(-naics)

sum(as.numeric(naics_salvatore$n_facilities))
#49145

facilities <- read_xlsx("data/raw/PFAS Point Source Data/Facilities_in_industries_that_may_be_handling_PFAS_01-03-2022.xlsx",
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

length(unique(facilities_naics$frs_id))
#33303

# facilities_naics %>% 
#   pivot_longer(names_to = "reporting_req", values_to = "naics_code", CAA_NAICS:RCRA_NAICS) %>% 
#   group_by(naics_code) %>% 
#   summarize(n_frsid = length(unique(frs_id))) %>% 
#   filter(naics_code %in% c(naics_salvatore$naics_code)) %>% 
#   full_join(naics_salvatore) %>% 
#   mutate(diff = abs(as.numeric(n_facilities)-n_frsid)) %>%
#   View()


# frs_notTRI <- read_csv("data/raw/PFAS Point Source Data/FRS_facilities_notin_TRI.csv")




facilities_salvatore <- facilities %>% 
  mutate(frs_id = str_extract(`ECHO Facility Report`, "(?<=fid=)\\d*")) %>% 
  filter(!Industry %in% c("Airports", "Fire Training", "National Defense")) %>% 
  filter(!is.na(Latitude)) %>% 
  filter(frs_id %in% facilities_naics$frs_id) %>% 
  filter(TRI_FLAG == "N")

length(unique(facilities_salvatore$frs_id))

# table(facilities_salvatore$frs_id %in% c(frs_notTRI$REGISTRY_ID))

# facilities_salvatore %>% 
#   filter(!frs_id %in% frs_notTRI$REGISTRY_ID) %>% 
#   View()



facilities_sf_salvatore <- facilities_salvatore %>% 
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  rename(loc_name = Facility) %>% 
  clean_names() %>% 
  select(any_of(keepcols)) 




################################################################################
# 1g. SPILLS  ####
################################################################################

spills <- read_xlsx("data/raw/PFAS Point Source Data/Initial_Calls_Reported_to_NRC_Indicating_AFFF_Usage_02-23-2022.xlsx",
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

production <- read_xlsx("data/raw/PFAS Point Source Data/PFAS_Production_Data_02-28-2022_0.xlsx", sheet = 2)

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


# WWTPfacilities <- read_csv("data/raw/PFAS Point Source Data/WWTP facility_details.csv")
# WWTPfacilities <- read_csv("data/raw/PFAS Point Source Data/CWNSLoads09212017.csv") 
# 
# wwtp_sf <- WWTPfacilities %>% 
#   filter(MAJOR_MINOR == "Major") %>% 
#   filter(!is.na(LAT_updated)) %>% 
#   sf::st_as_sf(coords = c("LONG_updated", "LAT_updated")) %>% 
#   mutate(loc_name = as.character(CWNS_NBR)) %>% 
#   clean_names() %>% 
#   select(any_of(keepcols)) %>% 
#   distinct()
# 
# 

################################################################################
# 2. JOIN TOGETHER ####
################################################################################

# all_ppps_eh249 <- bind_rows(lst(epastewardship_county_sf, 
#                                 fire_sf, airports_sf,
#                                 fed_agencies_sf, superfund_sf,
#                                 facilities_sf_eh249, spills_sf, 
#                                 production_sf), .id = "dataset") 


all_ppps_salvatore <- bind_rows(lst(epastewardship_county_sf, 
                                    fire_sf, airports_sf,
                                    fed_agencies_sf, superfund_sf, 
                                    facilities_sf_salvatore, spills_sf, 
                                    production_sf), .id = "dataset") 




#save as RData file so it can be opened in 1c
save(all_ppps_salvatore, 
     file = "data/processed/all_ppps_1a_output.RData")

# save(all_ppps_eh249, all_ppps_salvatore, 
#      file = "data/processed/all_ppps_1a_output.RData")




################################################################################
# GARBAGE <3 ####
################################################################################

facilities_eh249 <- facilities %>% 
  mutate(frs_id = str_extract(`ECHO Facility Report`, "(?<=fid=)\\d*")) %>% 
  filter(!Industry %in% c("Airports", "Fire Training", "National Defense")) %>% 
  filter(!is.na(Latitude)) %>% 
  filter(TRI_FLAG == "N")

facilities_sf_eh249 <- facilities_eh249 %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  rename(loc_name = Facility) %>%
  clean_names() %>%
  select(any_of(keepcols))
