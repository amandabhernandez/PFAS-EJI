### AUTHOR: AHz
### WRITTEN IN: R version 4.2.2
### PURPOSE: 



################################################################################
# 0. LOAD DEPENDENCIES  ####
################################################################################

source("0 - setup.R")


################################################################################
# 1. LOAD EJI  ####
################################################################################

### load csv from CDC site
eji_data_fromCDC <- read_csv("data/raw/EJI/DataRecords.csv") %>% 
  filter(!is.na(RPL_EJI))

# table(eji_data_fromCDC$StateAbbr, is.na(eji_data_fromCDC$RPL_EJI), useNA = "ifany")
# tabyl(eji_data_fromCDC, StateAbbr, statefp) %>% View
# 
# eji_data_fromCDC %>% 
#   filter(is.na(StateAbbr)) %>% 
#   View()





################################################################################
# 2. GET CENSUS TRACT INFO  ####
################################################################################

p_load(tigris)

state_list <- unique(stateabbnamekey$stateabb)


#census_data_bind <-  rbind_tigris(census_list)


fips <- tigris::fips_codes


p_load(tidycensus)


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


write_csv(bind_rows(tidycensus_list_5yr_2019), "output/tidycensus_5yrest_2019.csv")

################################################################################
# 2b. GET CENSUS TRACT INFO (URBAN/RURAL)  ####
################################################################################

# urban and rural decenial data doesn't exist at the census tract level, we could
# aggregate up to the county level, but I think it makes sense to use CDC index
# 
# https://www.cdc.gov/nchs/data_access/urban_rural.htm#Data_Files_and_Documentation

nchsur13 <- readxl::read_xlsx("data/raw/NCHSURCodes2013.xlsx")  %>% 
  clean_names() %>% 
  select(fips_code, x2013_code) %>% 
  rename(urb_rur_code = x2013_code) %>% 
  mutate(fips_code = case_when(nchar(fips_code) == 4 ~ paste0("0", fips_code),
                               TRUE ~ as.character(fips_code)))

save(nchsur13, file = "data/processed/nchs_urban_rural_classification.RData")

################################################################################
# 3. BIND CENSUS TRACT AND EJI SPATIAL DATA  ####
################################################################################



## 5yr estimate tracts

tidy_tracts <- bind_rows(tidycensus_list_5yr_2019) %>%   
  janitor::clean_names() %>% 
  select(-variable, -estimate, -moe)

eji_spatial_tidy <- tidy_tracts %>% 
  left_join(eji_data_fromCDC,  #%>% 
              #select(1:geoid, contains("EJI"))
            by = "geoid"
            ) %>% 
  left_join(fips, by = c( "statefp" = "state_code", "countyfp" = "county_code" )) 
#73128 (full)
#73128 (left)

save(eji_spatial_tidy, tidy_tracts,
     file = "data/processed/eji_spatial_1b_output.RData")

#why do I need to make EJI spatial? 


# stopifnot()

# eji_spatial_tidy %>%
#   filter(is.na(name)) %>% 
#   View()
# 
# 
# eji_spatial_tidy %>%
#   filter(is.na(RPL_EJI)) %>% 
#   View()



table(is.na(eji_spatial_tidy$RPL_EJI))
#1379 where EJI is empty, but exists in census data, going to keep so we know that
#ppps are matching somewhere

#get stuff that's missing -- look at specific case
