### AUTHOR: AHz
### WRITTEN IN: R version 4.2.2
### PURPOSE: 



################################################################################
# 0. LOAD DEPENDENCIES  ####
################################################################################

source("0 - setup.R")


################################################################################
# 1. LOAD DATA ####
################################################################################

load("census_eji_all_ppps_wo_spatial.RData")



################################################################################
# 2. MAKE PPPS COUNTS ####
################################################################################

ppps_salvatore_count <- ppps_salvatore_tract_df %>% 
  group_by(geoid) %>% 
  count() %>% 
  filter(!is.na(geoid))

ppps_eh249_count <- ppps_eh249_tract_df %>% 
  group_by(geoid) %>% 
  count() %>% 
  filter(!is.na(geoid))


################################################################################
# 3. SUMMARIZE COUNTS ####
################################################################################


#how many census tracts total
length(unique(eji_tracts_df$geoid))
#84,414 census tracts 
#73,056 census tracts

#how many census tracts in EJI
length(unique(eji_data_fromCDC$geoid))
#73,868
#71,677 (12/21/22) 

#how many census tracts with PFAS point source data
length(unique(ppps_salvatore_count$geoid))
#16,274 census tracts with PFAS point source data
#18,548 (with wwtp)

#how many census tracts with PFAS point source data
length(unique(ppps_eh249_count$geoid))
#20,354 census tracts with PFAS point source data 
#25,681 census tracts with PFAS point source data (12/21/22) -- diff likely bc 
#we fixed the census tract matching issue
#27,217 (wwtp)

#as a percent? 
(length(unique(ppps_eh249_count$geoid))/length(unique(eji_data_fromCDC$geoid)))*100
#23%
#35.8%
#37.9% (wwtp)


#as a percent? 
(length(unique(ppps_salvatore_count$geoid))/length(unique(eji_data_fromCDC$geoid)))*100
#22%
#25.9% (wwtp)

#how many facilities in ppps dataset?
sum(ppps_eh249_count$n)
#83,692
#110,691 (12/21/22)
#114,860 (wwtp)


#how many facilities in ppps dataset?
sum(ppps_salvatore_count$n)
#32,125 (12/21/22)
#36,294 (wwtp)


################################################################################
# 4. JOIN COUNTS AND EJI  ####
################################################################################


eji_w_ppps_eh249 <-eji_tracts_df %>% 
  full_join(ppps_eh249_count)  %>% 
  select(1:geoid, contains("EJI"), "RPL_EBM", "RPL_SVM",  "RPL_HVM", n) %>% 
  mutate(n_ppps = ifelse(is.na(n), 0, as.numeric(n)),
         n_ppps_groups = case_when(n_ppps == 0 ~ "0",
                                   n_ppps == 1 ~ "1",
                                   n_ppps == 2 ~ "2",
                                   n_ppps == 3 ~ "3",
                                   n_ppps == 4 ~ "4",
                                   n_ppps == 5 ~ "5",
                                   n_ppps <= 10 ~ "≤10",
                                   n_ppps <= 20 ~ "≤20",
                                   n_ppps <= 50 ~ "≤50",
                                   n_ppps <= 100 ~ "≤100",
                                   n_ppps >100 ~ ">100"
         ),
         n_ppps_groups = factor(n_ppps_groups, levels = c("0", "1", "2", "3", "4", "5",
                                                          "≤10", "≤20", "≤50", 
                                                          "≤100", ">100"
         ))) %>%
  distinct()



table(eji_w_ppps_eh249$n_ppps_groups)


eji_w_ppps_salvatore <-eji_data_fromCDC %>% 
  full_join(ppps_salvatore_count)  %>% 
  select(1:geoid, contains("EJI"), "RPL_EBM", "RPL_SVM",  "RPL_HVM", n) %>% 
  mutate(n_ppps = ifelse(is.na(n), 0, as.numeric(n)),
         n_ppps_groups = case_when(n_ppps == 0 ~ "0",
                                   n_ppps == 1 ~ "1",
                                   n_ppps == 2 ~ "2",
                                   n_ppps == 3 ~ "3",
                                   n_ppps == 4 ~ "4",
                                   n_ppps == 5 ~ "5",
                                   n_ppps <= 10 ~ "≤10",
                                   n_ppps <= 20 ~ "≤20",
                                   n_ppps <= 50 ~ "≤50",
                                   n_ppps > 50 ~ ">50"
                                   # n_ppps <= 100 ~ "≤100",
                                   # n_ppps >100 ~ ">100"
         ),
         n_ppps_groups = factor(n_ppps_groups, levels = c("0", "1", "2", "3", "4", "5",
                                                          "≤10", "≤20", "≤50", ">50"
                                                          # "≤100", ">100"
         ))) %>%
  distinct()

table(eji_w_ppps_salvatore$n_ppps_groups)




################################################################################
# 5. MORE CHECKS!!!  ####
################################################################################


#what is the avg number of ppps in each census tract?
summary(eji_w_ppps_eh249$n_ppps)

summary(eji_w_ppps_salvatore$n_ppps)

#how many census tracts in final dataset? 
length(unique(eji_w_ppps_eh249$geoid))
#73,868
#71,677 (12/21/22)

length(unique(eji_w_ppps_salvatore$geoid))
#71,677

#they should match if we do a left_join rather than a full (full keeps all the ppps 
# even if they don't have a census tract match in EJI)

#how many ppps in final dataset? 
sum(eji_w_ppps_eh249$n_ppps)
#83,692
#110,691
#114,860 (wwtp)

#how many ppps in final dataset? 
sum(eji_w_ppps_salvatore$n_ppps)
#32,125 
#36,294 (wwtp)


stopifnot(sum(eji_w_ppps_eh249$n_ppps) == sum(ppps_eh249_count$n))
stopifnot(sum(eji_w_ppps_salvatore$n_ppps) == sum(ppps_salvatore_count$n))

