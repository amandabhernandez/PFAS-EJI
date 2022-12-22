### AUTHOR: AHz
### WRITTEN IN: R version 4.2.1
### Purpose: Conduct prelim analysis


################################################################################
#  0. SET UP  ####
################################################################################

library(pacman)
p_load(tidyverse)
p_load(janitor)
p_load(broom)
p_load(sf)
p_load(flextable)

options (stringsAsFactors = FALSE)

source_file_loc <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(source_file_loc)


################################################################################
# 1. LOAD DATA ####
################################################################################

load("census_eji_all_ppps.RData")


# setdiff(colnames(sites_census_tract), colnames(eji_spatial_tidy))



################################################################################
# 2. MERGE EJI AND PPPS ####
################################################################################

ppps_salvatore_tract_df <- st_drop_geometry(ppps_salvatore_tract) %>% 
  distinct() %>% 
  select(dataset:geoid)

ppps_eh249_tract_df <- st_drop_geometry(ppps_eh249_tract) %>% 
  distinct() %>% 
  select(dataset:geoid)

ppps_salvatore_count <- ppps_salvatore_tract_df %>% 
  group_by(geoid) %>% 
  count() %>% 
  filter(!is.na(geoid))

ppps_eh249_count <- ppps_eh249_tract_df %>% 
  group_by(geoid) %>% 
  count() %>% 
  filter(!is.na(geoid))

eji_tracts_df <- st_drop_geometry(eji_spatial_tidy)  %>% 
  distinct()



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



eji_w_ppps_eh249 <-eji_data_fromCDC %>% 
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

################################################################################
# 3. VISUALIZE SOME STUFF ####
################################################################################



ggplot(eji_w_ppps_eh249, aes(x = as.factor(n_ppps_groups), y = SPL_EJI)) +
  ggforce::geom_sina(alpha = 0.5, color = "lightgrey", show.legend = TRUE) +
  geom_boxplot(width = 0.1, guides = FALSE, outlier.shape = NA, size = 1, color = "#3a3838") +
  xlab("# of Potential PFAS Point Sources") + 
  ylab("Sum of Env, Social, and Health Module Rankings") + 
  theme_bw(base_size = 22) + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "snow2"),
        strip.text = element_text(color = "black", face = "bold", size = 18),
        text = element_text(family = "Arial")
  ) +
  ggtitle("EJI with EH249 PFAS Point Sources")

ggsave("eji census tracts with eh249 ppps boxplots.png", width = 18, height = 10, units = "in")

ggplot(eji_w_ppps_salvatore, aes(x = as.factor(n_ppps_groups), y = SPL_EJI)) +
  ggforce::geom_sina(alpha = 0.5, color = "lightgrey", show.legend = TRUE) +
  geom_boxplot(width = 0.1, guides = FALSE, outlier.shape = NA, size = 1, color = "#3a3838") +
  xlab("# of Potential PFAS Point Sources") + 
  ylab("Sum of Env, Social, and Health Module Rankings") + 
  theme_bw(base_size = 22) + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "snow2"),
        strip.text = element_text(color = "black", face = "bold", size = 18),
        text = element_text(family = "Arial")
  ) +
  ggtitle("EJI with Salvatore subset PFAS Point Sources")

ggsave("eji census tracts with salvatore ppps boxplots.png", width = 18, height = 10, units = "in")



eji_w_ppps_eh249 %>% 
  pivot_longer(names_to = "metric", values_to = "score", SPL_EJI:RPL_HVM) %>% 
  mutate(metric = factor(metric, levels = c("RPL_EJI", "SPL_EJI", "RPL_EBM", "RPL_HVM", "RPL_SVM"),
                         labels = c("EJI Rank", "Sum of all modules", "Env Burden Module",
                                    "Health Vulnerability Module", "Social Vulnerability Module"))) %>% 
  ggplot(aes(x = as.factor(n_ppps_groups), y = score)) +
  ggforce::geom_sina(alpha = 0.5, color = "lightgrey", show.legend = TRUE) +
  geom_boxplot(width = 0.1, guides = FALSE, outlier.shape = NA, size = 1, color = "#3a3838") +
  xlab("# of Potential PFAS Point Sources") + 
  ylab("Sum of Env, Social, and Health Module Rankings") + 
  facet_wrap(~metric, ncol = 1, scales = "free_y") +
  theme_bw(base_size = 22) + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "snow2"),
        strip.text = element_text(color = "black", face = "bold", size = 18),
        text = element_text(family = "Arial")
  ) +
  ggtitle("EJI with EH249 PFAS Point Sources")

ggsave("eji census tracts with eh249 ppps boxplots for diff variables.png", width = 18, height = 20, units = "in")

eji_w_ppps_salvatore %>% 
  pivot_longer(names_to = "metric", values_to = "score", SPL_EJI:RPL_HVM) %>% 
  mutate(metric = factor(metric, levels = c("RPL_EJI", "SPL_EJI", "RPL_EBM", "RPL_HVM", "RPL_SVM"),
                         labels = c("EJI Rank", "Sum of all modules", "Env Burden Module",
                                    "Health Vulnerability Module", "Social Vulnerability Module"))) %>% 
  ggplot(aes(x = as.factor(n_ppps_groups), y = score)) +
  ggforce::geom_sina(alpha = 0.5, color = "lightgrey", show.legend = TRUE) +
  geom_boxplot(width = 0.1, guides = FALSE, outlier.shape = NA, size = 1, color = "#3a3838") +
  xlab("# of Potential PFAS Point Sources") + 
  ylab("Sum of Env, Social, and Health Module Rankings") + 
  facet_wrap(~metric, ncol = 1, scales = "free_y") +
  theme_bw(base_size = 22) + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "snow2"),
        strip.text = element_text(color = "black", face = "bold", size = 18),
        text = element_text(family = "Arial")
  ) +
  ggtitle("EJI with salvatore PFAS Point Sources")

ggsave("eji census tracts with salvatore ppps boxplots for diff variables.png", width = 18, height = 20, units = "in")



### group by dataset


ppps_salvatore_count_dataset <- ppps_salvatore_tract_df %>% 
  group_by(geoid, dataset) %>% 
  count() %>% 
  filter(!is.na(geoid))

ppps_eh249_count_dataset <- ppps_eh249_tract_df %>% 
  group_by(geoid, dataset) %>% 
  count() %>% 
  filter(!is.na(geoid))


eji_w_ppps_salvatore_dataset <-eji_data_fromCDC %>% 
  select(1:geoid, contains("EJI")) %>% 
  full_join(ppps_salvatore_count_dataset)  %>% 
  distinct() %>% 
  mutate(dataset = case_when(is.na(dataset) ~ "all ppps",
                             TRUE ~ dataset)) %>% 
  pivot_wider(names_from = dataset, values_from = n, values_fill = 0) %>% 
  pivot_longer(names_to = "dataset", values_to = "n", `all ppps`:superfund_sf) %>% 
  select(1:geoid, contains("EJI"), dataset, n) %>% 
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
         ),
         n_ppps_groups = factor(n_ppps_groups, levels = c("0", "1", "2", "3", "4", "5",
                                                          "≤10", "≤20", "≤50", ">50")))  %>% 
  mutate( dataset = factor(dataset, levels = c("all ppps", "airports_sf", "epastewardship_county_sf",
                                               "facilities_sf_salvatore", "fed_agencies_sf", "fire_sf", "production_sf",
                                               "spills_sf", "superfund_sf", "wwtp_sf"),
                           labels = c("All Potential\nPFAS Point Sources", "14 CFR Part\n139 Airports",
                                      "EPA Stewardship Program\nParticipating Facility",
                                      "Facilities in Industries\nthat May be Handling PFAS",
                                      "Federal Agency Locations with\nKnown or Suspected\nPFAS Detections",
                                      "Fire Training Sites", "Facilities that Manufacture\nor Import PFAS",
                                      "Known PFAS\nSpills/Release Incidents", 
                                      "Superfund Sites\nwith PFAS Detections", 
                                      "Wastewater Treatment Plants")))


eji_w_ppps_eh249_dataset <-eji_data_fromCDC %>% 
  select(1:geoid, contains("EJI")) %>% 
  full_join(ppps_eh249_count_dataset)  %>% 
  distinct() %>% 
  mutate(dataset = case_when(is.na(dataset) ~ "all ppps",
                             TRUE ~ dataset)) %>% 
  pivot_wider(names_from = dataset, values_from = n, values_fill = 0) %>% 
  pivot_longer(names_to = "dataset", values_to = "n", `all ppps`:superfund_sf) %>% 
  select(1:geoid, contains("EJI"), dataset, n) %>% 
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
                                                          "≤10", "≤20", "≤50", "≤100", ">100")))  %>% 
  mutate( dataset = factor(dataset, levels = c("all ppps", "airports_sf", "epastewardship_county_sf",
                                               "facilities_sf_eh249", "fed_agencies_sf", "fire_sf", "production_sf",
                                               "spills_sf", "superfund_sf", "wwtp_sf"),
                           labels = c("All Potential\nPFAS Point Sources", "14 CFR Part\n139 Airports",
                                      "EPA Stewardship Program\nParticipating Facility",
                                      "Facilities in Industries\nthat May be Handling PFAS",
                                      "Federal Agency Locations with\nKnown or Suspected\nPFAS Detections",
                                      "Fire Training Sites", "Facilities that Manufacture\nor Import PFAS",
                                      "Known PFAS\nSpills/Release Incidents", 
                                      "Superfund Sites\nwith PFAS Detections",
                                      "Wastewater Treatment Plants")))


# table(eji_w_ppps_eh249_dataset$dataset, eji_w_ppps_eh249_dataset$n_ppps_groups, useNA = "ifany")

# all_tracts_together_w_datasets_too <- all_tracts_ppps %>%
#   mutate(dataset = "all ppps") %>%
#   bind_rows(all_tracts_ppps_dataset) %>%
#   mutate( dataset = factor(dataset, levels = c("all ppps", "airports_sf", "epastewardship_county_sf",
#                                                "facilities_sf", "fed_agencies_sf", "fire_sf", "production_sf",
#                                                "spills_sf", "superfund_sf"),
#                            labels = c("All Potential\nPFAS Point Sources", "14 CFR Part\n139 Airports",
#                                       "EPA Stewardship Program\nParticipating Facility",
#                                       "Facilities in Industries\nthat May be Handling PFAS",
#                                       "Federal Agency Locations with\nKnown or Suspected\nPFAS Detections",
#                                       "Fire Training Sites", "Facilities that Manufacture\nor Import PFAS",
#                                       "Known PFAS\nSpills/Release Incidents", "Superfund Sites\nwith PFAS Detections")))


ggplot(eji_w_ppps_eh249_dataset %>% 
         filter(dataset != "All Potential\nPFAS Point Sources"), aes(x = n_ppps_groups, y = SPL_EJI)) +
  ggforce::geom_sina(alpha = 0.5, color = "lightgrey") +
  geom_boxplot(width = 0.1, guides = FALSE, outlier.shape = NA, size = 1, color = "#3a3838") +
  xlab("# of Potential PFAS Point Sources") + 
  ylab("Sum of Env, Social, and Health Module Rankings") + 
  facet_wrap(~dataset, scales = "free_x") + 
  theme_bw(base_size = 22) + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "snow2"),
        strip.text = element_text(color = "black", face = "bold", size = 18),
        text = element_text(family = "Arial")
  ) + 
  ggtitle("EJI with EH249 PFAS Point Sources")

ggsave("eji census tracts with eh249 ppps boxplots by dataset.png", width = 18, height = 15, units = "in")



ggplot(eji_w_ppps_salvatore_dataset%>% 
         filter(dataset != "All Potential\nPFAS Point Sources"), aes(x = n_ppps_groups, y = SPL_EJI)) +
  ggforce::geom_sina(alpha = 0.5, color = "lightgrey") +
  geom_boxplot(width = 0.1, guides = FALSE, outlier.shape = NA, size = 1, color = "#3a3838") +
  xlab("# of Potential PFAS Point Sources") + 
  ylab("Sum of Env, Social, and Health Module Rankings") + 
  facet_wrap(~dataset, scales = "free_x") + 
  theme_bw(base_size = 22) + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "snow2"),
        strip.text = element_text(color = "black", face = "bold", size = 18),
        text = element_text(family = "Arial")
  ) + 
  ggtitle("EJI with salvatore PFAS Point Sources")


ggsave("eji census tracts with salvatore ppps boxplots by dataset.png", width = 18, height = 15, units = "in")


################################################################################
# 4. SUMMARY STATS####
################################################################################

all_tracts_ppps %>% 
  group_by(.) %>% 
  summarise(sum_ppps = sum(n_ppps))

gtsummary::tbl_summary(all_tracts_ppps, 
                       by = n_ppps_groups, 
                       digits = everything() ~ 2, 
                       missing = "no",
                       include = c("RPL_EJI", "RPL_EBM", "RPL_SVM",  "RPL_HVM"),
                       type = list(c("RPL_EJI", "RPL_EBM", "RPL_SVM",  "RPL_HVM") ~ 'continuous2'),
                       statistic = list(c("RPL_EJI", "RPL_EBM", "RPL_SVM",  "RPL_HVM") ~ c("{median} ({p25} - {p75})"))) 

table2 <- all_tracts_ppps %>% 
  pivot_longer(names_to = "metric", values_to = "RPL", c("RPL_EJI", "RPL_EBM", "RPL_SVM",  "RPL_HVM")) %>% 
  mutate(metric = factor(metric, levels = c("RPL_EJI", "RPL_EBM", "RPL_SVM",  "RPL_HVM"),
                         labels = c("EJI Rank", "Environmental Burden Rank", "Social Vulnerability Rank",  "Health Vulnerability Rank"))) %>% 
  group_by(n_ppps_groups, metric) %>% 
  summarize(n = n(), 
            median = median(RPL, na.rm = TRUE), 
            p25 = quantile(RPL, 0.25, na.rm = TRUE),
            p75 = quantile(RPL, 0.75, na.rm = TRUE)) %>% 
  mutate(`Median (IQR)` = paste0(median, " (", p25, " - ", p75, ")")) %>% 
  select(-c(median:p75)) %>% 
  pivot_wider(names_from = metric, values_from = `Median (IQR)`)  %>% 
  flextable()


write_csv(table1, "median ranks by n_ppps.csv")

rmarkdown::render("final_project_figures.Rmd",  output_file = "final_project_figures.docx")