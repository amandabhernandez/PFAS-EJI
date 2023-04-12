### AUTHOR: AHz
### WRITTEN IN: R version 4.2.2
### PURPOSE: 



################################################################################
# 0. LOAD DEPENDENCIES  ####
################################################################################

source("scripts/0 - setup.R")

p_load(gtsummary)


pfas_mod_data <- percentilerank_pfas %>% 
  mutate(EBM_quartile = cut(RPL_EBM, breaks = 4, labels = c("1", "2", "3", "4")), 
         SVM_quartile = cut(RPL_SVM, breaks = 4, labels = c("1", "2", "3", "4")),
         EJI_quartile = cut(RPL_EJI, breaks = 4,  labels = c("1", "2", "3", "4")),
         EBM_rank_change = new_RPL_EBM-RPL_EBM,
         EBM_sum_change = new_SPL_EBM-SPL_EBM,
         EJI_rank_change = new_RPL_EJI-RPL_EJI,
         EJI_sum_change = new_SPL_EJI-SPL_EJI) %>% 
  mutate(hasPFAS = case_when(!is.na(perc_area_pfas) ~ 1,
                             TRUE ~ 0))

pfas_mod_data_format <- pfas_mod_data %>% 
  mutate(`Number of PFAS point sources` = ifelse(hasPFAS == 1, "≥ 1", "0"),
         `% of census tract within 1-mile of a PFAS source` = perc_area_pfas_0*100,
         `Percentile rank of proportion of census tract within 1-mile of a PFAS source` = percent_rank_pfas,
         `Sum of the HVM, EBM, and SVM module percentile ranks` = SPL_EJI,
         `Sum of the HVM, EBM, and SVM module percentile ranks after accounting for PFAS sources` = new_SPL_EJI,
         `Change in EJI sum` = EJI_sum_change,
         `Change in EJI rank` = EJI_rank_change,
         `Sum of the EBM variable percentile ranks` = SPL_EBM,
         `Sum of the EBM variable percentile ranks after accounting for PFAS sources` = new_SPL_EBM, 
         `Change in EBM sum` = EBM_sum_change,
         `Change in EBM rank` = EBM_rank_change,
         `Sum of the SVM variable percentile ranks` = SPL_SVM,
         `Total number of HVM tertile flags` = F_HVM) 
# scales::label_percent(perc_area_pfas_0)


save(pfas_mod_data, pfas_mod_data_format, file = "data/processed/pfas_mod_formatting.RData")

################################################################################
# TABLE 1  (actually) ####
################################################################################


# ppps_salvatore_tract %>% 
#   group_by(geoid, dataset) %>% 
#   count() %>%
#   tbl_summary(by = c("dataset"),
#               include = c("n"))

tbl_summary(st_drop_geometry(ppps_salvatore_tract), 
            include = c("dataset"))

################################################################################
# TABLE 1  ####
################################################################################

table1a <- tbl_summary(pfas_mod_data_format,
            include = c("% of census tract within a 1-mile buffer of a PFAS source", 
                        "Sum of the HVM, EBM, and SVM module percentile ranks", 
                        "Sum of the EBM variable percentile ranks", 
                        "Sum of the SVM variable percentile ranks",
                        "Total number of HVM tertile flags"),
            type = list(c("% of census tract within a 1-mile buffer of a PFAS source", 
                          "Sum of the HVM, EBM, and SVM module percentile ranks", 
                          "Sum of the EBM variable percentile ranks", 
                          "Sum of the SVM variable percentile ranks",
                          "Total number of HVM tertile flags") ~ 'continuous2'),
            statistic = list(all_continuous() ~ c("{median} ({p25} - {p75})", 
                                                  "{mean} ({sd})"))) 

table1b <- tbl_summary(pfas_mod_data_format, 
                       by = c("Number of PFAS point sources"), 
                       include = c("Sum of the HVM, EBM, and SVM module percentile ranks", 
                                   "Sum of the EBM variable percentile ranks", 
                                   "Sum of the SVM variable percentile ranks",
                                   "Total number of HVM tertile flags"),
                       type = list(c("Sum of the HVM, EBM, and SVM module percentile ranks", 
                                     "Sum of the EBM variable percentile ranks", 
                                     "Sum of the SVM variable percentile ranks",
                                     "Total number of HVM tertile flags") ~ 'continuous2'),
                       statistic = list(all_continuous() ~ c("{median} ({p25} - {p75})", 
                                                             "{mean} ({sd})"))) %>% 
  add_p()


tbl_merge(list(table1a, table1b), 
          tab_spanner = c("All Census Tracts", "Number of PFAS point sources")) 


################################################################################
# S1: Collinearity  ####
################################################################################


################################################################################
# TABLE 2/3: MODEL  ####
################################################################################

#univariate regression
pfas_mod_data_format %>% 
  select(hasPFAS,`Sum of the EBM variable percentile ranks`,
           `Sum of the SVM variable percentile ranks`,
           `Total number of HVM tertile flags`) %>% 
  tbl_uvregression(method = glm,
                   y = hasPFAS, 
                   method.args = list(family = binomial),
                   exponentiate = TRUE)

#multivariate regression
mod3 <- glm(hasPFAS ~ `Sum of the EBM variable percentile ranks` + 
              `Sum of the SVM variable percentile ranks` + 
              `Total number of HVM tertile flags`, 
            data = pfas_mod_data_format, family = binomial())

gtsummary::tbl_regression(mod3, exponentiate = TRUE)

################################################################################
# TABLE 4: NEW/OLD RANKS ####
################################################################################


table4 <- tbl_summary(pfas_mod_data_format,
                       include = c("Percentile rank of proportion of census tract within 1-mile buffer of a PFAS source",
                                   "Sum of the HVM, EBM, and SVM module percentile ranks after accounting for PFAS sources", 
                                   "Sum of the EBM variable percentile ranks after accounting for PFAS sources", 
                                   "Change in EJI sum",
                                   "Change in EBM sum",
                                   "Change in EBM rank"),
                       type = list(c("Percentile rank of proportion of census tract within 1-mile buffer of a PFAS source",
                                     "Sum of the HVM, EBM, and SVM module percentile ranks after accounting for PFAS sources", 
                                     "Sum of the EBM variable percentile ranks after accounting for PFAS sources", 
                                     "Change in EJI sum",
                                     "Change in EBM sum",
                                     "Change in EBM rank") ~ 'continuous2'),
                       statistic = list(all_continuous() ~ c("{median} ({p25} - {p75})", 
                                                             "{mean} ({sd})"))) 

table4

################################################################################
# FIGURE 1: MAP  ####
################################################################################

pfas_recalc_spatial %>% 
  ggplot(aes(fill = EJI_rank_change)) +
  geom_sf(color = NA) +
  scale_fill_gradient2(low = "red", high =  "slateblue4", mid = "snow2",
                       midpoint = 0, 
                       na.value="slategray")
