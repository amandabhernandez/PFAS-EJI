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
         SVM_Theme1_quart = cut(RPL_SVM_DOM1, breaks = 4,  labels = c("1", "2", "3", "4")),
         SVM_Theme2_quart = cut(RPL_SVM_DOM2, breaks = 4,  labels = c("1", "2", "3", "4")),
         SVM_Theme3_quart = cut(RPL_SVM_DOM3, breaks = 4,  labels = c("1", "2", "3", "4")),
         SVM_Theme4_quart = cut(RPL_SVM_DOM4, breaks = 4,  labels = c("1", "2", "3", "4")),
         EBM_Theme1_quart = cut(RPL_EBM_DOM1, breaks = 4,  labels = c("1", "2", "3", "4")),
         EBM_Theme2_quart = cut(RPL_EBM_DOM2, breaks = 4,  labels = c("1", "2", "3", "4")),
         EBM_Theme3_quart = cut(RPL_EBM_DOM3, breaks = 4,  labels = c("1", "2", "3", "4")),
         EBM_Theme4_quart = cut(RPL_EBM_DOM4, breaks = 4,  labels = c("1", "2", "3", "4")),
         EBM_Theme5_quart = cut(RPL_EBM_DOM5, breaks = 4,  labels = c("1", "2", "3", "4")),
         EBM_rank_change = new_RPL_EBM-RPL_EBM,
         EBM_sum_change = new_SPL_EBM-SPL_EBM,
         EJI_rank_change = new_RPL_EJI-RPL_EJI,
         EJI_sum_change = new_SPL_EJI-SPL_EJI) %>% 
  mutate(hasPFAS = case_when(!is.na(perc_area_pfas) ~ 1,
                             TRUE ~ 0))

pfas_mod_data_format <- pfas_mod_data %>% 
  mutate(`Number of PFAS point sources` = ifelse(hasPFAS == 1, "≥ 1", "0"),
         `% of census tract within 1-mile of a PFAS source` = perc_area_pfas_0*100,
         `Percentile rank of proportion of census tract within 1-mile of a PFAS source` = percent_rank_pfas) %>% 
  rename(
         `Sum of the HVM, EBM, and SVM module percentile ranks` = SPL_EJI,
         `EJI percentile rank` = RPL_EJI,
         #`Sum of the HVM, EBM, and SVM module percentile ranks after accounting for PFAS sources` = new_SPL_EJI,
         #`Change in EJI sum` = EJI_sum_change,
         #`Change in EJI rank` = EJI_rank_change,
         `Sum of the EBM variable percentile ranks` = SPL_EBM,
         `EBM percentile rank` = RPL_EBM,
         `EBM Theme 1: Air Pollution` = RPL_EBM_DOM1, 
         `EBM Theme 2: Potentially Hazardous + Toxic Sites` = RPL_EBM_DOM2, 
         `EBM Theme 3: Built Env` = RPL_EBM_DOM3, 
         `EBM Theme 4: Transportation Infrastructure` = RPL_EBM_DOM4,
         `EBM Theme 5: Water Pollution` = RPL_EBM_DOM5,
         `Quartile of EBM Theme 1: Air Pollution` = EBM_Theme1_quart, 
         `Quartile of EBM Theme 2: Potentially Hazardous + Toxic Sites` = EBM_Theme2_quart, 
         `Quartile of EBM Theme 3: Built Env` = EBM_Theme3_quart, 
         `Quartile of EBM Theme 4: Transportation Infrastructure` = EBM_Theme4_quart,
         `Quartile of EBM Theme 5: Water Pollution` = EBM_Theme5_quart,
         #`Sum of the EBM variable percentile ranks after accounting for PFAS sources` = new_SPL_EBM, 
         #`Change in EBM sum` = EBM_sum_change,
         #`Change in EBM rank` = EBM_rank_change,
         `Sum of the SVM variable percentile ranks` = SPL_SVM,
         `SVM percentile rank` = RPL_SVM,
         `SVM Theme 1: Race/Ethnicity` = RPL_SVM_DOM1, 
         `SVM Theme 2: SES` = RPL_SVM_DOM2, 
         `SVM Theme 3: Household Characteristics` = RPL_SVM_DOM3, 
         `SVM Theme 4: Housing Type` = RPL_SVM_DOM4,
         `Quartile of SVM Theme 1: Race/Ethnicity` = SVM_Theme1_quart, 
         `Quartile of SVM Theme 2: SES` = SVM_Theme2_quart, 
         `Quartile of SVM Theme 3: Household Characteristics` = SVM_Theme3_quart, 
         `Quartile of SVM Theme 4: Housing Type` = SVM_Theme4_quart,
         `% Minority` = EP_MINRTY, 
         `% Below Poverty` = EP_POV200,
         `% No HS Diploma` = EP_NOHSDP,
         `% Unemployed` = EP_UNEMP,
         `% Renter` = EP_RENTER,
         `% Household earning < $75,000` = EP_HOUBDN,
         `% Uninsured` = EP_UNINSUR,
         `% No internet` = EP_NOINT,
         `% Aged 65 and older` = EP_AGE65,
         `% Aged 17 and younger` = EP_AGE17,
         `% Living with disability`= `EP_DISABL`,
         `% Limited English` = EP_LIMENG,
         `% Mobile home` = EP_MOBILE,
         `% Group quarters` = EP_GROUPQ,
         `Social/Env percentile rank` = RPL_SER
         #`Total number of HVM tertile flags` = F_HVM
         ) 
# scales::label_percent(perc_area_pfas_0)


save(pfas_mod_data, pfas_mod_data_format, file = "data/processed/pfas_mod_formatting.RData")


SVM_components_clean <- c("% Minority", 
                          "% Below Poverty", 
                          "% No HS Diploma", 
                          "% Unemployed",
                          "% Renter",
                          "% Household earning < $75,000",
                          "% Uninsured",
                          "% No internet",
                          "% Aged 65 and older",
                          "% Aged 17 and younger",
                          "% Living with disability",
                          "% Limited English",
                          "% Mobile home",
                          "% Group quarters")

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

## Correlation between PFAS sources and EBM variables  {.scrollable}


load("../output/figures/cor_plots.RData")

cor_plot[[2]] %>% 
  plotly::ggplotly()


## Correlation between PFAS sources and SVM variables {.scrollable}


#| fig-asp: 1

cor_plot[[3]]%>% 
  plotly::ggplotly()




################################################################################
# TABLE 2/3: MODEL  ####
################################################################################


############ QUARTILE MODEL ############## 

mod_quarts_uv <- pfas_mod_data_format %>% 
  select(hasPFAS,EBM_quartile, SVM_quartile) %>% 
  gtsummary::tbl_uvregression(method = glm,
                              y = hasPFAS, 
                              method.args = list(family = binomial),
                              exponentiate = TRUE)


mod_quarts_multi <- glm(hasPFAS ~ EBM_quartile + SVM_quartile, 
                        data = pfas_mod_data, family = binomial()) %>% 
  gtsummary::tbl_regression(exponentiate = TRUE)


save(mod_quarts_uv, mod_quarts_multi, file = "output/models/quartile_mod.RData")


############ QUARTILE 3 PART MODEL ############## 


# model 1 is the same as the quartile model


# model 2 
mod_quarts_themes_uv <- pfas_mod_data_format %>% 
  select(hasPFAS, contains("Quartile of")) %>% 
  gtsummary::tbl_uvregression(method = glm,
                              y = hasPFAS, 
                              method.args = list(family = binomial),
                              exponentiate = TRUE)


mod_quarts_themes_multi <- glm(hasPFAS ~ 
                          `Quartile of SVM Theme 1: Race/Ethnicity` +
                          `Quartile of SVM Theme 2: SES` +
                          `Quartile of SVM Theme 3: Household Characteristics` +
                          `Quartile of SVM Theme 4: Housing Type` +
                          `Quartile of EBM Theme 1: Air Pollution` +
                          `Quartile of EBM Theme 2: Potentially Hazardous + Toxic Sites` +
                          `Quartile of EBM Theme 3: Built Env` +
                          `Quartile of EBM Theme 4: Transportation Infrastructure` + 
                          `Quartile of EBM Theme 5: Water Pollution`, 
                          data = pfas_mod_data_format, family = binomial()) %>% 
  gtsummary::tbl_regression(exponentiate = TRUE)


save(mod_quarts_themes_uv, mod_quarts_themes_multi, file = "output/models/quartile_mod2.RData")


# model 3
mod_quarts_themes_uv <- pfas_mod_data_format %>% 
  select(hasPFAS, contains("Quartile of")) %>% 
  gtsummary::tbl_uvregression(method = glm,
                              y = hasPFAS, 
                              method.args = list(family = binomial),
                              exponentiate = TRUE)


mod_quarts_themes_multi <- glm(hasPFAS ~ 
                                 `Quartile of SVM Theme 1: Race/Ethnicity` +
                                 `Quartile of SVM Theme 2: SES` +
                                 `Quartile of SVM Theme 3: Household Characteristics` +
                                 `Quartile of SVM Theme 4: Housing Type` +
                                 `Quartile of EBM Theme 1: Air Pollution` +
                                 `Quartile of EBM Theme 2: Potentially Hazardous + Toxic Sites` +
                                 `Quartile of EBM Theme 3: Built Env` +
                                 `Quartile of EBM Theme 4: Transportation Infrastructure` + 
                                 `Quartile of EBM Theme 5: Water Pollution`, 
                               data = pfas_mod_data_format, family = binomial()) %>% 
  gtsummary::tbl_regression(exponentiate = TRUE)


save(mod_quarts_themes_uv, mod_quarts_themes_multi, file = "output/models/quartile_mod2.RData")




############ SVM 3 PART MODEL ############## 

# model 1
RPL_SVM_mod <- pfas_mod_data_format %>% 
  select(hasPFAS, `SVM percentile rank`) %>% 
  tbl_uvregression(method = glm,
                   y = hasPFAS, 
                   method.args = list(family = binomial),
                   exponentiate = TRUE)

# model 2

RPL_SVM_dom_univ <- pfas_mod_data_format %>% 
  select(hasPFAS, `SVM Theme 1: Race/Ethnicity`, `SVM Theme 2: SES`,
         `SVM Theme 3: Household Characteristics`, `SVM Theme 4: Housing Type`) %>% 
  tbl_uvregression(method = glm,
                   y = hasPFAS, 
                   method.args = list(family = binomial),
                   exponentiate = TRUE)


RPL_SVM_dom_multi <-  glm(hasPFAS ~ `SVM Theme 1: Race/Ethnicity` + `SVM Theme 2: SES` + 
                            `SVM Theme 3: Household Characteristics` +  `SVM Theme 4: Housing Type`, 
                          data = pfas_mod_data_format, family = binomial()) 

# model 3

RPL_SVM_comp_univ <- pfas_mod_data %>% 
  select(hasPFAS, EPL_MINRTY, EPL_POV200, EPL_NOHSDP , EPL_UNEMP , EPL_RENTER , EPL_HOUBDN ,  EPL_UNINSUR ,
         EPL_NOINT , EPL_AGE65 , EPL_AGE17 , EPL_DISABL , EPL_LIMENG , EPL_MOBILE , EPL_GROUPQ) %>% 
  tbl_uvregression(method = glm,
                   y = hasPFAS, 
                   method.args = list(family = binomial),
                   exponentiate = TRUE)




RPL_SVM_comp_multi <- glm(as.formula(
  paste0("hasPFAS ~ ", paste0(eji_codebook_revised$variable_name[which(
    eji_codebook_revised$module == "SVI" &
      str_detect(eji_codebook_revised$variable_name, "EPL_"))],
    collapse = "+"))), 
  data = pfas_mod_data, family = binomial()) 


# actual estimates, not the percentiles
# RPL_SVM_comp_univ <- pfas_mod_data %>% 
#   select(hasPFAS, EP_MINRTY, EP_POV200, EP_NOHSDP , EP_UNEMP , EP_RENTER , EP_HOUBDN ,  EP_UNINSUR ,
#          EP_NOINT , EP_AGE65 , EP_AGE17 , EP_DISABL , EP_LIMENG , EP_MOBILE , EP_GROUPQ) %>% 
#   tbl_uvregression(method = glm,
#                    y = hasPFAS, 
#                    method.args = list(family = binomial),
#                    exponentiate = TRUE)
# 
# RPL_SVM_comp_multi <- glm(as.formula(
#   paste0("hasPFAS ~ ", paste0(eji_codebook_revised$variable_name[which(
#     eji_codebook_revised$module == "SVI" &
#       str_detect(eji_codebook_revised$variable_name, "EP_"))],
#     collapse = "+"))), 
#   data = pfas_mod_data, family = binomial()) 


save(RPL_SVM_mod, 
     RPL_SVM_dom_univ, RPL_SVM_dom_multi, 
     RPL_SVM_comp_univ, RPL_SVM_comp_multi,
     file = "output/models/svm_3part_mod.RData")


############ EBM 3 PART MODEL ############## 

# model 1

RPL_EBM_mod <- pfas_mod_data_format %>% 
  select(hasPFAS, `EBM percentile rank`) %>% 
  tbl_uvregression(method = glm,
                   y = hasPFAS, 
                   method.args = list(family = binomial),
                   exponentiate = TRUE)


# model 2

RPL_EBM_dom_univ <- pfas_mod_data_format %>% 
  select(hasPFAS, contains("EBM Theme")) %>% 
  tbl_uvregression(method = glm,
                   y = hasPFAS, 
                   method.args = list(family = binomial),
                   exponentiate = TRUE)


RPL_EBM_dom_multi <-  glm(hasPFAS ~ `EBM Theme 1: Air Pollution` + 
                            `EBM Theme 2: Potentially Hazardous + Toxic Sites` + 
                            `EBM Theme 3: Built Env` +  `EBM Theme 4: Transportation Infrastructure` + 
                            `EBM Theme 5: Water Pollution`, 
                          data = pfas_mod_data_format, family = binomial())

# model 3

RPL_EBM_comp_univ <- pfas_mod_data %>% 
  select(hasPFAS,
         any_of(
           eji_codebook_revised$variable_name[which(
             eji_codebook_revised$module == "EBM" &
               str_detect(eji_codebook_revised$variable_name,  "E_"))])) %>%
  tbl_uvregression(method = glm,
                   y = hasPFAS, 
                   method.args = list(family = binomial),
                   exponentiate = TRUE)


RPL_EBM_comp_multi <- glm(as.formula(paste0(
  "hasPFAS ~ ",
  paste0(eji_codebook_revised$variable_name[which(
    eji_codebook_revised$module == "EBM" &
      str_detect(eji_codebook_revised$variable_name, "E_"))], 
    collapse = "+"))), data = pfas_mod_data, family = binomial())


# save(RPL_EBM_mod, 
#      RPL_EBM_dom_univ, RPL_EBM_dom_multi, 
#      RPL_EBM_comp_univ, RPL_EBM_comp_multi,
#      file = "output/models/ebm_3part_mod.RData")

save(RPL_EBM_mod, file = "output/models/ebm_mod.Rdata")

save(RPL_EBM_dom_univ, RPL_EBM_dom_multi, 
     file = "output/models/ebm_dom_mod.RData")

save(RPL_EBM_comp_univ, RPL_EBM_comp_multi,
          file = "output/models/ebm_comp_mod.RData")


############ SVM + EBM 3 PART MODEL ############## 


# model 1

RPL_SVM_EBM_mod <- glm(hasPFAS ~ `EBM percentile rank` + `SVM percentile rank`,
                   data = pfas_mod_data_format, family = binomial()) 



# model 2


RPL_SVM_EBM_dom_multi <-  glm(hasPFAS ~ 
                                `SVM Theme 1: Race/Ethnicity` + 
                                `SVM Theme 2: SES` + 
                                `SVM Theme 3: Household Characteristics` +  
                                `SVM Theme 4: Housing Type` +
                                `EBM Theme 1: Air Pollution` + 
                                `EBM Theme 2: Potentially Hazardous + Toxic Sites` + 
                                `EBM Theme 3: Built Env` +  
                                `EBM Theme 4: Transportation Infrastructure` + 
                                `EBM Theme 5: Water Pollution`, 
                              data = pfas_mod_data_format, family = binomial()) 



# model 3


RPL_SVM_EBM_comp_multi <- glm(as.formula(paste0(
  "hasPFAS ~ ", 
  paste0(eji_codebook_revised$variable_name[which(
    eji_codebook_revised$module == "SVI" &
      str_detect(eji_codebook_revised$variable_name, "E_"))],
    collapse = "+"), " + ",
  paste0(eji_codebook_revised$variable_name[which(
    eji_codebook_revised$module == "EBM" &
      str_detect(eji_codebook_revised$variable_name, "E_"))], 
    collapse = "+"))), data = pfas_mod_data, family = binomial()) 




save(RPL_SVM_EBM_mod, 
     RPL_SVM_EBM_dom_multi, 
     RPL_SVM_EBM_comp_multi,
     file = "output/models/svm_ebm_3part_mod.RData")




################################################################################
# FIGURE 1: MAP  ####
################################################################################

pfas_recalc_spatial %>% 
  mutate(perc_area_pfas = ifelse(perc_area_pfas == 0, NA, perc_area_pfas*100)) %>% 
  ggplot(aes(fill = perc_area_pfas)) +
  geom_sf(color = NA) +
  scale_fill_continuous(name = "% of census tract within\n1-mile of a PFAS source",
                        low = "lightpink", high =  "maroon4", na.value="snow2") + 
  theme_bw(base_size = 22)

################################################################################
# OLD/GARBAGE <3  ####
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
