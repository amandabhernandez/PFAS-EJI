### AUTHOR: AHz
### WRITTEN IN: R version 4.2.2
### PURPOSE: 



################################################################################
# 0. LOAD DEPENDENCIES  ####
################################################################################

source("scripts/0 - setup.R")
load("data/processed/all_ppps_spatial_1c_output.RData")

p_load(gtsummary, officer)

theme_gtsummary_compact()



format_gtreg <- function(gtsumm_tbl_reg, 
                         combine_est_ci = FALSE, 
                         hide_p = FALSE, 
                         as_flex = TRUE) {
  
  gtsumm_tbl_reg$table_body <- gtsumm_tbl_reg$table_body %>% 
    mutate(across(contains("ci_"), ~case_when(!is.na(.x) ~ paste0("(", .x, ")"),
                                              TRUE ~ NA)))
  
  if(combine_est_ci == TRUE) {
    
    gtsumm_tbl_reg$table_body <- gtsumm_tbl_reg$table_body %>% 
      mutate(across(contains(c("estimate_")), 
                    ~case_when(!is.na(.x) ~ paste0(round(.x, 2)),
                               TRUE ~ NA))) 
    
    
    est_col <- colnames(gtsumm_tbl_reg$table_body %>% select(contains("estimate_")))
    
    for(i in est_col){
      ci_col <- colnames(gtsumm_tbl_reg$table_body %>% 
                           select(contains(paste0("ci_", str_extract(i, "\\d")))))
      for(p in 1:nrow(gtsumm_tbl_reg$table_body)){
        gtsumm_tbl_reg$table_body[p, i] <- ifelse(!is.na(gtsumm_tbl_reg$table_body[p, i]), 
                                                  paste0(as.character(gtsumm_tbl_reg$table_body[p, i]), " ", 
                                                         gtsumm_tbl_reg$table_body[p, c(ci_col)]), NA)
      }
    }
    
    gtsumm_tbl_reg$table_styling$fmt_fun <- gtsumm_tbl_reg$table_styling$fmt_fun %>% 
      filter(!str_detect(column, "estimate_"))
    
    gtsumm_tbl_reg$table_styling$header <- gtsumm_tbl_reg$table_styling$header %>% 
      mutate(hide = case_when(str_detect(column, "ci_") ~ TRUE,
                              TRUE ~ hide),
             label = case_when(str_detect(column, "estimate_") ~ "**OR (95% CI)**",
                               TRUE ~ label))
    
  }
  
  if(hide_p == TRUE){
    
    gtsumm_tbl_reg$table_styling$header <- gtsumm_tbl_reg$table_styling$header %>% 
      mutate(hide = case_when(str_detect(column, "p.value") ~ TRUE,
                              TRUE ~ hide))
  }
  
  if(as_flex == TRUE){
    return(as_flex_table(gtsumm_tbl_reg))
  }
  else{
    return(gtsumm_tbl_reg)
  }
  
}


# tbl_theme <- function(dat, gt = "", caption = "", footer = ""){
#   if(gt == FALSE) {
#     dat_flex <- dat %>% 
#       as_flextable(show_coltype = FALSE)
#   }
#   else if(gt == TRUE)  {
#     dat_flex <- dat %>% 
#       as_flex_table(show_coltype = FALSE) 
#   }
#   
#   else{
#     dat_flex <- dat
#   }
#   
#   dat_flex %>% 
#     set_caption(caption = caption) %>% 
#     add_footer_lines(footer) %>% 
#     color(part = "footer", color = "#666666") %>% 
#     set_table_properties(layout = "autofit", width = 1)
# }


################################################################################
# 1. PROCESSING   ####
################################################################################

load("data/processed/nchs_urban_rural_classification.RData")

pfas_mod_data <- percentilerank_pfas %>% 
  # this should happen in an earlier script but its all chaos 
  mutate(fips_code = paste0(statefp, countyfp)) %>% 
  left_join(nchsur13) %>% 
  mutate(EBM_quartile = cut(RPL_EBM, breaks = 4, labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")), 
         #EBM_tertile = cut(RPL_EBM, breaks = 3, labels = c("Low", "Moderate", "High")),
         SVM_quartile = cut(RPL_SVM, breaks = 4, labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")), 
         #SVM_tertile = cut(RPL_SVM, breaks = 3, labels = c("Low", "Moderate", "High")),
         SER_quartile = cut(RPL_SER, breaks = 4, labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")), 
         #SER_tertile = cut(RPL_SER, breaks = 3, labels = c("Low", "Moderate", "High")),         
         EJI_quartile = cut(RPL_EJI, breaks = 4,  labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")), 
         #EJI_tertile =  cut(RPL_EJI, breaks = 3, labels = c("Low", "Moderate", "High")),
         SVM_Theme1_quart = cut(RPL_SVM_DOM1, breaks = 4,  labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")), 
         SVM_Theme2_quart = cut(RPL_SVM_DOM2, breaks = 4,  labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")), 
         SVM_Theme3_quart = cut(RPL_SVM_DOM3, breaks = 4,  labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")), 
         SVM_Theme4_quart = cut(RPL_SVM_DOM4, breaks = 4,  labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")), 
         EBM_Theme1_quart = cut(RPL_EBM_DOM1, breaks = 4,  labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")), 
         EBM_Theme2_quart = cut(RPL_EBM_DOM2, breaks = 4,  labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")), 
         EBM_Theme3_quart = cut(RPL_EBM_DOM3, breaks = 4,  labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")), 
         EBM_Theme4_quart = cut(RPL_EBM_DOM4, breaks = 4,  labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")), 
         EBM_Theme5_quart = cut(RPL_EBM_DOM5, breaks = 4,  labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")), 
         urb_rur_code = factor(urb_rur_code, levels = c("1", "2", "3", "4", "5", "6"),
                               labels = c("Large central metro", "Large fringe metro",
                                          "Medium metro", "Small metro",
                                          "Micropolitan", "Noncore")),
         urban_rural = case_when(urb_rur_code %in% c("Micropolitan", "Noncore") ~ "Rural",
                                 TRUE ~ "Urban"), 
         # EBM_rank_change = new_RPL_EBM-RPL_EBM,
         # EBM_sum_change = new_SPL_EBM-SPL_EBM,
         # EJI_rank_change = new_RPL_EJI-RPL_EJI,
         # EJI_sum_change = new_SPL_EJI-SPL_EJI
         ) %>% 
  mutate(hasPFAS = case_when(!is.na(perc_area_pfas) ~ 1,
                             TRUE ~ 0)) 

pfas_mod_data_format <- pfas_mod_data %>% 
  mutate(`Number of PFAS point sources` = ifelse(hasPFAS == 1, "≥ 1", "0"),
         `% of census tract area within 1-mile of a PFAS source` = perc_area_pfas_0*100,
         `Percentile rank of proportion of census tract within 1-mile of a PFAS source` = percent_rank_pfas) %>% 
  rename(
    #EJI 
    `Sum of the HVM, EBM, and SVM module percentile ranks` = SPL_EJI,
    `EJI percentile rank` = RPL_EJI,
    `Quartile of EJI` = EJI_quartile, 
    #SER
    `Social/Env percentile rank` = RPL_SER,
    `Quartile of combined Social Vulnerability & Environmental Burden` = SER_quartile,
    `Tertile of Social/Env percentile rank` = SER_tertile,
    #EBM
    `Sum of the EBM variable percentile ranks` = SPL_EBM,
    `EBM percentile rank` = RPL_EBM,
    `Quartile of EBM` = EBM_quartile,
    `Tertile of EBM` = EBM_tertile,
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
    # SVM 
    `Sum of the SVM variable percentile ranks` = SPL_SVM,
    `SVM percentile rank` = RPL_SVM,
    `Quartile of SVM` = SVM_quartile,
    `Tertile of SVM` = SVM_tertile,
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
    `% Group quarters` = EP_GROUPQ
  ) 
# scales::label_percent(perc_area_pfas_0)


save(pfas_mod_data, pfas_mod_data_format, file = "data/processed/pfas_mod_formatting.RData")


# SVM_components_clean <- c("% Minority", 
#                           "% Below Poverty", 
#                           "% No HS Diploma", 
#                           "% Unemployed",
#                           "% Renter",
#                           "% Household earning < $75,000",
#                           "% Uninsured",
#                           "% No internet",
#                           "% Aged 65 and older",
#                           "% Aged 17 and younger",
#                           "% Living with disability",
#                           "% Limited English",
#                           "% Mobile home",
#                           "% Group quarters")

################################################################################
# TABLE 1 ####
################################################################################

tbl1 <- tbl_summary(st_drop_geometry(ppps_salvatore_tract) %>% 
              mutate(dataset = factor(dataset,
                                      labels = c("Part 139 airports",
                                                 "EPA Stewardship Program participating facilities",
                                                 "Facilities in industries that may be handling PFAS",
                                                 "Federal agency locations with known or suspected PFAS detections",
                                                 "Fire training sites",
                                                 "Facilities that manufacture or import PFAS",
                                                 "Superfund sites with PFAS detections",
                                                 "Known PFAS spills/release incidents"))), 
            label = dataset ~ "Dataset",
            include = c("dataset")) %>% 
  as_flex_table()

tbl1

length(unique(ppps_salvatore_tract$geoid))/length(unique(pfas_mod_data_format$geoid))

################## SUPPLEMENTAL TABLE 1: NAICS codes #############################

load("output/tables/table_s1.RData")



supp_tbl1 <- table_s1 %>% 
  rename(`NAICS code` = naics_code,
         `Industry description` = industry_title,
         `Number of facilities in study` = n) %>% 
  as_flextable(show_coltype = FALSE, max_row = 50) %>% 
  set_table_properties(layout = "autofit", width = 1) %>% 
  autofit()

supp_tbl1

################################################################################
# TABLE 2  ####
################################################################################


tbl2_df <- pfas_mod_data_format %>% 
  group_by(.) %>% 
  summarize(med = round(median(`% of census tract area within 1-mile of a PFAS source`), 2),
            iqr = paste0(" (", round(quantile(`% of census tract area within 1-mile of a PFAS source`, .25), 2), " - ",
                         round(quantile(`% of census tract area within 1-mile of a PFAS source`, .75), 2), ")"))%>% 
  mutate(Module = "All U.S. Census Tracts",
         Quartile = "") %>% 
  bind_rows(pfas_mod_data_format %>% 
              group_by(`Quartile of combined Social Vulnerability & Environmental Burden`) %>% 
              summarize(med = round(median(`% of census tract area within 1-mile of a PFAS source`), 2),
                        iqr = paste0(" (", round(quantile(`% of census tract area within 1-mile of a PFAS source`, .25), 2), " - ",
                                     round(quantile(`% of census tract area within 1-mile of a PFAS source`, .75), 2), ")"))%>% 
              rename(`Quartile` = `Quartile of combined Social Vulnerability & Environmental Burden`) %>% 
              mutate(Module = "Combined Social Vulnerability & Environmental Burden")) %>% 
  bind_rows(pfas_mod_data_format %>% 
              group_by(`Quartile of EBM`) %>% 
              summarize(med = round(median(`% of census tract area within 1-mile of a PFAS source`), 2),
                        iqr = paste0(" (", round(quantile(`% of census tract area within 1-mile of a PFAS source`, .25), 2), " - ",
                                     round(quantile(`% of census tract area within 1-mile of a PFAS source`, .75), 2), ")"))%>% 
              rename(`Quartile` = `Quartile of EBM`) %>% 
              mutate(Module = "Environmental burden\n(percentile rank)")) %>% 
  bind_rows(pfas_mod_data_format %>% 
              group_by(`Quartile of SVM`) %>% 
              summarize(med = round(median(`% of census tract area within 1-mile of a PFAS source`), 2),
                        iqr = paste0(" (", round(quantile(`% of census tract area within 1-mile of a PFAS source`, .25), 2), " - ",
                                     round(quantile(`% of census tract area within 1-mile of a PFAS source`, .75), 2), ")")) %>% 
              rename(`Quartile` = `Quartile of SVM`) %>% 
              mutate(Module = "Social vulnerability\n(percentile rank)")) %>%
  mutate(stat = "% of census tract area within 1-mile of a PFAS source")

tbl2 <- tabulator(tbl2_df, rows = c("Module", "Quartile"),
                       columns = "stat", 
          `Median` = as_paragraph(as_chunk(med)),
          `IQR` = as_paragraph(as_chunk(iqr))) %>% 
  as_flextable(separate_with = "Module")%>% 
  set_table_properties(layout = "autofit", width = 1)

tbl2

################## SUPPLEMENTAL VERSION OF TABLE 2 #############################

supp_tbl2_themes <- pfas_mod_data_format %>% 
  pivot_longer(names_to = "theme", values_to = "Quartile", contains(c("Quartile of SVM ", "Quartile of EBM "))) %>% 
  group_by(theme, Quartile) %>% 
  summarize(med = round(median(`% of census tract area within 1-mile of a PFAS source`), 2),
            iqr = paste0(" (", round(quantile(`% of census tract area within 1-mile of a PFAS source`, .25), 2), " - ",
                         round(quantile(`% of census tract area within 1-mile of a PFAS source`, .75), 2), ")")) %>% 
  separate(theme, into = c("Module", "Theme"), sep = "(?<=EBM|SVM) ") %>% 
  mutate(stat = "% of census tract area within 1-mile of a PFAS source",
         Module = str_extract(Module, "EBM|SVM"),
         Module = case_when(Module == "EBM" ~ "Environmental burden\n(percentile rank)",
                            Module == "SVM" ~ "Social vulnerability\n(percentile rank)",)) 


supp_tbl2 <- tabulator(supp_tbl2_themes, rows = c("Module", "Theme", "Quartile"),
          columns = "stat", 
          `Median` = as_paragraph(as_chunk(med)),
          `IQR` = as_paragraph(as_chunk(iqr))) %>% 
  as_flextable(separate_with = "Theme") %>% 
  set_table_properties(layout = "autofit", width = 1)

supp_tbl2

################################################################################
# S1: Collinearity  ####
################################################################################

## Correlation between PFAS sources and EBM variables  {.scrollable}


load("output/figures/cor_plots.RData")

cor_plot[[2]] %>% 
  plotly::ggplotly()


## Correlation between PFAS sources and SVM variables {.scrollable}


#| fig-asp: 1

cor_plot[[3]]%>% 
  plotly::ggplotly()




################################################################################
# TABLE 3: MODEL  ####
################################################################################


############ QUARTILE MODEL 1 ############## 

# mod1_quarts <- pfas_mod_data_format %>% 
#   select(hasPFAS,`Quartile of combined Social Vulnerability & Environmental Burden`) %>% 
#   gtsummary::tbl_uvregression(method = glm,
#                               y = hasPFAS, 
#                               method.args = list(family = binomial),
#                               exponentiate = TRUE) %>% 
#   tbl_theme(gt = TRUE)
# 
# mod1_quarts

############ QUARTILE MODEL 2 ############## 

mod2_quarts_uv <- pfas_mod_data_format %>% 
  select(hasPFAS,`Quartile of combined Social Vulnerability & Environmental Burden`, 
         `Quartile of SVM`, `Quartile of EBM`) %>% 
  gtsummary::tbl_uvregression(method = glm,
                              y = hasPFAS, 
                              method.args = list(family = binomial),
                              exponentiate = TRUE) 

mod2_quarts_multi <- glm(hasPFAS ~`Quartile of SVM` + `Quartile of EBM`, 
                        data = pfas_mod_data_format, family = binomial()) %>% 
  gtsummary::tbl_regression(exponentiate = TRUE)




mod2 <- tbl_merge(list(mod2_quarts_uv, mod2_quarts_multi), 
                  tab_spanner = c("Unadjusted", "Adjusted"))

mod2_formatted <- format_gtreg(mod2, combine_est_ci = TRUE, hide_p = TRUE, as_flex = TRUE)

mod2_formatted



# save(mod_quarts_uv, mod_quarts_multi, file = "output/models/quartile_mod.RData")


############ QUARTILE MODEL 3 ############## 

mod3_quarts_themes_uv <- pfas_mod_data_format %>% 
  select(hasPFAS, contains(c("Quartile of SVM ", "Quartile of EBM "))) %>% 
  gtsummary::tbl_uvregression(method = glm,
                              y = hasPFAS, 
                              method.args = list(family = binomial),
                              exponentiate = TRUE)


mod3_quarts_themes_multi <- glm(hasPFAS ~ 
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



mod3 <- tbl_merge(list(mod3_quarts_themes_uv, mod3_quarts_themes_multi), tab_spanner = c("Unadjusted", "Adjusted"))


mod3_formatted <- format_gtreg(mod3, combine_est_ci = TRUE, hide_p = TRUE, as_flex = TRUE)

mod3_formatted




################################################################################
# FIGURE 1: MAP  ####
################################################################################

load("data/processed/ppps_w_buffer_perc_tract_intersecting_FIX_70hrs.RData")

pfas_map_sf <- ppps_perc %>% 
  filter(!is.na(StateAbbr)) %>% 
  mutate(perc_area_pfas = ifelse(perc_area_pfas == 0, NA, perc_area_pfas*100)) %>% 
  left_join(pfas_mod_data_format[,c("geoid", "Social/Env percentile rank")])
  # st_transform(4269) 

pfas_map_sf %>% 
  ggplot(aes(fill = perc_area_pfas)) +
  geom_sf(color = NA) +
  scale_fill_continuous(name = "% of census tract area\nwithin 1-mile of a PFAS source",
                        low = "lightpink", high =  "maroon4", na.value="snow2") + 
  theme_bw(base_size = 22)


beepr::beep(10)
ggsave(paste0("output/figures/newproj_REVISED2_percent_area_pfas_0_map_",Sys.Date(),".png"), height = 20, width = 30)
beepr::beep(10)


p_load("biscale", cowplot)

bivar_map_df <- bi_class(pfas_map_sf, x = `Social/Env percentile rank`, y = perc_area_pfas_0, style = "quantile", dim = 3)

bivar_map <- ggplot() +
  geom_sf(data = bivar_map_df, mapping = aes(fill = bi_class), color = NA, show.legend = FALSE) +
  bi_scale_fill(pal = "DkBlue2", dim = 3) +
  # labs(
  #   title = "PFAS and Income in St. Louis, MO",
  #   subtitle = "Gray Pink (GrPink) Palette"
  # ) +
  bi_theme()

legend <- bi_legend(pal = "DkBlue2", 
                    dim = 3,
                    xlab = "Higher Cumulative\nSocial/Environmental Burden",
                    ylab = "Higher % of census tract\nwithin 1-mile of PFAS point source",
                    size = 20)

ggdraw() + 
  draw_plot(legend, 0.25, 0.25, 0.5, 0.5)


ggsave(paste0("output/figures/testlegend_bivariate_SER_PFAS_plot",Sys.Date(),".png"), height = 20, width = 30)


finalPlot <- ggdraw() +
  draw_plot(bivar_map, 0, 0, 1, 1) +
  draw_plot(legend, 0.25, 0.25, 0.5, 0.5)

finalPlot

beepr::beep(10)
ggsave(paste0("output/figures/bivariate_SER_PFAS_plot",Sys.Date(),".png"), height = 20, width = 30)
beepr::beep(10)


################################################################################
# SAVE TABLES TO DOC  ####
################################################################################

sect_properties <- prop_section(
  page_size = page_size(
    orient = "landscape",
    width = 8.3, height = 11.7
  ),
  type = "continuous",
  page_margins = page_mar()
)



flextable::save_as_docx(
  tbl1,
  `Table 2` = tbl2,
  `Model2` = mod2_formatted,
  `Table 1 (supplemental)` = supp_tbl1,
  `Table 2 (supplemental)` = supp_tbl2,
  `Model3` = mod3_formatted,
  path = "my_results.docx", pr_section = sect_properties
)



################################################################################
# SUPER SUPPLEMENTAL WORK ZONE  ####
################################################################################


pfas_mod_data_strat_RU <- pfas_mod_data %>% 
  group_by(urban_rural) %>% 
  mutate(EBM_quartile_RU = cut(RPL_EBM, breaks = 4, labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")), 
         #EBM_tertile = cut(RPL_EBM, breaks = 3, labels = c("Low", "Moderate", "High")),
         SVM_quartile_RU = cut(RPL_SVM, breaks = 4, labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")), 
         #SVM_tertile = cut(RPL_SVM, breaks = 3, labels = c("Low", "Moderate", "High")),
         SER_quartile_RU = cut(RPL_SER, breaks = 4, labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")), 
         #SER_tertile = cut(RPL_SER, breaks = 3, labels = c("Low", "Moderate", "High")),         
         EJI_quartile_RU = cut(RPL_EJI, breaks = 4,  labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")), 
         #EJI_tertile =  cut(RPL_EJI, breaks = 3, labels = c("Low", "Moderate", "High")),
         SVM_Theme1_quart_RU = cut(RPL_SVM_DOM1, breaks = 4,  labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")), 
         SVM_Theme2_quart_RU = cut(RPL_SVM_DOM2, breaks = 4,  labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")), 
         SVM_Theme3_quart_RU = cut(RPL_SVM_DOM3, breaks = 4,  labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")), 
         SVM_Theme4_quart_RU = cut(RPL_SVM_DOM4, breaks = 4,  labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")), 
         EBM_Theme1_quart_RU = cut(RPL_EBM_DOM1, breaks = 4,  labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")), 
         EBM_Theme2_quart_RU = cut(RPL_EBM_DOM2, breaks = 4,  labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")), 
         EBM_Theme3_quart_RU = cut(RPL_EBM_DOM3, breaks = 4,  labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")), 
         EBM_Theme4_quart_RU = cut(RPL_EBM_DOM4, breaks = 4,  labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")), 
         EBM_Theme5_quart_RU = cut(RPL_EBM_DOM5, breaks = 4,  labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")))




supp_tbl2_themes_RU <- pfas_mod_data_strat_RU %>% 
  group_by(urban_rural) %>% 
  summarize(med = round(median(perc_area_pfas_0*100), 2),
            iqr = paste0(" (", round(quantile(perc_area_pfas_0*100, .25), 2), " - ",
                         round(quantile(perc_area_pfas_0*100, .75), 2), ")")) %>% 
  mutate(Module = "All U.S. Census Tracts",
         Quartile = "") %>% 
  bind_rows(pfas_mod_data_strat_RU %>% 
  pivot_longer(names_to = "theme", values_to = "Quartile", contains(c("quart_RU", "SVM_quartile_RU", "EBM_quartile_RU"))) %>% 
  group_by(urban_rural, theme, Quartile) %>% 
  summarize(med = round(median(perc_area_pfas_0*100), 2),
            iqr = paste0(" (", round(quantile(perc_area_pfas_0*100, .25), 2), " - ",
                         round(quantile(perc_area_pfas_0*100, .75), 2), ")")) %>% 
  #separate(theme, into = c("Module", "Theme"), sep = "(?<=EBM_|SVM_) ") %>% 
  mutate(stat = "% of census tract area within 1-mile of a PFAS source",
         Module = str_extract(theme, "EBM|SVM"),
         Module = case_when(Module == "EBM" ~ "Environmental burden",
                            Module == "SVM" ~ "Social vulnerability")) 
  )

supp_tbl2_RU <- tabulator(supp_tbl2_themes_RU, rows = c("Module", "theme", "urban_rural", "Quartile"),
                       columns = "stat", 
                       `Median` = as_paragraph(as_chunk(med)),
                       `IQR` = as_paragraph(as_chunk(iqr))) %>% 
  as_flextable(separate_with = "theme") %>% 
  set_table_properties(layout = "autofit", width = 1)

supp_tbl2_RU

# takeaway... there's just basicallyfewer pfas sources in rural areas 
# notable is EBM theme 1, theme 4; SVM no effect on SES 




mod2_quarts_uv_RU <- pfas_mod_data_strat_RU %>% 
  select(hasPFAS,SER_quartile, 
         SVM_quartile, EBM_quartile, urban_rural) %>% 
  tbl_strata(
    strata = urban_rural, 
    .tbl_fun = ~.x %>% 
      gtsummary::tbl_uvregression(method = glm,
                                  y = hasPFAS, 
                                  method.args = list(family = binomial),
                                  exponentiate = TRUE) 
  )



mod2_quarts_multi_rural <- glm(hasPFAS ~ SVM_quartile + EBM_quartile, 
                               data = pfas_mod_data_strat_RU[which(pfas_mod_data_strat_RU$urban_rural == "Rural"),], family = binomial()) %>% 
  gtsummary::tbl_regression(exponentiate = TRUE)

mod2_quarts_multi_urban <- glm(hasPFAS ~ SVM_quartile + EBM_quartile, 
                               data = pfas_mod_data_strat_RU[which(pfas_mod_data_strat_RU$urban_rural == "Urban"),], family = binomial()) %>% 
  gtsummary::tbl_regression(exponentiate = TRUE)


mod2_adj_RU <-  tbl_merge(list(mod2_quarts_multi_rural, mod2_quarts_multi_urban), tab_spanner = c("Rural", "Urban"))




mod2_RU<- tbl_stack(list(mod2_quarts_uv_RU, mod2_adj_RU),
                    group_header = c("Unadjusted", "Adjusted"))
mod2_RU




# mod 3

mod3_quarts_uv_RU <- pfas_mod_data_strat_RU %>% 
  select(hasPFAS, contains("quart_RU"), urban_rural) %>% 
  tbl_strata(
    strata = urban_rural, 
    .tbl_fun = ~.x %>% 
      gtsummary::tbl_uvregression(method = glm,
                                  y = hasPFAS, 
                                  method.args = list(family = binomial),
                                  exponentiate = TRUE) 
  )




mod3_quarts_multi_rural <- glm(hasPFAS ~ SVM_Theme1_quart_RU + SVM_Theme2_quart_RU + 
                                 SVM_Theme3_quart_RU + SVM_Theme4_quart_RU + 
                                 EBM_Theme1_quart_RU + EBM_Theme2_quart_RU + EBM_Theme3_quart_RU + 
                                 EBM_Theme4_quart_RU + EBM_Theme5_quart_RU, 
                               data = pfas_mod_data_strat_RU[which(pfas_mod_data_strat_RU$urban_rural == "Rural"),], family = binomial()) %>% 
  gtsummary::tbl_regression(exponentiate = TRUE)

mod3_quarts_multi_urban <-  glm(hasPFAS ~ SVM_Theme1_quart_RU + SVM_Theme2_quart_RU + 
                                  SVM_Theme3_quart_RU + SVM_Theme4_quart_RU + 
                                  EBM_Theme1_quart_RU + EBM_Theme2_quart_RU + EBM_Theme3_quart_RU + 
                                  EBM_Theme4_quart_RU + EBM_Theme5_quart_RU, 
                                data = pfas_mod_data_strat_RU[which(pfas_mod_data_strat_RU$urban_rural == "Urban"),], family = binomial()) %>% 
  gtsummary::tbl_regression(exponentiate = TRUE)


mod3_adj_RU <-  tbl_merge(list(mod3_quarts_multi_rural, mod3_quarts_multi_urban), tab_spanner = c("Rural", "Urban"))



mod3_RU<- tbl_stack(list(mod3_quarts_uv_RU, mod3_adj_RU),
                    group_header = c("Unadjusted", "Adjusted"))
mod3_RU





flextable::save_as_docx(
  `Unadjusted and adjusted models with SVM and EBM quartiles -- stratified by rural vs urban` = as_flex_table(mod2_RU),
  `Unadjusted and adjusted models with SVM and EBM themes -- stratified by rural vs urban` = as_flex_table(mod3_RU),
  path = "urban_rural_results.docx", pr_section = sect_properties
)



uv_svm_cols <- eji_codebook_revised$variable_name[which(
  eji_codebook_revised$module == "SVI" &
    str_detect(eji_codebook_revised$variable_name, "EP_"))]

uv_ebm_cols <- eji_codebook_revised$variable_name[which(
  eji_codebook_revised$module == "EBM" &
    str_detect(eji_codebook_revised$variable_name, "E_"))]


mod3_RU_uv <- pfas_mod_data_strat_RU %>%
  select(hasPFAS, urban_rural, any_of(c(uv_svm_cols, uv_ebm_cols))) %>% 
  tbl_strata(
    strata = urban_rural, 
    .tbl_fun = ~.x %>% 
      gtsummary::tbl_uvregression(method = glm,
                                  y = hasPFAS, 
                                  method.args = list(family = binomial),
                                  exponentiate = TRUE) 
  )

mod3_RU_uv


pfas_mod_data_strat_RU %>% 
  group_by(urban_rural) %>% 
  summarize(across(any_of(c(uv_svm_cols, uv_ebm_cols)), median)) %>% 
  pivot_longer(names_to = "variable", values_to = "median", -urban_rural) %>% 
  pivot_wider(names_from = "urban_rural", values_from = "median") %>% 
    View()



################################################################################
# OLD/GARBAGE <3  ####
################################################################################


################################################################################
# TABLE 3 MODELING EXPLORATORY STUFF  ####
################################################################################


############ TERTILE MODEL 1 ############## 

# mod1_tert_uv <- pfas_mod_data_format %>% 
#   select(hasPFAS,`Tertile of SVM`, `Tertile of EBM`) %>% 
#   gtsummary::tbl_uvregression(method = glm,
#                               y = hasPFAS, 
#                               method.args = list(family = binomial),
#                               exponentiate = TRUE)
# 
# 
# mod1_tert_multi <- glm(hasPFAS ~`Tertile of SVM` + `Tertile of EBM`, 
#                         data = pfas_mod_data_format, family = binomial()) %>% 
#   gtsummary::tbl_regression(exponentiate = TRUE)
# 
# 
# tbl_merge(list(mod1_tert_uv, mod1_tert_multi), tab_spanner = c("Unadjusted", "Adjusted"))
# 


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

tidy(RPL_SVM_comp_multi, exponentiate = TRUE)


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
# TABLE 2 (OLD)  ####
################################################################################


# table1a <- tbl_summary(pfas_mod_data_format,
#             include = c("% of census tract area within 1-mile of a PFAS source",
#                         "Social/Env percentile rank",
#                         "EBM percentile rank",
#                         "SVM percentile rank"
#                         ),
#             type = list(c("% of census tract area within 1-mile of a PFAS source",
#                           "Social/Env percentile rank",
#                           "EBM percentile rank",
#                           "SVM percentile rank"
#             ) ~ 'continuous2'),
#             statistic = list(all_continuous() ~ c("{median} ({p25} - {p75})", 
#                                                   "{mean} ({sd})"))) 
# 
# table1b <- tbl_summary(pfas_mod_data_format, 
#                        by = c("Number of PFAS point sources"), 
#                        include = c(
#                                    "Social/Env percentile rank",
#                                    "EBM percentile rank",
#                                    "SVM percentile rank"
#                        ),
#                        type = list(c(
#                                      "Social/Env percentile rank",
#                                      "EBM percentile rank",
#                                      "SVM percentile rank"
#                        ) ~ 'continuous2'),
#                        statistic = list(all_continuous() ~ c("{median} ({p25} - {p75})", 
#                                                              "{mean} ({sd})"))) %>% 
#   add_p()
# 
# 
# tbl_merge(list(table1a, table1b), 
#           tab_spanner = c("All Census Tracts", "Number of PFAS point sources"))  %>% 
#   tbl_theme(gt = TRUE)

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
