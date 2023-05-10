### AUTHOR: AHz
### WRITTEN IN: R version 4.2.2
### PURPOSE: Script to run all the tables and figures in EH508 thesis submission



################################################################################
# 0. LOAD DEPENDENCIES  ####
################################################################################

source("scripts/0 - setup.R")
load("data/processed/all_ppps_spatial_1c_output.RData")
load("data/processed/nchs_urban_rural_classification.RData")

p_load(gtsummary, officer)

theme_gtsummary_compact()



format_gtreg <- function(gtsumm_tbl_reg, 
                         combine_est_ci = FALSE, 
                         hide_p = FALSE, 
                         var_header = "Characteristic",
                         as_flex = TRUE) {
  
  gtsumm_tbl_reg$table_body <- gtsumm_tbl_reg$table_body %>% 
    mutate(across(contains("ci_"), ~case_when(!is.na(.x) ~ paste0("(", .x, ")"),
                                              TRUE ~ NA)))
  
  gtsumm_tbl_reg$table_styling$header <- gtsumm_tbl_reg$table_styling$header %>% 
    mutate(label = case_when(column == "label" ~ paste0("**", var_header, "**"),
                             TRUE ~ label))
  
  #combine odds ratio and CI 
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
    
    gtsumm_tbl_reg$table_styling$footnote_abbrev <- gtsumm_tbl_reg$table_styling$footnote_abbrev %>% 
      mutate(footnote = case_when(str_detect(column, "estimate_") ~ "OR = Odds Ratio, CI = Confidence Interval",
                                  TRUE ~ footnote))
    
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


################################################################################
# 1. PROCESSING   ####
################################################################################


pfas_mod_data <- ppps_perc_df %>% 
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
         ) %>% 
  mutate(hasPFAS = case_when(!is.na(perc_area_pfas) ~ 1,
                             TRUE ~ 0)) 

pfas_mod_data_format <- pfas_mod_data %>% 
  mutate(`Number of PFAS point sources` = ifelse(hasPFAS == 1, "≥ 1", "0"),
         `% of census tract area within 1-mile of a PFAS source` = perc_area_pfas_0*100) %>% 
  rename(
    #EJI 
    `Sum of the HVM, EBM, and SVM module percentile ranks` = SPL_EJI,
    `EJI percentile rank` = RPL_EJI,
    `Quartile of EJI` = EJI_quartile, 
    #SER
    `Combined SV & EB percentile rank` = RPL_SER,
    `Quartile of Combined EB & SV` = SER_quartile,
    #`Tertile of Combined EB & SV percentile rank` = SER_tertile,
    #EBM
    `Sum of the EBM variable percentile ranks` = SPL_EBM,
    `EB percentile rank` = RPL_EBM,
    `Quartile of EB` = EBM_quartile,
    #`Tertile of EB` = EBM_tertile,
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
    `Sum of the SV variable percentile ranks` = SPL_SVM,
    `SV percentile rank` = RPL_SVM,
    `Quartile of SV` = SVM_quartile,
    #`Tertile of SV` = SVM_tertile,
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

save(pfas_mod_data, pfas_mod_data_format, file = "data/processed/pfas_mod_formatting.RData")


################################################################################
# TABLE 1 ####
################################################################################

tbl1 <- tbl_summary(st_drop_geometry(ppps_salvatore_tract) %>% 
              mutate(dataset = factor(dataset,
                                      levels = c("airports_sf", "epastewardship_county_sf",
                                                 "facilities_sf_salvatore", "fed_agencies_sf", 
                                                 "fire_sf", "production_sf",
                                                 "spills_sf", "superfund_sf"), 
                                      labels = c("Part 139 airports",
                                                 "EPA Stewardship Program participating facilities",
                                                 "Facilities in industries that may be handling PFAS",
                                                 "Federal agency locations with known or suspected PFAS detections",
                                                 "Fire training sites",
                                                 "Facilities that manufacture or import PFAS",
                                                 "Known PFAS spills/release incidents",
                                                 "Superfund sites with PFAS detections"
                                                 ))),
            label = dataset ~ "Source Type",
            include = c("dataset")) %>% 
  as_flex_table()

tbl1

#how many census tracts had a PFAS source directly in them? 
length(unique(ppps_salvatore_tract$geoid))/length(unique(pfas_mod_data_format$geoid))



################################################################################
# FIGURE 1: MAP  ####
################################################################################

load("data/processed/ppps_w_buffer_perc_tract_intersecting_FIX_70hrs.RData")


# map 1: univariate -- look at percent area within 1 mile of PFAS source
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


# map 2: bivariate -- look at percent area within 1-mile of PFAS source + 
p_load("biscale", cowplot)

bivar_map_df <- bi_class(pfas_map_sf, x = `Social/Env percentile rank`, y = perc_area_pfas_0, style = "quantile", dim = 3)

bivar_map_sfmp <- sf::st_cast(bivar_map_df, "MULTIPOLYGON")


bivar_map <- ggplot() +
  geom_sf(data = bivar_map_sfmp, mapping = aes(fill = bi_class), color = NA, show.legend = FALSE) +
  bi_scale_fill(pal = "DkBlue2", dim = 3) +
  # labs(
  #   title = "PFAS and Combined Social + Env Burden"
  # ) +
  bi_theme()

legend <- bi_legend(pal = "DkBlue2", 
                    dim = 3,
                    xlab = "Higher Combined Social + Environmental Burden",
                    ylab = "Higher % area within 1-mile of a PFAS source",
                    size = 20)

legend
ggsave(paste0("output/figures/bivar_legend",Sys.Date(),".png"), height = 10, width = 10)


finalPlot <- ggdraw() +
  draw_plot(bivar_map, 0, 0, 1, 1) +
  #draw_image("output/figures/bivar_legend2023-05-07.png", x = .90, y = .20,  hjust = 0.5, vjust = 0.5, scale = 0.35)
  draw_image("output/figures/bivar_legend2023-05-07.png", x = .10, y = .20,  hjust = 0.5, vjust = 0.5, scale = 0.35)
  # draw_plot(legend, x = .90, y = .20,  hjust = 0.5, vjust = 0.5, scale = 0.35)

finalPlot

beepr::beep(10)
ggsave(paste0("output/figures/bivariate_SER_PFAS_plot_",Sys.Date(),".png"), height = 20, width = 35)
beepr::beep(10)

# plotly::ggplotly(bivar_map)


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
              group_by(`Quartile of Combined EB & SV`) %>% 
              summarize(med = round(median(`% of census tract area within 1-mile of a PFAS source`), 2),
                        iqr = paste0(" (", round(quantile(`% of census tract area within 1-mile of a PFAS source`, .25), 2), " - ",
                                     round(quantile(`% of census tract area within 1-mile of a PFAS source`, .75), 2), ")"))%>% 
              rename(`Quartile` = `Quartile of Combined EB & SV`) %>% 
              mutate(Module = "Combined SV & EB")) %>% 
  bind_rows(pfas_mod_data_format %>% 
              group_by(`Quartile of EB`) %>% 
              summarize(med = round(median(`% of census tract area within 1-mile of a PFAS source`), 2),
                        iqr = paste0(" (", round(quantile(`% of census tract area within 1-mile of a PFAS source`, .25), 2), " - ",
                                     round(quantile(`% of census tract area within 1-mile of a PFAS source`, .75), 2), ")"))%>% 
              rename(`Quartile` = `Quartile of EB`) %>% 
              mutate(Module = "EB")) %>% 
  bind_rows(pfas_mod_data_format %>% 
              group_by(`Quartile of SV`) %>% 
              summarize(med = round(median(`% of census tract area within 1-mile of a PFAS source`), 2),
                        iqr = paste0(" (", round(quantile(`% of census tract area within 1-mile of a PFAS source`, .25), 2), " - ",
                                     round(quantile(`% of census tract area within 1-mile of a PFAS source`, .75), 2), ")")) %>% 
              rename(`Quartile` = `Quartile of SV`) %>% 
              mutate(Module = "SV")) %>%
  mutate(stat = "% of census tract area within 1-mile of a PFAS source")

tbl2 <- tabulator(tbl2_df, rows = c("Module", "Quartile"),
                       columns = "stat", 
          `Median` = as_paragraph(as_chunk(med)),
          `IQR` = as_paragraph(as_chunk(iqr))) %>% 
  as_flextable(separate_with = "Module")%>% 
  set_table_properties(layout = "autofit", width = 1) %>% 
  font(fontname = "Times New Roman", part = "all")

tbl2 



################################################################################
# TABLE 3: MODEL  ####
################################################################################


mod2_quarts_uv <- pfas_mod_data_format %>% 
  select(hasPFAS,`Quartile of Combined EB & SV`, 
         `Quartile of SV`, `Quartile of EB`) %>% 
  gtsummary::tbl_uvregression(method = glm,
                              y = hasPFAS, 
                              method.args = list(family = binomial),
                              exponentiate = TRUE) 

mod2_quarts_multi <- glm(hasPFAS ~`Quartile of SV` + `Quartile of EB`, 
                        data = pfas_mod_data_format, family = binomial()) 

mod2_quarts_multi

mod2 <- tbl_merge(list(mod2_quarts_uv, tbl_regression(mod2_quarts_multi, exponentiate = TRUE)), 
                  tab_spanner = c("**Unadjusted**", "**Adjusted**"))

mod2_formatted <- format_gtreg(mod2, var_header = "Module", combine_est_ci = TRUE, hide_p = TRUE, as_flex = TRUE)%>% 
  font(fontname = "Times New Roman", part = "all")

mod2_formatted


################################################################################
# SUPPLEMENTAL TABLE 1: NAICS CODES  ####
################################################################################

load("output/tables/table_s1.RData")

supp_tbl1 <- table_s1 %>% 
  rename(`NAICS code` = naics_code,
         `Industry description` = industry_title,
         `Number of facilities in study` = n) %>% 
  as_flextable(show_coltype = FALSE, max_row = 50) %>% 
  set_table_properties(layout = "autofit", width = 1) %>% 
  autofit() %>% 
  font(fontname = "Times New Roman", part = "all")

supp_tbl1


################################################################################
# SUPPLEMENTAL TABLE 3: THEME SUMMARY TABLE  ####
################################################################################


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
  set_table_properties(layout = "autofit", width = 1) %>% 
  font(fontname = "Times New Roman", part = "all")

supp_tbl2


################################################################################
# SUPPLEMENTAL TABLE 4: THEME MODEL  ####
################################################################################


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
                                data = pfas_mod_data_format, family = binomial()) 
  



mod3 <- tbl_merge(list(mod3_quarts_themes_uv, 
                       tbl_regression(mod3_quarts_themes_multi, exponentiate = TRUE)), 
                  tab_spanner = c("Unadjusted", "Adjusted"))


mod3_formatted <- format_gtreg(mod3, combine_est_ci = TRUE, hide_p = TRUE, as_flex = TRUE) %>% 
  font(fontname = "Times New Roman", part = "all")

mod3_formatted



################################################################################
# SUPPLEMENTAL TABLE 5: RURAL/URBAN STRAT MODEL ####
################################################################################


mod2_quarts_uv_RU <- pfas_mod_data_strat_RU %>% 
  select(hasPFAS,`Quartile of combined EB and SV`, 
         `Quartile of SV`, `Quartile of EB`, urban_rural) %>% 
  tbl_strata(
    strata = urban_rural, 
    .tbl_fun = ~.x %>% 
      gtsummary::tbl_uvregression(method = glm,
                                  y = hasPFAS, 
                                  method.args = list(family = binomial),
                                  exponentiate = TRUE) 
  )



mod2_quarts_multi_rural <- glm(hasPFAS ~ `Quartile of SV` + `Quartile of EB`, 
                               data = pfas_mod_data_strat_RU[which(pfas_mod_data_strat_RU$urban_rural == "Rural"),], 
                               family = binomial()) %>% 
  gtsummary::tbl_regression(exponentiate = TRUE)

mod2_quarts_multi_urban <- glm(hasPFAS ~ `Quartile of SV` + `Quartile of EB`, 
                               data = pfas_mod_data_strat_RU[which(pfas_mod_data_strat_RU$urban_rural == "Urban"),], 
                               family = binomial()) %>% 
  gtsummary::tbl_regression(exponentiate = TRUE)


mod2_adj_RU <-  tbl_merge(list(mod2_quarts_multi_rural, mod2_quarts_multi_urban), tab_spanner = c("Rural", "Urban"))




mod2_RU<- tbl_stack(list(mod2_quarts_uv_RU, mod2_adj_RU),
                    group_header = c("Unadjusted", "Adjusted"))



mod2_RU_formatted <- format_gtreg(mod2_RU, combine_est_ci = TRUE, hide_p = TRUE, as_flex = TRUE) %>% 
  font(fontname = "Times New Roman", part = "all")

mod2_RU_formatted






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
  `Table 1` = tbl1,
  `Table 2` = tbl2,
  `Table 3` = mod2_formatted,
  `Table S1` = supp_tbl1,
  `Table S3` = supp_tbl2,
  `Table S4` = mod3_formatted,
  `Table S5` = mod2_RU_formatted,
  path = "output/tables/thesis_submission_results.docx", pr_section = sect_properties
)






################################################################################
# SUPER SUPPLEMENTAL WORK ZONE  ####
################################################################################


pfas_mod_data_strat_RU <- pfas_mod_data %>% 
  mutate(`Number of PFAS point sources` = ifelse(hasPFAS == 1, "≥ 1", "0"),
         `% area within 1-mile of a PFAS source` = perc_area_pfas_0*100) %>% 
  group_by(urban_rural) %>% 
  mutate(`Quartile of EB` = cut(RPL_EBM, breaks = 4, labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")), 
         #EBM_tertile = cut(RPL_EBM, breaks = 3, labels = c("Low", "Moderate", "High")),
         `Quartile of SV` = cut(RPL_SVM, breaks = 4, labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")), 
         #SVM_tertile = cut(RPL_SVM, breaks = 3, labels = c("Low", "Moderate", "High")),
         `Quartile of combined EB and SV` = cut(RPL_SER, breaks = 4, labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")), 
         #SER_tertile = cut(RPL_SER, breaks = 3, labels = c("Low", "Moderate", "High")),         
         #EJI_quartile_RU = cut(RPL_EJI, breaks = 4,  labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")), 
         #EJI_tertile =  cut(RPL_EJI, breaks = 3, labels = c("Low", "Moderate", "High")),
         `Quartile of SV Theme 1: Race/Ethnicity` = cut(RPL_SVM_DOM1, breaks = 4,  labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")), 
         `Quartile of SV Theme 2: SES` = cut(RPL_SVM_DOM2, breaks = 4,  labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")), 
         `Quartile of SV Theme 3: Household Characteristics` = cut(RPL_SVM_DOM3, breaks = 4,  labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")), 
         `Quartile of SV Theme 4: Housing Type` = cut(RPL_SVM_DOM4, breaks = 4,  labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")), 
         `Quartile of EB Theme 1: Air Pollution` = cut(RPL_EBM_DOM1, breaks = 4,  labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")), 
         `Quartile of EB Theme 2: Potentially Hazardous + Toxic Sites` = cut(RPL_EBM_DOM2, breaks = 4,  labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")), 
         `Quartile of EB Theme 3: Built Env` = cut(RPL_EBM_DOM3, breaks = 4,  labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")), 
         `Quartile of EB Theme 4: Transportation Infrastructure` = cut(RPL_EBM_DOM4, breaks = 4,  labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")), 
         `Quartile of EB Theme 5: Water Pollution` = cut(RPL_EBM_DOM5, breaks = 4,  labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")))




supp_tbl2_themes_RU <- pfas_mod_data_strat_RU %>% 
  group_by(urban_rural) %>% 
  summarize(med = round(median(`% area within 1-mile of a PFAS source`), 2),
            iqr = paste0(" (", round(quantile(`% area within 1-mile of a PFAS source`, .25), 2), " - ",
                         round(quantile(`% area within 1-mile of a PFAS source`, .75), 2), ")")) %>% 
  mutate(Module = "All U.S. Census Tracts",
         Quartile = "") %>% 
  bind_rows(pfas_mod_data_strat_RU %>% 
  pivot_longer(names_to = "theme", values_to = "Quartile", contains(c("Quartile of "))) %>% 
  group_by(urban_rural, theme, Quartile) %>% 
  summarize(med = round(median(`% area within 1-mile of a PFAS source`), 2),
            iqr = paste0(" (", round(quantile(`% area within 1-mile of a PFAS source`, .25), 2), " - ",
                         round(quantile(`% area within 1-mile of a PFAS source`, .75), 2), ")")) %>% 
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



# 
# mod3_quarts_multi_rural <- glm(hasPFAS ~ SVM_Theme1_quart_RU + SVM_Theme2_quart_RU + 
#                                  SVM_Theme3_quart_RU + SVM_Theme4_quart_RU + 
#                                  EBM_Theme1_quart_RU + EBM_Theme2_quart_RU + EBM_Theme3_quart_RU + 
#                                  EBM_Theme4_quart_RU + EBM_Theme5_quart_RU, 
#                                data = pfas_mod_data_strat_RU[which(pfas_mod_data_strat_RU$urban_rural == "Rural"),], family = binomial()) %>% 
#   gtsummary::tbl_regression(exponentiate = TRUE)
# 
# mod3_quarts_multi_urban <-  glm(hasPFAS ~ SVM_Theme1_quart_RU + SVM_Theme2_quart_RU + 
#                                   SVM_Theme3_quart_RU + SVM_Theme4_quart_RU + 
#                                   EBM_Theme1_quart_RU + EBM_Theme2_quart_RU + EBM_Theme3_quart_RU + 
#                                   EBM_Theme4_quart_RU + EBM_Theme5_quart_RU, 
#                                 data = pfas_mod_data_strat_RU[which(pfas_mod_data_strat_RU$urban_rural == "Urban"),], family = binomial()) %>% 
#   gtsummary::tbl_regression(exponentiate = TRUE)
# 
# 
# mod3_adj_RU <-  tbl_merge(list(mod3_quarts_multi_rural, mod3_quarts_multi_urban), tab_spanner = c("Rural", "Urban"))
# 
# 
# 
# mod3_RU<- tbl_stack(list(mod3_quarts_uv_RU, mod3_adj_RU),
#                     group_header = c("Unadjusted", "Adjusted"))
# mod3_RU
# 
# 



flextable::save_as_docx(
  `Unadjusted and adjusted models with SVM and EBM quartiles -- stratified by rural vs urban` = mod2_RU_formatted,
  # `Unadjusted and adjusted models with SVM and EBM themes -- stratified by rural vs urban` = as_flex_table(mod3_RU),
  path = "urban_rural_results2.docx", pr_section = sect_properties
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
