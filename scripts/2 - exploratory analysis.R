### AUTHOR: AHz
### WRITTEN IN: R version 4.2.2
### PURPOSE: 



################################################################################
# 0. LOAD DEPENDENCIES  ####
################################################################################

source("scripts/0 - setup.R")

load("data/processed/all_ppps_w_count.RData")
load("data/processed/all_ppps_WOspatial_1c_output.RData")


################################################################################
# 1. INSPECT CORRELATIONS  ####
################################################################################

# 
# eji_w_ppps_salvatore_wallEJI <-eji_data_fromCDC %>% 
#   full_join(ppps_salvatore_count)  %>% 
#   mutate(n_ppps = ifelse(is.na(n), 0, as.numeric(n)),
#          n_ppps_groups = case_when(n_ppps == 0 ~ "0",
#                                    n_ppps == 1 ~ "1",
#                                    n_ppps == 2 ~ "2",
#                                    n_ppps == 3 ~ "3",
#                                    n_ppps == 4 ~ "4",
#                                    n_ppps == 5 ~ "5",
#                                    n_ppps <= 10 ~ "≤10",
#                                    n_ppps <= 20 ~ "≤20",
#                                    n_ppps <= 50 ~ "≤50",
#                                    n_ppps <= 100 ~ "≤100",
#                                    n_ppps >100 ~ ">100"
#          ),
#          n_ppps_groups = factor(n_ppps_groups, levels = c("0", "1", "2", "3", "4", "5",
#                                                           "≤10", "≤20", "≤50", 
#                                                           "≤100", ">100"
#          ))) %>%
#   distinct()
# 
# 
# eji_w_ppps_eh249_wallEJI <-eji_data_fromCDC %>% 
#   full_join(ppps_eh249_count)  %>% 
#   mutate(n_ppps = ifelse(is.na(n), 0, as.numeric(n)),
#          n_ppps_groups = case_when(n_ppps == 0 ~ "0",
#                                    n_ppps == 1 ~ "1",
#                                    n_ppps == 2 ~ "2",
#                                    n_ppps == 3 ~ "3",
#                                    n_ppps == 4 ~ "4",
#                                    n_ppps == 5 ~ "5",
#                                    n_ppps <= 10 ~ "≤10",
#                                    n_ppps <= 20 ~ "≤20",
#                                    n_ppps <= 50 ~ "≤50",
#                                    n_ppps <= 100 ~ "≤100",
#                                    n_ppps >100 ~ ">100"
#          ),
#          n_ppps_groups = factor(n_ppps_groups, levels = c("0", "1", "2", "3", "4", "5",
#                                                           "≤10", "≤20", "≤50", 
#                                                           "≤100", ">100"
#          ))) %>%
#   distinct()
# 


###### SPEARMAN CORR ######



eji_ppps_spearman_salvatore <- eji_w_ppps_salvatore %>%
  select(E_TOTPOP:EPL_MHLTH, n, n_ppps)

eji_ppps_spearman_eh249 <- eji_w_ppps_eh249 %>%
  select(E_TOTPOP:EPL_MHLTH, n, n_ppps)

eji_ppps_perc_spearman <- ppps_perc_df %>% 
  select(E_TOTPOP:EPL_MHLTH, perc_area_pfas_0) %>% 
  filter(perc_area_pfas_0 > 0)


get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}



colname_order <- eji_codebook_revised %>% 
  group_by(var_desc) %>%
  arrange(variable_name, .by_group=TRUE) %>% 
  pull(variable_name)


eji_ppps_spearman_list <- list("eh249" = eji_ppps_spearman_eh249, 
                               "salvatore" = eji_ppps_spearman_salvatore,
                               "perc_area_pfas" = eji_ppps_perc_spearman)

for(l in 1:length(eji_ppps_spearman_list)){
  
  
  for(i in unique(eji_codebook$module)) {
    
    eji_spearman_module <- eji_ppps_spearman_list[[l]] %>% 
      select(all_of(names(.[which(names(.) %in% eji_codebook$variable_name[which(eji_codebook$module == i)])])),
             any_of(c("n", "n_ppps", "perc_area_pfas_0"))) %>%
      select(any_of(colname_order), any_of(c("n", "n_ppps", "perc_area_pfas_0")))
    
    cor.matrix <- cor(eji_spearman_module, method = "pearson",
                      use = "complete.obs")
    
    reshape2::melt(get_upper_tri(cor.matrix)) %>%
      ggplot(aes(x=Var1, y=Var2, fill=round(value, 2))) +
      geom_tile() +
      geom_text(aes(label = round(value,2))) +
      scale_fill_gradient2(limit = c(-1,1), breaks = c(-1, -.75 ,-.5, -.25, 0, .25,.5, .75, 1),
                           low = "#29af7f", high =  "#b8de29", mid = "white",
                           name = "Cor value") +
      scale_x_discrete(position = "top") +
      theme(panel.background = element_rect(fill = "white"),
            #axis.text.y = element_text(size=12),
            axis.text.x = element_text(angle = 90, vjust = -0.5)) +
      xlab("")+
      ylab("") + 
      ggtitle(paste0("Spearman correlation for ", i, " module in ", names(eji_ppps_spearman_list[l])))
    
    ggsave(paste0("output/", names(eji_ppps_spearman_list[l]), " ", i,
                  " spearman corr plot.png"), height = 20, width = 20, units = "in")
    
  }
  
}

## to resolve: the n_ppps salvatore approach is more closely correlated with EPL_TRI 
## than the eh 249 approach... hmm....




################################################################################
# 2. VISUALIZE SOME STUFF  ####
################################################################################


ggplot(ppps_perc_df, aes(x = perc_area_pfas)) %>% 
  density_plot() + 
  ggtitle("Distribution of perc_area_pfas for census tracts with a PFAS point source")

ggplot(ppps_perc_df, aes(x = perc_area_pfas_0)) %>% 
  density_plot() + 
  ggtitle("Distribution of perc_area_pfas for all census tracts in EJI")


# cr <- ppps_perc_df %>% 
#   mutate(quantile_area_pfas = cut(perc_area_pfas_0, breaks=c(0, .25, .5, .75, 1.1), 
#                                   include.lowest = TRUE, right = FALSE),
#          quantile_area_pfas = case_when(perc_area_pfas_0 == 0 ~ "0",
#                                         TRUE ~ quantile_area_pfas))
# 
# 
# ggplot(cr, aes(quantile_area_pfas, y = SPL_EJI)) %>% 
#   sina_boxplot()

ggplot(ppps_perc_df, aes(x = perc_area_pfas_0, y = SPL_EJI)) +
  geom_point() + 
  # ggforce::geom_sina(alpha = 0.5, color = "lightgrey", show.legend = TRUE) +
  # geom_boxplot(width = 0.1, guides = FALSE, outlier.shape = NA, size = 1, color = "#3a3838") +
  xlab("% of census tract with a PFAS source") + 
  ylab("Sum of Env, Social, and Health Module Rankings") + 
  theme_bw(base_size = 22) + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "snow2"),
        strip.text = element_text(color = "black", face = "bold", size = 18),
        text = element_text(family = "Arial")
  ) +
  ggtitle("EJI with percent area pfas")




################################################################################
# 3. SUMMARY STATS####
################################################################################




# gtsummary::tbl_summary(ppps_perc_df, 
#                        digits = everything() ~ 2, 
#                        missing = "no",
#                        include = c("RPL_EJI", "RPL_EBM", "RPL_SVM",  "RPL_HVM", "perc_area_pfas_0"),
#                        type = list(c("RPL_EJI", "RPL_EBM", "RPL_SVM",  "RPL_HVM", "perc_area_pfas_0") ~ 'continuous2'),
#                        statistic = list(c("RPL_EJI", "RPL_EBM", "RPL_SVM",  "RPL_HVM", "perc_area_pfas_0") ~ c("{median} ({p25} - {p75})"))) 








# ############################# GARBAGE <3 ######################################
#
# 
# ggplot(eji_w_ppps_eh249, aes(x = as.factor(n_ppps_groups), y = SPL_EJI)) +
#   ggforce::geom_sina(alpha = 0.5, color = "lightgrey", show.legend = TRUE) +
#   geom_boxplot(width = 0.1, guides = FALSE, outlier.shape = NA, size = 1, color = "#3a3838") +
#   xlab("# of Potential PFAS Point Sources") + 
#   ylab("Sum of Env, Social, and Health Module Rankings") + 
#   theme_bw(base_size = 22) + 
#   theme(panel.grid.major.y = element_blank(),
#         panel.grid.major.x = element_line(color = "snow2"),
#         strip.text = element_text(color = "black", face = "bold", size = 18),
#         text = element_text(family = "Arial")
#   ) +
#   ggtitle("EJI with EH249 PFAS Point Sources")
# 
# ggsave("eji census tracts with eh249 ppps boxplots.png", width = 18, height = 10, units = "in")
# 
# ggplot(eji_w_ppps_salvatore, aes(x = as.factor(n_ppps_groups), y = SPL_EJI)) +
#   ggforce::geom_sina(alpha = 0.5, color = "lightgrey", show.legend = TRUE) +
#   geom_boxplot(width = 0.1, guides = FALSE, outlier.shape = NA, size = 1, color = "#3a3838") +
#   xlab("# of Potential PFAS Point Sources") + 
#   ylab("Sum of Env, Social, and Health Module Rankings") + 
#   theme_bw(base_size = 22) + 
#   theme(panel.grid.major.y = element_blank(),
#         panel.grid.major.x = element_line(color = "snow2"),
#         strip.text = element_text(color = "black", face = "bold", size = 18),
#         text = element_text(family = "Arial")
#   ) +
#   ggtitle("EJI with Salvatore subset PFAS Point Sources")
# 
# ggsave("eji census tracts with salvatore ppps boxplots.png", width = 18, height = 10, units = "in")
# 
# 
# 
# 
# eji_w_ppps_eh249 %>% 
#   pivot_longer(names_to = "metric", values_to = "score", c("RPL_EJI", "SPL_EJI", "RPL_EBM", "RPL_HVM", "RPL_SVM")) %>% 
#   mutate(metric = factor(metric, levels = c("RPL_EJI", "SPL_EJI", "RPL_EBM", "RPL_HVM", "RPL_SVM"),
#                          labels = c("EJI Rank", "Sum of all modules", "Env Burden Module",
#                                     "Health Vulnerability Module", "Social Vulnerability Module"))) %>% 
#   ggplot(aes(x = as.factor(n_ppps_groups), y = score)) +
#   ggforce::geom_sina(alpha = 0.5, color = "lightgrey", show.legend = TRUE) +
#   geom_boxplot(width = 0.1, guides = FALSE, outlier.shape = NA, size = 1, color = "#3a3838") +
#   xlab("# of Potential PFAS Point Sources") + 
#   ylab("Sum of Env, Social, and Health Module Rankings") + 
#   facet_wrap(~metric, ncol = 1, scales = "free_y") +
#   theme_bw(base_size = 22) + 
#   theme(panel.grid.major.y = element_blank(),
#         panel.grid.major.x = element_line(color = "snow2"),
#         strip.text = element_text(color = "black", face = "bold", size = 18),
#         text = element_text(family = "Arial")
#   ) +
#   ggtitle("EJI with EH249 PFAS Point Sources")
# 
# ggsave("eji census tracts with eh249 ppps boxplots for diff variables.png", width = 18, height = 20, units = "in")
# 
# eji_w_ppps_salvatore %>% 
#   pivot_longer(names_to = "metric", values_to = "score", SPL_EJI:RPL_HVM) %>% 
#   mutate(metric = factor(metric, levels = c("RPL_EJI", "SPL_EJI", "RPL_EBM", "RPL_HVM", "RPL_SVM"),
#                          labels = c("EJI Rank", "Sum of all modules", "Env Burden Module",
#                                     "Health Vulnerability Module", "Social Vulnerability Module"))) %>% 
#   ggplot(aes(x = as.factor(n_ppps_groups), y = score)) +
#   ggforce::geom_sina(alpha = 0.5, color = "lightgrey", show.legend = TRUE) +
#   geom_boxplot(width = 0.1, guides = FALSE, outlier.shape = NA, size = 1, color = "#3a3838") +
#   xlab("# of Potential PFAS Point Sources") + 
#   ylab("Sum of Env, Social, and Health Module Rankings") + 
#   facet_wrap(~metric, ncol = 1, scales = "free_y") +
#   theme_bw(base_size = 22) + 
#   theme(panel.grid.major.y = element_blank(),
#         panel.grid.major.x = element_line(color = "snow2"),
#         strip.text = element_text(color = "black", face = "bold", size = 18),
#         text = element_text(family = "Arial")
#   ) +
#   ggtitle("EJI with salvatore PFAS Point Sources")
# 
# ggsave("eji census tracts with salvatore ppps boxplots for diff variables.png", width = 18, height = 20, units = "in")



### group by dataset
# 
# 
# ppps_salvatore_count_dataset <- ppps_salvatore_tract_df %>% 
#   group_by(geoid, dataset) %>% 
#   count() %>% 
#   filter(!is.na(geoid))
# 
# ppps_eh249_count_dataset <- ppps_eh249_tract_df %>% 
#   group_by(geoid, dataset) %>% 
#   count() %>% 
#   filter(!is.na(geoid))
# 
# 
# eji_w_ppps_salvatore_dataset <-eji_data_fromCDC %>% 
#   select(1:geoid, contains("EJI")) %>% 
#   full_join(ppps_salvatore_count_dataset)  %>% 
#   distinct() %>% 
#   mutate(dataset = case_when(is.na(dataset) ~ "all ppps",
#                              TRUE ~ dataset)) %>% 
#   pivot_wider(names_from = dataset, values_from = n, values_fill = 0) %>% 
#   pivot_longer(names_to = "dataset", values_to = "n", `all ppps`:superfund_sf) %>% 
#   select(1:geoid, contains("EJI"), dataset, n) %>% 
#   mutate(n_ppps = ifelse(is.na(n), 0, as.numeric(n)),
#          n_ppps_groups = case_when(n_ppps == 0 ~ "0",
#                                    n_ppps == 1 ~ "1",
#                                    n_ppps == 2 ~ "2",
#                                    n_ppps == 3 ~ "3",
#                                    n_ppps == 4 ~ "4",
#                                    n_ppps == 5 ~ "5",
#                                    n_ppps <= 10 ~ "≤10",
#                                    n_ppps <= 20 ~ "≤20",
#                                    n_ppps <= 50 ~ "≤50",
#                                    n_ppps > 50 ~ ">50"
#          ),
#          n_ppps_groups = factor(n_ppps_groups, levels = c("0", "1", "2", "3", "4", "5",
#                                                           "≤10", "≤20", "≤50", ">50")))  %>% 
#   mutate( dataset = factor(dataset, levels = c("all ppps", "airports_sf", "epastewardship_county_sf",
#                                                "facilities_sf_salvatore", "fed_agencies_sf", "fire_sf", "production_sf",
#                                                "spills_sf", "superfund_sf", "wwtp_sf"),
#                            labels = c("All Potential\nPFAS Point Sources", "14 CFR Part\n139 Airports",
#                                       "EPA Stewardship Program\nParticipating Facility",
#                                       "Facilities in Industries\nthat May be Handling PFAS",
#                                       "Federal Agency Locations with\nKnown or Suspected\nPFAS Detections",
#                                       "Fire Training Sites", "Facilities that Manufacture\nor Import PFAS",
#                                       "Known PFAS\nSpills/Release Incidents", 
#                                       "Superfund Sites\nwith PFAS Detections", 
#                                       "Wastewater Treatment Plants")))
# 
# 
# eji_w_ppps_eh249_dataset <-eji_data_fromCDC %>% 
#   select(1:geoid, contains("EJI")) %>% 
#   full_join(ppps_eh249_count_dataset)  %>% 
#   distinct() %>% 
#   mutate(dataset = case_when(is.na(dataset) ~ "all ppps",
#                              TRUE ~ dataset)) %>% 
#   pivot_wider(names_from = dataset, values_from = n, values_fill = 0) %>% 
#   pivot_longer(names_to = "dataset", values_to = "n", `all ppps`:superfund_sf) %>% 
#   select(1:geoid, contains("EJI"), dataset, n) %>% 
#   mutate(n_ppps = ifelse(is.na(n), 0, as.numeric(n)),
#          n_ppps_groups = case_when(n_ppps == 0 ~ "0",
#                                    n_ppps == 1 ~ "1",
#                                    n_ppps == 2 ~ "2",
#                                    n_ppps == 3 ~ "3",
#                                    n_ppps == 4 ~ "4",
#                                    n_ppps == 5 ~ "5",
#                                    n_ppps <= 10 ~ "≤10",
#                                    n_ppps <= 20 ~ "≤20",
#                                    n_ppps <= 50 ~ "≤50",
#                                    n_ppps <= 100 ~ "≤100",
#                                    n_ppps >100 ~ ">100"
#          ),
#          n_ppps_groups = factor(n_ppps_groups, levels = c("0", "1", "2", "3", "4", "5",
#                                                           "≤10", "≤20", "≤50", "≤100", ">100")))  %>% 
#   mutate( dataset = factor(dataset, levels = c("all ppps", "airports_sf", "epastewardship_county_sf",
#                                                "facilities_sf_eh249", "fed_agencies_sf", "fire_sf", "production_sf",
#                                                "spills_sf", "superfund_sf", "wwtp_sf"),
#                            labels = c("All Potential\nPFAS Point Sources", "14 CFR Part\n139 Airports",
#                                       "EPA Stewardship Program\nParticipating Facility",
#                                       "Facilities in Industries\nthat May be Handling PFAS",
#                                       "Federal Agency Locations with\nKnown or Suspected\nPFAS Detections",
#                                       "Fire Training Sites", "Facilities that Manufacture\nor Import PFAS",
#                                       "Known PFAS\nSpills/Release Incidents", 
#                                       "Superfund Sites\nwith PFAS Detections",
#                                       "Wastewater Treatment Plants")))
# 
# 
# # table(eji_w_ppps_eh249_dataset$dataset, eji_w_ppps_eh249_dataset$n_ppps_groups, useNA = "ifany")
# 
# # all_tracts_together_w_datasets_too <- all_tracts_ppps %>%
# #   mutate(dataset = "all ppps") %>%
# #   bind_rows(all_tracts_ppps_dataset) %>%
# #   mutate( dataset = factor(dataset, levels = c("all ppps", "airports_sf", "epastewardship_county_sf",
# #                                                "facilities_sf", "fed_agencies_sf", "fire_sf", "production_sf",
# #                                                "spills_sf", "superfund_sf"),
# #                            labels = c("All Potential\nPFAS Point Sources", "14 CFR Part\n139 Airports",
# #                                       "EPA Stewardship Program\nParticipating Facility",
# #                                       "Facilities in Industries\nthat May be Handling PFAS",
# #                                       "Federal Agency Locations with\nKnown or Suspected\nPFAS Detections",
# #                                       "Fire Training Sites", "Facilities that Manufacture\nor Import PFAS",
# #                                       "Known PFAS\nSpills/Release Incidents", "Superfund Sites\nwith PFAS Detections")))
# 
# 
# ggplot(eji_w_ppps_eh249_dataset %>% 
#          filter(dataset != "All Potential\nPFAS Point Sources"), aes(x = n_ppps_groups, y = SPL_EJI)) +
#   ggforce::geom_sina(alpha = 0.5, color = "lightgrey") +
#   geom_boxplot(width = 0.1, guides = FALSE, outlier.shape = NA, size = 1, color = "#3a3838") +
#   xlab("# of Potential PFAS Point Sources") + 
#   ylab("Sum of Env, Social, and Health Module Rankings") + 
#   facet_wrap(~dataset, scales = "free_x") + 
#   theme_bw(base_size = 22) + 
#   theme(panel.grid.major.y = element_blank(),
#         panel.grid.major.x = element_line(color = "snow2"),
#         strip.text = element_text(color = "black", face = "bold", size = 18),
#         text = element_text(family = "Arial")
#   ) + 
#   ggtitle("EJI with EH249 PFAS Point Sources")
# 
# ggsave("eji census tracts with eh249 ppps boxplots by dataset.png", width = 18, height = 15, units = "in")
# 
# 
# 
# ggplot(eji_w_ppps_salvatore_dataset%>% 
#          filter(dataset != "All Potential\nPFAS Point Sources"), aes(x = n_ppps_groups, y = SPL_EJI)) +
#   ggforce::geom_sina(alpha = 0.5, color = "lightgrey") +
#   geom_boxplot(width = 0.1, guides = FALSE, outlier.shape = NA, size = 1, color = "#3a3838") +
#   xlab("# of Potential PFAS Point Sources") + 
#   ylab("Sum of Env, Social, and Health Module Rankings") + 
#   facet_wrap(~dataset, scales = "free_x") + 
#   theme_bw(base_size = 22) + 
#   theme(panel.grid.major.y = element_blank(),
#         panel.grid.major.x = element_line(color = "snow2"),
#         strip.text = element_text(color = "black", face = "bold", size = 18),
#         text = element_text(family = "Arial")
#   ) + 
#   ggtitle("EJI with salvatore PFAS Point Sources")
# 
# 
# ggsave("eji census tracts with salvatore ppps boxplots by dataset.png", width = 18, height = 15, units = "in")
# 
# all_tracts_ppps %>% 
# group_by(.) %>% 
#   summarise(sum_ppps = sum(n_ppps))
# 
# gtsummary::tbl_summary(all_tracts_ppps, 
#                        by = n_ppps_groups, 
#                        digits = everything() ~ 2, 
#                        missing = "no",
#                        include = c("RPL_EJI", "RPL_EBM", "RPL_SVM",  "RPL_HVM"),
#                        type = list(c("RPL_EJI", "RPL_EBM", "RPL_SVM",  "RPL_HVM") ~ 'continuous2'),
#                        statistic = list(c("RPL_EJI", "RPL_EBM", "RPL_SVM",  "RPL_HVM") ~ c("{median} ({p25} - {p75})"))) 
# 
# table2 <- all_tracts_ppps %>% 
#   pivot_longer(names_to = "metric", values_to = "RPL", c("RPL_EJI", "RPL_EBM", "RPL_SVM",  "RPL_HVM")) %>% 
#   mutate(metric = factor(metric, levels = c("RPL_EJI", "RPL_EBM", "RPL_SVM",  "RPL_HVM"),
#                          labels = c("EJI Rank", "Environmental Burden Rank", "Social Vulnerability Rank",  "Health Vulnerability Rank"))) %>% 
#   group_by(n_ppps_groups, metric) %>% 
#   summarize(n = n(), 
#             median = median(RPL, na.rm = TRUE), 
#             p25 = quantile(RPL, 0.25, na.rm = TRUE),
#             p75 = quantile(RPL, 0.75, na.rm = TRUE)) %>% 
#   mutate(`Median (IQR)` = paste0(median, " (", p25, " - ", p75, ")")) %>% 
#   select(-c(median:p75)) %>% 
#   pivot_wider(names_from = metric, values_from = `Median (IQR)`)  %>% 
#   flextable()
