### AUTHOR: AHz
### WRITTEN IN: R version 4.2.2
### PURPOSE: 



################################################################################
# 0. LOAD DEPENDENCIES  ####
################################################################################

source("scripts/0 - setup.R")
load("data/processed/all_ppps_1a_output.RData")
load("data/processed/eji_spatial_1b_output.RData")
load("data/processed/all_ppps_WOspatial_1c_output.RData")

options(scipen = 999)

################################################################################
# 1. RECALCULATE EJI  ####
################################################################################

ebm_cols <- eji_codebook_revised$variable_name[which(eji_codebook_revised$module == "EBM" & 
                                                       str_detect(eji_codebook_revised$variable_name, "EPL_"))]

# 
percentilerank_pfas <- ppps_perc_df %>%
  filter(!is.na(statefp)) %>%
  mutate(percent_rank_pfas = rank(perc_area_pfas_0, ties.method = "min")/length(perc_area_pfas_0),
         percent_rank_pfas_0 = case_when(perc_area_pfas_0 == 0 ~ 0,
                                         TRUE ~ percent_rank_pfas)) %>%
  mutate(new_SPL_EBM = rowSums(select(., any_of(c(ebm_cols, "percent_rank_pfas_0"))), na.rm = TRUE),
         new_RPL_EBM = rank(new_SPL_EBM, ties.method = "average")/length(new_SPL_EBM)) %>%
         # check to make sure I'm doing this right
  mutate(check_EBM = rowSums(select(., all_of(c(ebm_cols))), na.rm = TRUE),
         check_EBM_percent_rank = rank(check_EBM, ties.method = "average")/length(check_EBM)) %>% 
         # it's basically right -- off by like .02 or something... let's call it good enough for now
  mutate(new_SPL_EJI = rowSums(select(., all_of(c("new_RPL_EBM", "RPL_SVM", "RPL_HVM")))),
         new_RPL_EJI = rank(new_SPL_EJI, ties.method = "average")/length(new_SPL_EJI))

# percentilerank_pfas %>% 
#   rowwise() %>% 
#   mutate(check = check_EBM-SPL_EBM) %>% 
#   summary(check)

summary(percentilerank_pfas$check_EBM)
summary(percentilerank_pfas$SPL_EBM)

percentilerank_pfas <- ppps_perc_df %>% 
  filter(!is.na(statefp)) %>%
  mutate(percent_rank_pfas = percent_rank(perc_area_pfas_0),) %>%
  mutate(new_SPL_EBM = rowSums(select(., any_of(c(ebm_cols, "percent_rank_pfas_0"))), na.rm = TRUE),
         new_RPL_EBM = percent_rank(new_SPL_EBM)) %>% 
  # check to make sure I'm doing this right
  # check_EBM = rowSums(select(., any_of(c(ebm_cols))), na.rm = TRUE),
  # check_EBM_percent_rank = rank(check_EBM, ties.method = "average")/length(check_EBM),
  # it's basically right -- off by like .02 or something... let's call it good enough for now
  mutate(new_SPL_EJI = rowSums(select(., all_of(c("new_RPL_EBM", "RPL_SVM", "RPL_HVM")))),
         new_RPL_EJI = percent_rank(new_SPL_EJI))



save(percentilerank_pfas,
     file = "data/processed/newEJI_wpfasrank_1e_output.RData")

################################################################################
# 2. SUMMARIZE NEW EJI  ####
################################################################################


summary(percentilerank_pfas$percent_rank_pfas)

ggplot(percentilerank_pfas, aes(x =  percent_rank_pfas)) %>% 
  density_plot() + 
  ggtitle("Distribution of perc_area_pfas for all census tracts in EJI")

ggplot(percentilerank_pfas, aes(x = new_SPL_EJI)) %>% 
  density_plot() + 
  ggtitle("Distribution of EJI score for all census tracts in EJI")

ggplot(percentilerank_pfas, aes(x = SPL_EBM)) %>% 
  density_plot() + 
  ggtitle("Distribution of perc_area_pfas for all census tracts in EJI")


percentilerank_pfas %>% 
  #filter(percent_rank_pfas_0 != 0) %>% 
  ggplot(aes(x = new_RPL_EJI, y = RPL_EJI)) + 
  geom_point() + 
  geom_abline(slope = 1, color = "red")


percentilerank_pfas %>% 
  #filter(percent_rank_pfas_0 != 0) %>% 
  ggplot(aes(x = new_SPL_EJI, y = SPL_EJI)) + 
  geom_point() + 
  geom_abline(slope = 1, color = "red")


difference_ranks <- percentilerank_pfas %>% 
  mutate(EBM_quartile = cut(RPL_EBM, breaks = 4, labels = c("1", "2", "3", "4")), 
         EJI_quartile = cut(RPL_EJI, breaks = 4,  labels = c("1", "2", "3", "4")),
         EBM_rank_change = new_RPL_EBM-RPL_EBM,
         EBM_sum_change = new_SPL_EBM-SPL_EBM,
         EJI_rank_change = new_RPL_EJI-RPL_EJI,
         EJI_sum_change = new_SPL_EJI-SPL_EJI)



summary(difference_ranks$EBM_rank_change)
summary(difference_ranks$EBM_sum_change)

summary(difference_ranks$EJI_rank_change)
summary(difference_ranks$EJI_sum_change)


ggplot(difference_ranks, aes(y = EJI_rank_change, x = EBM_rank_change, 
                             color = perc_area_pfas)) + 
  geom_point() + 
  geom_hline(yintercept = 0, color = "red") + 
  geom_vline(xintercept = 0, color = "red") + 
  scale_color_continuous(low = "pink", high = "palevioletred", na.value = "snow2")


ggplot(difference_ranks, aes(x = EBM_rank_change)) %>% 
  density_plot() + 
  ggtitle("Distribution of EBM rank changes")

ggplot(difference_ranks, aes(x = EJI_rank_change)) %>% 
  density_plot() + 
  ggtitle("Distribution of EJI rank changes")

# ggplot(difference_ranks, aes(x = EBM_sum_change, y = EJI_sum_change)) + 
#   geom_point() + 
#   geom_hline(yintercept = 0, color = "red") + 
#   geom_vline(xintercept = 0, color = "red")

ggplot(difference_ranks, aes(x = EJI_quartile, y = EJI_rank_change)) %>% 
  sina_boxplot()+ 
  geom_hline(yintercept = 0, color = "red") 

ggplot(difference_ranks, aes(x = EBM_quartile, y = percent_rank_pfas_0)) %>% 
  sina_boxplot()


ggplot(difference_ranks, aes(y = EJI_sum_change, x = EBM_sum_change)) + 
  geom_point() + 
  geom_hline(yintercept = 0, color = "red") + 
  geom_vline(xintercept = 0, color = "red") + 
  facet_wrap(~EJI_quartile)

ggplot(difference_ranks, aes(y = EBM_rank_change, x = perc_area_pfas_0)) + 
  geom_point() + 
  geom_hline(yintercept = 0, color = "red") + 
  facet_wrap(~EJI_quartile)

# ggplot(difference_ranks, aes(y = EJI_sum_change, x = perc_area_pfas_0)) + 
#   geom_point() + 
#   geom_hline(yintercept = 0, color = "red") + 
#   facet_wrap(~EJI_quartile)



gtsummary::tbl_summary(difference_ranks, 
            by = c("EJI_quartile"),
            include = c("EBM_rank_change", "EBM_sum_change", "EJI_rank_change",
                        "EJI_sum_change"))



################################################################################
# 3. IDK MAN ####
################################################################################


load("data/processed/ppps_w_buffer_perc_tract_intersecting.RData")


pfas_recalc_spatial <- ppps_perc %>% 
  filter(!is.na(StateAbbr)) %>% 
  left_join(percentilerank_pfas[, c("geoid", "perc_area_pfas_0", "percent_rank_pfas")]) %>% 
  left_join(difference_ranks[, c("geoid", "EBM_rank_change", "EBM_sum_change", "EJI_rank_change", "EJI_sum_change")])

pfas_recalc_spatial %>% 
  ggplot(aes(fill = percent_rank_pfas)) +
  geom_sf(color = NA) +
  scale_fill_continuous(low = "snow2", high =  "mediumpurple4", na.value="white")

beepr::beep(10)
ggsave("output/figures/percent_rank_pfas_map_0.png", height = 20, width = 30)
beepr::beep(10)


pfas_recalc_spatial %>% 
  mutate(perc_area_pfas = ifelse(perc_area_pfas == 0, NA, perc_area_pfas)) %>% 
  ggplot(aes(fill = perc_area_pfas)) +
  geom_sf(color = NA) +
  scale_fill_continuous(low = "lightpink", high =  "maroon4", na.value="snow2")

beepr::beep(10)
ggsave("output/figures/REVISED_percent_area_pfas_0_map.png", height = 20, width = 30)
beepr::beep(10)



pfas_recalc_spatial %>% 
  ggplot(aes(fill = EJI_rank_change)) +
  geom_sf(color = NA) +
  scale_fill_continuous(low = "steelblue4", high =  "springgreen", na.value="snow2")

beepr::beep(10)
ggsave("output/figures/REVISED_EJI_rank_change_map.png", height = 20, width = 30)
beepr::beep(10)
