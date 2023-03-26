### AUTHOR: AHz
### WRITTEN IN: R version 4.2.2
### PURPOSE: 



################################################################################
# 0. LOAD DEPENDENCIES  ####
################################################################################

source("scripts/0 - setup.R")
load("data/processed/all_ppps_1a_output.RData")
load("data/processed/eji_spatial_1b_output.RData")

options(scipen = 999)

################################################################################
# 1. RECALCULATE EJI  ####
################################################################################

ebm_cols <- eji_codebook_revised$variable_name[which(eji_codebook_revised$module == "EBM" & 
                                                       str_detect(eji_codebook_revised$variable_name, "EPL_"))]


percentilerank_pfas <- ppps_perc_df %>% 
  filter(!is.na(statefp)) %>% 
  #select(1, any_of(c(ebm_cols, "RPL_EBM", "SPL_EBM")), perc_area_pfas_0) %>% 
  mutate(percent_rank_pfas = rank(perc_area_pfas_0, ties.method = "min")/length(perc_area_pfas_0),
         percent_rank_pfas_0 = case_when(perc_area_pfas_0 == 0 ~ 0,
                                         TRUE ~ percent_rank_pfas)) %>%
  mutate(new_EBM_sum = rowSums(select(., any_of(c(ebm_cols, "percent_rank_pfas_0"))), na.rm = TRUE),
         new_EBM_percent_rank = rank(new_EBM_sum, ties.method = "average")/length(new_EBM_sum)) %>% 
         # check to make sure I'm doing this right
         # check_EBM = rowSums(select(., any_of(c(ebm_cols))), na.rm = TRUE),
         # check_EBM_percent_rank = rank(check_EBM, ties.method = "average")/length(check_EBM),
         # it's basically right -- off by like .02 or something... let's call it good enough for now
  mutate(new_EJI_sum = rowSums(select(., any_of(c("new_EBM_percent_rank", "RPL_SVM", "RPL_HVM")))),
         new_EJI_percent_rank = rank(new_EJI_sum, ties.method = "average")/length(new_EJI_sum))


################################################################################
# 2. SUMMARIZE NEW EJI  ####
################################################################################


summary(percentilerank_pfas$percent_rank_pfas_0)
summary(percentilerank_pfas$new_EBM_percent_rank)
summary(percentilerank_pfas$new_EJI_sum)


ggplot(percentilerank_pfas, aes(x = percent_rank_pfas_0)) %>% 
  density_plot() + 
  ggtitle("Distribution of perc_area_pfas for all census tracts in EJI")

ggplot(percentilerank_pfas, aes(x = new_EJI_sum)) %>% 
  density_plot() + 
  ggtitle("Distribution of perc_area_pfas for all census tracts in EJI")

ggplot(percentilerank_pfas, aes(x = SPL_EBM)) %>% 
  density_plot() + 
  ggtitle("Distribution of perc_area_pfas for all census tracts in EJI")


percentilerank_pfas %>% 
  #filter(percent_rank_pfas_0 != 0) %>% 
  ggplot(aes(x = new_EJI_percent_rank, y = RPL_EJI)) + 
  geom_point() + 
  geom_abline(slope = 1, color = "red")


percentilerank_pfas %>% 
  #filter(percent_rank_pfas_0 != 0) %>% 
  ggplot(aes(x = new_EJI_sum, y = SPL_EJI)) + 
  geom_point() + 
  geom_abline(slope = 1, color = "red")


difference_ranks <- percentilerank_pfas %>% 
  mutate(EJI_rank_change = new_EJI_percent_rank-RPL_EJI,
         EJI_sum_change = new_EJI_sum-SPL_EJI)


summary(difference_ranks$EJI_rank_change)
summary(difference_ranks$EJI_sum_change)


ggplot(difference_ranks, aes(x = perc_area_pfas_0, y = EJI_rank_change)) + 
  geom_point() + 
  geom_hline(yintercept = 0, color = "red")


################################################################################
# 3. IDK MAN ####
################################################################################
