### AUTHOR: AHz
### WRITTEN IN: R version 4.2.2
### PURPOSE: Script to generate all figures in EH508 thesis presentation



################################################################################
# 0. LOAD DEPENDENCIES  ####
################################################################################

source("scripts/0 - setup.R")
load("data/processed/pfas_mod_formatting.RData")



################################################################################
# 1.  ####
################################################################################


p_load(collapsibleTree) 

# input data must be a nested data frame:
eji_tree_diag <- read_csv("data/raw/EJI/indicators_table.csv")

# Represent this tree:
p <- collapsibleTree(eji_tree_diag, c("Module", "Theme", "Variable"), 
                     root = "Combined EB + SV", collapsed = FALSE, zoomable = TRUE,
                     fill = c(
                       # The root
                       "#3b4994",
                       # Unique regions
                       "#5ac8c8", 
                       "#be64ac", 
                       rep("#ace4e4", 4),
                       rep("#a5add3", 5),
                       rep("#b0d5df", length(unique(eji_tree_diag$Variable[which(eji_tree_diag$Module == "Social Vulnerability")]))), 
                       rep("#dfb0d6",  length(unique(eji_tree_diag$Variable[which(eji_tree_diag$Module == "Environmental Burden")])))
                       
                     ))
p

htmlwidgets::saveWidget(p, file="output/figures/dendrogram_interactive.html")


################################################################################
# 1.  ####
################################################################################



pfasmapPlot <- pfas_map_sf %>% 
  ggplot(aes(fill = perc_area_pfas)) +
  geom_sf(color = NA, show.legend = FALSE) +
  scale_fill_continuous(name = "% of census tract area\nwithin 1-mile of a PFAS source",
                        low = "#dfb0d6", high =  "#be64ac", na.value="#e8e8e8") + 
  theme_bw(base_size = 22) + 
  labs(
    title = "% area within 1-mile of a PFAS source"
  ) +
  bi_theme()

pfasmapPlot
ggsave(paste0("output/figures/PFAS_perc_map_pres",Sys.Date(),".png"), height = 20, width = 30)
beepr::beep(10)

SERmapPlot <- bivar_map_df %>%
  ggplot(aes(fill = `Social/Env percentile rank`)) +
  geom_sf(color = NA, show.legend = FALSE) +
  scale_fill_continuous(name = "Combined EB & SV percentile rank",
                        low = "#e8e8e8", high =  "#5ac8c8", na.value="white") + 
  theme_bw(base_size = 22) + 
  labs(
    title = "Combined Social + Env Burden percentile rank"
  ) +
  bi_theme()

SERmapPlot
ggsave(paste0("output/figures/RPL_SER_map",Sys.Date(),".png"), height = 20, width = 30)
beepr::beep(10)



right_subset <- plot_grid(pfasmapPlot, SERmapPlot, ncol = 1)

right_subset

ggsave(paste0("output/figures/univar_map_inset",Sys.Date(),".png"), height = 20, width = 20)

bivar_map_img <- ggdraw() +
  draw_image("output/figures/bivariate_SER_PFAS_plot_2023-05-07.png") 

right_inset <- ggdraw()+ 
  draw_image("output/figures/univar_map_inset2023-05-07.png")


# ggdraw() + 
#   draw_image("output/figures/bivariate_SER_PFAS_plot2023-05-07.png", scale = 0.75, hjust = 0.5)  + 
#   draw_image("output/figures/univar_map_inset2023-05-07.png", scale = 0.5, hjust = -0.5)
# 
# ggsave(paste0("output/figures/bivar_map_winset2",Sys.Date(),".png"), height = 20, width = 30)
  
map_w_inset <- plot_grid(bivar_map_img, right_inset, rel_widths = c(2, 1))

# map_w_inset <- plot_grid(finalPlot, right_subset, 
#           nrow = 1)

map_w_inset

ggsave(paste0("output/figures/bivar_map_winset",Sys.Date(),".png"), height = 20, width = 30)
beepr::beep(10)


################################################################################
# 2.  ####
################################################################################


sina_boxplot_quart <- pfas_mod_data_format %>% 
  pivot_longer(names_to = "Module", values_to = "Quartile", 
               c("Quartile of Combined EB & SV", "Quartile of EB",
                 "Quartile of SV")) %>% 
  filter(!is.na(Quartile)) 

SER_sinabox <- ggplot(sina_boxplot_quart %>% 
                        filter(Module == "Quartile of Combined EB & SV"), aes(x = Quartile, 
                               y = `% of census tract area within 1-mile of a PFAS source`, 
                               fill = Quartile)) +
  ggforce::geom_sina(alpha = 0.2, size = 2, show.legend = TRUE, color = "lightgrey") +
  geom_boxplot(width = 0.07, guides = FALSE, outlier.shape = NA, 
               size = 1, alpha = 0.5) + 
  scale_fill_manual(values = c("#b0d5df", "#ace4e4", "#5ac8c8", "#5698b9")) + 
  theme_bw(base_size = 22) +
  theme(legend.position = "none") + 
  labs(x = "", y = "") + 
  facet_wrap(~Module, ncol = 1)

SVEB_sinabox <- ggplot(sina_boxplot_quart %>% 
                        filter(Module != "Quartile of Combined EB & SV"), aes(x = Quartile, 
                                                                              y = `% of census tract area within 1-mile of a PFAS source`, 
                                                                              fill = Quartile)) +
  ggforce::geom_sina(alpha = 0.2, size = 2, show.legend = TRUE, color = "lightgrey") +
  geom_boxplot(width = 0.07, guides = FALSE, outlier.shape = NA, 
               size = 1, alpha = 0.5) + 
  scale_fill_manual(values = c("#b0d5df", "#ace4e4", "#5ac8c8", "#5698b9")) + 
  theme_bw(base_size = 22) +
  theme(legend.position = "none") + 
  labs(x = "", y = "") + 
  facet_wrap(~Module, nrow = 1)

sinaplotgrid <- cowplot::plot_grid(SER_sinabox, SVEB_sinabox, ncol=1) 

ggdraw(add_sub(sinaplotgrid, "% of census tract area within 1-mile of a PFAS source", 
               angle = 90, x = 0.01, y = 1.5, size = 22,  vpadding=grid::unit(1, "lines")))

ggsave("output/figures/sina_boxplot_quartiles_plotgrid_labs.png", height = 15, width = 20)


ggdraw(add_sub(sinaplotgrid, "% of census tract area within 1-mile of a PFAS source", 
               angle = 90, x = 0.01, y = 1.8, size = 22,  vpadding=grid::unit(1, "lines")))

ggsave("output/figures/sina_boxplot_quartiles_plotgrid_labs.png", height = 20, width = 20)



# ggplot(sina_boxplot_quart) +
#   # geom_density_ridges(scale = 1) + 
#   # facet_wrap(~Module)
#   stat_density_ridges(aes(y = Quartile, 
#                              x = `% of census tract area within 1-mile of a PFAS source`,
#                              fill =  as.character(stat(quantile)), group = Quartile),
#     geom = "density_ridges_gradient", 
#     calc_ecdf = TRUE,
#     quantiles = 4, quantile_lines = TRUE
#   ) +
#   # ggforce::geom_sina(alpha = 0.5, size = 2, show.legend = TRUE) +
#   scale_fill_manual(values = c("#b0d5df", "#ace4e4", "#5ac8c8", "#5698b9"), 
#                     name = "Quartiles") + 
#   facet_wrap(~Module) 
# 
# 
# ggplot(sina_boxplot_quart) +
#   # geom_density_ridges(scale = 1) + 
#   # facet_wrap(~Module)
#   stat_density_ridges(aes(y = Quartile, 
#                           x = `% of census tract area within 1-mile of a PFAS source`,
#                           fill =  as.character(stat(quantile)), group = Quartile),
#                       geom = "density_ridges_gradient", 
#                       calc_ecdf = TRUE,
#                       quantiles = 4, quantile_lines = TRUE
#   ) +
#   # ggforce::geom_sina(alpha = 0.5, size = 2, show.legend = TRUE) +
#   scale_fill_manual(values = c("#b0d5df", "#ace4e4", "#5ac8c8", "#5698b9"), 
#                     name = "Quartiles") + 
#   facet_wrap(~Module) 


################################################################################
# 2.  FOREST PLOTS FOR MODELS ####
################################################################################


pfas_mod_data_format %>% 
  select(hasPFAS,`Quartile of Combined EB & SV`, 
         `Quartile of SV`, `Quartile of EB`) 


mod2_uv <- glm(hasPFAS ~`Quartile of Combined EB & SV`, 
    data = pfas_mod_data_format, family = binomial()) 

glm_est <- tidy(mod2_quarts_multi, exponentiate = TRUE, conf.int = TRUE) %>% 
  mutate(Model = "Adjusted") %>% 
  bind_rows(tidy(mod2_uv, exponentiate = TRUE, conf.int = TRUE) %>% 
              mutate(Model = "Unadjusted"))  %>% 
  filter(term != "(Intercept)") 

glm_clean <- glm_est%>% 
  separate(term, into = c("Module", "Quartile"), "`(?=Q\\d)") %>% 
  mutate(Module = str_remove(Module, "`Quartile of "),
         label = paste0(round(estimate, 2), "(", round(conf.low, 2), "-", round(conf.high, 2), ")"))

ggplot(glm_clean, aes(x=Module, y=estimate, color=fct_rev(Quartile))) +
  geom_hline(yintercept = 1, linetype = 2, color = "grey50", alpha = 0.5) + 
  geom_point(position=position_dodge(0.5), size=5) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), position=position_dodge(0.5), width=0) +
  geom_text(aes(label = label, group = fct_rev(Quartile)), position=position_dodge(0.5), vjust = -1, 
            color = "black", size = 7) + 
  scale_color_manual(values = c( "#5698b9", "#ace4e4", "#5ac8c8")) + 
  labs(x="Module", y="OR (95% CI)") +
  theme_bw(base_size = 25) + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid = element_blank()) + 
  ylim(0, 15) + 
  guides(color = guide_legend(title = "Quartile", reverse = TRUE)) + 
  facet_grid(fct_rev(Model)~., scales = "free_y", space = "free_y") + 
  coord_flip() + 
  ggtitle("Odds ratios (95% CI) represent the odds of having a PFAS source present (ref = Q1)")

ggsave("output/figures/glm_forest.png", height = 20, width = 20)





glm_themes_est <- tidy(mod3_quarts_themes_multi, exponentiate = TRUE, conf.int = TRUE) %>% 
  filter(term != "(Intercept)") 

glm__themes_clean <- glm_themes_est %>% 
  separate(term, into = c("Theme", "Quartile"), "`(?=Q\\d)") %>% 
  mutate(Theme = str_remove(Theme, "`Quartile of "),
         label = paste0(round(estimate, 2), "(", round(conf.low, 2), "-", round(conf.high, 2), ")"),
         Module = str_extract(Theme, "\\w{2}"))


ggplot(glm__themes_clean, aes(x=factor(fct_rev(Theme)), y=estimate, color=fct_rev(Quartile))) +
  geom_hline(yintercept = 1, linetype = 2, color = "grey50", alpha = 0.5) + 
  geom_point(position=position_dodge(0.5), size=5) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), position=position_dodge(0.5), width=0) +
  geom_text(aes(label = label, group = fct_rev(Quartile)), 
            position=position_dodge(0.5), hjust = -0.25, 
            color = "black", size = 7) + 
  scale_color_manual(values = c( "#5698b9", "#ace4e4", "#5ac8c8")) + 
  labs(x="", y="OR (95% CI)") +
  theme_bw(base_size = 25) + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid = element_blank()) + 
  ylim(0, 10) + 
  guides(color = guide_legend(title = "Quartile", reverse = TRUE)) + 
  facet_wrap(~Module, scales = "free_y", ncol = 1,  strip.position="right") + 
  coord_flip() + 
  ggtitle("Adjusted odds ratios (95% CI) represent the odds of having a PFAS source present (ref = Q1)")


ggsave("output/figures/glm_themes_forest.png", height = 20, width = 25)


# 
#   geom_point(position=position_dodge(0.5), size=3) +
#   # geom_errorbar allows us to plot the 95% confidence limits
#   geom_errorbar(aes(ymin=conf.low, ymax=conf.high), 
#                 position=position_dodge(0.5), width=0, size = 1) +
#   geom_text(aes(label = label, group = Quartile), position=position_dodge(0.5), vjust = -1.5, 
#             color = "black", size = 5) + 
#   # scale_color_brewer allows me to control the colors for plotting the different models
#   scale_color_manual(values = c( "#ace4e4", "#5ac8c8", "#5698b9")) + 
#   labs(x="Module", y="OR (95% CI)") +
#   theme_minimal(base_size = 22) + 
#   geom_hline(yintercept = 1, linetype = 2, color = "grey50", alpha = 0.5) + 
#   theme(panel.grid.major.y = element_blank(),
#         panel.grid = element_blank()) + 
#   # ylim(0, 15) + 
#   # facet_wrap(~Model, scales = "free_y", ncol = 1) + 
#   coord_flip()
# 


mod2_quarts_multi_rural <- glm(hasPFAS ~ `Quartile of SV` + `Quartile of EB`, 
                               data = pfas_mod_data_strat_RU[which(pfas_mod_data_strat_RU$urban_rural == "Rural"),], 
                               family = binomial()) 

mod2_quarts_multi_urban <- glm(hasPFAS ~ `Quartile of SV` + `Quartile of EB`, 
                               data = pfas_mod_data_strat_RU[which(pfas_mod_data_strat_RU$urban_rural == "Urban"),], 
                               family = binomial()) 




glm_est_RU <- tidy(mod2_quarts_multi_rural, exponentiate = TRUE, conf.int = TRUE) %>% 
  mutate(Model = "Rural") %>% 
  bind_rows(tidy(mod2_quarts_multi_urban, exponentiate = TRUE, conf.int = TRUE) %>% 
              mutate(Model = "Urban"))  %>% 
  filter(term != "(Intercept)") 

glm_clean_RU <- glm_est_RU %>% 
  separate(term, into = c("Module", "Quartile"), "`(?=Q\\d)") %>% 
  mutate(Module = str_remove(Module, "`Quartile of "),
         label = paste0(round(estimate, 2), "(", round(conf.low, 2), "-", round(conf.high, 2), ")"))

ggplot(glm_clean_RU, aes(x=Module, y=estimate, color=fct_rev(Quartile))) +
  geom_hline(yintercept = 1, linetype = 2, color = "grey50", alpha = 0.5) + 
  geom_point(position=position_dodge(0.5), size=5) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), position=position_dodge(0.5), width=0) +
  geom_text(aes(label = label, group = fct_rev(Quartile)), position=position_dodge(0.5), vjust = -1, 
            color = "black", size = 7) + 
  scale_color_manual(values = c( "#5698b9", "#ace4e4", "#5ac8c8")) + 
  labs(x="Module", y="OR (95% CI)") +
  theme_bw(base_size = 25) + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid = element_blank()) + 
  ylim(0, 15) + 
  guides(color = guide_legend(title = "Quartile", reverse = TRUE)) + 
  facet_grid(fct_rev(Model)~., scales = "free_y", space = "free_y") + 
  coord_flip() + 
  ggtitle("Odds ratios (95% CI) represent the odds of having a PFAS source present (ref = Q1)")

ggsave("output/figures/glm_forest_RU.png", height = 20, width = 20)

