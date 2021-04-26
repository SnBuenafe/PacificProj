# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.
# Modified from Jase's code.

# This function runs prioritizr and solves the spatial plan problem using the
# Minimum set objective function.
# It requires the following inputs:
# 1. cost_file: PUs with the cellsID and the costs per PU (.rds)
# 2. commercial_targetfile: lists the targets per commercial feature x province (.rds)
# 3. bycatch_targetfile: lists the targets per bycatch feature x province (.rds)
# 4. commercial_file: file containing the commercial features (.rds)
# 5. bycatch_file: file containing the bycatch features (.rds)
# 6. climate_scenario: climate scenario (e.g. SSP126)
# 7. outdir: path where the solution will be saved.

# Function is found at 10a_PrioritizrFxn.R

source("scripts/10a_PrioritizrFxn.R")

# Using AQM as the bycatch

#######################################
## RUNNING FOR EACH CLIMATE SCENARIO ##
#######################################

PRIORITIZR_run01 <- prioritizr_run(cost_file = "outputs/cost_layer/costlayer.rds",
      commercial_targetfile = "outputs/final_features/08b_targets/SSP126/target_commercialSSP126.rds",
      bycatch_targetfile = "outputs/final_features/08b_targets/SSP126/target_bycatchSSP126.rds",
      commercial_file = "outputs/final_features/07a_25perc/commercialSSP126_25percentile.rds",
      bycatch_file = "outputs/final_features/07a_25perc/bycatchSSP126_25percentile.rds",
      climate_scenario = "SSP126",
      outdir = "outputs/prioritizr_run/")

PRIORITIZR_run02 <- prioritizr_run(cost_file = "outputs/cost_layer/costlayer.rds",
      commercial_targetfile = "outputs/final_features/08b_targets/SSP245/target_commercialSSP245.rds",
      bycatch_targetfile = "outputs/final_features/08b_targets/SSP245/target_bycatchSSP245.rds",
      commercial_file = "outputs/final_features/07a_25perc/commercialSSP245_25percentile.rds",
      bycatch_file = "outputs/final_features/07a_25perc/bycatchSSP245_25percentile.rds",
      climate_scenario = "SSP245",
      outdir = "outputs/prioritizr_run/")

PRIORITIZR_run03 <- prioritizr_run(cost_file = "outputs/cost_layer/costlayer.rds",
      commercial_targetfile = "outputs/final_features/08b_targets/SSP585/target_commercialSSP585.rds",
      bycatch_targetfile = "outputs/final_features/08b_targets/SSP585/target_bycatchSSP585.rds",
      commercial_file = "outputs/final_features/07a_25perc/commercialSSP585_25percentile.rds",
      bycatch_file = "outputs/final_features/07a_25perc/bycatchSSP585_25percentile.rds",
      climate_scenario = "SSP585",
      outdir = "outputs/prioritizr_run/")

###############
## PLOTTING  ##
###############

library(RColorBrewer)
library(patchwork)
pal_rich <- c("FALSE" = NA, "TRUE" = "sienna3")
solution <- c("FALSE", "TRUE")

plot1 <- ggplot() + 
            geom_sf(data = PRIORITIZR_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
            scale_fill_manual(name = "Solution",
                              values = pal_rich,
                              labels = solution) +
            geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
            coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
                     ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
                     expand = TRUE) +
            ggtitle("SSP 1-2.6") +
            theme_bw()

plot2 <- ggplot() + 
            geom_sf(data = PRIORITIZR_run02, aes(fill = solution_1), color = "grey64", size = 0.02) +
            scale_fill_manual(name = "Solution",
                              values = pal_rich,
                              labels = solution) +
            geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
            coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
                     ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
                     expand = TRUE) +
            ggtitle("SSP 2-4.5") +
            theme_bw()

plot3 <- ggplot() + 
            geom_sf(data = PRIORITIZR_run03, aes(fill = solution_1), color = "grey64", size = 0.02) +
            scale_fill_manual(name = "Solution",
                              values = pal_rich,
                              labels = solution) +
            geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
            coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
                     ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
                     expand = TRUE) +
            ggtitle("SSP 5-8.5") +
            theme_bw()

solution_plots1 <- plot1 + plot2 + plot3 +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(tag_levels = 'i',
                  title = 'Solutions for Different Scenarios:',
                  subtitle = 'Using Global-Fitted Commercial and AquaMaps Bycatch Data')
solution_plots1
ggsave("pdfs/solutions1.pdf")

# Using IUCN as the bycatch

#######################################
## RUNNING FOR EACH CLIMATE SCENARIO ##
#######################################

PRIORITIZR_run04 <- prioritizr_run(cost_file = "outputs/cost_layer/costlayer.rds",
                                   commercial_targetfile = "outputs/final_features/08b_targets/SSP126/target_commercialSSP126.rds",
                                   bycatch_targetfile = "outputs/final_features/08b_targets/SSP126/target_bycatchIUCNSSP126.rds",
                                   commercial_file = "outputs/final_features/07a_25perc/commercialSSP126_25percentile.rds",
                                   bycatch_file = "outputs/final_features/07a_25perc/bycatchIUCNSSP126_25percentile.rds",
                                   climate_scenario = "SSP126",
                                   outdir = "outputs/prioritizr_run/")

PRIORITIZR_run05 <- prioritizr_run(cost_file = "outputs/cost_layer/costlayer.rds",
                                   commercial_targetfile = "outputs/final_features/08b_targets/SSP245/target_commercialSSP245.rds",
                                   bycatch_targetfile = "outputs/final_features/08b_targets/SSP245/target_bycatchIUCNSSP245.rds",
                                   commercial_file = "outputs/final_features/07a_25perc/commercialSSP245_25percentile.rds",
                                   bycatch_file = "outputs/final_features/07a_25perc/bycatchIUCNSSP245_25percentile.rds",
                                   climate_scenario = "SSP245",
                                   outdir = "outputs/prioritizr_run/")

PRIORITIZR_run06 <- prioritizr_run(cost_file = "outputs/cost_layer/costlayer.rds",
                                   commercial_targetfile = "outputs/final_features/08b_targets/SSP585/target_commercialSSP585.rds",
                                   bycatch_targetfile = "outputs/final_features/08b_targets/SSP585/target_bycatchIUCNSSP585.rds",
                                   commercial_file = "outputs/final_features/07a_25perc/commercialSSP585_25percentile.rds",
                                   bycatch_file = "outputs/final_features/07a_25perc/bycatchIUCNSSP585_25percentile.rds",
                                   climate_scenario = "SSP585",
                                   outdir = "outputs/prioritizr_run/")

###############
## PLOTTING  ##
###############

plot4 <- ggplot() + 
  geom_sf(data = PRIORITIZR_run04, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 1-2.6") +
  theme_bw()

plot5 <- ggplot() + 
  geom_sf(data = PRIORITIZR_run05, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 2-4.5") +
  theme_bw()

plot6 <- ggplot() + 
  geom_sf(data = PRIORITIZR_run06, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 5-8.5") +
  theme_bw()

solution_plots2 <- plot4 + plot5 + plot6 +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(tag_levels = 'i',
                  title = 'Solutions for Different Scenarios:',
                  subtitle = 'Using Global-Fitted Commercial and IUCN Bycatch Data')
solution_plots2
ggsave("pdfs/solutions2.pdf")
