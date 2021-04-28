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

# Plotting generalities
library(RColorBrewer)
library(patchwork)
pal_rich <- c("FALSE" = NA, "TRUE" = "sienna3")
solution <- c("FALSE", "TRUE")

########################################
## Runs using IUCN as bycatch + Plots ##
########################################
###############################
## Target 100% (0 - 1) ##
###############################

PRIORITIZR_IUCN_target100_run01 <- prioritizr_run(cost_file = "outputs/cost_layer/costlayer.rds",
                                        commercial_targetfile = "outputs/final_features/08b_targets/01_Target100/SSP126/target_commercialSSP126.rds",
                                        bycatch_targetfile = "outputs/final_features/08b_targets/01_Target100/SSP126/target_bycatchIUCNSSP126.rds",
                                        commercial_file = "outputs/final_features/07a_25perc/commercialSSP126_25percentile.rds",
                                        bycatch_file = "outputs/final_features/07a_25perc/bycatchIUCNSSP126_25percentile.rds",
                                        climate_scenario = "SSP126",
                                        outdir = "outputs/prioritizr_run/IUCN/01_Target100/")

PRIORITIZR_IUCN_target100_run02 <- prioritizr_run(cost_file = "outputs/cost_layer/costlayer.rds",
                                        commercial_targetfile = "outputs/final_features/08b_targets/01_Target100/SSP245/target_commercialSSP245.rds",
                                        bycatch_targetfile = "outputs/final_features/08b_targets/01_Target100/SSP245/target_bycatchIUCNSSP245.rds",
                                        commercial_file = "outputs/final_features/07a_25perc/commercialSSP245_25percentile.rds",
                                        bycatch_file = "outputs/final_features/07a_25perc/bycatchIUCNSSP245_25percentile.rds",
                                        climate_scenario = "SSP245",
                                        outdir = "outputs/prioritizr_run/IUCN/01_Target100/")

PRIORITIZR_IUCN_target100_run03 <- prioritizr_run(cost_file = "outputs/cost_layer/costlayer.rds",
                                        commercial_targetfile = "outputs/final_features/08b_targets/01_Target100/SSP585/target_commercialSSP585.rds",
                                        bycatch_targetfile = "outputs/final_features/08b_targets/01_Target100/SSP585/target_bycatchIUCNSSP585.rds",
                                        commercial_file = "outputs/final_features/07a_25perc/commercialSSP585_25percentile.rds",
                                        bycatch_file = "outputs/final_features/07a_25perc/bycatchIUCNSSP585_25percentile.rds",
                                        climate_scenario = "SSP585",
                                        outdir = "outputs/prioritizr_run/IUCN/01_Target100/")

# Plotting

target100_iucn1 <- ggplot() + 
  geom_sf(data = PRIORITIZR_IUCN_target100_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 1-2.6") +
  theme_bw()

target100_iucn2 <- ggplot() + 
  geom_sf(data = PRIORITIZR_IUCN_target100_run02, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 2-4.5") +
  theme_bw()

target100_iucn3 <- ggplot() + 
  geom_sf(data = PRIORITIZR_IUCN_target100_run03, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 5-8.5") +
  theme_bw()

target100_iucn <- target100_iucn1 + target100_iucn2 + target100_iucn3
target100_iucn +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(tag_levels = 'i',
                  title = 'Solutions for different scenarios using global-fitted commercial and IUCN bycatch data:',
                  subtitle = 'Maximum Target 100% (0 - 1)')
ggsave("pdfs/solutions/IUCN/Solutions_Target100.pdf")

###############################
## Target 90% (0.1 - 0.9) ##
###############################

PRIORITIZR_IUCN_target90_run01 <- prioritizr_run(cost_file = "outputs/cost_layer/costlayer.rds",
                                        commercial_targetfile = "outputs/final_features/08b_targets/02_Target90/SSP126/target_commercialSSP126.rds",
                                        bycatch_targetfile = "outputs/final_features/08b_targets/02_Target90/SSP126/target_bycatchIUCNSSP126.rds",
                                        commercial_file = "outputs/final_features/07a_25perc/commercialSSP126_25percentile.rds",
                                        bycatch_file = "outputs/final_features/07a_25perc/bycatchIUCNSSP126_25percentile.rds",
                                        climate_scenario = "SSP126",
                                        outdir = "outputs/prioritizr_run/IUCN/02_Target90/")

PRIORITIZR_IUCN_target90_run02 <- prioritizr_run(cost_file = "outputs/cost_layer/costlayer.rds",
                                        commercial_targetfile = "outputs/final_features/08b_targets/02_Target90/SSP245/target_commercialSSP245.rds",
                                        bycatch_targetfile = "outputs/final_features/08b_targets/02_Target90/SSP245/target_bycatchIUCNSSP245.rds",
                                        commercial_file = "outputs/final_features/07a_25perc/commercialSSP245_25percentile.rds",
                                        bycatch_file = "outputs/final_features/07a_25perc/bycatchIUCNSSP245_25percentile.rds",
                                        climate_scenario = "SSP245",
                                        outdir = "outputs/prioritizr_run/IUCN/02_Target90/")

PRIORITIZR_IUCN_target90_run03 <- prioritizr_run(cost_file = "outputs/cost_layer/costlayer.rds",
                                        commercial_targetfile = "outputs/final_features/08b_targets/02_Target90/SSP585/target_commercialSSP585.rds",
                                        bycatch_targetfile = "outputs/final_features/08b_targets/02_Target90/SSP585/target_bycatchIUCNSSP585.rds",
                                        commercial_file = "outputs/final_features/07a_25perc/commercialSSP585_25percentile.rds",
                                        bycatch_file = "outputs/final_features/07a_25perc/bycatchIUCNSSP585_25percentile.rds",
                                        climate_scenario = "SSP585",
                                        outdir = "outputs/prioritizr_run/IUCN/02_Target90/")

# Plotting

target90_iucn1 <- ggplot() + 
  geom_sf(data = PRIORITIZR_IUCN_target90_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 1-2.6") +
  theme_bw()

target90_iucn2 <- ggplot() + 
  geom_sf(data = PRIORITIZR_IUCN_target90_run02, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 2-4.5") +
  theme_bw()

target90_iucn3 <- ggplot() + 
  geom_sf(data = PRIORITIZR_IUCN_target90_run03, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 5-8.5") +
  theme_bw()

target90_iucn <- target90_iucn1 + target90_iucn2 + target90_iucn3
target90_iucn +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(tag_levels = 'i',
                  title = 'Solutions for different scenarios using global-fitted commercial and IUCN bycatch data:',
                  subtitle = 'Maximum Target 90% (0.1 - 0.9)')
ggsave("pdfs/solutions/IUCN/Solutions_Target90.pdf")

###############################
## Target 80% (0.1 - 0.8) ##
###############################

PRIORITIZR_IUCN_target80_run01 <- prioritizr_run(cost_file = "outputs/cost_layer/costlayer.rds",
                                                 commercial_targetfile = "outputs/final_features/08b_targets/03_Target80/SSP126/target_commercialSSP126.rds",
                                                 bycatch_targetfile = "outputs/final_features/08b_targets/03_Target80/SSP126/target_bycatchIUCNSSP126.rds",
                                                 commercial_file = "outputs/final_features/07a_25perc/commercialSSP126_25percentile.rds",
                                                 bycatch_file = "outputs/final_features/07a_25perc/bycatchIUCNSSP126_25percentile.rds",
                                                 climate_scenario = "SSP126",
                                                 outdir = "outputs/prioritizr_run/IUCN/03_Target80/")

PRIORITIZR_IUCN_target80_run02 <- prioritizr_run(cost_file = "outputs/cost_layer/costlayer.rds",
                                                 commercial_targetfile = "outputs/final_features/08b_targets/03_Target80/SSP245/target_commercialSSP245.rds",
                                                 bycatch_targetfile = "outputs/final_features/08b_targets/03_Target80/SSP245/target_bycatchIUCNSSP245.rds",
                                                 commercial_file = "outputs/final_features/07a_25perc/commercialSSP245_25percentile.rds",
                                                 bycatch_file = "outputs/final_features/07a_25perc/bycatchIUCNSSP245_25percentile.rds",
                                                 climate_scenario = "SSP245",
                                                 outdir = "outputs/prioritizr_run/IUCN/03_Target80/")

PRIORITIZR_IUCN_target80_run03 <- prioritizr_run(cost_file = "outputs/cost_layer/costlayer.rds",
                                                 commercial_targetfile = "outputs/final_features/08b_targets/03_Target80/SSP585/target_commercialSSP585.rds",
                                                 bycatch_targetfile = "outputs/final_features/08b_targets/03_Target80/SSP585/target_bycatchIUCNSSP585.rds",
                                                 commercial_file = "outputs/final_features/07a_25perc/commercialSSP585_25percentile.rds",
                                                 bycatch_file = "outputs/final_features/07a_25perc/bycatchIUCNSSP585_25percentile.rds",
                                                 climate_scenario = "SSP585",
                                                 outdir = "outputs/prioritizr_run/IUCN/03_Target80/")

# Plotting

target80_iucn1 <- ggplot() + 
  geom_sf(data = PRIORITIZR_IUCN_target80_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 1-2.6") +
  theme_bw()

target80_iucn2 <- ggplot() + 
  geom_sf(data = PRIORITIZR_IUCN_target80_run02, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 2-4.5") +
  theme_bw()

target80_iucn3 <- ggplot() + 
  geom_sf(data = PRIORITIZR_IUCN_target80_run03, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 5-8.5") +
  theme_bw()

target80_iucn <- target80_iucn1 + target80_iucn2 + target80_iucn3
target80_iucn +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(tag_levels = 'i',
                  title = 'Solutions for different scenarios using global-fitted commercial and IUCN bycatch data:',
                  subtitle = 'Maximum Target 80% (0.1 - 0.8)')
ggsave("pdfs/solutions/IUCN/Solutions_Target80.pdf")

###############################
## Target 70% (0.1 - 0.7) ##
###############################

PRIORITIZR_IUCN_target70_run01 <- prioritizr_run(cost_file = "outputs/cost_layer/costlayer.rds",
                                                 commercial_targetfile = "outputs/final_features/08b_targets/04_Target70/SSP126/target_commercialSSP126.rds",
                                                 bycatch_targetfile = "outputs/final_features/08b_targets/04_Target70/SSP126/target_bycatchIUCNSSP126.rds",
                                                 commercial_file = "outputs/final_features/07a_25perc/commercialSSP126_25percentile.rds",
                                                 bycatch_file = "outputs/final_features/07a_25perc/bycatchIUCNSSP126_25percentile.rds",
                                                 climate_scenario = "SSP126",
                                                 outdir = "outputs/prioritizr_run/IUCN/04_Target70/")

PRIORITIZR_IUCN_target70_run02 <- prioritizr_run(cost_file = "outputs/cost_layer/costlayer.rds",
                                                 commercial_targetfile = "outputs/final_features/08b_targets/04_Target70/SSP245/target_commercialSSP245.rds",
                                                 bycatch_targetfile = "outputs/final_features/08b_targets/04_Target70/SSP245/target_bycatchIUCNSSP245.rds",
                                                 commercial_file = "outputs/final_features/07a_25perc/commercialSSP245_25percentile.rds",
                                                 bycatch_file = "outputs/final_features/07a_25perc/bycatchIUCNSSP245_25percentile.rds",
                                                 climate_scenario = "SSP245",
                                                 outdir = "outputs/prioritizr_run/IUCN/04_Target70/")

PRIORITIZR_IUCN_target70_run03 <- prioritizr_run(cost_file = "outputs/cost_layer/costlayer.rds",
                                                 commercial_targetfile = "outputs/final_features/08b_targets/04_Target70/SSP585/target_commercialSSP585.rds",
                                                 bycatch_targetfile = "outputs/final_features/08b_targets/04_Target70/SSP585/target_bycatchIUCNSSP585.rds",
                                                 commercial_file = "outputs/final_features/07a_25perc/commercialSSP585_25percentile.rds",
                                                 bycatch_file = "outputs/final_features/07a_25perc/bycatchIUCNSSP585_25percentile.rds",
                                                 climate_scenario = "SSP585",
                                                 outdir = "outputs/prioritizr_run/IUCN/04_Target70/")

# Plotting

target70_iucn1 <- ggplot() + 
  geom_sf(data = PRIORITIZR_IUCN_target70_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 1-2.6") +
  theme_bw()

target70_iucn2 <- ggplot() + 
  geom_sf(data = PRIORITIZR_IUCN_target70_run02, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 2-4.5") +
  theme_bw()

target70_iucn3 <- ggplot() + 
  geom_sf(data = PRIORITIZR_IUCN_target70_run03, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 5-8.5") +
  theme_bw()

target70_iucn <- target70_iucn1 + target70_iucn2 + target70_iucn3
target70_iucn +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(tag_levels = 'i',
                  title = 'Solutions for different scenarios using global-fitted commercial and IUCN bycatch data:',
                  subtitle = 'Maximum Target 70% (0.1 - 0.7)')
ggsave("pdfs/solutions/IUCN/Solutions_Target70.pdf")

###############################
## Target 60% (0.1 - 0.6) ##
###############################

PRIORITIZR_IUCN_target60_run01 <- prioritizr_run(cost_file = "outputs/cost_layer/costlayer.rds",
                                                 commercial_targetfile = "outputs/final_features/08b_targets/05_Target60/SSP126/target_commercialSSP126.rds",
                                                 bycatch_targetfile = "outputs/final_features/08b_targets/05_Target60/SSP126/target_bycatchIUCNSSP126.rds",
                                                 commercial_file = "outputs/final_features/07a_25perc/commercialSSP126_25percentile.rds",
                                                 bycatch_file = "outputs/final_features/07a_25perc/bycatchIUCNSSP126_25percentile.rds",
                                                 climate_scenario = "SSP126",
                                                 outdir = "outputs/prioritizr_run/IUCN/05_Target60/")

PRIORITIZR_IUCN_target60_run02 <- prioritizr_run(cost_file = "outputs/cost_layer/costlayer.rds",
                                                 commercial_targetfile = "outputs/final_features/08b_targets/05_Target60/SSP245/target_commercialSSP245.rds",
                                                 bycatch_targetfile = "outputs/final_features/08b_targets/05_Target60/SSP245/target_bycatchIUCNSSP245.rds",
                                                 commercial_file = "outputs/final_features/07a_25perc/commercialSSP245_25percentile.rds",
                                                 bycatch_file = "outputs/final_features/07a_25perc/bycatchIUCNSSP245_25percentile.rds",
                                                 climate_scenario = "SSP245",
                                                 outdir = "outputs/prioritizr_run/IUCN/05_Target60/")

PRIORITIZR_IUCN_target60_run03 <- prioritizr_run(cost_file = "outputs/cost_layer/costlayer.rds",
                                                 commercial_targetfile = "outputs/final_features/08b_targets/05_Target60/SSP585/target_commercialSSP585.rds",
                                                 bycatch_targetfile = "outputs/final_features/08b_targets/05_Target60/SSP585/target_bycatchIUCNSSP585.rds",
                                                 commercial_file = "outputs/final_features/07a_25perc/commercialSSP585_25percentile.rds",
                                                 bycatch_file = "outputs/final_features/07a_25perc/bycatchIUCNSSP585_25percentile.rds",
                                                 climate_scenario = "SSP585",
                                                 outdir = "outputs/prioritizr_run/IUCN/05_Target60/")

# Plotting

target60_iucn1 <- ggplot() + 
  geom_sf(data = PRIORITIZR_IUCN_target60_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 1-2.6") +
  theme_bw()

target60_iucn2 <- ggplot() + 
  geom_sf(data = PRIORITIZR_IUCN_target60_run02, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 2-4.5") +
  theme_bw()

target60_iucn3 <- ggplot() + 
  geom_sf(data = PRIORITIZR_IUCN_target60_run03, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 5-8.5") +
  theme_bw()

target60_iucn <- target60_iucn1 + target60_iucn2 + target60_iucn3
target60_iucn +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(tag_levels = 'i',
                  title = 'Solutions for different scenarios using global-fitted commercial and IUCN bycatch data:',
                  subtitle = 'Maximum Target 60% (0.1 - 0.6)')
ggsave("pdfs/solutions/IUCN/Solutions_Target60.pdf")

###############################
## Target 50% (0.1 - 0.5) ##
###############################

PRIORITIZR_IUCN_target50_run01 <- prioritizr_run(cost_file = "outputs/cost_layer/costlayer.rds",
                                                 commercial_targetfile = "outputs/final_features/08b_targets/06_Target50/SSP126/target_commercialSSP126.rds",
                                                 bycatch_targetfile = "outputs/final_features/08b_targets/06_Target50/SSP126/target_bycatchIUCNSSP126.rds",
                                                 commercial_file = "outputs/final_features/07a_25perc/commercialSSP126_25percentile.rds",
                                                 bycatch_file = "outputs/final_features/07a_25perc/bycatchIUCNSSP126_25percentile.rds",
                                                 climate_scenario = "SSP126",
                                                 outdir = "outputs/prioritizr_run/IUCN/06_Target50/")

PRIORITIZR_IUCN_target50_run02 <- prioritizr_run(cost_file = "outputs/cost_layer/costlayer.rds",
                                                 commercial_targetfile = "outputs/final_features/08b_targets/06_Target50/SSP245/target_commercialSSP245.rds",
                                                 bycatch_targetfile = "outputs/final_features/08b_targets/06_Target50/SSP245/target_bycatchIUCNSSP245.rds",
                                                 commercial_file = "outputs/final_features/07a_25perc/commercialSSP245_25percentile.rds",
                                                 bycatch_file = "outputs/final_features/07a_25perc/bycatchIUCNSSP245_25percentile.rds",
                                                 climate_scenario = "SSP245",
                                                 outdir = "outputs/prioritizr_run/IUCN/06_Target50/")

PRIORITIZR_IUCN_target50_run03 <- prioritizr_run(cost_file = "outputs/cost_layer/costlayer.rds",
                                                 commercial_targetfile = "outputs/final_features/08b_targets/06_Target50/SSP585/target_commercialSSP585.rds",
                                                 bycatch_targetfile = "outputs/final_features/08b_targets/06_Target50/SSP585/target_bycatchIUCNSSP585.rds",
                                                 commercial_file = "outputs/final_features/07a_25perc/commercialSSP585_25percentile.rds",
                                                 bycatch_file = "outputs/final_features/07a_25perc/bycatchIUCNSSP585_25percentile.rds",
                                                 climate_scenario = "SSP585",
                                                 outdir = "outputs/prioritizr_run/IUCN/06_Target50/")

# Plotting

target50_iucn1 <- ggplot() + 
  geom_sf(data = PRIORITIZR_IUCN_target50_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 1-2.6") +
  theme_bw()

target50_iucn2 <- ggplot() + 
  geom_sf(data = PRIORITIZR_IUCN_target50_run02, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 2-4.5") +
  theme_bw()

target50_iucn3 <- ggplot() + 
  geom_sf(data = PRIORITIZR_IUCN_target50_run03, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 5-8.5") +
  theme_bw()

target50_iucn <- target50_iucn1 + target50_iucn2 + target50_iucn3
target50_iucn +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(tag_levels = 'i',
                  title = 'Solutions for different scenarios using global-fitted commercial and IUCN bycatch data:',
                  subtitle = 'Maximum Target 50% (0.1 - 0.5)')
ggsave("pdfs/solutions/IUCN/Solutions_Target50.pdf")

###############################
## Target 40% (0.1 - 0.4) ##
###############################

PRIORITIZR_IUCN_target40_run01 <- prioritizr_run(cost_file = "outputs/cost_layer/costlayer.rds",
                                                 commercial_targetfile = "outputs/final_features/08b_targets/07_Target40/SSP126/target_commercialSSP126.rds",
                                                 bycatch_targetfile = "outputs/final_features/08b_targets/07_Target40/SSP126/target_bycatchIUCNSSP126.rds",
                                                 commercial_file = "outputs/final_features/07a_25perc/commercialSSP126_25percentile.rds",
                                                 bycatch_file = "outputs/final_features/07a_25perc/bycatchIUCNSSP126_25percentile.rds",
                                                 climate_scenario = "SSP126",
                                                 outdir = "outputs/prioritizr_run/IUCN/07_Target40/")

PRIORITIZR_IUCN_target40_run02 <- prioritizr_run(cost_file = "outputs/cost_layer/costlayer.rds",
                                                 commercial_targetfile = "outputs/final_features/08b_targets/07_Target40/SSP245/target_commercialSSP245.rds",
                                                 bycatch_targetfile = "outputs/final_features/08b_targets/07_Target40/SSP245/target_bycatchIUCNSSP245.rds",
                                                 commercial_file = "outputs/final_features/07a_25perc/commercialSSP245_25percentile.rds",
                                                 bycatch_file = "outputs/final_features/07a_25perc/bycatchIUCNSSP245_25percentile.rds",
                                                 climate_scenario = "SSP245",
                                                 outdir = "outputs/prioritizr_run/IUCN/07_Target40/")

PRIORITIZR_IUCN_target40_run03 <- prioritizr_run(cost_file = "outputs/cost_layer/costlayer.rds",
                                                 commercial_targetfile = "outputs/final_features/08b_targets/07_Target40/SSP585/target_commercialSSP585.rds",
                                                 bycatch_targetfile = "outputs/final_features/08b_targets/07_Target40/SSP585/target_bycatchIUCNSSP585.rds",
                                                 commercial_file = "outputs/final_features/07a_25perc/commercialSSP585_25percentile.rds",
                                                 bycatch_file = "outputs/final_features/07a_25perc/bycatchIUCNSSP585_25percentile.rds",
                                                 climate_scenario = "SSP585",
                                                 outdir = "outputs/prioritizr_run/IUCN/07_Target40/")

# Plotting

target40_iucn1 <- ggplot() + 
  geom_sf(data = PRIORITIZR_IUCN_target40_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 1-2.6") +
  theme_bw()

target40_iucn2 <- ggplot() + 
  geom_sf(data = PRIORITIZR_IUCN_target40_run02, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 2-4.5") +
  theme_bw()

target40_iucn3 <- ggplot() + 
  geom_sf(data = PRIORITIZR_IUCN_target40_run03, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 5-8.5") +
  theme_bw()

target40_iucn <- target40_iucn1 + target40_iucn2 + target40_iucn3
target40_iucn +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(tag_levels = 'i',
                  title = 'Solutions for different scenarios using global-fitted commercial and IUCN bycatch data:',
                  subtitle = 'Maximum Target 40% (0.1 - 0.4)')
ggsave("pdfs/solutions/IUCN/Solutions_Target40.pdf")

###############################
## Target 30% (0.1 - 0.3) ##
###############################

PRIORITIZR_IUCN_target30_run01 <- prioritizr_run(cost_file = "outputs/cost_layer/costlayer.rds",
                                                 commercial_targetfile = "outputs/final_features/08b_targets/08_Target30/SSP126/target_commercialSSP126.rds",
                                                 bycatch_targetfile = "outputs/final_features/08b_targets/08_Target30/SSP126/target_bycatchIUCNSSP126.rds",
                                                 commercial_file = "outputs/final_features/07a_25perc/commercialSSP126_25percentile.rds",
                                                 bycatch_file = "outputs/final_features/07a_25perc/bycatchIUCNSSP126_25percentile.rds",
                                                 climate_scenario = "SSP126",
                                                 outdir = "outputs/prioritizr_run/IUCN/08_Target30/")

PRIORITIZR_IUCN_target30_run02 <- prioritizr_run(cost_file = "outputs/cost_layer/costlayer.rds",
                                                 commercial_targetfile = "outputs/final_features/08b_targets/08_Target30/SSP245/target_commercialSSP245.rds",
                                                 bycatch_targetfile = "outputs/final_features/08b_targets/08_Target30/SSP245/target_bycatchIUCNSSP245.rds",
                                                 commercial_file = "outputs/final_features/07a_25perc/commercialSSP245_25percentile.rds",
                                                 bycatch_file = "outputs/final_features/07a_25perc/bycatchIUCNSSP245_25percentile.rds",
                                                 climate_scenario = "SSP245",
                                                 outdir = "outputs/prioritizr_run/IUCN/08_Target30/")

PRIORITIZR_IUCN_target30_run03 <- prioritizr_run(cost_file = "outputs/cost_layer/costlayer.rds",
                                                 commercial_targetfile = "outputs/final_features/08b_targets/08_Target30/SSP585/target_commercialSSP585.rds",
                                                 bycatch_targetfile = "outputs/final_features/08b_targets/08_Target30/SSP585/target_bycatchIUCNSSP585.rds",
                                                 commercial_file = "outputs/final_features/07a_25perc/commercialSSP585_25percentile.rds",
                                                 bycatch_file = "outputs/final_features/07a_25perc/bycatchIUCNSSP585_25percentile.rds",
                                                 climate_scenario = "SSP585",
                                                 outdir = "outputs/prioritizr_run/IUCN/08_Target30/")

# Plotting

target30_iucn1 <- ggplot() + 
  geom_sf(data = PRIORITIZR_IUCN_target30_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 1-2.6") +
  theme_bw()

target30_iucn2 <- ggplot() + 
  geom_sf(data = PRIORITIZR_IUCN_target30_run02, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 2-4.5") +
  theme_bw()

target30_iucn3 <- ggplot() + 
  geom_sf(data = PRIORITIZR_IUCN_target30_run03, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 5-8.5") +
  theme_bw()

target30_iucn <- target30_iucn1 + target30_iucn2 + target30_iucn3
target30_iucn +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(tag_levels = 'i',
                  title = 'Solutions for different scenarios using global-fitted commercial and IUCN bycatch data:',
                  subtitle = 'Maximum Target 30% (0.1 - 0.3)')
ggsave("pdfs/solutions/IUCN/Solutions_Target30.pdf")

###############################
## Target 20% (0.1 - 0.2) ##
###############################

PRIORITIZR_IUCN_target20_run01 <- prioritizr_run(cost_file = "outputs/cost_layer/costlayer.rds",
                                                 commercial_targetfile = "outputs/final_features/08b_targets/09_Target20/SSP126/target_commercialSSP126.rds",
                                                 bycatch_targetfile = "outputs/final_features/08b_targets/09_Target20/SSP126/target_bycatchIUCNSSP126.rds",
                                                 commercial_file = "outputs/final_features/07a_25perc/commercialSSP126_25percentile.rds",
                                                 bycatch_file = "outputs/final_features/07a_25perc/bycatchIUCNSSP126_25percentile.rds",
                                                 climate_scenario = "SSP126",
                                                 outdir = "outputs/prioritizr_run/IUCN/09_Target20/")

PRIORITIZR_IUCN_target20_run02 <- prioritizr_run(cost_file = "outputs/cost_layer/costlayer.rds",
                                                 commercial_targetfile = "outputs/final_features/08b_targets/09_Target20/SSP245/target_commercialSSP245.rds",
                                                 bycatch_targetfile = "outputs/final_features/08b_targets/09_Target20/SSP245/target_bycatchIUCNSSP245.rds",
                                                 commercial_file = "outputs/final_features/07a_25perc/commercialSSP245_25percentile.rds",
                                                 bycatch_file = "outputs/final_features/07a_25perc/bycatchIUCNSSP245_25percentile.rds",
                                                 climate_scenario = "SSP245",
                                                 outdir = "outputs/prioritizr_run/IUCN/09_Target20/")

PRIORITIZR_IUCN_target20_run03 <- prioritizr_run(cost_file = "outputs/cost_layer/costlayer.rds",
                                                 commercial_targetfile = "outputs/final_features/08b_targets/09_Target20/SSP585/target_commercialSSP585.rds",
                                                 bycatch_targetfile = "outputs/final_features/08b_targets/09_Target20/SSP585/target_bycatchIUCNSSP585.rds",
                                                 commercial_file = "outputs/final_features/07a_25perc/commercialSSP585_25percentile.rds",
                                                 bycatch_file = "outputs/final_features/07a_25perc/bycatchIUCNSSP585_25percentile.rds",
                                                 climate_scenario = "SSP585",
                                                 outdir = "outputs/prioritizr_run/IUCN/09_Target20/")

# Plotting

target20_iucn1 <- ggplot() + 
  geom_sf(data = PRIORITIZR_IUCN_target20_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 1-2.6") +
  theme_bw()

target20_iucn2 <- ggplot() + 
  geom_sf(data = PRIORITIZR_IUCN_target20_run02, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 2-4.5") +
  theme_bw()

target20_iucn3 <- ggplot() + 
  geom_sf(data = PRIORITIZR_IUCN_target20_run03, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 5-8.5") +
  theme_bw()

target20_iucn <- target20_iucn1 + target20_iucn2 + target20_iucn3
target20_iucn +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(tag_levels = 'i',
                  title = 'Solutions for different scenarios using global-fitted commercial and IUCN bycatch data:',
                  subtitle = 'Maximum Target 20% (0.1 - 0.2)')
ggsave("pdfs/solutions/IUCN/Solutions_Target20.pdf")

###############################
## Target 10% (0 - 0.1) ##
###############################

PRIORITIZR_IUCN_target10_run01 <- prioritizr_run(cost_file = "outputs/cost_layer/costlayer.rds",
                                                 commercial_targetfile = "outputs/final_features/08b_targets/10_Target10/SSP126/target_commercialSSP126.rds",
                                                 bycatch_targetfile = "outputs/final_features/08b_targets/10_Target10/SSP126/target_bycatchIUCNSSP126.rds",
                                                 commercial_file = "outputs/final_features/07a_25perc/commercialSSP126_25percentile.rds",
                                                 bycatch_file = "outputs/final_features/07a_25perc/bycatchIUCNSSP126_25percentile.rds",
                                                 climate_scenario = "SSP126",
                                                 outdir = "outputs/prioritizr_run/IUCN/10_Target10/")

PRIORITIZR_IUCN_target10_run02 <- prioritizr_run(cost_file = "outputs/cost_layer/costlayer.rds",
                                                 commercial_targetfile = "outputs/final_features/08b_targets/10_Target10/SSP245/target_commercialSSP245.rds",
                                                 bycatch_targetfile = "outputs/final_features/08b_targets/10_Target10/SSP245/target_bycatchIUCNSSP245.rds",
                                                 commercial_file = "outputs/final_features/07a_25perc/commercialSSP245_25percentile.rds",
                                                 bycatch_file = "outputs/final_features/07a_25perc/bycatchIUCNSSP245_25percentile.rds",
                                                 climate_scenario = "SSP245",
                                                 outdir = "outputs/prioritizr_run/IUCN/10_Target10/")

PRIORITIZR_IUCN_target10_run03 <- prioritizr_run(cost_file = "outputs/cost_layer/costlayer.rds",
                                                 commercial_targetfile = "outputs/final_features/08b_targets/10_Target10/SSP585/target_commercialSSP585.rds",
                                                 bycatch_targetfile = "outputs/final_features/08b_targets/10_Target10/SSP585/target_bycatchIUCNSSP585.rds",
                                                 commercial_file = "outputs/final_features/07a_25perc/commercialSSP585_25percentile.rds",
                                                 bycatch_file = "outputs/final_features/07a_25perc/bycatchIUCNSSP585_25percentile.rds",
                                                 climate_scenario = "SSP585",
                                                 outdir = "outputs/prioritizr_run/IUCN/10_Target10/")

# Plotting

target10_iucn1 <- ggplot() + 
  geom_sf(data = PRIORITIZR_IUCN_target10_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 1-2.6") +
  theme_bw()

target10_iucn2 <- ggplot() + 
  geom_sf(data = PRIORITIZR_IUCN_target10_run02, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 2-4.5") +
  theme_bw()

target10_iucn3 <- ggplot() + 
  geom_sf(data = PRIORITIZR_IUCN_target10_run03, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 5-8.5") +
  theme_bw()

target10_iucn <- target10_iucn1 + target10_iucn2 + target10_iucn3
target10_iucn +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(tag_levels = 'i',
                  title = 'Solutions for different scenarios using global-fitted commercial and IUCN bycatch data:',
                  subtitle = 'Maximum Target 10% (0 - 0.1)')
ggsave("pdfs/solutions/IUCN/Solutions_Target10.pdf")
