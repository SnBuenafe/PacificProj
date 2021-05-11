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

source("scripts/10a_Prioritizr_fPrioritizr.R")

# Plotting generalities
library(RColorBrewer)
library(patchwork)
pal_rich <- c("FALSE" = "lightsteelblue2", "TRUE" = "sienna3")
solution <- c("Not selected PUs", "Selected PUs")
world_sf <- readRDS("outputs/01_StudyArea/01a_StudyArea/PacificCenterLand.rds")

source("scripts/study_area/fCreateRobinsonBoundary.R")
Bndry <- fCreateRobinsonBoundary(west = 78, east = 140, north = 51, south = 60)

#######################################
## Runs using AQM as bycatch + Plots ##
#######################################

###############################
## Target 100% (0 - 1) ##
###############################

PRIORITIZR_AQM_target100_run01 <- fPrioritizrRun(cost_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
      commercial_targetfile = "outputs/09_Target/09b-c_TargetRuns/01_Target100/SSP126/target_commercialSSP126.rds",
      bycatch_targetfile = "outputs/09_Target/09b-c_TargetRuns/01_Target100/SSP126/target_bycatchSSP126.rds",
      commercial_file = "outputs/08_Filter/08b_Filter25/commercialSSP126_25percentile.rds",
      bycatch_file = "outputs/08_Filter/08b_Filter25/bycatchSSP126_25percentile.rds",
      climate_scenario = "SSP126",
      outdir = "outputs/10_Prioritizr/10b-c_AQMRuns/01_Target100/",
      outexcel = "excel/AQM/",
      target_name = "Target100")

PRIORITIZR_AQM_target100_run02 <- fPrioritizrRun(cost_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
      commercial_targetfile = "outputs/09_Target/09b-c_TargetRuns/01_Target100/SSP245/target_commercialSSP245.rds",
      bycatch_targetfile = "outputs/09_Target/09b-c_TargetRuns/01_Target100/SSP245/target_bycatchSSP245.rds",
      commercial_file = "outputs/08_Filter/08b_Filter25/commercialSSP245_25percentile.rds",
      bycatch_file = "outputs/08_Filter/08b_Filter25/bycatchSSP245_25percentile.rds",
      climate_scenario = "SSP245",
      outdir = "outputs/10_Prioritizr/10b-c_AQMRuns/01_Target100/",
      outexcel = "excel/AQM/",
      target_name = "Target100")

PRIORITIZR_AQM_target100_run03 <- fPrioritizrRun(cost_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
      commercial_targetfile = "outputs/09_Target/09b-c_TargetRuns/01_Target100/SSP585/target_commercialSSP585.rds",
      bycatch_targetfile = "outputs/09_Target/09b-c_TargetRuns/01_Target100/SSP585/target_bycatchSSP585.rds",
      commercial_file = "outputs/08_Filter/08b_Filter25/commercialSSP585_25percentile.rds",
      bycatch_file = "outputs/08_Filter/08b_Filter25/bycatchSSP585_25percentile.rds",
      climate_scenario = "SSP585",
      outdir = "outputs/10_Prioritizr/10b-c_AQMRuns/01_Target100/",
      outexcel = "excel/AQM/",
      target_name = "Target100")

# Plotting
target100_plot1 <- ggplot() + 
            geom_sf(data = PRIORITIZR_AQM_target100_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
            scale_fill_manual(name = "Solution",
                              values = pal_rich,
                              labels = solution) +
            geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
            coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
                     ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
                     expand = TRUE) +
            ggtitle("SSP 1-2.6") +
            theme_bw()

target100_plot2 <- ggplot() + 
            geom_sf(data = PRIORITIZR_AQM_target100_run02, aes(fill = solution_1), color = "grey64", size = 0.02) +
            scale_fill_manual(name = "Solution",
                              values = pal_rich,
                              labels = solution) +
            geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
            coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
                     ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
                     expand = TRUE) +
            ggtitle("SSP 2-4.5") +
            theme_bw()

target100_plot3 <- ggplot() + 
            geom_sf(data = PRIORITIZR_AQM_target100_run03, aes(fill = solution_1), color = "grey64", size = 0.02) +
            scale_fill_manual(name = "Solution",
                              values = pal_rich,
                              labels = solution) +
            geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
            coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
                     ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
                     expand = TRUE) +
            ggtitle("SSP 5-8.5") +
            theme_bw()

target100_plots <- target100_plot1 + target100_plot2 + target100_plot3
target100_plots +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(tag_levels = 'i',
                  title = 'Solutions for different scenarios using global-fitted commercial and AQM bycatch data:',
                  subtitle = 'Maximum Target 100% (0.1 - 1)')
ggsave("pdfs/10_Prioritizr/AQM/Solutions_Target100.pdf", width = 21, height = 29.7)

###############################
## Target 90% (0.1 - 0.9) ##
###############################

PRIORITIZR_AQM_target90_run01 <- fPrioritizrRun(cost_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                                 commercial_targetfile = "outputs/09_Target/09b-c_TargetRuns/02_Target90/SSP126/target_commercialSSP126.rds",
                                                 bycatch_targetfile = "outputs/09_Target/09b-c_TargetRuns/02_Target90/SSP126/target_bycatchSSP126.rds",
                                                 commercial_file = "outputs/08_Filter/08b_Filter25/commercialSSP126_25percentile.rds",
                                                 bycatch_file = "outputs/08_Filter/08b_Filter25/bycatchSSP126_25percentile.rds",
                                                 climate_scenario = "SSP126",
                                                 outdir = "outputs/10_Prioritizr/10b-c_AQMRuns/02_Target90/",
                                                outexcel = "excel/AQM/",
                                                target_name = "Target90")

PRIORITIZR_AQM_target90_run02 <- fPrioritizrRun(cost_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                                 commercial_targetfile = "outputs/09_Target/09b-c_TargetRuns/02_Target90/SSP245/target_commercialSSP245.rds",
                                                 bycatch_targetfile = "outputs/09_Target/09b-c_TargetRuns/02_Target90/SSP245/target_bycatchSSP245.rds",
                                                 commercial_file = "outputs/08_Filter/08b_Filter25/commercialSSP245_25percentile.rds",
                                                 bycatch_file = "outputs/08_Filter/08b_Filter25/bycatchSSP245_25percentile.rds",
                                                 climate_scenario = "SSP245",
                                                 outdir = "outputs/10_Prioritizr/10b-c_AQMRuns/02_Target90/",
                                                outexcel = "excel/AQM/",
                                                target_name = "Target90")

PRIORITIZR_AQM_target90_run03 <- fPrioritizrRun(cost_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                                 commercial_targetfile = "outputs/09_Target/09b-c_TargetRuns/02_Target90/SSP585/target_commercialSSP585.rds",
                                                 bycatch_targetfile = "outputs/09_Target/09b-c_TargetRuns/02_Target90/SSP585/target_bycatchSSP585.rds",
                                                 commercial_file = "outputs/08_Filter/08b_Filter25/commercialSSP585_25percentile.rds",
                                                 bycatch_file = "outputs/08_Filter/08b_Filter25/bycatchSSP585_25percentile.rds",
                                                 climate_scenario = "SSP585",
                                                 outdir = "outputs/10_Prioritizr/10b-c_AQMRuns/02_Target90/",
                                                outexcel = "excel/AQM/",
                                                target_name = "Target90")

# Plotting
target90_plot1 <- ggplot() + 
  geom_sf(data = PRIORITIZR_AQM_target90_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 1-2.6") +
  theme_bw()

target90_plot2 <- ggplot() + 
  geom_sf(data = PRIORITIZR_AQM_target90_run02, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 2-4.5") +
  theme_bw()

target90_plot3 <- ggplot() + 
  geom_sf(data = PRIORITIZR_AQM_target90_run03, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 5-8.5") +
  theme_bw()

target90_plots <- target90_plot1 + target90_plot2 + target90_plot3
target90_plots +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(tag_levels = 'i',
                  title = 'Solutions for different scenarios using global-fitted commercial and AQM bycatch data:',
                  subtitle = 'Maximum Target 90% (0.1 - 0.9)')
ggsave("pdfs/10_Prioritizr/AQM/Solutions_Target90.pdf", width = 21, height = 29.7)

###############################
## Target 80% (0.1 - 0.8) ##
###############################

PRIORITIZR_AQM_target80_run01 <- fPrioritizrRun(cost_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                                commercial_targetfile = "outputs/09_Target/09b-c_TargetRuns/03_Target80/SSP126/target_commercialSSP126.rds",
                                                bycatch_targetfile = "outputs/09_Target/09b-c_TargetRuns/03_Target80/SSP126/target_bycatchSSP126.rds",
                                                commercial_file = "outputs/08_Filter/08b_Filter25/commercialSSP126_25percentile.rds",
                                                bycatch_file = "outputs/08_Filter/08b_Filter25/bycatchSSP126_25percentile.rds",
                                                climate_scenario = "SSP126",
                                                outdir = "outputs/10_Prioritizr/10b-c_AQMRuns/03_Target80/",
                                                outexcel = "excel/AQM/",
                                                target_name = "Target80")

PRIORITIZR_AQM_target80_run02 <- fPrioritizrRun(cost_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                                commercial_targetfile = "outputs/09_Target/09b-c_TargetRuns/03_Target80/SSP245/target_commercialSSP245.rds",
                                                bycatch_targetfile = "outputs/09_Target/09b-c_TargetRuns/03_Target80/SSP245/target_bycatchSSP245.rds",
                                                commercial_file = "outputs/08_Filter/08b_Filter25/commercialSSP245_25percentile.rds",
                                                bycatch_file = "outputs/08_Filter/08b_Filter25/bycatchSSP245_25percentile.rds",
                                                climate_scenario = "SSP245",
                                                outdir = "outputs/10_Prioritizr/10b-c_AQMRuns/03_Target80/",
                                                outexcel = "excel/AQM/",
                                                target_name = "Target80")

PRIORITIZR_AQM_target80_run03 <- fPrioritizrRun(cost_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                                commercial_targetfile = "outputs/09_Target/09b-c_TargetRuns/03_Target80/SSP585/target_commercialSSP585.rds",
                                                bycatch_targetfile = "outputs/09_Target/09b-c_TargetRuns/03_Target80/SSP585/target_bycatchSSP585.rds",
                                                commercial_file = "outputs/08_Filter/08b_Filter25/commercialSSP585_25percentile.rds",
                                                bycatch_file = "outputs/08_Filter/08b_Filter25/bycatchSSP585_25percentile.rds",
                                                climate_scenario = "SSP585",
                                                outdir = "outputs/10_Prioritizr/10b-c_AQMRuns/03_Target80/",
                                                outexcel = "excel/AQM/",
                                                target_name = "Target80")

# Plotting
target80_plot1 <- ggplot() + 
  geom_sf(data = PRIORITIZR_AQM_target80_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 1-2.6") +
  theme_bw()

target80_plot2 <- ggplot() + 
  geom_sf(data = PRIORITIZR_AQM_target80_run02, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 2-4.5") +
  theme_bw()

target80_plot3 <- ggplot() + 
  geom_sf(data = PRIORITIZR_AQM_target80_run03, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 5-8.5") +
  theme_bw()

target80_plots <- target80_plot1 + target80_plot2 + target80_plot3
target80_plots +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(tag_levels = 'i',
                  title = 'Solutions for different scenarios using global-fitted commercial and AQM bycatch data:',
                  subtitle = 'Maximum Target 80% (0.1 - 0.8)')
ggsave("pdfs/10_Prioritizr/AQM/Solutions_Target80.pdf", width = 21, height = 29.7)

###############################
## Target 70% (0.1 - 0.7) ##
###############################

PRIORITIZR_AQM_target70_run01 <- fPrioritizrRun(cost_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                                commercial_targetfile = "outputs/09_Target/09b-c_TargetRuns/04_Target70/SSP126/target_commercialSSP126.rds",
                                                bycatch_targetfile = "outputs/09_Target/09b-c_TargetRuns/04_Target70/SSP126/target_bycatchSSP126.rds",
                                                commercial_file = "outputs/08_Filter/08b_Filter25/commercialSSP126_25percentile.rds",
                                                bycatch_file = "outputs/08_Filter/08b_Filter25/bycatchSSP126_25percentile.rds",
                                                climate_scenario = "SSP126",
                                                outdir = "outputs/10_Prioritizr/10b-c_AQMRuns/04_Target70/",
                                                outexcel = "excel/AQM/",
                                                target_name = "Target70")

PRIORITIZR_AQM_target70_run02 <- fPrioritizrRun(cost_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                                commercial_targetfile = "outputs/09_Target/09b-c_TargetRuns/04_Target70/SSP245/target_commercialSSP245.rds",
                                                bycatch_targetfile = "outputs/09_Target/09b-c_TargetRuns/04_Target70/SSP245/target_bycatchSSP245.rds",
                                                commercial_file = "outputs/08_Filter/08b_Filter25/commercialSSP245_25percentile.rds",
                                                bycatch_file = "outputs/08_Filter/08b_Filter25/bycatchSSP245_25percentile.rds",
                                                climate_scenario = "SSP245",
                                                outdir = "outputs/10_Prioritizr/10b-c_AQMRuns/04_Target70/",
                                                outexcel = "excel/AQM/",
                                                target_name = "Target70")

PRIORITIZR_AQM_target70_run03 <- fPrioritizrRun(cost_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                                commercial_targetfile = "outputs/09_Target/09b-c_TargetRuns/04_Target70/SSP585/target_commercialSSP585.rds",
                                                bycatch_targetfile = "outputs/09_Target/09b-c_TargetRuns/04_Target70/SSP585/target_bycatchSSP585.rds",
                                                commercial_file = "outputs/08_Filter/08b_Filter25/commercialSSP585_25percentile.rds",
                                                bycatch_file = "outputs/08_Filter/08b_Filter25/bycatchSSP585_25percentile.rds",
                                                climate_scenario = "SSP585",
                                                outdir = "outputs/10_Prioritizr/10b-c_AQMRuns/04_Target70/",
                                                outexcel = "excel/AQM/",
                                                target_name = "Target70")

# Plotting
target70_plot1 <- ggplot() + 
  geom_sf(data = PRIORITIZR_AQM_target70_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 1-2.6") +
  theme_bw()

target70_plot2 <- ggplot() + 
  geom_sf(data = PRIORITIZR_AQM_target70_run02, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 2-4.5") +
  theme_bw()

target70_plot3 <- ggplot() + 
  geom_sf(data = PRIORITIZR_AQM_target70_run03, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 5-8.5") +
  theme_bw()

target70_plots <- target70_plot1 + target70_plot2 + target70_plot3
target70_plots +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(tag_levels = 'i',
                  title = 'Solutions for different scenarios using global-fitted commercial and AQM bycatch data:',
                  subtitle = 'Maximum Target 70% (0.1 - 0.7)')
ggsave("pdfs/10_Prioritizr/AQM/Solutions_Target70.pdf", width = 21, height = 29.7)

###############################
## Target 60% (0.1 - 0.6) ##
###############################

PRIORITIZR_AQM_target60_run01 <- fPrioritizrRun(cost_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                                commercial_targetfile = "outputs/09_Target/09b-c_TargetRuns/05_Target60/SSP126/target_commercialSSP126.rds",
                                                bycatch_targetfile = "outputs/09_Target/09b-c_TargetRuns/05_Target60/SSP126/target_bycatchSSP126.rds",
                                                commercial_file = "outputs/08_Filter/08b_Filter25/commercialSSP126_25percentile.rds",
                                                bycatch_file = "outputs/08_Filter/08b_Filter25/bycatchSSP126_25percentile.rds",
                                                climate_scenario = "SSP126",
                                                outdir = "outputs/10_Prioritizr/10b-c_AQMRuns/05_Target60/",
                                                outexcel = "excel/AQM/",
                                                target_name = "Target60")

PRIORITIZR_AQM_target60_run02 <- fPrioritizrRun(cost_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                                commercial_targetfile = "outputs/09_Target/09b-c_TargetRuns/05_Target60/SSP245/target_commercialSSP245.rds",
                                                bycatch_targetfile = "outputs/09_Target/09b-c_TargetRuns/05_Target60/SSP245/target_bycatchSSP245.rds",
                                                commercial_file = "outputs/08_Filter/08b_Filter25/commercialSSP245_25percentile.rds",
                                                bycatch_file = "outputs/08_Filter/08b_Filter25/bycatchSSP245_25percentile.rds",
                                                climate_scenario = "SSP245",
                                                outdir = "outputs/10_Prioritizr/10b-c_AQMRuns/05_Target60/",
                                                outexcel = "excel/AQM/",
                                                target_name = "Target60")

PRIORITIZR_AQM_target60_run03 <- fPrioritizrRun(cost_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                                commercial_targetfile = "outputs/09_Target/09b-c_TargetRuns/05_Target60/SSP585/target_commercialSSP585.rds",
                                                bycatch_targetfile = "outputs/09_Target/09b-c_TargetRuns/05_Target60/SSP585/target_bycatchSSP585.rds",
                                                commercial_file = "outputs/08_Filter/08b_Filter25/commercialSSP585_25percentile.rds",
                                                bycatch_file = "outputs/08_Filter/08b_Filter25/bycatchSSP585_25percentile.rds",
                                                climate_scenario = "SSP585",
                                                outdir = "outputs/10_Prioritizr/10b-c_AQMRuns/05_Target60/",
                                                outexcel = "excel/AQM/",
                                                target_name = "Target60")

# Plotting
target60_plot1 <- ggplot() + 
  geom_sf(data = PRIORITIZR_AQM_target60_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 1-2.6") +
  theme_bw()

target60_plot2 <- ggplot() + 
  geom_sf(data = PRIORITIZR_AQM_target60_run02, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 2-4.5") +
  theme_bw()

target60_plot3 <- ggplot() + 
  geom_sf(data = PRIORITIZR_AQM_target60_run03, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 5-8.5") +
  theme_bw()

target60_plots <- target60_plot1 + target60_plot2 + target60_plot3
target60_plots +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(tag_levels = 'i',
                  title = 'Solutions for different scenarios using global-fitted commercial and AQM bycatch data:',
                  subtitle = 'Maximum Target 60% (0.1 - 0.6)')
ggsave("pdfs/10_Prioritizr/AQM/Solutions_Target60.pdf", width = 21, height = 29.7)

###############################
## Target 50% (0.1 - 0.5) ##
###############################

PRIORITIZR_AQM_target50_run01 <- fPrioritizrRun(cost_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                                commercial_targetfile = "outputs/09_Target/09b-c_TargetRuns/06_Target50/SSP126/target_commercialSSP126.rds",
                                                bycatch_targetfile = "outputs/09_Target/09b-c_TargetRuns/06_Target50/SSP126/target_bycatchSSP126.rds",
                                                commercial_file = "outputs/08_Filter/08b_Filter25/commercialSSP126_25percentile.rds",
                                                bycatch_file = "outputs/08_Filter/08b_Filter25/bycatchSSP126_25percentile.rds",
                                                climate_scenario = "SSP126",
                                                outdir = "outputs/10_Prioritizr/10b-c_AQMRuns/06_Target50/",
                                                outexcel = "excel/AQM/",
                                                target_name = "Target50")

PRIORITIZR_AQM_target50_run02 <- fPrioritizrRun(cost_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                                commercial_targetfile = "outputs/09_Target/09b-c_TargetRuns/06_Target50/SSP245/target_commercialSSP245.rds",
                                                bycatch_targetfile = "outputs/09_Target/09b-c_TargetRuns/06_Target50/SSP245/target_bycatchSSP245.rds",
                                                commercial_file = "outputs/08_Filter/08b_Filter25/commercialSSP245_25percentile.rds",
                                                bycatch_file = "outputs/08_Filter/08b_Filter25/bycatchSSP245_25percentile.rds",
                                                climate_scenario = "SSP245",
                                                outdir = "outputs/10_Prioritizr/10b-c_AQMRuns/06_Target50/",
                                                outexcel = "excel/AQM/",
                                                target_name = "Target50")

PRIORITIZR_AQM_target50_run03 <- fPrioritizrRun(cost_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                                commercial_targetfile = "outputs/09_Target/09b-c_TargetRuns/06_Target50/SSP585/target_commercialSSP585.rds",
                                                bycatch_targetfile = "outputs/09_Target/09b-c_TargetRuns/06_Target50/SSP585/target_bycatchSSP585.rds",
                                                commercial_file = "outputs/08_Filter/08b_Filter25/commercialSSP585_25percentile.rds",
                                                bycatch_file = "outputs/08_Filter/08b_Filter25/bycatchSSP585_25percentile.rds",
                                                climate_scenario = "SSP585",
                                                outdir = "outputs/10_Prioritizr/10b-c_AQMRuns/06_Target50/",
                                                outexcel = "excel/AQM/",
                                                target_name = "Target50")

# Plotting
target50_plot1 <- ggplot() + 
  geom_sf(data = PRIORITIZR_AQM_target50_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 1-2.6") +
  theme_bw()

target50_plot2 <- ggplot() + 
  geom_sf(data = PRIORITIZR_AQM_target50_run02, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 2-4.5") +
  theme_bw()

target50_plot3 <- ggplot() + 
  geom_sf(data = PRIORITIZR_AQM_target50_run03, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 5-8.5") +
  theme_bw()

target50_plots <- target50_plot1 + target50_plot2 + target50_plot3
target50_plots +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(tag_levels = 'i',
                  title = 'Solutions for different scenarios using global-fitted commercial and AQM bycatch data:',
                  subtitle = 'Maximum Target 50% (0.1 - 0.5)')
ggsave("pdfs/10_Prioritizr/AQM/Solutions_Target50.pdf", width = 21, height = 29.7)

###############################
## Target 40% (0.1 - 0.4) ##
###############################

PRIORITIZR_AQM_target40_run01 <- fPrioritizrRun(cost_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                                commercial_targetfile = "outputs/09_Target/09b-c_TargetRuns/07_Target40/SSP126/target_commercialSSP126.rds",
                                                bycatch_targetfile = "outputs/09_Target/09b-c_TargetRuns/07_Target40/SSP126/target_bycatchSSP126.rds",
                                                commercial_file = "outputs/08_Filter/08b_Filter25/commercialSSP126_25percentile.rds",
                                                bycatch_file = "outputs/08_Filter/08b_Filter25/bycatchSSP126_25percentile.rds",
                                                climate_scenario = "SSP126",
                                                outdir = "outputs/10_Prioritizr/10b-c_AQMRuns/07_Target40/",
                                                outexcel = "excel/AQM/",
                                                target_name = "Target40")

PRIORITIZR_AQM_target40_run02 <- fPrioritizrRun(cost_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                                commercial_targetfile = "outputs/09_Target/09b-c_TargetRuns/07_Target40/SSP245/target_commercialSSP245.rds",
                                                bycatch_targetfile = "outputs/09_Target/09b-c_TargetRuns/07_Target40/SSP245/target_bycatchSSP245.rds",
                                                commercial_file = "outputs/08_Filter/08b_Filter25/commercialSSP245_25percentile.rds",
                                                bycatch_file = "outputs/08_Filter/08b_Filter25/bycatchSSP245_25percentile.rds",
                                                climate_scenario = "SSP245",
                                                outdir = "outputs/10_Prioritizr/10b-c_AQMRuns/07_Target40/",
                                                outexcel = "excel/AQM/",
                                                target_name = "Target40")

PRIORITIZR_AQM_target40_run03 <- fPrioritizrRun(cost_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                                commercial_targetfile = "outputs/09_Target/09b-c_TargetRuns/07_Target40/SSP585/target_commercialSSP585.rds",
                                                bycatch_targetfile = "outputs/09_Target/09b-c_TargetRuns/07_Target40/SSP585/target_bycatchSSP585.rds",
                                                commercial_file = "outputs/08_Filter/08b_Filter25/commercialSSP585_25percentile.rds",
                                                bycatch_file = "outputs/08_Filter/08b_Filter25/bycatchSSP585_25percentile.rds",
                                                climate_scenario = "SSP585",
                                                outdir = "outputs/10_Prioritizr/10b-c_AQMRuns/07_Target40/",
                                                outexcel = "excel/AQM/",
                                                target_name = "Target40")

# Plotting
target40_plot1 <- ggplot() + 
  geom_sf(data = PRIORITIZR_AQM_target40_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 1-2.6") +
  theme_bw()

target40_plot2 <- ggplot() + 
  geom_sf(data = PRIORITIZR_AQM_target40_run02, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 2-4.5") +
  theme_bw()

target40_plot3 <- ggplot() + 
  geom_sf(data = PRIORITIZR_AQM_target40_run03, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 5-8.5") +
  theme_bw()

target40_plots <- target40_plot1 + target40_plot2 + target40_plot3
target40_plots +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(tag_levels = 'i',
                  title = 'Solutions for different scenarios using global-fitted commercial and AQM bycatch data:',
                  subtitle = 'Maximum Target 40% (0.1 - 0.4)')
ggsave("pdfs/10_Prioritizr/AQM/Solutions_Target40.pdf", width = 21, height = 29.7)

###############################
## Target 30% (0.1 - 0.3) ##
###############################

PRIORITIZR_AQM_target30_run01 <- fPrioritizrRun(cost_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                                commercial_targetfile = "outputs/09_Target/09b-c_TargetRuns/08_Target30/SSP126/target_commercialSSP126.rds",
                                                bycatch_targetfile = "outputs/09_Target/09b-c_TargetRuns/08_Target30/SSP126/target_bycatchSSP126.rds",
                                                commercial_file = "outputs/08_Filter/08b_Filter25/commercialSSP126_25percentile.rds",
                                                bycatch_file = "outputs/08_Filter/08b_Filter25/bycatchSSP126_25percentile.rds",
                                                climate_scenario = "SSP126",
                                                outdir = "outputs/10_Prioritizr/10b-c_AQMRuns/08_Target30/",
                                                outexcel = "excel/AQM/",
                                                target_name = "Target30")

PRIORITIZR_AQM_target30_run02 <- fPrioritizrRun(cost_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                                commercial_targetfile = "outputs/09_Target/09b-c_TargetRuns/08_Target30/SSP245/target_commercialSSP245.rds",
                                                bycatch_targetfile = "outputs/09_Target/09b-c_TargetRuns/08_Target30/SSP245/target_bycatchSSP245.rds",
                                                commercial_file = "outputs/08_Filter/08b_Filter25/commercialSSP245_25percentile.rds",
                                                bycatch_file = "outputs/08_Filter/08b_Filter25/bycatchSSP245_25percentile.rds",
                                                climate_scenario = "SSP245",
                                                outdir = "outputs/10_Prioritizr/10b-c_AQMRuns/08_Target30/",
                                                outexcel = "excel/AQM/",
                                                target_name = "Target30")

PRIORITIZR_AQM_target30_run03 <- fPrioritizrRun(cost_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                                commercial_targetfile = "outputs/09_Target/09b-c_TargetRuns/08_Target30/SSP585/target_commercialSSP585.rds",
                                                bycatch_targetfile = "outputs/09_Target/09b-c_TargetRuns/08_Target30/SSP585/target_bycatchSSP585.rds",
                                                commercial_file = "outputs/08_Filter/08b_Filter25/commercialSSP585_25percentile.rds",
                                                bycatch_file = "outputs/08_Filter/08b_Filter25/bycatchSSP585_25percentile.rds",
                                                climate_scenario = "SSP585",
                                                outdir = "outputs/10_Prioritizr/10b-c_AQMRuns/08_Target30/",
                                                outexcel = "excel/AQM/",
                                                target_name = "Target30")

# Plotting
target30_plot1 <- ggplot() + 
  geom_sf(data = PRIORITIZR_AQM_target30_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 1-2.6") +
  theme_bw()

target30_plot2 <- ggplot() + 
  geom_sf(data = PRIORITIZR_AQM_target30_run02, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 2-4.5") +
  theme_bw()

target30_plot3 <- ggplot() + 
  geom_sf(data = PRIORITIZR_AQM_target30_run03, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 5-8.5") +
  theme_bw()

target30_plots <- target30_plot1 + target30_plot2 + target30_plot3
target30_plots +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(tag_levels = 'i',
                  title = 'Solutions for different scenarios using global-fitted commercial and AQM bycatch data:',
                  subtitle = 'Maximum Target 30% (0.1 - 0.3)')
ggsave("pdfs/10_Prioritizr/AQM/Solutions_Target30.pdf", width = 21, height = 29.7)

###############################
## Target 20% (0.1 - 0.2) ##
###############################

PRIORITIZR_AQM_target20_run01 <- fPrioritizrRun(cost_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                                commercial_targetfile = "outputs/09_Target/09b-c_TargetRuns/09_Target20/SSP126/target_commercialSSP126.rds",
                                                bycatch_targetfile = "outputs/09_Target/09b-c_TargetRuns/09_Target20/SSP126/target_bycatchSSP126.rds",
                                                commercial_file = "outputs/08_Filter/08b_Filter25/commercialSSP126_25percentile.rds",
                                                bycatch_file = "outputs/08_Filter/08b_Filter25/bycatchSSP126_25percentile.rds",
                                                climate_scenario = "SSP126",
                                                outdir = "outputs/10_Prioritizr/10b-c_AQMRuns/09_Target20/",
                                                outexcel = "excel/AQM/",
                                                target_name = "Target20")

PRIORITIZR_AQM_target20_run02 <- fPrioritizrRun(cost_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                                commercial_targetfile = "outputs/09_Target/09b-c_TargetRuns/09_Target20/SSP245/target_commercialSSP245.rds",
                                                bycatch_targetfile = "outputs/09_Target/09b-c_TargetRuns/09_Target20/SSP245/target_bycatchSSP245.rds",
                                                commercial_file = "outputs/08_Filter/08b_Filter25/commercialSSP245_25percentile.rds",
                                                bycatch_file = "outputs/08_Filter/08b_Filter25/bycatchSSP245_25percentile.rds",
                                                climate_scenario = "SSP245",
                                                outdir = "outputs/10_Prioritizr/10b-c_AQMRuns/09_Target20/",
                                                outexcel = "excel/AQM/",
                                                target_name = "Target20")

PRIORITIZR_AQM_target20_run03 <- fPrioritizrRun(cost_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                                commercial_targetfile = "outputs/09_Target/09b-c_TargetRuns/09_Target20/SSP585/target_commercialSSP585.rds",
                                                bycatch_targetfile = "outputs/09_Target/09b-c_TargetRuns/09_Target20/SSP585/target_bycatchSSP585.rds",
                                                commercial_file = "outputs/08_Filter/08b_Filter25/commercialSSP585_25percentile.rds",
                                                bycatch_file = "outputs/08_Filter/08b_Filter25/bycatchSSP585_25percentile.rds",
                                                climate_scenario = "SSP585",
                                                outdir = "outputs/10_Prioritizr/10b-c_AQMRuns/09_Target20/",
                                                outexcel = "excel/AQM/",
                                                target_name = "Target20")

# Plotting
target20_plot1 <- ggplot() + 
  geom_sf(data = PRIORITIZR_AQM_target20_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 1-2.6") +
  theme_bw()

target20_plot2 <- ggplot() + 
  geom_sf(data = PRIORITIZR_AQM_target20_run02, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 2-4.5") +
  theme_bw()

target20_plot3 <- ggplot() + 
  geom_sf(data = PRIORITIZR_AQM_target20_run03, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 5-8.5") +
  theme_bw()

target20_plots <- target20_plot1 + target20_plot2 + target20_plot3
target20_plots +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(tag_levels = 'i',
                  title = 'Solutions for different scenarios using global-fitted commercial and AQM bycatch data:',
                  subtitle = 'Maximum Target 20% (0.1 - 0.2)')
ggsave("pdfs/10_Prioritizr/AQM/Solutions_Target20.pdf", width = 21, height = 29.7)

###############################
## Target 10% (0 - 0.1) ##
###############################

PRIORITIZR_AQM_target10_run01 <- fPrioritizrRun(cost_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                                commercial_targetfile = "outputs/09_Target/09b-c_TargetRuns/10_Target10/SSP126/target_commercialSSP126.rds",
                                                bycatch_targetfile = "outputs/09_Target/09b-c_TargetRuns/10_Target10/SSP126/target_bycatchSSP126.rds",
                                                commercial_file = "outputs/08_Filter/08b_Filter25/commercialSSP126_25percentile.rds",
                                                bycatch_file = "outputs/08_Filter/08b_Filter25/bycatchSSP126_25percentile.rds",
                                                climate_scenario = "SSP126",
                                                outdir = "outputs/10_Prioritizr/10b-c_AQMRuns/10_Target10/",
                                                outexcel = "excel/AQM/",
                                                target_name = "Target10")

PRIORITIZR_AQM_target10_run02 <- fPrioritizrRun(cost_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                                commercial_targetfile = "outputs/09_Target/09b-c_TargetRuns/10_Target10/SSP245/target_commercialSSP245.rds",
                                                bycatch_targetfile = "outputs/09_Target/09b-c_TargetRuns/10_Target10/SSP245/target_bycatchSSP245.rds",
                                                commercial_file = "outputs/08_Filter/08b_Filter25/commercialSSP245_25percentile.rds",
                                                bycatch_file = "outputs/08_Filter/08b_Filter25/bycatchSSP245_25percentile.rds",
                                                climate_scenario = "SSP245",
                                                outdir = "outputs/10_Prioritizr/10b-c_AQMRuns/10_Target10/",
                                                outexcel = "excel/AQM/",
                                                target_name = "Target10")

PRIORITIZR_AQM_target10_run03 <- fPrioritizrRun(cost_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                                commercial_targetfile = "outputs/09_Target/09b-c_TargetRuns/10_Target10/SSP585/target_commercialSSP585.rds",
                                                bycatch_targetfile = "outputs/09_Target/09b-c_TargetRuns/10_Target10/SSP585/target_bycatchSSP585.rds",
                                                commercial_file = "outputs/08_Filter/08b_Filter25/commercialSSP585_25percentile.rds",
                                                bycatch_file = "outputs/08_Filter/08b_Filter25/bycatchSSP585_25percentile.rds",
                                                climate_scenario = "SSP585",
                                                outdir = "outputs/10_Prioritizr/10b-c_AQMRuns/10_Target10/",
                                                outexcel = "excel/AQM/",
                                                target_name = "Target10")

# Plotting
target10_plot1 <- ggplot() + 
  geom_sf(data = PRIORITIZR_AQM_target10_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 1-2.6") +
  theme_bw()

target10_plot2 <- ggplot() + 
  geom_sf(data = PRIORITIZR_AQM_target10_run02, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 2-4.5") +
  theme_bw()

target10_plot3 <- ggplot() + 
  geom_sf(data = PRIORITIZR_AQM_target10_run03, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  ggtitle("SSP 5-8.5") +
  theme_bw()

target10_plots <- target10_plot1 + target10_plot2 + target10_plot3
target10_plots +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(tag_levels = 'i',
                  title = 'Solutions for different scenarios using global-fitted commercial and AQM bycatch data:',
                  subtitle = 'Maximum Target 10% (0 - 0.1)')
ggsave("pdfs/10_Prioritizr/AQM/Solutions_Target10.pdf", width = 21, height = 29.7)
