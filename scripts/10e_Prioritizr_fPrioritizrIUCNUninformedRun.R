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

###########################################################
## Climate-uninformed Runs using IUCN as bycatch + Plots ##
###########################################################

###############################
## Target 100% (0 - 1) ##
###############################
PRIORITIZR_IUCN_target100_uninformed_run01 <- fPrioritizrRun(cost_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                                  commercial_targetfile = "outputs/09_Target/09b-c_TargetRuns/01_Target100/uninformed/target_commercialuninformed.rds",
                                                  bycatch_targetfile = "outputs/09_Target/09b-c_TargetRuns/01_Target100/uninformed/target_bycatchIUCNuninformed.rds",
                                                  commercial_file = "outputs/08_Filter/08c_Filter100/commercialuninformed_100percentile.rds",
                                                  bycatch_file = "outputs/08_Filter/08c_Filter100/bycatchIUCNuninformed_100percentile.rds",
                                                  climate_scenario = "uninformed",
                                                  outdir = "outputs/10_Prioritizr/10d-e_IUCNRuns/01_Target100/",
                                                  outexcel = "excel/IUCN/",
                                                  target_name = "Target100")
# Plotting
uninformed_target100_iucn1 <- ggplot() + 
  geom_sf(data = PRIORITIZR_IUCN_target100_uninformed_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Climate uninformed solution using global-fitted commercial and IUCN bycatch data:",
       subtitle = "Maximum Target 100% (0.1 - 1)") +
  theme_bw()
uninformed_target100_iucn1
ggsave("pdfs/10_Prioritizr/IUCN/Uninformed_Solutions_Target100.pdf", width = 21, height = 29.7)
###############################
## Target 90% (0.1 - 0.9) ##
###############################
PRIORITIZR_IUCN_target90_uninformed_run01 <- fPrioritizrRun(cost_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                                             commercial_targetfile = "outputs/09_Target/09b-c_TargetRuns/02_Target90/uninformed/target_commercialuninformed.rds",
                                                             bycatch_targetfile = "outputs/09_Target/09b-c_TargetRuns/02_Target90/uninformed/target_bycatchIUCNuninformed.rds",
                                                             commercial_file = "outputs/08_Filter/08c_Filter100/commercialuninformed_100percentile.rds",
                                                             bycatch_file = "outputs/08_Filter/08c_Filter100/bycatchIUCNuninformed_100percentile.rds",
                                                             climate_scenario = "uninformed",
                                                             outdir = "outputs/10_Prioritizr/10d-e_IUCNRuns/02_Target90/",
                                                            outexcel = "excel/IUCN/",
                                                            target_name = "Target90")
# Plotting
uninformed_target90_iucn1 <- ggplot() + 
  geom_sf(data = PRIORITIZR_IUCN_target90_uninformed_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Climate uninformed solution using global-fitted commercial and IUCN bycatch data:",
       subtitle = "Maximum Target 90% (0.1 - 0.9)") +
  theme_bw()
uninformed_target90_iucn1
ggsave("pdfs/10_Prioritizr/IUCN/Uninformed_Solutions_Target90.pdf", width = 21, height = 29.7)
###############################
## Target 80% (0.1 - 0.8) ##
###############################
PRIORITIZR_IUCN_target80_uninformed_run01 <- fPrioritizrRun(cost_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                                            commercial_targetfile = "outputs/09_Target/09b-c_TargetRuns/03_Target80/uninformed/target_commercialuninformed.rds",
                                                            bycatch_targetfile = "outputs/09_Target/09b-c_TargetRuns/03_Target80/uninformed/target_bycatchIUCNuninformed.rds",
                                                            commercial_file = "outputs/08_Filter/08c_Filter100/commercialuninformed_100percentile.rds",
                                                            bycatch_file = "outputs/08_Filter/08c_Filter100/bycatchIUCNuninformed_100percentile.rds",
                                                            climate_scenario = "uninformed",
                                                            outdir = "outputs/10_Prioritizr/10d-e_IUCNRuns/03_Target80/",
                                                            outexcel = "excel/IUCN/",
                                                            target_name = "Target80")
# Plotting
uninformed_target80_iucn1 <- ggplot() + 
  geom_sf(data = PRIORITIZR_IUCN_target80_uninformed_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Climate uninformed solution using global-fitted commercial and IUCN bycatch data:",
       subtitle = "Maximum Target 80% (0 .1- 0.8)") +
  theme_bw()
uninformed_target80_iucn1
ggsave("pdfs/10_Prioritizr/IUCN/Uninformed_Solutions_Target80.pdf", width = 21, height = 29.7)
###############################
## Target 70% (0.1 - 0.7) ##
###############################
PRIORITIZR_IUCN_target70_uninformed_run01 <- fPrioritizrRun(cost_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                                            commercial_targetfile = "outputs/09_Target/09b-c_TargetRuns/04_Target70/uninformed/target_commercialuninformed.rds",
                                                            bycatch_targetfile = "outputs/09_Target/09b-c_TargetRuns/04_Target70/uninformed/target_bycatchIUCNuninformed.rds",
                                                            commercial_file = "outputs/08_Filter/08c_Filter100/commercialuninformed_100percentile.rds",
                                                            bycatch_file = "outputs/08_Filter/08c_Filter100/bycatchIUCNuninformed_100percentile.rds",
                                                            climate_scenario = "uninformed",
                                                            outdir = "outputs/10_Prioritizr/10d-e_IUCNRuns/04_Target70/",
                                                            outexcel = "excel/IUCN/",
                                                            target_name = "Target70")
# Plotting
uninformed_target70_iucn1 <- ggplot() + 
  geom_sf(data = PRIORITIZR_IUCN_target70_uninformed_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Climate uninformed solution using global-fitted commercial and IUCN bycatch data:",
       subtitle = "Maximum Target 70% (0.1 - 0.7)") +
  theme_bw()
uninformed_target70_iucn1
ggsave("pdfs/10_Prioritizr/IUCN/Uninformed_Solutions_Target70.pdf", width = 21, height = 29.7)
###############################
## Target 60% (0.1 - 0.6) ##
###############################
PRIORITIZR_IUCN_target60_uninformed_run01 <- fPrioritizrRun(cost_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                                            commercial_targetfile = "outputs/09_Target/09b-c_TargetRuns/05_Target60/uninformed/target_commercialuninformed.rds",
                                                            bycatch_targetfile = "outputs/09_Target/09b-c_TargetRuns/05_Target60/uninformed/target_bycatchIUCNuninformed.rds",
                                                            commercial_file = "outputs/08_Filter/08c_Filter100/commercialuninformed_100percentile.rds",
                                                            bycatch_file = "outputs/08_Filter/08c_Filter100/bycatchIUCNuninformed_100percentile.rds",
                                                            climate_scenario = "uninformed",
                                                            outdir = "outputs/10_Prioritizr/10d-e_IUCNRuns/05_Target60/",
                                                            outexcel = "excel/IUCN/",
                                                            target_name = "Target60")
# Plotting
uninformed_target60_iucn1 <- ggplot() + 
  geom_sf(data = PRIORITIZR_IUCN_target60_uninformed_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Climate uninformed solution using global-fitted commercial and IUCN bycatch data:",
       subtitle = "Maximum Target 60% (0.1 - 0.6)") +
  theme_bw()
uninformed_target60_iucn1
ggsave("pdfs/10_Prioritizr/IUCN/Uninformed_Solutions_Target60.pdf", width = 21, height = 29.7)
###############################
## Target 50% (0.1 - 0.5) ##
###############################
PRIORITIZR_IUCN_target50_uninformed_run01 <- fPrioritizrRun(cost_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                                            commercial_targetfile = "outputs/09_Target/09b-c_TargetRuns/06_Target50/uninformed/target_commercialuninformed.rds",
                                                            bycatch_targetfile = "outputs/09_Target/09b-c_TargetRuns/06_Target50/uninformed/target_bycatchIUCNuninformed.rds",
                                                            commercial_file = "outputs/08_Filter/08c_Filter100/commercialuninformed_100percentile.rds",
                                                            bycatch_file = "outputs/08_Filter/08c_Filter100/bycatchIUCNuninformed_100percentile.rds",
                                                            climate_scenario = "uninformed",
                                                            outdir = "outputs/10_Prioritizr/10d-e_IUCNRuns/06_Target50/",
                                                            outexcel = "excel/IUCN/",
                                                            target_name = "Target50")
# Plotting
uninformed_target50_iucn1 <- ggplot() + 
  geom_sf(data = PRIORITIZR_IUCN_target50_uninformed_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Climate uninformed solution using global-fitted commercial and IUCN bycatch data:",
       subtitle = "Maximum Target 50% (0.1 - 0.5)") +
  theme_bw()
uninformed_target50_iucn1
ggsave("pdfs/10_Prioritizr/IUCN/Uninformed_Solutions_Target50.pdf", width = 21, height = 29.7)
###############################
## Target 40% (0.1 - 0.4) ##
###############################
PRIORITIZR_IUCN_target40_uninformed_run01 <- fPrioritizrRun(cost_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                                            commercial_targetfile = "outputs/09_Target/09b-c_TargetRuns/07_Target40/uninformed/target_commercialuninformed.rds",
                                                            bycatch_targetfile = "outputs/09_Target/09b-c_TargetRuns/07_Target40/uninformed/target_bycatchIUCNuninformed.rds",
                                                            commercial_file = "outputs/08_Filter/08c_Filter100/commercialuninformed_100percentile.rds",
                                                            bycatch_file = "outputs/08_Filter/08c_Filter100/bycatchIUCNuninformed_100percentile.rds",
                                                            climate_scenario = "uninformed",
                                                            outdir = "outputs/10_Prioritizr/10d-e_IUCNRuns/07_Target40/",
                                                            outexcel = "excel/IUCN/",
                                                            target_name = "Target40")
# Plotting
uninformed_target40_iucn1 <- ggplot() + 
  geom_sf(data = PRIORITIZR_IUCN_target40_uninformed_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Climate uninformed solution using global-fitted commercial and IUCN bycatch data:",
       subtitle = "Maximum Target 40% (0.1 - 0.4)") +
  theme_bw()
uninformed_target40_iucn1
ggsave("pdfs/10_Prioritizr/IUCN/Uninformed_Solutions_Target40.pdf", width = 21, height = 29.7)
###############################
## Target 30% (0.1 - 0.3) ##
###############################
PRIORITIZR_IUCN_target30_uninformed_run01 <- fPrioritizrRun(cost_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                                            commercial_targetfile = "outputs/09_Target/09b-c_TargetRuns/08_Target30/uninformed/target_commercialuninformed.rds",
                                                            bycatch_targetfile = "outputs/09_Target/09b-c_TargetRuns/08_Target30/uninformed/target_bycatchIUCNuninformed.rds",
                                                            commercial_file = "outputs/08_Filter/08c_Filter100/commercialuninformed_100percentile.rds",
                                                            bycatch_file = "outputs/08_Filter/08c_Filter100/bycatchIUCNuninformed_100percentile.rds",
                                                            climate_scenario = "uninformed",
                                                            outdir = "outputs/10_Prioritizr/10d-e_IUCNRuns/08_Target30/",
                                                            outexcel = "excel/IUCN/",
                                                            target_name = "Target30")
# Plotting
uninformed_target30_iucn1 <- ggplot() + 
  geom_sf(data = PRIORITIZR_IUCN_target30_uninformed_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Climate uninformed solution using global-fitted commercial and IUCN bycatch data:",
       subtitle = "Maximum Target 30% (0.1 - 0.3)") +
  theme_bw()
uninformed_target30_iucn1
ggsave("pdfs/10_Prioritizr/IUCN/Uninformed_Solutions_Target30.pdf", width = 21, height = 29.7)
###############################
## Target 20% (0.1 - 0.2) ##
###############################
PRIORITIZR_IUCN_target20_uninformed_run01 <- fPrioritizrRun(cost_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                                            commercial_targetfile = "outputs/09_Target/09b-c_TargetRuns/09_Target20/uninformed/target_commercialuninformed.rds",
                                                            bycatch_targetfile = "outputs/09_Target/09b-c_TargetRuns/09_Target20/uninformed/target_bycatchIUCNuninformed.rds",
                                                            commercial_file = "outputs/08_Filter/08c_Filter100/commercialuninformed_100percentile.rds",
                                                            bycatch_file = "outputs/08_Filter/08c_Filter100/bycatchIUCNuninformed_100percentile.rds",
                                                            climate_scenario = "uninformed",
                                                            outdir = "outputs/10_Prioritizr/10d-e_IUCNRuns/09_Target20/",
                                                            outexcel = "excel/IUCN/",
                                                            target_name = "Target20")
# Plotting
uninformed_target20_iucn1 <- ggplot() + 
  geom_sf(data = PRIORITIZR_IUCN_target20_uninformed_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Climate uninformed solution using global-fitted commercial and IUCN bycatch data:",
       subtitle = "Maximum Target 20% (0.1 - 0.2)") +
  theme_bw()
uninformed_target20_iucn1
ggsave("pdfs/10_Prioritizr/IUCN/Uninformed_Solutions_Target20.pdf", width = 21, height = 29.7)
###############################
## Target 10% (0 - 0.1) ##
###############################
PRIORITIZR_IUCN_target10_uninformed_run01 <- fPrioritizrRun(cost_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                                            commercial_targetfile = "outputs/09_Target/09b-c_TargetRuns/10_Target10/uninformed/target_commercialuninformed.rds",
                                                            bycatch_targetfile = "outputs/09_Target/09b-c_TargetRuns/10_Target10/uninformed/target_bycatchIUCNuninformed.rds",
                                                            commercial_file = "outputs/08_Filter/08c_Filter100/commercialuninformed_100percentile.rds",
                                                            bycatch_file = "outputs/08_Filter/08c_Filter100/bycatchIUCNuninformed_100percentile.rds",
                                                            climate_scenario = "uninformed",
                                                            outdir = "outputs/10_Prioritizr/10d-e_IUCNRuns/10_Target10/",
                                                            outexcel = "excel/IUCN/",
                                                            target_name = "Target10")
# Plotting
uninformed_target10_iucn1 <- ggplot() + 
  geom_sf(data = PRIORITIZR_IUCN_target10_uninformed_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Climate uninformed solution using global-fitted commercial and IUCN bycatch data:",
       subtitle = "Maximum Target 10% (0 - 0.1)") +
  theme_bw()
uninformed_target10_iucn1
ggsave("pdfs/10_Prioritizr/IUCN/Uninformed_Solutions_Target10.pdf", width = 21, height = 29.7)
