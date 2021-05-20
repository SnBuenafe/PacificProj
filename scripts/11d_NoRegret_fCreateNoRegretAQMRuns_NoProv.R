# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.

# This function creates no-regret closures, requiring the ff. inputs:
# 1. inpdir: where the prioritizr_run results are found.
# 2. outdir: where to save the results of this function.
# 3. target: e.g. Target100

# Function is found in 11a

source("scripts/11a_NoRegret_fCreateNoRegret.R")

#######################################
## Defining plotting generalities ##
#######################################
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
## Target 100% (0.1 - 1) ##
###############################
NOREGRET_AQM_NoProv_target100_run01 <- fCreateNoRegret(inpdir = "outputs/10_Prioritizr/10b-c_AQMRuns/01_Target100/",
                                                outdir = "outputs/11_NoRegret/11d_AQMRuns_NoProv/",
                                                target_name = "Target100",
                                                pu_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                                outexcel = "excel/no_regret_AQM_NoProv/",
                                                climate_scenario = "noregret")

# Plotting
noregret_target100 <- ggplot() + 
  geom_sf(data = NOREGRET_AQM_NoProv_target100_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Target 100% (0.1 - 1)") +
  theme_bw()
#noregret_target100
###############################
## Target 90% (0.1 - 0.9) ##
###############################
NOREGRET_AQM_NoProv_target90_run01 <- fCreateNoRegret(inpdir = "outputs/10_Prioritizr/10b-c_AQMRuns/02_Target90/",
                                               outdir = "outputs/11_NoRegret/11d_AQMRuns_NoProv/",
                                               target_name = "Target90",
                                               pu_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                               outexcel = "excel/no_regret_AQM_NoProv/",
                                               climate_scenario = "noregret")

# Plotting
noregret_target90 <- ggplot() + 
  geom_sf(data = NOREGRET_AQM_NoProv_target90_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Target 90% (0.1 - 0.9)") +
  theme_bw()
#noregret_target90
###############################
## Target 80% (0.1 - 0.8) ##
###############################
NOREGRET_AQM_NoProv_target80_run01 <- fCreateNoRegret(inpdir = "outputs/10_Prioritizr/10b-c_AQMRuns/03_Target80/",
                                               outdir = "outputs/11_NoRegret/11d_AQMRuns_NoProv/",
                                               target_name = "Target80",
                                               pu_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                               outexcel = "excel/no_regret_AQM_NoProv/",
                                               climate_scenario = "noregret")

# Plotting
noregret_target80 <- ggplot() + 
  geom_sf(data = NOREGRET_AQM_NoProv_target80_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Target 80% (0.1 - 0.8)") +
  theme_bw()
#noregret_target80
###############################
## Target 70% (0.1 - 0.7) ##
###############################
NOREGRET_AQM_NoProv_target70_run01 <- fCreateNoRegret(inpdir = "outputs/10_Prioritizr/10b-c_AQMRuns/04_Target70/",
                                               outdir = "outputs/11_NoRegret/11d_AQMRuns_NoProv/",
                                               target_name = "Target70",
                                               pu_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                               outexcel = "excel/no_regret_AQM_NoProv/",
                                               climate_scenario = "noregret")

# Plotting
noregret_target70 <- ggplot() + 
  geom_sf(data = NOREGRET_AQM_NoProv_target70_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Target 70% (0.1 - 0.7)") +
  theme_bw()
#noregret_target70
###############################
## Target 60% (0.1 - 0.6) ##
###############################
NOREGRET_AQM_NoProv_target60_run01 <- fCreateNoRegret(inpdir = "outputs/10_Prioritizr/10b-c_AQMRuns/05_Target60/",
                                               outdir = "outputs/11_NoRegret/11d_AQMRuns_NoProv/",
                                               target_name = "Target60",
                                               pu_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                               outexcel = "excel/no_regret_AQM_NoProv/",
                                               climate_scenario = "noregret")

# Plotting
noregret_target60 <- ggplot() + 
  geom_sf(data = NOREGRET_AQM_NoProv_target60_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Target 60% (0.1 - 0.6)") +
  theme_bw()
#noregret_target60
###############################
## Target 50% (0.1 - 0.5) ##
###############################
NOREGRET_AQM_NoProv_target50_run01 <- fCreateNoRegret(inpdir = "outputs/10_Prioritizr/10b-c_AQMRuns/06_Target50/",
                                               outdir = "outputs/11_NoRegret/11d_AQMRuns_NoProv/",
                                               target_name = "Target50",
                                               pu_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                               outexcel = "excel/no_regret_AQM_NoProv/",
                                               climate_scenario = "noregret")

# Plotting
noregret_target50 <- ggplot() + 
  geom_sf(data = NOREGRET_AQM_NoProv_target50_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Target 50% (0.1 - 0.5)") +
  theme_bw()
#noregret_target50
###############################
## Target 40% (0.1 - 0.4) ##
###############################
NOREGRET_AQM_NoProv_target40_run01 <- fCreateNoRegret(inpdir = "outputs/10_Prioritizr/10b-c_AQMRuns/07_Target40/",
                                               outdir = "outputs/11_NoRegret/11d_AQMRuns_NoProv/",
                                               target_name = "Target40",
                                               pu_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                               outexcel = "excel/no_regret_AQM_NoProv/",
                                               climate_scenario = "noregret")

# Plotting
noregret_target40 <- ggplot() + 
  geom_sf(data = NOREGRET_AQM_NoProv_target40_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Target 40% (0.1 - 0.4)") +
  theme_bw()
#noregret_target40
###############################
## Target 30% (0.1 - 0.3) ##
###############################
NOREGRET_AQM_NoProv_target30_run01 <- fCreateNoRegret(inpdir = "outputs/10_Prioritizr/10b-c_AQMRuns/08_Target30/",
                                               outdir = "outputs/11_NoRegret/11d_AQMRuns_NoProv/",
                                               target_name = "Target30",
                                               pu_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                               outexcel = "excel/no_regret_AQM_NoProv/",
                                               climate_scenario = "noregret")

# Plotting
noregret_target30 <- ggplot() + 
  geom_sf(data = NOREGRET_AQM_NoProv_target30_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Target 30% (0.1 - 0.3)") +
  theme_bw()
#noregret_target30
###############################
## Target 20% (0.1 - 0.2) ##
###############################
NOREGRET_AQM_NoProv_target20_run01 <- fCreateNoRegret(inpdir = "outputs/10_Prioritizr/10b-c_AQMRuns/09_Target20/",
                                               outdir = "outputs/11_NoRegret/11d_AQMRuns_NoProv/",
                                               target_name = "Target20",
                                               pu_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                               outexcel = "excel/no_regret_AQM_NoProv/",
                                               climate_scenario = "noregret")

# Plotting
noregret_target20 <- ggplot() + 
  geom_sf(data = NOREGRET_AQM_NoProv_target20_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Target 20% (0.1 - 0.2)") +
  theme_bw()
#noregret_target20
###############################
## Target 10% (0 - 0.1) ##
###############################
NOREGRET_AQM_NoProv_target10_run01 <- fCreateNoRegret(inpdir = "outputs/10_Prioritizr/10b-c_AQMRuns/10_Target10/",
                                               outdir = "outputs/11_NoRegret/11d_AQMRuns_NoProv/",
                                               target_name = "Target10",
                                               pu_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                               outexcel = "excel/no_regret_AQM_NoProv/",
                                               climate_scenario = "noregret")

# Plotting
noregret_target10 <- ggplot() + 
  geom_sf(data = NOREGRET_AQM_NoProv_target10_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Target 10% (0 - 0.1)") +
  theme_bw()
#noregret_target10
###############################
## Plot Everything ##
###############################
AQM_noregret <- (noregret_target100 + noregret_target90 + noregret_target80 + noregret_target70 + noregret_target60 + noregret_target50 + 
                   noregret_target40 + noregret_target30 + plot_spacer() + noregret_target20 + noregret_target10 + plot_spacer()) +
  plot_layout(ncol = 4, nrow = 3, guides = "collect") +
  plot_annotation(tag_levels = 'i',
                  title = 'No-Regret Closures',
                  subtitle = 'Using AQM Bycatch & Global-fitted Commercial Data')
AQM_noregret
ggsave("pdfs/10_Prioritizr/AQM_NoProv/NoRegret_AQM.pdf", width = 30, height = 21)
