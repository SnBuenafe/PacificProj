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

########################################
## Runs using IUCN as bycatch + Plots ##
########################################

###############################
## Target 100% (0.1 - 1) ##
###############################
NOREGRET_IUCN_target100_run01 <- fCreateNoRegret(inpdir = "outputs/10_Prioritizr/10d-e_IUCNRuns/01_Target100/",
                                                outdir = "outputs/11_NoRegret/11c_IUCNRuns/",
                                                target = "Target100",
                                                pu_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                                outexcel = "excel/no_regret_IUCN/",
                                                climate_scenario = "noregret")

# Plotting
noregret_IUCNtarget100 <- ggplot() + 
  geom_sf(data = NOREGRET_IUCN_target100_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Target 100% (0.1 - 1)") +
  theme_bw()
#noregret_IUCNtarget100
###############################
## Target 90% (0.1 - 0.9) ##
###############################
NOREGRET_IUCN_target90_run01 <- fCreateNoRegret(inpdir = "outputs/10_Prioritizr/10d-e_IUCNRuns/02_Target90/",
                                                 outdir = "outputs/11_NoRegret/11c_IUCNRuns/",
                                                 target = "Target90",
                                                pu_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                                outexcel = "excel/no_regret_IUCN/",
                                                climate_scenario = "noregret")

# Plotting
noregret_IUCNtarget90 <- ggplot() + 
  geom_sf(data = NOREGRET_IUCN_target90_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Target 90% (0.1 - 0.9)") +
  theme_bw()
#noregret_IUCNtarget90
###############################
## Target 80% (0.1 - 0.8) ##
###############################
NOREGRET_IUCN_target80_run01 <- fCreateNoRegret(inpdir = "outputs/10_Prioritizr/10d-e_IUCNRuns/03_Target80/",
                                                outdir = "outputs/11_NoRegret/11c_IUCNRuns/",
                                                target = "Target80",
                                                pu_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                                outexcel = "excel/no_regret_IUCN/",
                                                climate_scenario = "noregret")

# Plotting
noregret_IUCNtarget80 <- ggplot() + 
  geom_sf(data = NOREGRET_IUCN_target80_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Target 80% (0.1 - 0.8)") +
  theme_bw()
#noregret_IUCNtarget80
###############################
## Target 70% (0.1 - 0.7) ##
###############################
NOREGRET_IUCN_target70_run01 <- fCreateNoRegret(inpdir = "outputs/10_Prioritizr/10d-e_IUCNRuns/04_Target70/",
                                                outdir = "outputs/11_NoRegret/11c_IUCNRuns/",
                                                target = "Target70",
                                                pu_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                                outexcel = "excel/no_regret_IUCN/",
                                                climate_scenario = "noregret")

# Plotting
noregret_IUCNtarget70 <- ggplot() + 
  geom_sf(data = NOREGRET_IUCN_target70_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Target 70% (0.1 - 0.7)") +
  theme_bw()
#noregret_IUCNtarget70
###############################
## Target 60% (0.1 - 0.6) ##
###############################
NOREGRET_IUCN_target60_run01 <- fCreateNoRegret(inpdir = "outputs/10_Prioritizr/10d-e_IUCNRuns/05_Target60/",
                                                outdir = "outputs/11_NoRegret/11c_IUCNRuns/",
                                                target = "Target60",
                                                pu_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                                outexcel = "excel/no_regret_IUCN/",
                                                climate_scenario = "noregret")

# Plotting
noregret_IUCNtarget60 <- ggplot() + 
  geom_sf(data = NOREGRET_IUCN_target60_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Target 60% (0.1 - 0.6)") +
  theme_bw()
#noregret_IUCNtarget60
###############################
## Target 50% (0.1 - 0.5) ##
###############################
NOREGRET_IUCN_target50_run01 <- fCreateNoRegret(inpdir = "outputs/10_Prioritizr/10d-e_IUCNRuns/06_Target50/",
                                                outdir = "outputs/11_NoRegret/11c_IUCNRuns/",
                                                target = "Target50",
                                                pu_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                                outexcel = "excel/no_regret_IUCN/",
                                                climate_scenario = "noregret")

# Plotting
noregret_IUCNtarget50 <- ggplot() + 
  geom_sf(data = NOREGRET_IUCN_target50_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Target 50% (0.1 - 0.5)") +
  theme_bw()
#noregret_IUCNtarget50
###############################
## Target 40% (0.1 - 0.4) ##
###############################
NOREGRET_IUCN_target40_run01 <- fCreateNoRegret(inpdir = "outputs/10_Prioritizr/10d-e_IUCNRuns/07_Target40/",
                                                outdir = "outputs/11_NoRegret/11c_IUCNRuns/",
                                                target = "Target40",
                                                pu_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                                outexcel = "excel/no_regret_IUCN/",
                                                climate_scenario = "noregret")

# Plotting
noregret_IUCNtarget40 <- ggplot() + 
  geom_sf(data = NOREGRET_IUCN_target40_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Target 40% (0.1 - 0.4)") +
  theme_bw()
#noregret_IUCNtarget40
###############################
## Target 30% (0.1 - 0.3) ##
###############################
NOREGRET_IUCN_target30_run01 <- fCreateNoRegret(inpdir = "outputs/10_Prioritizr/10d-e_IUCNRuns/08_Target30/",
                                                outdir = "outputs/11_NoRegret/11c_IUCNRuns/",
                                                target = "Target30",
                                                pu_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                                outexcel = "excel/no_regret_IUCN/",
                                                climate_scenario = "noregret")

# Plotting
noregret_IUCNtarget30 <- ggplot() + 
  geom_sf(data = NOREGRET_IUCN_target30_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Target 30% (0.1 - 0.3)") +
  theme_bw()
#noregret_IUCNtarget30
###############################
## Target 20% (0.1 - 0.2) ##
###############################
NOREGRET_IUCN_target20_run01 <- fCreateNoRegret(inpdir = "outputs/10_Prioritizr/10d-e_IUCNRuns/09_Target20/",
                                                outdir = "outputs/11_NoRegret/11c_IUCNRuns/",
                                                target = "Target20",
                                                pu_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                                outexcel = "excel/no_regret_IUCN/",
                                                climate_scenario = "noregret")

# Plotting
noregret_IUCNtarget20 <- ggplot() + 
  geom_sf(data = NOREGRET_IUCN_target20_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Target 20% (0.1 - 0.2)") +
  theme_bw()
#noregret_IUCNtarget20
###############################
## Target 10% (0 - 0.1) ##
###############################
NOREGRET_IUCN_target10_run01 <- fCreateNoRegret(inpdir = "outputs/10_Prioritizr/10d-e_IUCNRuns/10_Target10/",
                                                outdir = "outputs/11_NoRegret/11c_IUCNRuns/",
                                                target = "Target10",
                                                pu_file = "outputs/06_Cost/Large_Medium/costlayer.rds",
                                                outexcel = "excel/no_regret_IUCN/",
                                                climate_scenario = "noregret")

# Plotting
noregret_IUCNtarget10 <- ggplot() + 
  geom_sf(data = NOREGRET_IUCN_target10_run01, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Target 10% (0 - 0.1)") +
  theme_bw()
#noregret_IUCNtarget10
###############################
## Plot Everything ##
###############################
IUCN_noregret <- (noregret_IUCNtarget100 + noregret_IUCNtarget90 + noregret_IUCNtarget80 + noregret_IUCNtarget70 + noregret_IUCNtarget60 + noregret_IUCNtarget50 + 
                   noregret_IUCNtarget40 + noregret_IUCNtarget30 + plot_spacer() + noregret_IUCNtarget20 + noregret_IUCNtarget10 + plot_spacer()) +
  plot_layout(ncol = 4, nrow = 3, guides = "collect") +
  plot_annotation(tag_levels = 'i',
                  title = 'No-Regret Closures',
                  subtitle = 'Using IUCN Bycatch & Global-fitted Commercial Data')
IUCN_noregret
#ggsave("pdfs/10_Prioritizr/IUCN/NoRegret_IUCN.pdf", width = 30, height = 21)
