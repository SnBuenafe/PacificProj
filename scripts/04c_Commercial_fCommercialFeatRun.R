# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.
# most are modified from IBM's code.

# This code creates a function that intersects the predictions / probabilities of spawning areas from GAMs of 4 commercial species:
# Yellowfin Tuna, Albacore, Bigeye Tuna and Swordfish, and the study area (in PUs).
# creates a .rds file PUs x features layer.

# Function is found in 04b.

# Inputs include the following:
# 1. input: species codes: YFT, SKP, ALB, SWO
# 2. inpdir: directory where the layer is found in .csv format
# 3. outdir: directory where to save raster layers (in .rds)
# 4. PU: .rds file
# 5. prob_threshold: median of the predictions; everything else < the prob_threshold is not included in the data.

source("scripts/04b_Commercial_fCommercialFeat.R")

######################################
# Defining generalities for plotting #
######################################
library(patchwork)
library(RColorBrewer)
rob_pacific <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" # Best to define these first so you don't make mistakes below
world_sf <- readRDS('outputs/01_StudyArea/01a_StudyArea/PacificCenterLand.rds')

source("scripts/study_area/fCreateRobinsonBoundary.R")
Bndry <- fCreateRobinsonBoundary(west = 78, east = 140, north = 51, south = 60)

#####################################
# Running for global-fitted models  #
#####################################

# Yellowfin Tuna
PRED_global_YFT <- fCommercialFeat(input = "YFT",
                                   inpdir = "inputs/mercer/yft.csv",
                                   prob_threshold = 0.08098143, #median of predictions of YFT
                                   PU = "outputs/01_StudyArea/01a_StudyArea/PacificABNJGrid_04deg.rds",
                                   outdir = "outputs/04_commercial/04b_fCommercialFeat/")

# Albacore
PRED_global_ALB <- fCommercialFeat(input = "ALB",
                                   inpdir = "inputs/mercer/alba.csv",
                                   prob_threshold = 0.00856913, #median of predictions of ALB
                                   PU = "outputs/01_StudyArea/01a_StudyArea/PacificABNJGrid_04deg.rds",
                                   outdir = "outputs/04_commercial/04b_fCommercialFeat/")

# Skipjack Tuna
PRED_global_SKP <- fCommercialFeat(input = "SKP",
                                   inpdir = "inputs/mercer/skip.csv",
                                   prob_threshold = 0.08989528, #median of predictions of SKP
                                   PU = "outputs/01_StudyArea/01a_StudyArea/PacificABNJGrid_04deg.rds",
                                   outdir = "outputs/04_commercial/04b_fCommercialFeat/")

# Swordfish
PRED_global_SWO <- fCommercialFeat(input = "SWO",
                                   inpdir = "inputs/mercer/sword.csv",
                                   prob_threshold = 0.01611418, #median of predictions of SWO
                                   PU = "outputs/01_StudyArea/01a_StudyArea/PacificABNJGrid_04deg.rds",
                                   outdir = "outputs/04_commercial/04b_fCommercialFeat/")

#####################################
# Plotting globally fitted models  #
#####################################
# YFT
yft <- ggplot()+
  geom_sf(data = PRED_global_YFT, fill = 'paleturquoise3', size = 0.04) +
  geom_sf(data = world_sf, size = 0.04, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Yellowfin Tuna") +
  theme_bw()

# ALB
alb <- ggplot()+
  geom_sf(data = PRED_global_ALB, fill = 'cornflowerblue', size = 0.04) +
  geom_sf(data = world_sf, size = 0.04, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Albacore") +
  theme_bw()

# SKP
skp <- ggplot()+
  geom_sf(data = PRED_global_SKP, fill = 'cadetblue4', size = 0.04) +
  geom_sf(data = world_sf, size = 0.04, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Skipjack Tuna") +
  theme_bw()

# SWO
swo <- ggplot()+
  geom_sf(data = PRED_global_SWO, fill = 'skyblue', size = 0.04) +
  geom_sf(data = world_sf, size = 0.04, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Swordfish") +
  theme_bw()

global_plots <- (yft + alb) / (skp + swo) +
  plot_annotation(tag_levels = "A",
                  tag_suffix = ")",
                  caption = 'spawning ground distribution from the refined GAMs of Mercer (2019) which uses larval abundance from Nishikawa et al. (1985)')
global_plots
ggsave("pdfs/04_Commercial/tuna_spawning.pdf", width = 10, height = 10, dpi = 300)
