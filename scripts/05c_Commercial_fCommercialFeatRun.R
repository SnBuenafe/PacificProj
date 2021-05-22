# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.
# most are modified from IBM's code.

# This code creates a function that intersects the predictions / probabilities of spawning areas from GAMs of 4 commercial species:
# Yellowfin Tuna, Albacore, Bigeye Tuna and Swordfish, and the study area (in PUs).
# creates a .rds file PUs x features layer.

# Function is found in 05b.

# Inputs include the following:
# 1. input: species codes: YFT, SKP, ALB, SWO
# 2. inpdir: directory where the layer is found in .csv format
# 3. outdir: directory where to save raster layers (in .rds)
# 4. PU: .rds file
# 5. prob_threshold: median of the predictions; everything else < the prob_threshold is not included in the data.

source("scripts/05b_Commercial_fCommercialFeat.R")

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
                                   prob_threshold = 0.07712687, #median of predictions of YFT
                                   PU = "outputs/01_StudyArea/01a_StudyArea/PacificABNJGrid_05deg.rds",
                                   outdir = "outputs/05_commercial/05b_fCommercialFeat/")

# Albacore
PRED_global_ALB <- fCommercialFeat(input = "ALB",
                                   inpdir = "inputs/mercer/alba.csv",
                                   prob_threshold = 0.01519237, #median of predictions of ALB
                                   PU = "outputs/01_StudyArea/01a_StudyArea/PacificABNJGrid_05deg.rds",
                                   outdir = "outputs/05_commercial/05b_fCommercialFeat/")

# Skipjack Tuna
PRED_global_SKP <- fCommercialFeat(input = "SKP",
                                   inpdir = "inputs/mercer/skip.csv",
                                   prob_threshold = 0.08615083, #median of predictions of SKP
                                   PU = "outputs/01_StudyArea/01a_StudyArea/PacificABNJGrid_05deg.rds",
                                   outdir = "outputs/05_commercial/05b_fCommercialFeat/")

# Swordfish
PRED_global_SWO <- fCommercialFeat(input = "SWO",
                                   inpdir = "inputs/mercer/sword.csv",
                                   prob_threshold = 0.01542815, #median of predictions of SWO
                                   PU = "outputs/01_StudyArea/01a_StudyArea/PacificABNJGrid_05deg.rds",
                                   outdir = "outputs/05_commercial/05b_fCommercialFeat/")

#####################################
# Plotting globally fitted models  #
#####################################
# YFT
myPalette <- colorRampPalette(brewer.pal(9, "BuGn"))
sc <- scale_colour_gradientn(name = "Probability of Spawning Area", 
                             colours = myPalette(100), 
                             limits=c(0, 0.4), 
                             aesthetics = c("color","fill"))

p1 <- ggplot()+
  geom_sf(data = PRED_global_YFT, aes(color = Prob, fill = Prob)) +
  sc +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Yellow Tuna") +
  theme_bw()

# ALB
sc <- scale_colour_gradientn(name = "Probability of Spawning Area", 
                             colours = myPalette(100), 
                             limits=c(0, 0.8), 
                             aesthetics = c("color","fill"))

p2 <- ggplot()+
  geom_sf(data = PRED_global_ALB, aes(color = Prob, fill = Prob)) +
  sc +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Albacore") +
  theme_bw()

# SKP
sc <- scale_colour_gradientn(name = "Probability of Spawning Area", 
                             colours = myPalette(100), 
                             limits=c(0, 0.4), 
                             aesthetics = c("color","fill"))

p3 <- ggplot()+
  geom_sf(data = PRED_global_SKP, aes(color = Prob, fill = Prob)) +
  sc +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Skipjack Tuna") +
  theme_bw()

# SWO
sc <- scale_colour_gradientn(name = "Probability of Spawning Area", 
                             colours = myPalette(100), 
                             limits=c(0, 0.4), 
                             aesthetics = c("color","fill"))

p4 <- ggplot()+
  geom_sf(data = PRED_global_SWO, aes(color = Prob, fill = Prob)) +
  sc +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Swordfish") +
  theme_bw()

global_plots <- (p1 + p2) / (p3 + p4) +
  plot_annotation(tag_levels = "i",
                  title = "Spawning areas")
# global_plots
# ggsave("pdfs/05_Commercial/tuna_spawning.pdf", width = 20, height = 10, dpi = 300)

#################################################
# Running & Plotting for Pacific-fitted models  #
#################################################

# Yellowfin Tuna
#PRED_pacific_YFT <- commercial_feat(input = "YFT_pac",
#                                    inpdir = "inputs/mercer/yft_pacific.csv",
#                                    prob_threshold = 0.06638632, #median of predictions of YFT
#                                   PU = "outputs/01_StudyArea/01a_StudyArea/PacificABNJGrid_05deg.rds",
#                                   outdir = "outputs/05_commercial/05b_fCommercialFeat/")

# Albacore
#PRED_pacific_ALB <- commercial_feat(input = "ALB_pac",
#                                    inpdir = "inputs/mercer/alba_pacific.csv",
#                                    prob_threshold = 0.006207977, #median of predictions of ALB
#                                    PU = "outputs/01_StudyArea/01a_StudyArea/PacificABNJGrid_05deg.rds",
#                                   outdir = "outputs/05_commercial/05b_fCommercialFeat/")

# Skipjack Tuna
#PRED_pacific_SKP <- commercial_feat(input = "SKP_pac",
#                                    inpdir = "inputs/mercer/skip_pacific.csv",
#                                    prob_threshold = 0.0830641, #median of predictions of SKP
#                                    PU = "outputs/01_StudyArea/01a_StudyArea/PacificABNJGrid_05deg.rds",
#                                   outdir = "outputs/05_commercial/05b_fCommercialFeat/")

# Swordfish
#PRED_pacific_SWO <- commercial_feat(input = "SWO_pac",
#                                    inpdir = "inputs/mercer/sword_pacific.csv",
#                                    prob_threshold = 0.01336439, #median of predictions of SWO
#                                    PU = "outputs/01_StudyArea/01a_StudyArea/PacificABNJGrid_05deg.rds",
#                                   outdir = "outputs/05_commercial/05b_fCommercialFeat/")
#####################################
# Plotting Pacific-fitted models  #
#####################################

# YFT - PAC
#sc <- scale_colour_gradientn(name = "Probability of Spawning Area", 
#                             colours = myPalette(100), 
#                             limits=c(0, 0.4), 
#                             aesthetics = c("color","fill"))

#p5 <- ggplot()+
#  geom_sf(data = PRED_pacific_YFT, aes(color = Prob, fill = Prob)) +
#  sc +
#  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
#  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
#           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
#           expand = TRUE) +
#  labs(title = "Yellow Tuna") +
#  theme_bw()

# ALB - PAC
#sc <- scale_colour_gradientn(name = "Probability of Spawning Area", 
#                             colours = myPalette(100), 
#                             limits=c(0, 0.9), 
#                             aesthetics = c("color","fill"))

#p6 <- ggplot()+
#  geom_sf(data = PRED_pacific_ALB, aes(color = Prob, fill = Prob)) +
#  sc +
#  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
#  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
#           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
#           expand = TRUE) +
#  labs(title = "Albacore") +
#  theme_bw()

# SKP - PAC
#sc <- scale_colour_gradientn(name = "Probability of Spawning Area", 
#                             colours = myPalette(100), 
#                             limits=c(0, 0.6), 
#                             aesthetics = c("color","fill"))

#p7 <- ggplot()+
#  geom_sf(data = PRED_pacific_SKP, aes(color = Prob, fill = Prob)) +
#  sc +
#  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
#  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
#           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
#           expand = TRUE) +
#  labs(title = "Skipjack Tuna") +
#  theme_bw()

# SWO - PAC
#sc <- scale_colour_gradientn(name = "Probability of Spawning Area", 
#                             colours = myPalette(100), 
#                             limits=c(0, 0.4), 
#                             aesthetics = c("color","fill"))

#p8 <- ggplot() +
#  geom_sf(data = PRED_pacific_SWO, aes(color = Prob, fill = Prob)) +
#  sc +
#  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
#  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
#           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
#           expand = TRUE) +
#  labs(title = "Skipjack Tuna") +
#  theme_bw()

#(p5 + p6) / (p7 + p8) +
#  plot_annotation(tag_levels = "i",
#                  title = "Spawning areas (Pacific Fitted)")
# ggsave("pdfs/05_Commercial/tuna_spawning_pacific.pdf", width = 20, height = 10, dpi = 300)
