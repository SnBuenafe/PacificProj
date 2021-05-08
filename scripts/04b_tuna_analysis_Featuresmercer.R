# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.
# most are modified from IBM's code.

# This code creates a function that intersects the predictions / probabilities of spawning areas from GAMs of 4 commercial species:
# Yellowfin Tuna, Albacore, Bigeye Tuna and Swordfish, and the study area (in PUs).
# creates a .rds file PUs x features layer.
# Function is run at the bottom of the file.

# Inputs include the following:
# input: species codes: YFT, SKP, ALB, SWO
# inpdir: directory where the layer is found in .csv format
# outdir: directory where to save raster layers (in .rds)
# pu: PU .shp file; "inputs/shapefiles/PacificABNJGrid_05deg/PacificABNJGrid_05deg.shp"
# prob_threshold: median of the predictions; everything else < the prob_threshold is not included in the data.

commercial_feat <- function(input, inpdir, prob_threshold, PU, data, outdir, ...) {

  ##################################
  ### Defining the main packages ###
  ##################################
  
  # List of packages that we will use
  list.of.packages <- c("raster", "sf", "tidyverse", "magrittr", "rnaturalearth", "rnaturalearthdata", 
                        "fasterize", "proj4", "exactextractr")
  # If is not installed, install the pacakge
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  # Load packages
  lapply(list.of.packages, require, character.only = TRUE)
  
  ############################
  ### Defining projections ###
  ############################

  rob_pacific <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" # Best to define these first so you don't make mistakes below
  longlat <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

  prob_threshold <- prob_threshold #median

  #####################
  ### Calling Files ###
  #####################
    
  # Calling .shp file of PUs
  shp_PU_sf <- st_read(PU) %>% 
    st_transform(crs = rob_pacific)
  
  shp_PU_sf1 <- shp_PU_sf %>%
    dplyr::mutate (cellsID = 1:nrow(shp_PU_sf), 
                   area_km2 = as.numeric(st_area(shp_PU_sf)/1e+06)) %>% 
    dplyr::select(cellsID, geometry)
  pu_min_area <- min(shp_PU_sf1$area_km2)

  # Selecting Lat, Long and Predictions
  temp_sp <- read_csv(inpdir) %>% 
    dplyr::select(Latitude, Longitude, Preds) %>% 
    dplyr::filter(Preds > prob_threshold) %>% 
    rename(Prob = Preds)
  temp_sp <- temp_sp[,c(2,1,3)] %>% 
    data.frame()

  sp_raster <- rasterFromXYZ(temp_sp)
  crs(sp_raster) <- CRS(longlat)
  
  ######################################
  ### Assigning area-weighted values ###
  ######################################
  
  # Creating layer of weights
  weight_rs <- raster::area(sp_raster)
  
  # Projecting the costs and weights into Robinson's (the same projection as the PUs)
  sp_rasterf <- projectRaster(sp_raster, crs = CRS(rob_pacific), method = "ngb", over = FALSE, res = 2667.6)
  weight_rsf <- projectRaster(weight_rs, crs = CRS(rob_pacific), method = "ngb", over = FALSE, res = 2667.6)

  names(sp_rasterf) <- "layer"
  
  # Getting predictions for each planning unit
  pred_bypu <- exact_extract(sp_rasterf, shp_PU_sf1, "weighted_mean", weights = weight_rsf)
  pu_file <- shp_PU_sf1 %>% 
    mutate(Prob = pred_bypu) %>% 
    mutate(area_km2 = as.numeric(st_area(shp_PU_sf1)/1e+06)) %>% 
    na.omit()

  # Saving RDS file of commercial species features
  saveRDS(pu_file, paste0(outdir, input, ".rds"))

return(pu_file)
}

#####################################
# Running for global-fitted models  #
#####################################

# Yellowfin Tuna
PRED_global_YFT <- commercial_feat(input = "YFT",
                         inpdir = "inputs/mercer/yft.csv",
                         prob_threshold = 0.07712687, #median of predictions of YFT
                         PU = "inputs/shapefiles/PacificABNJGrid_05deg/PacificABNJGrid_05deg.shp",
                         outdir = "outputs/commercial/04b_CommercialPredictions/")

# Albacore
PRED_global_ALB <- commercial_feat(input = "ALB",
                         inpdir = "inputs/mercer/alba.csv",
                         prob_threshold = 0.01519237, #median of predictions of ALB
                         PU = "inputs/shapefiles/PacificABNJGrid_05deg/PacificABNJGrid_05deg.shp",
                         outdir = "outputs/commercial/04b_CommercialPredictions/")

# Skipjack Tuna
PRED_global_SKP <- commercial_feat(input = "SKP",
                         inpdir = "inputs/mercer/skip.csv",
                         prob_threshold = 0.08615083, #median of predictions of SKP
                         PU = "inputs/shapefiles/PacificABNJGrid_05deg/PacificABNJGrid_05deg.shp",
                         outdir = "outputs/commercial/04b_CommercialPredictions/")

# Swordfish
PRED_global_SWO <- commercial_feat(input = "SWO",
                         inpdir = "inputs/mercer/sword.csv",
                         prob_threshold = 0.01542815, #median of predictions of SWO
                         PU = "inputs/shapefiles/PacificABNJGrid_05deg/PacificABNJGrid_05deg.shp",
                         outdir = "outputs/commercial/04b_CommercialPredictions/")

############
# Plotting #
############

#Defining generalities for plotting

rob_pacific <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" # Best to define these first so you don't make mistakes below

world_sf <- st_read("inputs/shapefiles/PacificCenterLand/PacificCenterLand.shp") %>% 
  st_transform(crs = rob_pacific)

test<-cbind(c(140, -78, -78, 140), #TopLeft, TopRight, BottomRight, BottomLeft
            c( 51, 51, -60, -60))
Cnr <- proj4::project(test, proj = rob_pacific)
print(Cnr)

Bndry <- tibble(V1 = Cnr[1:2,1] , V2 = Cnr[1:2,2]) %>% # Start with N boundary (51N)
  bind_rows(as_tibble(project(as.matrix(tibble(x = -78, y = seq(51, -60, by = -1))), proj = rob_pacific))) %>% # Then bind to E boundary (-78E)
  bind_rows(as_tibble(project(as.matrix(tibble(x = 140, y = seq(-60, 51, by = 1))), proj = rob_pacific))) %>% # Then W boundary (140E) - reverse x order
  as.matrix() %>%
  list() %>%
  st_polygon() %>%
  st_sfc(crs = rob_pacific)

#####################################
# Plotting globally fitted models  #
#####################################
library(patchwork)
library(RColorBrewer)

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

(p1 + p2) / (p3 + p4) +
          plot_annotation(tag_levels = "i",
                          title = "Spawning areas")
ggsave("pdfs/tuna_spawning.pdf", width = 20, height = 10, dpi = 300)

#################################################
# Running & Plotting for Pacific-fitted models  #
#################################################

# Yellowfin Tuna
PRED_pacific_YFT <- commercial_feat(input = "YFT_pac",
                         inpdir = "inputs/mercer/yft_pacific.csv",
                         prob_threshold = 0.06638632, #median of predictions of YFT
                         PU = "inputs/shapefiles/PacificABNJGrid_05deg/PacificABNJGrid_05deg.shp",
                         outdir = "outputs/commercial/04b_CommercialPredictions/")

sc <- scale_colour_gradientn(name = "Probability of Spawning Area", 
                             colours = myPalette(100), 
                             limits=c(0, 0.4), 
                             aesthetics = c("color","fill"))

p5 <- ggplot()+
  geom_sf(data = PRED_pacific_YFT, aes(color = Prob, fill = Prob)) +
  sc +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Yellow Tuna") +
  theme_bw()

# Albacore
PRED_pacific_ALB <- commercial_feat(input = "ALB_pac",
                         inpdir = "inputs/mercer/alba_pacific.csv",
                         prob_threshold = 0.006207977, #median of predictions of ALB
                         PU = "inputs/shapefiles/PacificABNJGrid_05deg/PacificABNJGrid_05deg.shp",
                         outdir = "outputs/commercial/04b_CommercialPredictions/")

sc <- scale_colour_gradientn(name = "Probability of Spawning Area", 
                             colours = myPalette(100), 
                             limits=c(0, 0.9), 
                             aesthetics = c("color","fill"))

p6 <- ggplot()+
  geom_sf(data = PRED_pacific_ALB, aes(color = Prob, fill = Prob)) +
  sc +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Albacore") +
  theme_bw()

# Skipjack Tuna
PRED_pacific_SKP <- commercial_feat(input = "SKP_pac",
                         inpdir = "inputs/mercer/skip_pacific.csv",
                         prob_threshold = 0.0830641, #median of predictions of SKP
                         PU = "inputs/shapefiles/PacificABNJGrid_05deg/PacificABNJGrid_05deg.shp",
                         outdir = "outputs/commercial/04b_CommercialPredictions/")

sc <- scale_colour_gradientn(name = "Probability of Spawning Area", 
                             colours = myPalette(100), 
                             limits=c(0, 0.6), 
                             aesthetics = c("color","fill"))

p7 <- ggplot()+
  geom_sf(data = PRED_pacific_SKP, aes(color = Prob, fill = Prob)) +
  sc +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Skipjack Tuna") +
  theme_bw()

# Swordfish

PRED_pacific_SWO <- commercial_feat(input = "SWO_pac",
                         inpdir = "inputs/mercer/sword_pacific.csv",
                         prob_threshold = 0.01336439, #median of predictions of SWO
                         PU = "inputs/shapefiles/PacificABNJGrid_05deg/PacificABNJGrid_05deg.shp",
                         outdir = "outputs/commercial/04b_CommercialPredictions/")

sc <- scale_colour_gradientn(name = "Probability of Spawning Area", 
                             colours = myPalette(100), 
                             limits=c(0, 0.4), 
                             aesthetics = c("color","fill"))

p8 <- ggplot()+
  geom_sf(data = PRED_pacific_SWO, aes(color = Prob, fill = Prob)) +
  sc +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Skipjack Tuna") +
  theme_bw()

(p5 + p6) / (p7 + p8) +
  plot_annotation(tag_levels = "i",
                  title = "Spawning areas (Pacific Fitted)")
ggsave("pdfs/tuna_spawning_pacific.pdf", width = 20, height = 10, dpi = 300)
