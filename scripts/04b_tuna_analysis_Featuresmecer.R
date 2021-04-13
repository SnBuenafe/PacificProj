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

  ###########################
  # Libraries to be used
  ###########################
  
  library(raster)
  library(sf)
  library(dplyr)
  library(magrittr)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(fasterize)
  library(ggplot2)
  library(readr)
  library(proj4)
  library(exactextractr)

  rob_pacific <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" # Best to define these first so you don't make mistakes below
  longlat <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

  prob_threshold <- prob_threshold #median
  
  # Calling .shp file of PUs
  shp_PU_sf <- st_read(PU) %>% 
    st_transform(crs = rob_pacific)
  
  shp_PU_sf1 <- shp_PU_sf %>%
    dplyr::mutate (cellsID = 1:nrow(shp_PU_sf), 
                   area_km2 = as.numeric(st_area(shp_PU_sf)/1e+06)) %>% 
    dplyr::select(cellsID, geometry)
  pu_min_area <- min(shp_PU_sf1$area_km2)

  temp_sp <- read_csv(inpdir) %>% 
    dplyr::select(Latitude, Longitude, Preds) %>% 
    dplyr::filter(Preds > prob_threshold) %>% 
    rename(Prob = Preds)
  temp_sp <- temp_sp[,c(2,1,3)] %>% 
    data.frame()

  sp_raster <- rasterFromXYZ(temp_sp)
  crs(sp_raster) <- CRS(longlat)
  
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

###########################
# RUNNING  #
##########################
# Yellowfin Tuna
run07 <- commercial_feat(input = "YFT",
                         inpdir = "inputs/mercer/yft.csv",
                         prob_threshold = 0.1716593, #median of predictions of YFT
                         PU = "inputs/shapefiles/PacificABNJGrid_05deg/PacificABNJGrid_05deg.shp",
                         outdir = "outputs/commercial/04b_CommercialPredictions/")

# Albacore
run08 <- commercial_feat(input = "ALB",
                         inpdir = "inputs/mercer/alba.csv",
                         prob_threshold = 0.05841855, #median of predictions of ALB
                         PU = "inputs/shapefiles/PacificABNJGrid_05deg/PacificABNJGrid_05deg.shp",
                         outdir = "outputs/commercial/04b_CommercialPredictions/")

# Skipjack Tuna
run09 <- commercial_feat(input = "SKP",
                         inpdir = "inputs/mercer/skip.csv",
                         prob_threshold = 0.08408474, #median of predictions of SKP
                         PU = "inputs/shapefiles/PacificABNJGrid_05deg/PacificABNJGrid_05deg.shp",
                         outdir = "outputs/commercial/04b_CommercialPredictions/")

# Swordfish
run10 <- commercial_feat(input = "SWO",
                         inpdir = "inputs/mercer/sword.csv",
                         prob_threshold = 0.01448773, #median of predictions of SWO
                         PU = "inputs/shapefiles/PacificABNJGrid_05deg/PacificABNJGrid_05deg.shp",
                         outdir = "outputs/commercial/04b_CommercialPredictions/")

###########################
# PLOTTING #
##########################

#Defining generalities for plotting

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

#########################
# Plotting
#########################
library(patchwork)

# YFT
library(RColorBrewer)
myPalette <- colorRampPalette(brewer.pal(9, "BuGn"))
sc <- scale_colour_gradientn(name = "Probability of Spawning Area", 
                             colours = myPalette(100), 
                             limits=c(0, 0.5), 
                             aesthetics = c("color","fill"))

p1 <- ggplot()+
        geom_sf(data = run07, aes(color = Prob, fill = Prob)) +
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
        geom_sf(data = run08, aes(color = Prob, fill = Prob)) +
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
                             limits=c(0, 0.7), 
                             aesthetics = c("color","fill"))

p3 <- ggplot()+
        geom_sf(data = run09, aes(color = Prob, fill = Prob)) +
        sc +
        geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
        coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
                 ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
                 expand = TRUE) +
        labs(title = "Skipjack Tuna") +
        theme_bw()

#SWO
sc <- scale_colour_gradientn(name = "Probability of Spawning Area", 
                             colours = myPalette(100), 
                             limits=c(0, 0.6), 
                             aesthetics = c("color","fill"))

p4 <- ggplot()+
        geom_sf(data = run10, aes(color = Prob, fill = Prob)) +
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

#####################################
# RUNNING PACIFIC FITTED MODELS  #
#####################################
# Yellowfin Tuna
run11 <- commercial_feat(input = "YFT_pac",
                         inpdir = "inputs/mercer/yft_pacific.csv",
                         prob_threshold = 0.06778708, #median of predictions of YFT
                         PU = "inputs/shapefiles/PacificABNJGrid_05deg/PacificABNJGrid_05deg.shp",
                         outdir = "outputs/commercial/04b_CommercialPredictions/")

sc <- scale_colour_gradientn(name = "Probability of Spawning Area", 
                             colours = myPalette(100), 
                             limits=c(0, 0.9), 
                             aesthetics = c("color","fill"))

p5 <- ggplot()+
  geom_sf(data = run11, aes(color = Prob, fill = Prob)) +
  sc +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Yellow Tuna") +
  theme_bw()

# Albacore
run12 <- commercial_feat(input = "ALB_pac",
                         inpdir = "inputs/mercer/alba_pacific.csv",
                         prob_threshold = 0.006294141, #median of predictions of ALB
                         PU = "inputs/shapefiles/PacificABNJGrid_05deg/PacificABNJGrid_05deg.shp",
                         outdir = "outputs/commercial/04b_CommercialPredictions/")

sc <- scale_colour_gradientn(name = "Probability of Spawning Area", 
                             colours = myPalette(100), 
                             limits=c(0, 0.9), 
                             aesthetics = c("color","fill"))

p6 <- ggplot()+
  geom_sf(data = run12, aes(color = Prob, fill = Prob)) +
  sc +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Albacore") +
  theme_bw()

# Skipjack Tuna
run13 <- commercial_feat(input = "SKP_pac",
                         inpdir = "inputs/mercer/skip_pacific.csv",
                         prob_threshold = 0.0824297, #median of predictions of SKP
                         PU = "inputs/shapefiles/PacificABNJGrid_05deg/PacificABNJGrid_05deg.shp",
                         outdir = "outputs/commercial/04b_CommercialPredictions/")

sc <- scale_colour_gradientn(name = "Probability of Spawning Area", 
                             colours = myPalette(100), 
                             limits=c(0, 0.9), 
                             aesthetics = c("color","fill"))

p7 <- ggplot()+
  geom_sf(data = run13, aes(color = Prob, fill = Prob)) +
  sc +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Skipjack Tuna") +
  theme_bw()

# Swordfish

run14 <- commercial_feat(input = "SWO_pac",
                         inpdir = "inputs/mercer/sword_pacific.csv",
                         prob_threshold = 0.01336354, #median of predictions of SWO
                         PU = "inputs/shapefiles/PacificABNJGrid_05deg/PacificABNJGrid_05deg.shp",
                         outdir = "outputs/commercial/04b_CommercialPredictions/")

sc <- scale_colour_gradientn(name = "Probability of Spawning Area", 
                             colours = myPalette(100), 
                             limits=c(0, 0.4), 
                             aesthetics = c("color","fill"))

p8 <- ggplot()+
  geom_sf(data = run14, aes(color = Prob, fill = Prob)) +
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
