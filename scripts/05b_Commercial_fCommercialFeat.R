# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.
# most are modified from IBM's code.

# This code creates a function that intersects the predictions / probabilities of spawning areas from GAMs of 4 commercial species:
# Yellowfin Tuna, Albacore, Bigeye Tuna and Swordfish, and the study area (in PUs).
# creates a .rds file PUs x features layer.

# Function is run in 04b Run.

# Inputs include the following:
# input: species codes: YFT, SKP, ALB, SWO
# inpdir: directory where the layer is found in .csv format
# outdir: directory where to save raster layers (in .rds)
# pu: PU .shp file; "inputs/shapefiles/PacificABNJGrid_05deg/PacificABNJGrid_05deg.shp"
# prob_threshold: median of the predictions; everything else < the prob_threshold is not included in the data.

fCommercialFeat <- function(input, inpdir, prob_threshold, PU, outdir, ...) {

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
  shp_PU_sf <- readRDS(PU) %>% 
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