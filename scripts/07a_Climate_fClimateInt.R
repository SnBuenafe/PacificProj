# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.
# most are modified from IBM's code.

# This code creates a function that intersects the climate features (RCE and climate velocity values for different scenarios) and the study area (in PUs).
# creates a .rds file PUs x features layer.
# Function is run in code 06b

# Inputs include the following:
# input: layer to be intersected; e.g. "RCE" or "velocity"
# scenario: SSP126, SSP245, SSP585
# inpdir: directory where the layer is found; "inputs/rasterfiles/Costlayer/02-epipelagic_Cost_Raster_Sum.tif"
# outdir: directory where to save raster layers
# pu: PU .shp or .rds file; "inputs/shapefiles/PacificABNJGrid_05deg/PacificABNJGrid_05deg.shp"

fClimateInt <- function(input, scenario, inpdir, outdir, pu, ...) {
  
  ####################################################################################
  ####### Defining packages needed
  ####################################################################################
  # List of pacakges that we will use
  list.of.packages <- c("raster", "sf", "tidyverse", "magrittr", "rnaturalearth",
                        "rnaturalearthdata", "fasterize", "proj4", "kader", "exactextractr")
  # If is not installed, install the package
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  # Load packages
  lapply(list.of.packages, require, character.only = TRUE)

  ###############################
  #### Defining generalities ####
  ###############################
  rob_pacific <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" # Best to define these first so you don't make mistakes below
  longlat <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
  ###############################
  #### Calling PU file ####
  ###############################
  if(stringr::str_detect(string = pu, pattern = ".rds") == TRUE) {
    shp_PU_sf <- readRDS(pu)
  } else if (stringr::str_detect(string = pu, pattern = ".shp") == TRUE) {
    shp_PU_sf <- st_read(pu)
  }

  shp_PU_sf <- shp_PU_sf %>% 
    st_transform(crs = rob_pacific)
  
  # assigning cellsID to each PU
  shp_PU_sf1 <- shp_PU_sf %>%
    dplyr::mutate (cellsID = 1:nrow(shp_PU_sf), 
                   area_km2 = as.numeric(st_area(shp_PU_sf)/1e+06)) %>% 
    dplyr::select(cellsID, geometry)
  pu_min_area <- min(shp_PU_sf1$area_km2)
  
  ##########################################
  #### Calling RCE/Velocity raster file ####
  ##########################################
  # calling the raster file layer
  layer_rs <- readAll(raster(inpdir))
  crs(layer_rs) <- CRS(longlat)
  
  ##########################################
  #### Manipulating raster file ####
  ##########################################
  
  # Creating layer of weights.
  weight_rs <- raster::area(layer_rs)
  
  # Projecting the costs and weights into Robinson's (the same projection as the PUs)
  layer_rsf <- projectRaster(layer_rs, crs = CRS(rob_pacific), method = "ngb", over = FALSE, res = 2667.6)
  weight_rsf <- projectRaster(weight_rs, crs = CRS(rob_pacific), method = "ngb", over = FALSE, res = 2667.6)

  names(layer_rsf) <- "value"
  
  # Getting cost value by planning unit
  value_bypu <- exact_extract(layer_rsf, shp_PU_sf1, "weighted_mean", weights = weight_rsf)
  
  # Assigning weighted (by area) RCE/Velocity values per planning unit.
  if(input == "RCE") {
    pu_file <- shp_PU_sf1 %>% 
      dplyr::mutate(value = value_bypu)
    pu_file <- pu_file %>% 
      dplyr::mutate(area_km2 = as.numeric(st_area(geometry)/1e+06)) %>% 
      dplyr::mutate(value = ifelse(is.na(value), median(filter(pu_file, pu_file$value!=0)$value),value)) %>% 
      dplyr::mutate(trans_value = kader:::cuberoot(value))
  } else if(input == "velocity") {
    pu_file <- shp_PU_sf1 %>% 
      dplyr::mutate(value = value_bypu)
    pu_file <- pu_file %>% 
      dplyr::mutate(area_km2 = as.numeric(st_area(geometry)/1e+06)) %>% 
      dplyr::mutate(value = ifelse(is.na(value), median(filter(pu_file, pu_file$value!=0)$value),value)) %>% 
      dplyr::mutate(trans_value = value*10)
  }
  
  # Assigning categories.
  if(input == "RCE"){
    pu_filef <- pu_file %>% dplyr::mutate(rce_categ = ifelse(trans_value <= 0.2, 1,
                                                      ifelse(trans_value >0.2 & trans_value <= 0.4, 2,
                                                      ifelse(trans_value >0.4 & trans_value <= 0.6, 3,
                                                      ifelse(trans_value >0.6 & trans_value <= 0.8, 4,
                                                      ifelse(trans_value >0.8 & trans_value <= 1.1, 5,
                                                      ifelse(trans_value >1.1 & trans_value <= 1.2, 6,
                                                      ifelse(trans_value >1.2 & trans_value <= 1.5, 7,
                                                      ifelse(trans_value >1.5 & trans_value <= 2, 8,
                                                      ifelse(trans_value >2 & trans_value <= 4, 9,
                                                      ifelse(trans_value >4 & trans_value <= 6, 10, 11)))))))))))
  }else if(input == "velocity"){
    pu_filef <- pu_file %>% dplyr::mutate(velo_categ = ifelse(trans_value <= -50, 1,
                                                       ifelse(trans_value >-50 & trans_value <= -20, 2,
                                                       ifelse(trans_value >-20 & trans_value <= -10, 3,
                                                       ifelse(trans_value >-10 & trans_value <= -5, 4,
                                                       ifelse(trans_value >-5 & trans_value <= 5, 5,
                                                       ifelse(trans_value >5 & trans_value <= 10, 6,
                                                       ifelse(trans_value >10 & trans_value <= 20, 7,
                                                       ifelse(trans_value >20 & trans_value <= 50, 8,
                                                       ifelse(trans_value >50 & trans_value <= 100, 9,
                                                       ifelse(trans_value >100 & trans_value <= 200, 10, 11)))))))))))
  }else {print("fail; input N/A")}
  
  # Saving RDS
  saveRDS(pu_filef, paste0(outdir, input, scenario, ".rds"))
  
  return(pu_filef)
  
}
