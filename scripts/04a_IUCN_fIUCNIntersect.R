# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.
# most are modified from IBM's code.

# This code creates a function that intersects the IUCN sea turtle data (feature) with planning units (PUs).
# creates a .rds file PUs x features layer.
# Function is run at the bottom of the file.

# Inputs include the following:
# 1. input = IUCN .shp file;
# 2. pu_file = PUs .rds file;
# 3. outdir = path of the outputs of the function.

# Code is run in 04a.

fIUCNIntersect <- function(input, pu_file, outdir, ...) {
  
  ####################################################################################
  ####### Defining packages needed
  ####################################################################################
  # List of pacakges that we will use
  list.of.packages <- c("raster", "sf", "tidyverse", "magrittr", "rnaturalearth",
                        "rnaturalearthdata", "fasterize", "doParallel", "foreach")
  # If is not installed, install the pacakge
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  # Load packages
  lapply(list.of.packages, require, character.only = TRUE)
  
  #########################
  ##### Calling files #####
  #########################
  
  # Calling IUCN data
  dat <- st_read(input)
  x <- unique(dat$BINOMIAL) # maybe make a list then do a for loop across the unique species???
  
  # Calling PU shapefile.
  shp_PU_sf <- readRDS(pu_file)
  
  shp_PU_sf <- shp_PU_sf %>%
    dplyr::mutate (cellsID = 1:nrow(shp_PU_sf), 
                   area_km2 = as.numeric(st_area(shp_PU_sf)/1e+06)) %>% 
    dplyr::select(cellsID, geometry)
  
  # Set up parallel structure
  ncores <- detectCores()
  cl <- makeCluster(ncores -1)
  registerDoParallel(cl)
  
  IUCN_list <- vector("list", length(x))
  
  # A parallel Loop
  IUCN_list <- foreach(i = 1:length(x), .packages = c("sf", "tidyverse", "dplyr", "magrittr", "fasterize", "raster")) %dopar% {
    
    sp <- x[i]
    
    iucn_dataf <- dat %>% 
        dplyr::select(BINOMIAL, geometry) %>% 
        dplyr::rename(species = BINOMIAL) %>% 
        dplyr::filter(species == sp)
  
    # Reproject shapefiles to Robinson's Projection
    source("scripts/study_area/fCreateMaskedPolygon.R")
    source("scripts/study_area/fConvert2PacificRobinson.R")
    iucn_pol_sf <- fCreateMaskedPolygon(df = iucn_dataf, res = 0.5, mask = NA, inverse = NA)
    iucn_robinson <- iucn_pol_sf %>% 
      fConvert2PacificRobinson()
  
    # Intersects every conservation feature with planning unit region
      pu_int <- st_intersection(shp_PU_sf, iucn_robinson) %>% 
        filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON"))
  
      IUCN_list[[i]] <- st_join(x = shp_PU_sf, y = pu_int,  by = "cellsID") %>% 
        na.omit() %>% 
        dplyr::group_by(cellsID.x) %>% 
        dplyr::summarise(cellsID = unique(cellsID.x)) %>% 
        dplyr::select(cellsID, geometry)
  
      # save file
      saveRDS(IUCN_list[[i]], paste0(outdir, unlist(strsplit(basename(sp)," "))[1], "_", unlist(strsplit(basename(sp)," "))[2], "_IUCN.rds"))
  }
  stopCluster(cl)
}



