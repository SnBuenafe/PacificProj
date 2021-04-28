# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.
# most are modified from IBM's code.

# prov_intersect function generally intersects the features with the provinces.
# it can intersect commercial and bycatch features (IUCN and AQM)
# codes are run for AQM in 03d, for commercial in 04d, and for IUCN in 09c
# code creates an .rds file of the feature x province

# Function requires the following inputs:
# 1. path: where the .rds files for each of the features are located (relevant only for commercial and IUCN)
# 2. pu_shp: .shp or .rds file of the PUs
# 3. fit: "global", "pacific", or "none"
# 4. outdir: path/directory where the .rds file should be saved
# 5. data: "commercial", "IUCN" or "AQM"
# 6. feature_file: .rds file with all the features together (relevant only for AQM)
# 7. prov_file: .rds file of the Longhurst provinces

prov_intersect <- function(path, pu_shp, fit, outdir, data, feature_file, prov_file, ...) {
  
  ##################################
  ### Defining the main packages ###
  ##################################
  
  # List of packages that we will use
  list.of.packages <- c("raster", "sf", "tidyverse", "magrittr", "rnaturalearth", "rnaturalearthdata", 
                        "fasterize", "proj4", "exactextractr", "parallel", 
                        "doParallel")
  # If is not installed, install the pacakge
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  # Load packages
  lapply(list.of.packages, require, character.only = TRUE)
  
  ##################################
  ### Reading the planning units ###
  ##################################
  
  if(stringr::str_detect(string = pu_shp, pattern = ".rds") == TRUE) {
    shp_PU_sf <- readRDS(pu_shp)
  } else if (stringr::str_detect(string = pu_shp, pattern = ".shp") == TRUE) {
    shp_PU_sf <- st_read(pu_shp)
  }
  
  # Assigning "cellsID" to each polygon.
  shp_PU_sf1 <- shp_PU_sf %>%
    dplyr::mutate (cellsID = 1:nrow(shp_PU_sf), 
                   area_km2 = as.numeric(st_area(shp_PU_sf)/1e+06)) %>% 
    dplyr::select(cellsID, geometry)
  pu_min_area <- min(shp_PU_sf1$area_km2)
  
  ##################################
  ### Reading the feature layer ###
  ##################################
  
  if(data %in% c("commercial", "IUCN")) {
    
      if(fit == "global"){
        dir <- path
        pattern1 <- c("YFT.rds","ALB.rds","SKP.rds","SWO.rds")
        files <- list.files(path = dir, pattern = paste0(pattern1, collapse = "|"), full.names = TRUE)
      }else if(fit == "pacific"){
        dir <- path
        pattern1 <- c("YFT_pac.rds","ALB_pac.rds","SKP_pac.rds","SWO_pac.rds")
        files <- list.files(path = dir, pattern = paste0(pattern1, collapse = "|"), full.names = TRUE)
      }else {
        dir <- path
        pattern1 <-  c(paste0("*", ".*.rds$"))
        files <- list.files(path = dir, pattern = paste0(pattern1, collapse = "|"), full.names = TRUE)
      }
    
    # Loop through each file
    files_list <- vector("list", length = length(files))
    
    # Begin the parallel structure
    ncores <- detectCores() - 1 
    cl <- makeCluster(ncores)
    registerDoParallel(cl)
    
    # Do a parallel loop
    PU_list <- foreach(i = 1:length(files), .packages = c("raster", "sf", "dplyr", "stringr", "lwgeom", "data.table")) %dopar% {
      
      # Reading the commercial spp' files.
      single <- readRDS(files[i])
      
      # Intersects every conservation feature with planning unit region
      pu_int <- st_intersection(shp_PU_sf1, single) %>% 
        filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) # we want just the polygons/multi not extra geometries
      
      # Filter the intersection with the planning unit sf object to get the exact distribution per planning units
      if(nrow(pu_int) > 0) { # to avoid empty sf objects 
        files_list[[i]] <- st_join(x = pu_int, y = shp_PU_sf1,  by = "cellsID") %>% 
          na.omit() %>% 
          dplyr::group_by(cellsID.x) %>% 
          dplyr::summarise(cellsID = unique(cellsID.x)) %>% 
          dplyr::select(cellsID, geometry) %>% 
          dplyr::mutate(area_km2 = as.numeric(st_area(geometry)/1e+06),
                        feature_names = paste(unlist(strsplit(basename(files[i]), "[.]"))[1])) %>% 
          ungroup()
        # dplyr::filter(area_km2 >= pu_min_area) %>% 
      }
    }
    
    PU_list_b <- do.call(rbind, PU_list)
  }else if(data == "AQM") {
    PU_list_b <- readRDS(feature_file)
  }else {}
  
  ##################################
  ##### Reading Longhurst file #####
  ##################################
  longhurst <- readRDS(prov_file)

  ########################################
  # Intersecting features with provinces #
  ########################################
  temp <- st_intersection(longhurst, PU_list_b) %>% 
    filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) 
  
  temp1 <- temp %>% 
    dplyr::mutate(feature = paste0(province,"_",feature_names)) 
  
  PU_list_c <- temp1 %>% 
    dplyr::group_by(province)
  
  ####################
  ## Writing object ##
  ####################
  
  if(data == "commercial"){
    pu_rds <- paste("commercial_features", ".rds", sep = "")
  } else if(data %in% c("AQM", "IUCN")) {
    pu_rds <- paste("bycatch_features", ".rds", sep = "")
  }
  saveRDS(PU_list_c, paste(outdir, pu_rds, sep = ""))
  return(PU_list_c)
  
}
