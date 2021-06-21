# with some modifications by Tin Buenafe, 2021 (tinbuenafe@gmail.com)

# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

# AIM: Create a general dataframe that would be the core for generate input file for prioritizr analyses (conventional MARXAN)
# path: folder's name where species conservation feature files are located
# outdir: where to put the final sf-.rds object
# pu_shp: .shp or .rds of the PUs
# data: "global", "IUCN", "AQM"

fFeaturesInt <- function(path, outdir, pu_shp, data, ...) { 
  
  ####################################################################################
  ####### Defining the main packages (trying to auto this)
  ####################################################################################
  # List of pacakges that we will use
  list.of.packages <- c("raster", "rgdal", "rgeos", "sf", "dplyr", "doParallel", "stringr", "sf", "lwgeom", "data.table")
  # If is not installed, install the pacakge
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  # Load packages
  lapply(list.of.packages, require, character.only = TRUE)
  
  #######################
  #### Calling files ####
  #######################
  if(stringr::str_detect(string = pu_shp, pattern = ".rds") == TRUE) {
    shp_PU_sf <- readRDS(pu_shp)
  } else if (stringr::str_detect(string = pu_shp, pattern = ".shp") == TRUE) {
    shp_PU_sf <- st_read(pu_shp)
  }
  # If no cellsID values were assigned to the original
  shp_PU_sf <- shp_PU_sf %>%
    dplyr::mutate (cellsID = 1:nrow(shp_PU_sf), 
                   area_km2 = as.numeric(st_area(shp_PU_sf)/1e+06)) %>% 
    dplyr::select(cellsID, geometry)
  pu_min_area <- min(shp_PU_sf$area_km2)
  
  if(data == "global") {
    # Reading conservation features .rds files (Global-Fitted)
    dir <- path
    pattern1 <-  c("ALB.rds","SKP.rds","SWO.rds","YFT.rds")
    files <- list.files(path = dir, pattern = paste0(pattern1, collapse = "|"), full.names = TRUE)
  }else {
      # Reading conservation features .rds files (AquaMaps/IUCN)
      dir <- path
      pattern1 <-  c(paste0("*", ".*.rds$"), paste0("*", ".*shp$"))
      files <- list.files(path = dir, pattern = paste0(pattern1, collapse = "|"), full.names = TRUE)
  }
  
  ########################################
  #### Intersecting features with PUs ####
  ########################################
  # Loop through each file
  files_list <- vector("list", length = length(files)) # to allocate results
  
  # Begin the parallel structure
  ncores <- detectCores() - 1 
  cl <- makeCluster(ncores)
  registerDoParallel(cl)
  
  # A parallel Loop
  PU_list <- foreach(i = 1:length(files), .packages = c("raster", "sf", "dplyr", "stringr", "lwgeom", "data.table")) %dopar% {
    # Reading conservation features
    if(stringr::str_detect(string = files[i], pattern = ".rds") == TRUE) {
      single <- readRDS(files[i])
    } else if (stringr::str_detect(string = files[i], pattern = ".shp") == TRUE) {
      single <- st_read(files[i])
    }
    # Intersects every conservation feature with planning unit region
    pu_int <- st_intersection(shp_PU_sf, single) %>% 
      filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) # we want just the polygons/multi not extra geometries
    # Filter the intersection with the planning unit sf object to get the exact distribution per planning units
    if(nrow(pu_int) > 0) { # to avoid empty sf objects 
      files_list[[i]] <- st_join(x = shp_PU_sf, y = pu_int,  by = "cellsID") %>% 
        na.omit() %>% 
        dplyr::group_by(cellsID.x) %>% 
        dplyr::summarise(cellsID = unique(cellsID.x)) %>% 
        dplyr::select(cellsID, geometry) %>% 
        dplyr::mutate(area_km2 = as.numeric(st_area(geometry)/1e+06))
      
      if(data %in% c("global","IUCN")) {
        files_list[[i]] <- files_list[[i]] %>% 
          mutate(feature_names = paste(unlist(strsplit(basename(files[i]), "[.]"))[1])) %>% 
          ungroup()
      }else if(data == "AQM") {
        files_list[[i]] <- files_list[[i]] %>% 
          mutate(feature_names = paste(unlist(strsplit(basename(files[i]), "_"))[1])) %>% 
          ungroup()
      }
      # dplyr::filter(area_km2 >= pu_min_area) %>% 
    }
  }
  stopCluster(cl)
  
  # Final sf with all species information and write that object
  PU_list_b <- do.call(rbind, PU_list)
  
  #################
  ## Save object ##
  #################
  if(data %in% c("global")) {
    pu_rds <- paste("commercial_features",".rds", sep = "")
  } else {
    pu_rds <- paste("bycatch_features", ".rds", sep = "")
  }
  saveRDS(PU_list_b, paste(outdir, pu_rds, sep = ""))
  return(PU_list_b)
}
