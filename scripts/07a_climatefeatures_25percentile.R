# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.
# most are modified from IBM's code.

# This code filters the planning units whose conservation features (feature x Province) fall 
# under the lower quartile of RCE | climate velocity for the specific climate scenario
# It saves a new .rds file (sf object). commercialSSP126_25percentile.rds

# The function filter_quartile() requires the following inputs:
# 1. feature = "commercial" or "bycatch"
# 2. scenario = e.g. "SSP126"
# 3. velocity_file = .rds file for climate velocity of the scenario
# 4. RCE_file = .rds file for RCE of the scenario
# 5. feature_prov = .rds file for the features x province
# 6. outdir = path of the output

# The function is run in 07b for different scenarios.

filter_quartile <- function(velocity_file, RCE_file, feature_prov, outdir, scenario, feature_n, ...) {
  
  library(dplyr)
  library(sf)
  library(tidyverse)
  library(doParallel)
  
  velocity <- readRDS(velocity_file) %>% 
    rename(velocity = value, velo_tvalue = trans_value)
  RCE <- readRDS(RCE_file) %>% 
    rename(RCE = value, RCE_tvalue = trans_value)
  
  # Intersects climate features for all PUs
  climate_int <- st_intersection(velocity, RCE) %>% 
    filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) %>% # we want just the polygons/multi not extra geometries
    select(-area_km2, -area_km2.1, -velo_categ, -rce_categ)
  
  # Calling features that are intersected with provinces
  feature <- readRDS(feature_prov)
  
  feat_int <- st_intersection(feature, climate_int) %>% 
    filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) %>%  # we want just the polygons/multi not extra geometries
    select(-feature_names)
  
  # Begin the parallel structure  
  list <- unique(feat_int$feature)
  temp <- list()
  temp_x <- list()
  
  ncores <- detectCores() - 1 
  cl <- makeCluster(ncores)
  registerDoParallel(cl)
  
  filter_PU <- foreach(i = 1:length(list), .packages = c("raster", "sf", "dplyr")) %dopar% {
      temp[[i]] <- feat_int %>% 
        filter(feature == list[i])
      
      qrt_RCE <- quantile(temp[[i]]$RCE_tvalue)
      qrt_velocity <- quantile(temp[[i]]$velo_tvalue)
      
      temp_x[[i]] <- temp[[i]] %>% 
        filter((RCE_tvalue <= qrt_RCE[2]) | (velo_tvalue <= qrt_velocity[2]))
  }
  stopCluster(cl)
  
  filter_PU_final <- do.call(rbind, filter_PU)
  
  saveRDS(filter_PU_final, paste0(outdir,feature_n,scenario,"_25percentile.rds"))
  
  return(filter_PU_final)
  
}
