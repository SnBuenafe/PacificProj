# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.
# most are modified from IBM's code.

# This code creates a function that intersects the predicted spawning areas (> median)
# Yellowfin Tuna, Albacore, Bigeye Tuna and Swordfish, and the Longhurst Provinces (in PUs).
# The function commercial_intersect() creates "new" features with the intersected Provinces and Commercial Spp.' spawning areas
# creates a .rds file spawning x longhurst provinces layer.
# Function is run at the bottom of the file.

# Inputs include the following:
# path: where the .rds files of the commercial species are located
# pu_shp: .shp or .rds file of the PUs
# fit: "pacific" or "global"
# outdir: where the .rds file should be saved.

commercial_intersect <- function(path, pu_shp, fit, outdir, ...) {
  
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
  library(parallel)
  library(doParallel)
  
  if(stringr::str_detect(string = pu_shp, pattern = ".rds") == TRUE) {
    shp_PU_sf <- readRDS(pu_shp)
  } else if (stringr::str_detect(string = pu_shp, pattern = ".shp") == TRUE) {
    shp_PU_sf <- st_read(pu_shp)
  }
  
  # If no cellsID values were assinged to the original
  shp_PU_sf1 <- shp_PU_sf %>%
    dplyr::mutate (cellsID = 1:nrow(shp_PU_sf), 
                   area_km2 = as.numeric(st_area(shp_PU_sf)/1e+06)) %>% 
    dplyr::select(cellsID, geometry)
  pu_min_area <- min(shp_PU_sf1$area_km2)
  
  if(fit == "global"){
      dir <- path
      pattern1 <- c("YFT.rds","ALB.rds","SKP.rds","SWO.rds")
      files <- list.files(path = dir, pattern = paste0(pattern1, collapse = "|"), full.names = TRUE)
    }else if(fit == "pacific"){
      dir <- path
      pattern1 <- c("YFT_pac.rds","ALB_pac.rds","SKP_pac.rds","SWO_pac.rds")
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
    
    # Call the Longhurst Province file
    longhurst <- readRDS(longhurst_rds)
    
    temp <- st_intersection(longhurst, PU_list_b) %>% 
      filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) 
    
    temp1 <- temp %>% 
      dplyr::mutate(feature = paste0(province,"_",feature_names)) 
    
    PU_list_c <- temp1 %>% 
      dplyr::group_by(province)
  
    # Write the object
    pu_rds <- paste("commercial_features", ".rds", sep = "")
    saveRDS(PU_list_c, paste(outdir, pu_rds, sep = ""))
    return(PU_list_c)
}

####################################
# Running the function
####################################

# global-fitted data
run15 <- commercial_intersect(path = "outputs/commercial/04b_CommercialPredictions",
                              pu_shp = "inputs/shapefiles/PacificABNJGrid_05deg/PacificABNJGrid_05deg.shp",
                              fit = "global",
                              outdir = "outputs/commercial/04d_CommercialxProvince/global/",
                              longhurst_rds = "outputs/Provinces/PacificABNJGrid_05deg_Longhurst.rds")

run15

# checking if it works
ggplot() +
  geom_sf(data = run15, aes(color = feature, fill = feature))

# pacific-fitted data
run16 <- commercial_intersect(path = "outputs/commercial/04b_CommercialPredictions",
                             pu_shp = "inputs/shapefiles/PacificABNJGrid_05deg/PacificABNJGrid_05deg.shp",
                             fit = "pacific",
                             outdir = "outputs/commercial/04d_CommercialxProvince/pacific/",
                             longhurst_rds = "outputs/Provinces/PacificABNJGrid_05deg_Longhurst.rds")

run16

library(wesanderson)
library(RColorBrewer)
nb.cols <- 34
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)

# checking if it works
ggplot() +
  geom_sf(data = run16, aes(color = feature, fill = feature)) +
  scale_color_manual(values = mycolors) +
  scale_fill_manual(values = mycolors) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Commercial Species") +
  theme_bw()

