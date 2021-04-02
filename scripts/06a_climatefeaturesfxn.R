# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.
# most are modified from IBM's code.

# This code creates a function intersects the climate features (RCE and climate velocity values for different scenarios) and the study area (in PUs).
# creates a .rds file PUs x features layer.
# Function is run in code 06b

# Inputs include the following:
# input: layer to be intersected; e.g. "RCE" or "velocity"
# scenario: SSP126, SSP245, SSP585
# inpdir: directory where the layer is found; "inputs/rasterfiles/Costlayer/02-epipelagic_Cost_Raster_Sum.tif"
# outdir: directory where to save raster layers
# pu: PU .shp or .rds file; "inputs/shapefiles/PacificABNJGrid_05deg/PacificABNJGrid_05deg.shp"

layer_intersect <- function(input, scenario, inpdir, outdir, pu, ...) {
  
  # define packages required for the function to work
  
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
  
  # defining projections that will be used
  
  rob_pacific <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" # Best to define these first so you don't make mistakes below
  longlat <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
  # calling the raster file layer
  layer_rs <- readAll(raster(inpdir))
  
  # Creating a empty raster at 0.5° resolution (you can increase the resolution to get a better border precision)
  rs <- raster(ncol = 360*2, nrow = 180*2) 
  rs[] <- 1:ncell(rs)
  crs(rs) <- CRS(longlat)
  
  #resampled to ensure that we have the same resolution as study area
  layer_rs1 <- resample(layer_rs, rs, resample = "ngb")
  
  #converting into an sf spatial polygon dataframe
  layer_rs2 <- as(layer_rs1, "SpatialPolygonsDataFrame")
  layer_sp <- spTransform(layer_rs2, CRS(longlat))
  
  # Define a long & slim polygon that overlaps the meridian line & set its CRS to match that of world
  polygon <- st_polygon(x = list(rbind(c(-0.0001, 90),
                                       c(0, 90),
                                       c(0, -90),
                                       c(-0.0001, -90),
                                       c(-0.0001, 90)))) %>%
    st_sfc() %>%
    st_set_crs(longlat)
  
  # Transform the species distribution polygon object to a Pacific-centred projection polygon object
  layer_robinson <- layer_sp %>% 
    st_as_sf() %>% 
    st_difference(polygon) %>% 
    st_transform(crs = rob_pacific)
  
  # There is a line in the middle of Antarctica. This is because we have split the map after reprojection. We need to fix this:
  bbox1 <-  st_bbox(layer_robinson)
  bbox1[c(1,3)]  <-  c(-1e-5,1e-5)
  polygon1 <- st_as_sfc(bbox1)
  crosses1 <- layer_robinson %>%
    st_intersects(polygon1) %>%
    sapply(length) %>%
    as.logical %>%
    which
  # Adding buffer 0
  layer_robinson[crosses1, ] %<>%
    st_buffer(0)
  
  # calling PU shapefile
  
  if(stringr::str_detect(string = pu, pattern = ".rds") == TRUE) {
    shp_PU_sf <- readRDS(pu)
  } else if (stringr::str_detect(string = pu, pattern = ".shp") == TRUE) {
    shp_PU_sf <- st_read(pu)
  }
  
  # making sure that PU shapefile is transformed to robinson's projection
  shp_PU_sf <- shp_PU_sf %>% 
    st_transform(crs = rob_pacific)
  
  shp_PU_sf <- shp_PU_sf %>%
    dplyr::mutate (cellsID = 1:nrow(shp_PU_sf), 
                   area_km2 = as.numeric(st_area(shp_PU_sf)/1e+06)) %>% 
    dplyr::select(cellsID, geometry)
  pu_min_area <- min(shp_PU_sf$area_km2)
  
  names(layer_robinson)[1] <- "xx"
  
  single <- layer_robinson %>% 
    rename(value = xx)
  
  # Intersects every cost with planning unit region
  pu_int <- st_intersection(shp_PU_sf, single) %>% 
    filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) # we want just the polygons/multi not extra geometries
  
  xx_list <- st_join(x = shp_PU_sf, y = pu_int,  by = "cellsID") %>% 
    na.omit() %>% 
    dplyr::group_by(cellsID.x) %>% 
    dplyr::summarise(cellsID = unique(cellsID.x), value = mean(value)) %>% 
    dplyr::select(cellsID, geometry, value) %>% 
    dplyr::mutate(area_km2 = as.numeric(st_area(geometry)/1e+06)) %>% 
    ungroup()
  
  # Saving RDS
  saveRDS(xx_list, paste0(outdir, input, scenario, ".rds"))
  
  return(xx_list)
  
}
