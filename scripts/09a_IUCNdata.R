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

# Code is run in 10b_IUCNxPU.

iucn_int <- function(input, pu_file, outdir, ...) {
  library(sf)
  library(rnaturalearth)
  library(tidyverse)
  library(fasterize)
  library(doParallel)
  library(foreach)
  library(dplyr)
  library(magrittr)
  library(raster)
  
  outdir = outdir
  
  # Defining generalities
  rob_pacific <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" # Best to define these first so you don't make mistakes below
  longlat <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
  # Define a long & slim polygon that overlaps the meridian line & set its CRS to match # that of world
  polygon <- st_polygon(x = list(rbind(c(-0.0001, 90),
                                       c(0, 90),
                                       c(0, -90),
                                       c(-0.0001, -90),
                                       c(-0.0001, 90)))) %>%
    st_sfc() %>%
    st_set_crs(4326)
  
  # Creating a empty raster at 0.5° resolution
  rs <- raster(ncol = 360*2, nrow = 180*2) 
  rs[] <- 1:ncell(rs)
  crs(rs) <- CRS(longlat)
  
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
    
    # Fasterize the iucn_dataf object using the generalities defined before foreach loop.
    iucn_rs <- fasterize(iucn_dataf, rs)
  
    # A process to delete certain agrupation of pixels
    iucn_clump <- clump(iucn_rs, directions = 8) 
    # Get frequency table    
    df_clump <- freq(iucn_clump) %>% 
      as.data.frame()
    # which rows of the data.frame are only represented by clumps under 9 pixels?
    str(which(df_clump$count <= 9))
    # which values do these correspond to?
    str(df_clump$value[which(df_clump$count <= 9)])
    # put these into a vector of clump ID's to be removed
    excludeID <- df_clump$value[which(df_clump$count <= 9)]
    # make a new raster to be sieved
    iucn_rs2 <- iucn_clump
    # assign NA to all clumps whose IDs are found in excludeID
    iucn_rs2[iucn_rs2 %in% excludeID] <- NA
    # We can plot the object to see if it is correct
    # plot(iucn_rs2)
  
    # From Raster to Polygon
    iucn_pol <- as(iucn_rs2,  "SpatialPolygonsDataFrame")
    iucn_pol$layer <- seq(1, length(iucn_pol))
    iucn_pol <- spTransform(iucn_pol, CRS(longlat))
    # Now to a sf object and create ONE BIG polygon that we can use to populate with PUs
    iucn_pol_sf <- st_as_sf(iucn_pol) %>% 
      dplyr::select(layer) %>% 
      summarise(total_layer = sum(layer, do_union = TRUE))
    # We can plot the object to see if it is correct
    #ggplot() +
      #geom_sf(data = iucn_pol_sf) # Looks GOOD!
  
    # Transform the High Seas object to a Pacific-centered projected shapefile  
      iucn <- iucn_pol_sf %>% 
        st_difference(polygon)
    # Perform transformation
      iucn_robinson <- iucn %>% 
        st_transform(crs = rob_pacific)
    # We can plot the object to see if it is correct
      #ggplot() +
        #geom_sf(data = iucn_robinson)
  
    # To fix the projection:
      bbox2 <-  st_bbox(iucn_robinson)
      bbox2[c(1,3)]  <-  c(-1e-5,1e-5)
      polygon3 <- st_as_sfc(bbox2)
      crosses2 <- iucn_robinson %>%
        st_intersects(polygon3) %>%
        sapply(length) %>%
        as.logical %>%
        which
    # Adding buffer 0
      iucn_robinson[crosses2, ] %<>%
        st_buffer(0) 
    # We can plot the object to see if it is correct
      #ggplot() +
        #geom_sf(data = iucn_robinson)
  
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



