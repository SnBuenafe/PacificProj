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
# input: species codes: YFT, BIG, ALB, SWO
# inpdir: directory where the layer is found in .csv format
# outdir: directory where to save raster layers (in .rds)
# pu: PU .shp file; "inputs/shapefiles/PacificABNJGrid_05deg/PacificABNJGrid_05deg.shp"

commercial_feat <- function(input, inpdir, prob_threshold, PU, outdir, ...) {

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

  rob_pacific <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" # Best to define these first so you don't make mistakes below
  longlat <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

  prob_threshold <- prob_threshold #top 20 percentile of YFT

  temp_sp <- read_csv(inpdir) %>% 
    dplyr::select(Latitude, Longitude, Preds) %>% 
    dplyr::filter(Preds > prob_threshold) %>% 
    rename(Prob = Preds)
  temp_sp <- temp_sp[,c(2,1,3)] %>% 
    data.frame()

  sp_raster <- rasterFromXYZ(temp_sp)

  # Creating a empty raster at 0.5° resolution (you can increase the resolution to get a better border precision)
  rs <- raster(ncol = 360*2, nrow = 180*2) 
  rs[] <- 1:ncell(rs)
  crs(rs) <- CRS(longlat)
  
  # Resampling to make sure that it's in the same resolution as sampling area
  sp_raster <- resample(sp_raster, rs, resample = "ngb")

  #converting into an sf spatial polygon dataframe
  sp_raster1 <- as(sp_raster, "SpatialPolygonsDataFrame")
  species_sp <- spTransform(sp_raster1, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

  # Define a long & slim polygon that overlaps the meridian line & set its CRS to match that of world
  polygon <- st_polygon(x = list(rbind(c(-0.0001, 90),
                                     c(0, 90),
                                     c(0, -90),
                                     c(-0.0001, -90),
                                     c(-0.0001, 90)))) %>%
    st_sfc() %>%
    st_set_crs(longlat)

  # Transform the species distribution polygon object to a Pacific-centred projection polygon object
  sp_robinson <- species_sp %>% 
    st_as_sf() %>% 
    st_difference(polygon) %>% 
    st_transform(crs = rob_pacific)

  # There is a line in the middle of Antarctica. This is because we have split the map after reprojection. We need to fix this:
  bbox1 <-  st_bbox(sp_robinson)
  bbox1[c(1,3)]  <-  c(-1e-5,1e-5)
  polygon1 <- st_as_sfc(bbox1)
  crosses1 <- sp_robinson %>%
    st_intersects(polygon1) %>%
    sapply(length) %>%
    as.logical %>%
    which
  # Adding buffer 0
  sp_robinson[crosses1, ] %<>%
    st_buffer(0)

  shp_PU_sf <- st_read(PU) %>% 
    st_transform(crs = rob_pacific)

  shp_PU_sf <- shp_PU_sf %>%
    dplyr::mutate (cellsID = 1:nrow(shp_PU_sf), 
                 area_km2 = as.numeric(st_area(shp_PU_sf)/1e+06)) %>% 
    dplyr::select(cellsID, geometry)
  pu_min_area <- min(shp_PU_sf$area_km2)

  single <- sp_robinson

  # Intersects every conservation feature with planning unit region
  pu_int <- st_intersection(shp_PU_sf, single) %>% 
    filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) # we want just the polygons/multi not extra geometries
  
  xx_list <- st_join(x = shp_PU_sf, y = pu_int,  by = "cellsID") %>% 
    na.omit() %>% 
    dplyr::group_by(cellsID.x) %>% 
    dplyr::summarise(cellsID = unique(cellsID.x), Prob = mean(Prob)) %>% 
    dplyr::select(cellsID, geometry, Prob) %>% 
    dplyr::mutate(area_km2 = as.numeric(st_area(geometry)/1e+06)) %>% 
    ungroup()

  # Saving RDS file of commercial species features
  saveRDS(xx_list, paste0(outdir, input, ".rds"))

return(xx_list)
}

###########################
# RUNNING  #
##########################
# Yellowfin Tuna
run07 <- commercial_feat(input = "YFT",
                         inpdir = "inputs/mercer/yft.csv",
                         prob_threshold = 0.1716593, #median of predictions of YFT
                         PU = "inputs/shapefiles/PacificABNJGrid_05deg/PacificABNJGrid_05deg.shp",
                         outdir = "outputs/commercial/")


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

# Plotting
library(RColorBrewer)
myPalette <- colorRampPalette(rev(brewer.pal(9, "BuGn")))
sc <- scale_colour_gradientn(name = "Probability of Spawning Area", 
                             colours = myPalette(100), 
                             limits=c(0, 0.5), 
                             aesthetics = c("color","fill"))

ggplot()+
  geom_sf(data = run07, aes(color = Prob, fill = Prob)) +
  sc +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  theme_bw()
