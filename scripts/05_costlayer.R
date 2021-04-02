# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.
# most are modified from IBM's code.

# This code intersects the cost layer and the study area (in PUs).
# creates a .rds file and .shp files of the PUs x cost layer.
# Inputs include the following:
# 1. input: raster file of the cost layer
# 2. pu_shp: .shp or .rds file of the PUs
# 3. outdir: path of the output
  
cost_pu <- function(input, pu_shp, outdir, ...) {

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
      
      # calling the raster layer of the cost layer
      epi_cost <- readAll(raster(input))
      
      # Creating a empty raster at 0.5° resolution (you can increase the resolution to get a better border precision)
      rs <- raster(ncol = 360*2, nrow = 180*2) 
      rs[] <- 1:ncell(rs)
      crs(rs) <- CRS(longlat)
      
      # Resampled to have the same resolution as study area (0.5 deg x 0.5 deg)
      epi_cost1 <- resample(epi_cost, rs, resample = "ngb")
      
      # Converting into an sf spatial polygon dataframe
      epi_cost2 <- as(epi_cost1, "SpatialPolygonsDataFrame")
      epi_cost_sp <- spTransform(epi_cost2, CRS(longlat))
      
      # Transforming the cost layer into Robinson's Projection
      # Define a long & slim polygon that overlaps the meridian line & set its CRS to match that of world
      polygon <- st_polygon(x = list(rbind(c(-0.0001, 90),
                                           c(0, 90),
                                           c(0, -90),
                                           c(-0.0001, -90),
                                           c(-0.0001, 90)))) %>%
        st_sfc() %>%
        st_set_crs(longlat)
      
      # Transform the species distribution polygon object to a Pacific-centred projection polygon object
      epi_cost_robinson <- epi_cost_sp %>% 
        st_as_sf() %>% 
        st_difference(polygon) %>% 
        st_transform(crs = rob_pacific)
      
      # There is a line in the middle of Antarctica. This is because we have split the map after reprojection. We need to fix this:
      bbox1 <-  st_bbox(epi_cost_robinson)
      bbox1[c(1,3)]  <-  c(-1e-5,1e-5)
      polygon1 <- st_as_sfc(bbox1)
      crosses1 <- epi_cost_robinson %>%
        st_intersects(polygon1) %>%
        sapply(length) %>%
        as.logical %>%
        which
      # Adding buffer 0
      epi_cost_robinson[crosses1, ] %<>%
        st_buffer(0)
      
      # Calling the PU shape file / .rds
      if(stringr::str_detect(string = pu_shp, pattern = ".rds") == TRUE) {
        shp_PU_sf <- readRDS(pu_shp)
      } else if (stringr::str_detect(string = pu_shp, pattern = ".shp") == TRUE) {
        shp_PU_sf <- st_read(pu_shp)
      }
      
      # transforming into robinson's projection
      shp_PU_sf <- shp_PU_sf %>% 
        st_transform(crs = rob_pacific)
      
      shp_PU_sf <- shp_PU_sf %>%
        dplyr::mutate (cellsID = 1:nrow(shp_PU_sf), 
                       area_km2 = as.numeric(st_area(shp_PU_sf)/1e+06)) %>% 
        dplyr::select(cellsID, geometry)
      pu_min_area <- min(shp_PU_sf$area_km2)
      
      # renaming cost layer
      single <- epi_cost_robinson %>% 
        rename(cost = X02.epipelagic_Cost_Raster_Sum)
      
      # Intersects every cost with planning unit region
      pu_int <- st_intersection(shp_PU_sf, single) %>% 
        filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) # we want just the polygons/multi not extra geometries
      
      xx_list <- st_join(x = shp_PU_sf, y = pu_int,  by = "cellsID") %>% 
        na.omit() %>% 
        dplyr::group_by(cellsID.x) %>% 
        dplyr::summarise(cellsID = unique(cellsID.x), cost = mean(cost)) %>% 
        dplyr::select(cellsID, geometry, cost) %>% 
        dplyr::mutate(area_km2 = as.numeric(st_area(geometry)/1e+06)) %>% 
        ungroup()
      
      # Saving RDS
      saveRDS(xx_list, paste0(outdir, "costlayer.rds"))

      return(xx_list)
  }
  
########################################
# RUNNING THE FUNCTION #
########################################
run01 <-  cost_pu(input = "inputs/rasterfiles/Costlayer/02-epipelagic_Cost_Raster_Sum.tif",
          pu_shp = "inputs/shapefiles/PacificABNJGrid_05deg/PacificABNJGrid_05deg.shp",
          outdir = "outputs/cost_layer/"
          )

###################################################
# PLOTTING THE INTERSECTION OF PUs AND COST LAYER #
###################################################

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

ggplot()+
  geom_sf(data = run01, aes(color = cost)) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  theme_bw()

