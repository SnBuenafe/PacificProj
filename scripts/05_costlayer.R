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
      library(exactextractr)
      
      rob_pacific <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" # Best to define these first so you don't make mistakes below
      longlat <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
      
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
      
      # calling the raster layer of the cost layer
      epi_cost <- readAll(raster(input))
      crs(epi_cost) <- CRS(longlat)
      weight_rs <- raster::area(epi_cost)
      
      # Projecting the costs and weights into Robinson's (the same projection as the PUs)
      cost_filef <- projectRaster(cost_file, crs = CRS(rob_pacific), method = "ngb", over = FALSE, res = 2667.6)
      weight_rsf <- projectRaster(weight_rs, crs = CRS(rob_pacific), method = "ngb", over = FALSE, res = 2667.6)
      
      names(cost_filef) <- "layer"
      
      # Getting cost value by planning unit
      cost_bypu <- exact_extract(cost_filef, shp_PU_sf1, "weighted_mean", weights = weight_rsf)
      pu_file <- shp_PU_sf1 %>% 
        mutate(cost = cost_bypu, cost_log = log10(cost_bypu+1)) %>% 
        mutate(cost_categ = ifelse(cost_log == 0, 1,
                                   ifelse(cost_log > 0 & cost_log <= 1, 2,
                                          ifelse(cost_log > 1 & cost_log <= 2, 3,
                                                 ifelse(cost_log > 2 & cost_log <= 3, 4,
                                                        ifelse(cost_log > 3 & cost_log <= 4, 5, 6))))))
      
      # Saving RDS
      saveRDS(pu_file, paste0(outdir, "costlayer.rds"))
      
      return(pu_file)
  }
  
########################################
# RUNNING THE FUNCTION #
########################################
run00 <-  cost_pu(input = "inputs/rasterfiles/Costlayer/02-epipelagic_Cost_Raster_Sum.tif",
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

library(RColorBrewer)
library(patchwork)
# Defining palette
myPalette <- colorRampPalette(rev(brewer.pal(11, "RdYlBu")))
sc <- scale_colour_gradientn(name = "log10(cost)", colours = myPalette(100), limits=c(0, 4), aesthetics = c("color","fill"))

ggplot()+
  geom_sf(data = run00, aes(color = cost_log, fill = cost_log)) +
  sc +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Cost Layer") +
  theme_bw() +
  ggsave("pdfs/CostLayer.pdf", width = 20, height = 10, dpi = 300)
