# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.
# most are modified from IBM's code.

# This code creates a function intersects the climate features (RCE and climate velocity values for different scenarios) and the study area (in PUs).
# creates a .rds file PUs x features layer.
# Function is found in code 06a

# Inputs include the following:
# input: layer to be intersected; e.g. "RCE" or "velocity"
# scenario: SSP126, SSP245, SSP585
# inpdir: directory where the layer is found; "inputs/rasterfiles/Costlayer/02-epipelagic_Cost_Raster_Sum.tif"
# outdir: directory where to save raster layers
# pu: PU .shp or .rds file; "inputs/shapefiles/PacificABNJGrid_05deg/PacificABNJGrid_05deg.shp"

source("scripts/06a_climatefeaturesfxn.R")

library(RColorBrewer)
library(patchwork)
library(sf)
library(proj4)
library(tidyverse)
# Defining palette
pal <- c("#313695","#4575b4","#74add1","#abd9e9","#e0f3f8","#fee090","#fdae61","#f46d43","#d73027","#a50026")
RCE_cat <- c("min","","","","","","","","","max")
velo_cat <- c("< -50", "-50 to -20", "-20 to -10", "-10 to -5", "-5 to 5", "5 to 10", "10 to 20", "20 to 50", "50 to 100", "100 to 200", "> 200")
pal1 <- rev(brewer.pal(11, "RdYlBu"))

rob_pacific <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" # Best to define these first so you don't make mistakes below
world_sf <- st_read("inputs/shapefiles/PacificCenterLand/PacificCenterLand.shp")
test<-cbind(c(140, -78, -78, 140), #TopLeft, TopRight, BottomRight, BottomLeft
            c( 51, 51, -60, -60))
Cnr <- proj4::project(test, proj = rob_pacific)
#make sure that the boundary limits are in line with the current projection
Bndry <- tibble(V1 = Cnr[1:2,1] , V2 = Cnr[1:2,2]) %>% # Start with N boundary (51N)
  bind_rows(as_tibble(project(as.matrix(tibble(x = -78, y = seq(51, -60, by = -1))), proj = rob_pacific))) %>% # Then bind to E boundary (-78E)
  bind_rows(as_tibble(project(as.matrix(tibble(x = 140, y = seq(-60, 51, by = 1))), proj = rob_pacific))) %>% # Then W boundary (140E) - reverse x order
  as.matrix() %>%
  list() %>%
  st_polygon() %>%
  st_sfc(crs = rob_pacific)

run01 <- layer_intersect(input = "RCE",
                scenario = "SSP126",
                inpdir = "inputs/rasterfiles/RCE/ssp126/02_EpipelagicLayer/02-ep_RCE_AEMean_ssp126_NA.tif", 
                outdir = "outputs/climate_features/RCE/", 
                pu = "inputs/shapefiles/PacificABNJGrid_05deg/PacificABNJGrid_05deg.shp"
                )

run01

# filtering out the lower quartile 
run01a <- run01 %>% 
  filter(value <= quantile(value, .25))
run01a

# Plotting RCE SSP126
p1 <- ggplot()+
  geom_sf(data = run01, aes(color = rce_categ)) +
  scale_color_gradientn(name = "RCE index",
                        colours = pal,
                        limits = c(1, 10),
                        breaks = seq(1, 10, 1),
                        labels = RCE_cat) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "SSP126") +
  theme_bw()

run02 <- layer_intersect(input = "RCE",
                         scenario = "SSP245",
                         inpdir = "inputs/rasterfiles/RCE/ssp245/02_EpipelagicLayer/02-ep_RCE_AEMean_ssp245_NA.tif", 
                         outdir = "outputs/climate_features/RCE/", 
                         pu = "inputs/shapefiles/PacificABNJGrid_05deg/PacificABNJGrid_05deg.shp"
                         )

run02

# Plotting RCE SSP245
p2 <- ggplot()+
  geom_sf(data = run02, aes(color = rce_categ)) +
  scale_color_gradientn(name = "RCE index",
                        colours = pal,
                        limits = c(1, 10),
                        breaks = seq(1, 10, 1),
                        labels = RCE_cat) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "SSP245") +
  theme_bw()

run03 <- layer_intersect(input = "RCE",
                         scenario = "SSP585",
                         inpdir = "inputs/rasterfiles/RCE/ssp585/02_EpipelagicLayer/02-ep_RCE_AEMean_ssp585_NA.tif", 
                         outdir = "outputs/climate_features/RCE/", 
                         pu = "inputs/shapefiles/PacificABNJGrid_05deg/PacificABNJGrid_05deg.shp"
)

run03

# Plotting RCE SSP585
p3 <- ggplot()+
  geom_sf(data = run03, aes(color = rce_categ)) +
  scale_color_gradientn(name = "RCE index",
                        colours = pal,
                        limits = c(1, 10),
                        breaks = seq(1, 10, 1),
                        aesthetics = c("color","fill"),
                        labels = RCE_cat) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "SSP585") +
  theme_bw()

run04 <- layer_intersect(input = "velocity",
                         scenario = "SSP126",
                         inpdir = "inputs/rasterfiles/VoCC_mag/ssp126/02_EpipelagicLayer/voccMag_02-ep_AEMean_ssp126_2050-2100.tif", 
                         outdir = "outputs/climate_features/velocity/", 
                         pu = "inputs/shapefiles/PacificABNJGrid_05deg/PacificABNJGrid_05deg.shp"
)

run04

# Plotting Climate Velocity SSP126
q1 <- ggplot()+
  geom_sf(data = run04, aes(color = velo_categ)) +
  scale_color_gradientn(name = expression('Climate velocity (km yr'^"-1"*')'),
                        colours = pal1,
                        limits = c(1, 11),
                        breaks = seq(1, 11, 1),
                        aesthetics = c("color","fill"),
                        labels = velo_cat) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "SSP126") +
  theme_bw()

run05 <- layer_intersect(input = "velocity",
                         scenario = "SSP245",
                         inpdir = "inputs/rasterfiles/VoCC_mag/ssp245/02_EpipelagicLayer/voccMag_02-ep_AEMean_ssp245_2050-2100.tif", 
                         outdir = "outputs/climate_features/velocity/", 
                         pu = "inputs/shapefiles/PacificABNJGrid_05deg/PacificABNJGrid_05deg.shp"
)

run05

# Plotting Climate Velocity SSP245
q2 <- ggplot()+
  geom_sf(data = run05, aes(color = velo_categ)) +
  scale_color_gradientn(name = expression('Climate velocity (km yr'^"-1"*')'),
                        colours = pal1,
                        limits = c(1, 11),
                        breaks = seq(1, 11, 1),
                        aesthetics = c("color","fill"),
                        labels = velo_cat) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "SSP245") +
  theme_bw()

run06 <- layer_intersect(input = "velocity",
                         scenario = "SSP585",
                         inpdir = "inputs/rasterfiles/VoCC_mag/ssp585/02_EpipelagicLayer/voccMag_02-ep_AEMean_ssp585_2050-2100.tif", 
                         outdir = "outputs/climate_features/velocity/", 
                         pu = "inputs/shapefiles/PacificABNJGrid_05deg/PacificABNJGrid_05deg.shp"
)

run06

# Plotting Climate Velocity SSP585

# Defining generalities
q3 <- ggplot()+
  geom_sf(data = run06, aes(color = velo_categ)) +
  scale_color_gradientn(name = expression('Climate velocity (km yr'^"-1"*')'),
                        colours = pal1,
                        limits = c(1, 11),
                        breaks = seq(1, 11, 1),
                        aesthetics = c("color","fill"),
                        labels = velo_cat) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "SSP585") +
  theme_bw()

# Arranging plots using patchwork

# Plotting RCE
plot_RCE <- (p1 | p2 | p3)
plot_RCE +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = 'i', tag_prefix = '(', tag_suffix = ')',
                  title = "RCE Plots") +
  ggsave("pdfs/RCE.pdf", width = 20, height = 10, dpi = 300)  

# Plotting Climate Velocity
plot_velo <- (q1 | q2 | q3)
plot_velo +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = 'i', tag_prefix = '(', tag_suffix = ')',
                  title = "Climate Velocity Plots") +
  ggsave("pdfs/ClimateVelo.pdf", width = 20, height = 10, dpi = 300)  
