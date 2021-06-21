# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.

# This code creates a function intersects the climate features (RCE and climate velocity values for different scenarios) and the study area (in PUs).
# creates a .rds file PUs x features layer.
# Function is found in code 05a

# Inputs include the following:
# input: layer to be intersected; e.g. "RCE" or "velocity"
# scenario: SSP126, SSP245, SSP585
# inpdir: directory where the layer is found; "inputs/rasterfiles/Costlayer/02-epipelagic_Cost_Raster_Sum.tif"
# outdir: directory where to save raster layers
# pu: PU .shp or .rds file; "inputs/shapefiles/PacificABNJGrid_05deg/PacificABNJGrid_05deg.shp"

source("scripts/05a_Climate_fClimateInt.R")

############################################
#### Defining generalities for plotting ####
############################################
library(RColorBrewer)
library(patchwork)
library(sf)
library(proj4)
library(tidyverse)

rob_pacific <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" # Best to define these first so you don't make mistakes below
world_sf <- st_read("inputs/shapefiles/PacificCenterLand/PacificCenterLand.shp")

source("scripts/study_area/fCreateRobinsonBoundary.R")
Bndry <- fCreateRobinsonBoundary(west = 78, east = 140, north = 51, south = 60)

# Defining palette and legends
pal <- c("#313695","#4575b4","#74add1","#abd9e9","#e0f3f8","#fee090","#fdae61","#f46d43","#d73027","#a50026")
RCE_cat <- c("min","","","","","","","","","max")
velo_cat <- c("< -50", "-50 to -20", "-20 to -10", "-10 to -5", "-5 to 5", "5 to 10", "10 to 20", "20 to 50", "50 to 100", "100 to 200", "> 200")
pal1 <- rev(brewer.pal(11, "RdYlBu"))

#########################
#### Running for RCE ####
#########################
RCE_SSP126 <- fClimateInt(input = "RCE",
                scenario = "SSP126",
                inpdir = "inputs/rasterfiles/RCE/ssp126/02_EpipelagicLayer/02-ep_RCE_AEMean_ssp126_NA.tif", 
                outdir = "outputs/05_Climate/RCE/", 
                pu = "outputs/01_StudyArea/01a_StudyArea/PacificABNJGrid_05deg.rds"
)
RCE_SSP245 <- fClimateInt(input = "RCE",
                          scenario = "SSP245",
                          inpdir = "inputs/rasterfiles/RCE/ssp245/02_EpipelagicLayer/02-ep_RCE_AEMean_ssp245_NA.tif", 
                          outdir = "outputs/05_Climate/RCE/", 
                          pu = "outputs/01_StudyArea/01a_StudyArea/PacificABNJGrid_05deg.rds"
)
RCE_SSP585 <- fClimateInt(input = "RCE",
                          scenario = "SSP585",
                          inpdir = "inputs/rasterfiles/RCE/ssp585/02_EpipelagicLayer/02-ep_RCE_AEMean_ssp585_NA.tif", 
                          outdir = "outputs/05_Climate/RCE/", 
                          pu = "outputs/01_StudyArea/01a_StudyArea/PacificABNJGrid_05deg.rds"
)

# plots for RCE
plotRCE_SSP126 <- ggplot()+
                      geom_sf(data = RCE_SSP126, aes(color = rce_categ)) +
                      scale_color_gradientn(name = "RCE index",
                                            colours = pal,
                                            limits = c(1, 10),
                                            breaks = seq(1, 10, 1),
                                            labels = RCE_cat) +
                      geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
                      coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
                               ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
                               expand = TRUE) +
                      labs(title = "SSP-1.26") +
                      theme_bw()
plotRCE_SSP245 <- ggplot()+
                      geom_sf(data = RCE_SSP245, aes(color = rce_categ)) +
                      scale_color_gradientn(name = "RCE index",
                                            colours = pal,
                                            limits = c(1, 10),
                                            breaks = seq(1, 10, 1),
                                            labels = RCE_cat) +
                      geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
                      coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
                               ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
                               expand = TRUE) +
                      labs(title = "SSP-2.45") +
                      theme_bw()
plotRCE_SSP585 <- ggplot()+
                      geom_sf(data = RCE_SSP585, aes(color = rce_categ)) +
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
                      labs(title = "SSP-5.85") +
                      theme_bw()

##############################
#### Running for Velocity ####
##############################
velo_SSP126 <- fClimateInt(input = "velocity",
                         scenario = "SSP126",
                         inpdir = "inputs/rasterfiles/VoCC_mag/ssp126/02_EpipelagicLayer/voccMag_02-ep_AEMean_ssp126_2050-2100.tif", 
                         outdir = "outputs/05_Climate/Velocity/", 
                         pu = "outputs/01_StudyArea/01a_StudyArea/PacificABNJGrid_05deg.rds"
)
velo_SSP245 <- fClimateInt(input = "velocity",
                           scenario = "SSP245",
                           inpdir = "inputs/rasterfiles/VoCC_mag/ssp245/02_EpipelagicLayer/voccMag_02-ep_AEMean_ssp245_2050-2100.tif", 
                           outdir = "outputs/05_Climate/Velocity/", 
                           pu = "outputs/01_StudyArea/01a_StudyArea/PacificABNJGrid_05deg.rds"
)
velo_SSP585 <- fClimateInt(input = "velocity",
                           scenario = "SSP585",
                           inpdir = "inputs/rasterfiles/VoCC_mag/ssp585/02_EpipelagicLayer/voccMag_02-ep_AEMean_ssp585_2050-2100.tif", 
                           outdir = "outputs/05_Climate/Velocity/", 
                           pu = "outputs/01_StudyArea/01a_StudyArea/PacificABNJGrid_05deg.rds"
)

# plots for climate velocity
plotvelo_SSP126 <- ggplot()+
                      geom_sf(data = velo_SSP126, aes(color = velo_categ)) +
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
                      labs(title = "SSP-1.26") +
                      theme_bw()
plotvelo_SSP245 <- ggplot()+
                      geom_sf(data = velo_SSP245, aes(color = velo_categ)) +
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
                      labs(title = "SSP-2.45") +
                      theme_bw()
plotvelo_SSP585 <- ggplot()+
                      geom_sf(data = velo_SSP585, aes(color = velo_categ)) +
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
                      labs(title = "SSP-5.85") +
                      theme_bw()

###################################
#### Plotting RCE and velocity ####
###################################
# Plotting RCE
plot_RCE <- (plotRCE_SSP126 | plotRCE_SSP245 | plotRCE_SSP585)
plot_RCE +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = 'A', tag_suffix = ')') +
  labs(caption = 'temperature data from GCMs of CMIP6')
ggsave("pdfs/05_Climate/RCE.pdf", width = 20, height = 10, dpi = 300)  

# Plotting Climate Velocity
plot_velo <- (plotvelo_SSP126 | plotvelo_SSP245 | plotvelo_SSP585)
plot_velo +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = 'A', tag_suffix = ')') +
  labs(caption = 'temperature data from GCMs of CMIP6')
ggsave("pdfs/05_Climate/ClimateVelo.pdf", width = 20, height = 10, dpi = 300)  
