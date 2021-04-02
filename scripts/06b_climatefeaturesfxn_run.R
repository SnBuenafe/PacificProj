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
# Defining palette
pal_rich <- rev(brewer.pal(9, "RdBu"))

run01 <- layer_intersect(input = "RCE",
                scenario = "SSP126",
                inpdir = "inputs/rasterfiles/RCE/ssp126/02_EpipelagicLayer/02-ep_RCE_AEMean_ssp126_NA.tif", 
                outdir = "outputs/climate_features/RCE/", 
                pu = "inputs/shapefiles/PacificABNJGrid_05deg/PacificABNJGrid_05deg.shp"
                )

run01

# Plotting RCE SSP126
p1 <- ggplot()+
  geom_sf(data = run01, aes(color = value)) +
  scale_color_gradientn(name = "RCE",
                        colours = pal_rich) +
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
  geom_sf(data = run02, aes(color = value)) +
  scale_color_gradientn(name = "RCE",
                        colours = pal_rich) +
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
  geom_sf(data = run03, aes(color = value)) +
  scale_color_gradientn(name = "RCE",
                        colours = pal_rich) +
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
  geom_sf(data = run04, aes(color = value)) +
  scale_color_gradientn(name = "Climate Velocity",
                        colours = pal_rich) +
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
  geom_sf(data = run05, aes(color = value)) +
  scale_color_gradientn(name = "Climate Velocity",
                        colours = pal_rich) +
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
  geom_sf(data = run06, aes(color = value)) +
  scale_color_gradientn(name = "Climate Velocity",
                       colours = pal_rich) +
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
  plot_annotation(tag_levels = 'i', tag_prefix = '(', tag_suffix = ')',
                  title = "RCE Plots") +
  ggsave("pdfs/RCE.pdf", width = 20, height = 10, dpi = 300)  

# Plotting Climate Velocity
plot_velo <- (q1 | q2 | q3)
plot_velo +
  plot_annotation(tag_levels = 'i', tag_prefix = '(', tag_suffix = ')',
                  title = "Climate Velocity Plots") +
  ggsave("pdfs/ClimateVelo.pdf", width = 20, height = 10, dpi = 300)  
