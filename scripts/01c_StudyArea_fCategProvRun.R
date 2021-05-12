# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.
# most are modified from IBM's code.

# This categorizes planning units according to their Longhurst Provinces.
# creates a .csv file, .rds file and .shp files of the PUs.
# Inputs include the following:
# 1. pu_file: .shp or .rds file of the planning units
# 2. province_file: .shp file of the Longhurst Provinces
# 3. prov_name: Longhurst
# 4. olayer: surface
# 5. outdir: path of the output

# Function fCategProv() is found in 01b.

source("scripts/01b_StudyArea_fCategProv.R")

###########################   
## RUNNING THE FUNCTION ###
###########################
longhurst_run01 <- fCategProv(pu_file = "outputs/01_StudyArea/01a_StudyArea/PacificABNJGrid_05deg.rds",
                              province_file = "inputs/shapefiles/Longhurst/Longhurst_world_v4_2010.shp", 
                              prov_name = "Longhurst",
                              olayer = "surface",
                              outdir = "outputs/01_StudyArea/01b_Longhurst/")
#longhurst_run01

###############   
## PLOTTING ###
###############
library(patchwork)
library(sf)
library(tidyverse)
library(proj4)
library(rgdal)

# Defining generalities
prov_code <- c("ANTA" = "#b2182b","ARCH" = "#ef8a62","CCAL" = "#fddbc7","CHIL" = "#d1e5f0","KURO" = "#67a9cf","No-Category" = "#2166ac",
               "NPPF" = "#8c510a","NPSW" = "#d8b365","NPTG" = "#f6e8c3", "PEQD" = "#c7eae5","PNEC" = "#5ab4ac","PSAE" = "#01665e",
               "PSAW" = "#762a83","SANT" = "#af8dc3","SPSG" = "#e7d4e8","SSTC" = "#d9f0d3","TASM" = "#7fbf7b","WARM" = "#1b7837")
# Boundary: 140E, 78W, 51N, 60S (input in degrees)
source("scripts/study_area/fCreateRobinsonBoundary.R")
Bndry <- fCreateRobinsonBoundary(west = 78, east = 140, north = 51, south = 60)
world_sf <- readRDS("outputs/01_StudyArea/01a_StudyArea/PacificCenterLand.rds")

# Plotting
longhurst <- ggplot()+
  geom_sf(data = longhurst_run01, aes(fill = province), colour = "grey64", size = 0.1) +
  scale_fill_manual(name = "Longhurst Provinces",
                    values = prov_code,
                    aesthetics = c("fill")) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  theme_bw()
#longhurst
#ggsave("pdfs/01_StudyArea/PacificABNJGrid_05deg_Longhurst.pdf", width = 20, height = 15, dpi = 300) 

########################################################
## Plotting with the Study Area (code copied from 01) ##
########################################################
PUs <- ggplot() +
    geom_sf(data = pacific_robinson, colour = NA, fill = NA, size = 0.2, show.legend = "line") +
    geom_sf(data = world_robinson, color = "grey20", fill="grey20", size = 0.1, show.legend = "line") +
    geom_sf(data = PUsPac, colour = "grey64", aes(fill = "ABNJ"), size = 0.1, show.legend = TRUE) + 
    scale_fill_manual(name = " ",
                      values = c("ABNJ" = "coral3")) +
    coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), # Set limits based on Bndry bbox
             ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
             expand = TRUE) +
    theme_bw()

plot <- PUs + longhurst
plot <- plot + plot_annotation(tag_levels = 'a', tag_suffix = ')',
                       title = "Study Area")
#plot
#ggsave("pdfs/01_StudyArea/StudyArea.pdf", width = 29.7, height = 21, dpi = 300)  
