# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.

# This code creates the study area (boundaries, grid) for the PacificProj.

##################################
### Defining the main packages ###
##################################
library(raster)
library(sf)
library(tidyverse)
library(magrittr)
library(rnaturalearth)
library(proj4)

#######################################################
### Defining the generalities and calling functions ###
#######################################################
rob_pacific <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" # Best to define these first so you don't make mistakes below
longlat <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

# different functions for creating the study area
source("scripts/study_area/fConvert2PacificRobinson.R")
source("scripts/study_area/fCreateSinglePolygon.R")
source("scripts/study_area/fCreateMaskedPolygon.R")
source("scripts/study_area/fCreate_PlanningUnits.R")

#######################################
### Polygons of seas in the Pacific ###
#######################################
ocean_sf <- ne_download(scale = "large", category = "physical", type = "geography_marine_polys", returnclass = "sf") %>% 
  filter(name %in% c("North Pacific Ocean", "South Pacific Ocean", "Philippine Sea", "Coral Sea", "Tasman Sea", "South China Sea", 
                     "Sea of Japan", "Sea of Okhotsk", "Celebes Sea", "Sulu Sea", "Banda Sea", "Luzon Strait", "Java Sea", 
                     "Yellow Sea", "East China Sea", "Arafura Sea", "Timor Sea", "Gulf of Thailand", "Gulf of Carpentaria", 
                     "Bay of Plenty", "Molucca Sea", "Bismarck Sea", "Solomon Sea", "Gulf of Tonkin", "Strait of Singapore", 
                     "Makassar Strait", "Ceram Sea", "Korea Strait", "Inner Sea", "Taiwan Strait", "Shelikhova Gulf", "Bo Hai", 
                     "Great Barrier Reef", "Bering Sea", "Gulf of Alaska", "Kronotskiy Gulf", "Uda Bay", "Uchiura Bay", 
                     "Tsugaru Strait", "Tatar Strait", "La Pérouse Strait", "East Korea Bay", "Qiongzhou Strait", "Cook Strait", 
                     "Torres Strait", "Gulf of Papua", "Hangzhou Bay", "Karaginskiy Gulf", "Gulf of Kamchatka", "Joseph Bonaparte Gulf", 
                     "Gulf of Sakhalin", "Bali Sea", "Davao Gulf", "Halmahera Sea", "Selat Bali", "Gulf of Tomini", "Flores Sea", 
                     "Sibuyan Sea", "Selat Dampier", "Gulf of Buli", "Gulf of Kau", "Bohol Sea", "Surigao Strait", "Ragay Gulf", 
                     "Samar Sea", "Tayabas Bay", "Leyte Gulf", "Visayan Sea", "Savu Sea", "Yangtze River", "Gulf of Anadyr'", 
                     "Golfo de California", "Cook Inlet", "Queen Charlotte Sound", "Bristol Bay", "Dixon Entrance", "Norton Sound", 
                     "Prince William Sound", "Smith Sound", "Queen Charlotte Strait", "Baird Inlet", "Hecate Strait", "Cordova Bay", "Columbia River",
                     "Salish Sea", "Golfo de Panamá", "Golfo Corcovado", "Golfo de Penas", "Golfo de Guayaquil", "Golfo de Tehuantepec",
                     "Dixon Entrance", "Smith Sound", "Queen Charlotte Strait", "Cordova Bay" ))
# "Chukchi Sea""Gulf of Olen‘k""Chaun Bay", "Ozero Mogotoyevo","Guba Gusinaya", "Strait of Malacca"

############################################################
# Creating Pacific-centered and Robinson projected land .shp
############################################################
# Using land mask for nature earth package to create a projected sf/shapefile object
world <- ne_countries(scale = 'small', returnclass = 'sf')

world_robinson <- fConvert2PacificRobinson(world)
saveRDS(world_robinson, "outputs/01_StudyArea/01a_StudyArea/PacificCenterLand.rds")
# st_write(world_robinson, dsn = "files/shapefiles/PacificCenterLand", driver = "ESRI Shapefile")

#############################################################
# Creating Pacific-centered and Robinson projected ABNJ .shp
#############################################################
res = 0.5
# Data from Marine Regions
mask = st_read("inputs/shapefiles/World_EEZ_v11_20191118/eez_v11.shp") %>% 
  filter(SOVEREIGN1 != "Antarctica")
inverse = TRUE
pacific_robinson <- fCreateMaskedPolygon(ocean_sf, res, mask, inverse) %>% 
  fConvert2PacificRobinson()

saveRDS(pacific_robinson, "outputs/01_StudyArea/01a_StudyArea/PacificCenterABNJ.rds")
# st_write(pacific_robinson, dsn = "inputs/shapefiles/PacificCenterABNJ", driver = "ESRI Shapefile", append = TRUE)

##################################
# Creating Pacific-centered EEZs
##################################
res = 0.5
# Data from Marine Regions
mask = st_read("inputs/shapefiles/World_EEZ_v11_20191118/eez_v11.shp") %>% 
  filter(SOVEREIGN1 != "Antarctica")
inverse = FALSE
eez_robinson <- fCreateMaskedPolygon(ocean_sf, res, mask, inverse) %>% 
  fConvert2PacificRobinson()

saveRDS(eez_robinson, "outputs/01_StudyArea/01a_StudyArea/PacificCenterEEZ.rds")
# st_write(eez_robinson, dsn = "inputs/shapefiles/PacificCenterEEZ", driver = "ESRI Shapefile", append = TRUE)

#######################################################
# Creating equal-sized grids (adapted from Jase's Code)
#######################################################
# Boundary: 140E, 78W, 51N, 60S (input in degrees)
source("scripts/study_area/fCreateRobinsonBoundary.R")
Bndry <- fCreateRobinsonBoundary(west = 78, east = 140, north = 51, south = 60)

# LandMass
LandMass <- pacific_robinson
# LandMass <- readRDS("inputs/rdsfiles/PacificCenterABNJ.rds")
  
# CellArea & Shape
#size of hexagons in km^2
#approximately 0.1 deg = 11.1km
#get the approximate area using the apothem (r) in https://www.omnicalculator.com/math/hexagon
#0.25 deg resolution == 669.9 km^2
#0.50 deg resolution == 2667.6 km^2
#0.10 deg resolution == 10670.0 km^2

CellArea <- 2667.6 # kms2 for 0.5 degree resolution
Shape = "Hexagon" # Hexagon or Square

# Running function
PUsPac <- fCreate_PlanningUnits(Bndry, LandMass, CellArea, Shape)
#saving the study area

saveRDS(PUsPac, file = "outputs/01_StudyArea/01a_StudyArea/PacificABNJGrid_05deg.rds")
#st_write(PUsPac, dsn = "inputs/shapefiles/PacificABNJGrid_05deg", driver = "ESRI Shapefile", append = FALSE)

##############
## Plotting ##
##############
study_area <- ggplot() +
    geom_sf(data = pacific_robinson, colour = NA, fill = NA, size = 0.2, show.legend = FALSE) +
    geom_sf(data = world_robinson, color = "grey20", fill="grey20", size = 0.1, show.legend = FALSE) +
    geom_sf(data = PUsPac, colour = "grey64", aes(fill = "ABNJ"), size = 0.1, show.legend = FALSE) + 
    scale_fill_manual(name = "Study Area",
      values = c("ABNJ" = "steelblue4")) +
    coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), # Set limits based on Bndry bbox
            ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
            expand = TRUE) +
    theme_bw() +
    theme(axis.text.x = element_text(size = 25),
          axis.text.y = element_text(size = 25))
study_area +
  labs(caption = 'shapefiles from Flanders Marine Institute (2019)')
ggsave("pdfs/01_StudyArea/PacificABNJGrid_05deg.pdf", width = 20, height = 15, dpi = 300)
ggsave("pdfs/01_StudyArea/PacificABNJGrid_05deg.png", width = 20, height = 10, dpi = 600)

inset_studyarea <- ggplot() +
  geom_sf(data = pacific_robinson, colour = NA, fill = NA, size = 0.2, show.legend = FALSE) +
  geom_sf(data = world_robinson, color = "grey20", fill="grey20", size = 0.1, show.legend = FALSE) +
  geom_sf(data = PUsPac, colour = "grey64", aes(fill = "ABNJ"), size = 0.1, show.legend = FALSE) + 
  scale_fill_manual(name = "Study Area",
                    values = c("ABNJ" = "steelblue4")) +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin/5, st_bbox(Bndry)$xmax/5), # Set limits based on Bndry bbox
           ylim = c(st_bbox(Bndry)$ymin/5, st_bbox(Bndry)$ymax/5),
           expand = TRUE) +
  theme_bw()
ggsave("pdfs/01_StudyArea/PacificABNJGrid_05deg_inset.png", width = 20, height = 15, dpi = 600)
