#code modified by Tin Buenafe, 2021 (tinbuenafe@gmail.com)

# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

# proj_type = moll_global <- "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs"
# proj_type = robin_global <- "+proj=robin +lon_0=0 +datum=WGS84 +units=m +no_defs"

# size of hexagons: 
# resolution 0.25 deg == grid_spacing(26860)
# resolution 0.5 deg == grid_spacing(53730)
# resolution 1 deg == grid_spacing(119300)

# size of squares: 
# resolution 0.25 deg == grid_spacing(25000)
# resolution 0.5 deg == grid_spacing(50000)
# resolution 1 deg == grid_spacing(111000)

library(raster)
library(sf)
library(dplyr)
library(magrittr)
library(rnaturalearth)
library(rnaturalearthdata)
library(fasterize)
library(ggplot2)
library(proj4) #needed for creating Bndry

#projections used
rob_pacific <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" # Best to define these first so you don't make mistakes below
longlat <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

#########################################################
# Create a land shapefile Pacific centered and projected  
#########################################################
# Using land mask for nature earth package to create a projected sf/shapefile object
world <- ne_countries(scale = 'small', returnclass = 'sf')
# Define a long & slim polygon that overlaps the meridian line & set its CRS to match # that of world
polygon <- st_polygon(x = list(rbind(c(-0.0001, 90),
                                     c(0, 90),
                                     c(0, -90),
                                     c(-0.0001, -90),
                                     c(-0.0001, 90)))) %>%
  st_sfc() %>%
  st_set_crs(4326)

# Modify world dataset to remove overlapping portions with world's polygons
world2 <- world %>% 
  st_difference(polygon)
# Perform transformation on modified version of world dataset
world_robinson <- world2 %>% 
  st_transform(crs = rob_pacific)
# Check the plot if just in case
ggplot() +
  geom_sf(data = world_robinson) 
# notice that there is a line in the middle of Antarctica. This is because we have
# split the map after reprojection. We need to fix this:

# Fix those extra boundaries
bbox <-  st_bbox(world_robinson)
bbox[c(1,3)]  <-  c(-1e-5,1e-5)
polygon2 <- st_as_sfc(bbox)
crosses <- world_robinson %>%
  st_intersects(polygon2) %>%
  sapply(length) %>%
  as.logical %>%
  which
# Adding buffer 0
world_robinson[crosses,] %<>%
  st_buffer(0) 
# Check the plot again
ggplot() +
  geom_sf(data = world_robinson) # OK now looks better!
# Save the object
# st_write(world_robinson, dsn = "files/shapefiles/PacificCenterLand", driver = "ESRI Shapefile")


##########################################################################################
# Create HighSeas shapefile Pacific centered that could be used to create planning units
##########################################################################################
# Land mask to inverted later
land <- world
# Creating a empty raster at 0.5° resolution (you can increase the resolution to get a better border precision)
rs <- raster(ncol = 720, nrow = 360) 
rs[] <- 1:ncell(rs)
geo.prj <- longlat
crs(rs) <- CRS(geo.prj)
# Fasterize the land object
land_rs <- fasterize(land, rs)
land_rs[] <- ifelse(is.na(land_rs[]), 1, NA) # only ocean cells!
land_rs <- setValues(raster(land_rs), land_rs[])

# Reading EEZ
eez <- st_read("files/shapefiles/World_EEZ_v11_20191118/eez_v11.shp") %>% 
  filter(SOVEREIGN1 != "Antarctica") # Antarctica HAS EEZs so we must exclude the EEZ region from the shp data
eez_sp <- as(eez, "Spatial")
# Creating the final raster
abnj_rs <- mask(land_rs, eez_sp, inverse = TRUE)
# We can plot the object to see if it is correct
plot(abnj_rs) 
# looks OK but there are some land pixels that should not be there 
# If you convert this to a polygon you would end up with "unwanted" land polyong

# A process to delete certain agrupation of pixels
abnj_clump <- clump(abnj_rs, directions = 8) 
# Get frequency table    
df_clump <- freq(abnj_clump) %>% 
  as.data.frame()
# which rows of the data.frame are only represented by clumps under 9 pixels?
str(which(df_clump$count <= 9))
# which values do these correspond to?
str(df_clump$value[which(df_clump$count <= 9)])
# put these into a vector of clump ID's to be removed
excludeID <- df_clump$value[which(df_clump$count <= 9)]
# make a new raster to be sieved
abnj_rs2 <- abnj_clump
# assign NA to all clumps whose IDs are found in excludeID
abnj_rs2[abnj_rs2 %in% excludeID] <- NA
# We can plot the object to see if it is correct
plot(abnj_rs2)

# From Raster to Polygon
abnj_pol <- as(abnj_rs2,  "SpatialPolygonsDataFrame")
abnj_pol$layer <- seq(1, length(abnj_pol))
abnj_pol <- spTransform(abnj_pol, CRS(geo.prj))
# Now to a sf object and create ONE BIG polygon that we can use to populate with PUs
abnj_pol_sf <- st_as_sf(abnj_pol) %>% 
  select(layer) %>% 
  summarise(total_layer = sum(layer, do_union = TRUE))
# We can plot the object to see if it is correct
ggplot() +
  geom_sf(data = abnj_pol_sf) # Looks GOOD!


# Transform the High Seas object to a Pacific-centered projected shapefile  
abnj <- abnj_pol_sf %>% 
  st_difference(polygon)
# Perform transformation
abnj_robinson <- abnj %>% 
  st_transform(crs = rob_pacific)
# We can plot the object to see if it is correct
ggplot() +
  geom_sf(data = abnj_robinson) # Looks weird and also there is some lines due the Split process

# To fix it the same code as above
bbox2 <-  st_bbox(abnj_robinson)
bbox2[c(1,3)]  <-  c(-1e-5,1e-5)
polygon3 <- st_as_sfc(bbox2)
crosses2 <- abnj_robinson %>%
  st_intersects(polygon3) %>%
  sapply(length) %>%
  as.logical %>%
  which
# Adding buffer 0
abnj_robinson[crosses2, ] %<>%
  st_buffer(0) 
# We can plot the object to see if it is correct
ggplot() +
  geom_sf(data = abnj_robinson) # Looks better
# Save the object
# st_write(abnj_robinson, dsn = "files/shapefiles/PacificCenterABNJ", driver = "ESRI Shapefile")

##########################################################################################
# Create equal-size grids (adapted from Jase's Code)
##########################################################################################

#load Create_Planning Units function from Jase (modified by Tin; modifications found within the function itself)
fCreate_PlanningUnits <- function(Bndry, LandMass, CellArea, Shape){
  
  if(Shape %in% c("hexagon", "Hexagon")){
    sq <- FALSE
    diameter <- 2 * sqrt((CellArea*1e6)/((3*sqrt(3)/2))) * sqrt(3)/2 # Diameter in m's
  }
  
  if(Shape %in% c("square", "Square")){
    sq < TRUE
    diameter <- sqrt(CellArea*1e6) # Diameter in m's
  }
  
  # First create planning units for the whole region
  PUs <- st_make_grid(Bndry,
                      square = sq,
                      cellsize = c(diameter, diameter),
                      what = "polygons") %>%
    st_sf()
  
  # Check cell size worked ok.
  print(paste0("Range of cellsize are ",
               round(as.numeric(range(units::set_units(st_area(PUs), "km^2")))[1])," km2 to ",
               round(as.numeric(range(units::set_units(st_area(PUs), "km^2")))[2])," km2")) # Check area
  
  # First get all the PUs partially/wholly within the planning region
  logi_Reg <- st_centroid(PUs) %>%
    st_intersects(Bndry) %>%
    lengths > 0 # Get logical vector instead of sparse geometry binary
  PUs <- PUs[logi_Reg == TRUE, ]
  
  # Second, get all the pu's with < 50 % area on land (approximated from the centroid)
  logi_Ocean <- st_centroid(PUs) %>%
    st_intersects(LandMass) %>%
    lengths > 0 # Get logical vector instead of sparse geometry binary
  PUs <- PUs[logi_Ocean==TRUE, ] #modified from ==FALSE to TRUE because LandMass = ABNJ areas
  
  return(PUs)
}

#first we need to get the xy coordinates of the boundaries of the study area
#for me it will be the areal boundaries of the tuna-fisheries RFMOs: IATTC and WCPFC (140E, 78W; 51N, 60S)
test<-cbind(c(140, -78, -78, 140), #TopLeft, TopRight, BottomRight, BottomLeft
            c( 51, 51, -60, -60))
Cnr <- project(test, proj = rob_pacific)
print(Cnr)

#then we create the parameters for the function fCreate_PlanningUnits (Bndry, LandMass, CellArea, Shape)

#make sure that the boundary limits are in line with the current projection
Bndry <- tibble(V1 = Cnr[1:2,1] , V2 = Cnr[1:2,2]) %>% # Start with N boundary (51N)
  bind_rows(as_tibble(project(as.matrix(tibble(x = -78, y = seq(51, -60, by = -1))), proj = rob_pacific))) %>% # Then bind to E boundary (-78E)
  bind_rows(as_tibble(project(as.matrix(tibble(x = 140, y = seq(-60, 51, by = 1))), proj = rob_pacific))) %>% # Then W boundary (140E) - reverse x order
  as.matrix() %>%
  list() %>%
  st_polygon() %>%
  st_sfc(crs = rob_pacific)

LandMass <- abnj_robinson

#check if the boundary is positioned right
ggplot() +
  geom_sf(data = world_robinson, colour = "grey20", fill="grey20", size = 0.1, show.legend = "line") +
  geom_sf(data = LandMass, colour = "grey66", fill = "grey66", size = 0.2, show.legend = "line") +
  geom_sf(data = Bndry, colour = "black", fill = NA, size = 0.3, show.legend = "line") +
  ggsave("pdfs/PacificABNJBoundaries.jpg", width = 20, height = 15, dpi = 300)
  
#size of hexagons in km^2
#approximately 0.1 deg = 11.1km
#get the approximate area using the apothem (r) in https://www.omnicalculator.com/math/hexagon
#0.25 deg resolution == 669.9 km^2
#0.50 deg resolution == 2667.6 km^2
#0.10 deg resolution == 10670.0 km^2

CellArea <- 2667.6 # kms2 for 0.5 degree resolution
Shape = "Hexagon" # Hexagon or Square

PUsPac <- fCreate_PlanningUnits(Bndry, LandMass, CellArea, Shape)
st_write(PUsPac, dsn = "files/shapefiles/PacificABNJGrid_05deg", driver = "ESRI Shapefile") #saving the study area
#print(PUsPac) #to know how many features/polygons

#plotting the study area with the planning units
ggplot() +
  geom_sf(data = LandMass, colour = NA, fill = NA, size = 0.2, show.legend = "line") +
  geom_sf(data = world_robinson, color = "grey20", fill="grey20", size=0.1, show.legend="line") +
  geom_sf(data = PUsPac, colour = "black", fill = NA, size = 0.1, show.legend = "line") + 
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), # Set limits based on Bndry bbox
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) #+
#  ggsave("pdfs/PacificABNJGrid_05deg.jpg", width = 20, height = 15, dpi = 300)
