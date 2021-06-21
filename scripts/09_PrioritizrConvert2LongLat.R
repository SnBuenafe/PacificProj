# Defining libraries needed
library(sf)
library(tidyverse)
library(proj4)

# Defining generalities for file manipulation
rob_pacific <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" # Best to define these first so you don't make mistakes below
longlat <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
test <- cbind(c('180','0.5'))
CnR <- proj4::project(test, proj = rob_pacific)

# Calling file
solution60_SSP585 <- readRDS('outputs/10_Prioritizr/10h-i_IUCNRuns_NoProv/05_Target60/solution_SSP585.rds')
ggplot() +
  geom_sf(data = solution60_SSP585, aes(color = solution_1))
# check plot to see the problem when reprojecting
temp_sf <- solution60_SSP585 %>% 
  st_transform(crs = longlat)
ggplot() +
  geom_sf(data = temp_sf, aes(color = solution_1))

# Manipulating sf object, by removing a 0.5º wide strip of data on the dateline
# This was done because there were issues reprojecting from Robinson's (m) to longitude-latitude (degrees) due to the overlap on the dateline
manip_sf <- solution60_SSP585 %>% 
  dplyr::filter(!between(x, -CnR[1,2]/2, CnR[1,2]/2)) %>% 
  st_transform(crs = longlat)
# plot is fixed but some data is lost
ggplot() +
  geom_sf(data = manip_sf, aes(color = solution_1))

# Trial 2
temp_sf2 <- solution60_SSP585

bbox <- st_bbox(temp_sf2)
bbox[c(1,3)] <- c(-2.5, 2.5)
polygon <- st_as_sfc(bbox)

manip_sf2 <- temp_sf2 %>% 
  st_difference(polygon) %>% 
  st_transform(crs = longlat)

manip_sf2

ggplot() +
  geom_sf(data = manip_sf2, aes(color = solution_1)) +
  theme_bw()

# Saving files
final_sf <- manip_sf2 %>% 
  dplyr::select(cellsID, solution_1, geometry) %>% 
  rename(solution = solution_1)

saveRDS(final_sf, 'outputs/BOATS/solution60_SSP585.rds')
st_write(final_sf, dsn = "outputs/BOATS/solution60_SSP585", driver = "ESRI Shapefile", append = TRUE)
