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
solution_BOATS <- readRDS('outputs/08_Prioritizr/08c_penalty/spatial_plans/solve_SSP585_target90.rds')
ggplot() +
  geom_sf(data = solution_BOATS, aes(color = solution_1))
# check plot to see the problem when reprojecting
temp_sf <- solution_BOATS %>% 
  st_transform(crs = longlat)
ggplot() +
  geom_sf(data = temp_sf, aes(color = solution_1))

# Manipulating sf object, by removing a 0.5º wide strip of data on the dateline
# This was done because there were issues reprojecting from Robinson's (m) to longitude-latitude (degrees) due to the overlap on the dateline
manip_sf <- solution_BOATS %>% 
  dplyr::filter(!between(x, -CnR[1,2]/2, CnR[1,2]/2)) %>% 
  st_transform(crs = longlat)
# plot is fixed but some data is lost
ggplot() +
  geom_sf(data = manip_sf, aes(color = solution_1))

# Trial 2: no data lost, i presume
temp_sf2 <- solution_BOATS

bbox <- st_bbox(temp_sf2)
bbox[c(1,3)] <- c(-2.5, 2.5)
polygon <- st_as_sfc(bbox)

manip_sf2 <- temp_sf2 %>% 
  st_difference(polygon) %>% 
  st_transform(crs = longlat)

manip_sf2

pal_rich <- c("FALSE" = "lightsteelblue2", "TRUE" = "steelblue4")
ggplot() +
  geom_sf(data = manip_sf2, aes(fill = as.logical(solution_1)), color = 'grey64', size = 0.01) +
  scale_fill_manual(name = 'Solution',
                    values = pal_rich) +
  theme_bw()

# Saving files
final_sf <- manip_sf2 %>% 
  dplyr::select(cellsID, solution_1, geometry) %>% 
  rename(solution = solution_1)

saveRDS(final_sf, 'outputs/BOATS/solution_BOATS.rds')
st_write(final_sf, dsn = "outputs/BOATS/solution_BOATS", driver = "ESRI Shapefile", append = TRUE)
