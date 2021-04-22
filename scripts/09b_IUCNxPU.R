# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.
# most are modified from IBM's code.

# This code creates a function that intersects the IUCN sea turtle data (feature) with planning units (PUs).
# creates a .rds file PUs x features layer.
# Function is run at the bottom of the file.

# Inputs include the following:
# 1. input = IUCN .shp file;
# 2. pu_file = PUs .rds file;
# 3. outdir = path of the outputs of the function.

# Function is detailed in 10a_IUCNdata.R

source("scripts/10a_IUCNdata.R")

############################
# RUNNING THE CODE
############################

run29 <- iucn_int(input = "inputs/rasterfiles/IUCN/data_0.shp",
                  pu_file = "inputs/rdsfiles/PacificABNJGrid_05deg.rds",
                  outdir = "outputs/IUCN_wflow/10a_IUCNxPU/"
)

test <- readRDS("outputs/IUCN_wflow/10a_IUCNxPU/Lepidochelys_olivacea_IUCN.rds")

world_sf <- st_read("inputs/shapefiles/PacificCenterLand/PacificCenterLand.shp") %>% 
  st_transform(crs = rob_pacific)

ggplot() + 
  geom_sf(data = test, colour = "coral3") + 
  geom_sf(data = world_sf, fill = "grey20", colour = NA) +
  geom_sf(data = world_eez, size = 1, aes(color = Boundary), fill = NA, show.legend = FALSE) +
  scale_color_manual(values = "grey30") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  theme_bw()