# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.
# most are modified from IBM's code.

# prov_intersect function generally intersects the features with the provinces.
# it can intersect commercial and bycatch features (IUCN and AQM)
# code creates an .rds file of the feature x province

# Function requires the following inputs:
# 1. path: where the .rds files for each of the features are located (relevant only for commercial and IUCN)
# 2. pu_shp: .shp or .rds file of the PUs
# 3. fit: "global", "pacific", or "none"
# 4. outdir: path/directory where the .rds file should be saved
# 5. data: "commercial", "IUCN" or "AQM"
# 6. feature_file: .rds file with all the features together (relevant only for AQM)
# 7. prov_file: .rds file of the Longhurst provinces

source("scripts/03c_FeatProvFxn.R")

###############################
##### RUNS FOR COMMERCIAL #####
###############################

COMMERCIAL_prov_run01 <- prov_intersect(path = "outputs/commercial/04b_CommercialPredictions",
  pu_shp = "inputs/shapefiles/PacificABNJGrid_05deg/PacificABNJGrid_05deg.shp",
  fit = "global",
  outdir = "outputs/commercial/04d_CommercialxProvince/global/",
  data = "commercial",
  feature_file = NA,
  prov_file = "outputs/Provinces/PacificABNJGrid_05deg_Longhurst.rds"
)

COMMERCIAL_prov_run02 <- prov_intersect(path = "outputs/commercial/04b_CommercialPredictions",
  pu_shp = "inputs/shapefiles/PacificABNJGrid_05deg/PacificABNJGrid_05deg.shp",
  fit = "pacific",
  outdir = "outputs/commercial/04d_CommercialxProvince/pacific/",
  data = "commercial",
  feature_file = NA,
  prov_file = "outputs/Provinces/PacificABNJGrid_05deg_Longhurst.rds"
)

####################
##### PLOTTING #####
####################

# checking if it works
ggplot() +
  geom_sf(data = COMMERCIAL_prov_run01, aes(color = feature, fill = feature))
# checking if it works
ggplot() +
  geom_sf(data = COMMERCIAL_prov_run02, aes(color = feature, fill = feature))
# both work!

library(wesanderson)
library(RColorBrewer)
nb.cols <- 34
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)

ggplot() +
  geom_sf(data = COMMERCIAL_prov_run02, aes(color = feature, fill = feature)) +
  scale_color_manual(values = mycolors) +
  scale_fill_manual(values = mycolors) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Commercial Species") +
  theme_bw()

