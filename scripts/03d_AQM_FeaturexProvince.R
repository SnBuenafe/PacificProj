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

#############################
##### RUNS FOR AQUAMAPS #####
#############################

AQM_prov_run01 <- prov_intersect(path = NA,
                  pu_shp = "inputs/rdsfiles/PacificABNJGrid_05deg.rds",
                  fit = "none",
                  outdir = "outputs/AQM_wflow/03d_AQMxProv/",
                  data = "AQM",
                  feature_file = "outputs/AQM_wflow/03b_features/bycatch_features_surface.rds",
                  prov_file = "outputs/Provinces/PacificABNJGrid_05deg_Longhurst.rds"
                  )

#############################
##### TRY PLOTTING #####
#############################
library(ggplot2)
# to check if it works
ggplot()+
  geom_sf(data = AQM_prov_run01, aes(color = feature))

# to calculate for the total area per feature (per province)
total_PU_area <- 2667.6 * 31917
AQM_temp <- AQM_prov_run01 %>% 
  dplyr::group_by(feature) %>% 
  dplyr::summarise(total_area = sum(area_km2), percent_area = (sum(area_km2)/total_PU_area)*100)
