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

source("scripts/03_Features_fProvIntersect.R")

#########################
##### RUNS FOR IUCN #####
#########################

IUCN_prov_run01 <- fProvIntersect(path = "outputs/04_IUCN/04b_fIUCNIntersect/",
    pu_shp = "outputs/01_StudyArea/01a_StudyArea/PacificABNJGrid_05deg.rds",
    fit = "none",
    outdir = "outputs/04_IUCN/04c_fProvIntersect/",
    data = "IUCN",
    feature_file = NA,
    prov_file = "outputs/01_StudyArea/01b_Longhurst/PacificABNJGrid_05deg_Longhurst.rds"
)

####################
##### PLOTTING #####
####################

# checking if it works
ggplot() +
  geom_sf(data = IUCN_prov_run01, aes(color = feature, fill = feature))