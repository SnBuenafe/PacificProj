# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.
# most are modified from IBM's code.

# This uses the same function as the function that intersects AquaMaps distribution maps with the PUs.
# See 03_Features_fFeaturesInt.R for full function,
# and 03b_AQM_fFeaturesIntRun.R for the runs for AquaMaps
# What this code does is it intersects the commercial features with the PUs
# This makes distribution maps.

# This is just an extra script and not really part of the workflow.
# 05e script automatically makes distribution maps per new feature (Province x Commercial spp.)

source("scripts/03_Features_fFeaturesInt.R")

global_features <- fFeaturesInt(path = "outputs/05_Commercial/05b_fCommercialFeat/",
                                outdir = "outputs/05_Commercial/05d_fFeaturesInt/Global/",
                                pu_shp = "outputs/01_StudyArea/01a_StudyArea/PacificABNJGrid_05deg.rds",
                                data = "global")

pacific_features <- fFeaturesInt(path = "outputs/05_Commercial/05b_fCommercialFeat/",
                                 outdir = "outputs/05_Commercial/05d_fFeaturesInt/Pacific/",
                                 pu_shp = "outputs/01_StudyArea/01a_StudyArea/PacificABNJGrid_05deg.rds",
                                 data = "pacific")

# testing it out
#ggplot() +
#  geom_sf(data = run_c, aes(color = feature_names))

#ggplot() +
#  geom_sf(data = run_d, aes(color = feature_names))
