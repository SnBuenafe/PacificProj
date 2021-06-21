# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.
# most are modified from IBM's code.

# This uses the same function as the function that intersects AquaMaps distribution maps with the PUs.
# See 03_Features_fFeaturesInt.R for full function,
# What this code does is it intersects the commercial features with the PUs
# This makes distribution maps.

source("scripts/02_Features_fFeaturesInt.R")

global_features <- fFeaturesInt(path = "outputs/04_Commercial/04b_fCommercialFeat/",
                                outdir = "outputs/04_Commercial/04d_fFeaturesInt/Global/",
                                pu_shp = "outputs/01_StudyArea/01a_StudyArea/PacificABNJGrid_04deg.rds",
                                data = "global")
