# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.
# most are modified from IBM's code.

# This uses the same function as the function that intersects AquaMaps distribution maps with the PUs.
# See 03a_AQM_Featuresfxn.R for full function,
# and 03b_AQM_FeaturesfxnRun.R for the runs for AquaMaps
# What this code does is it intersects the commercial features with the PUs
# This makes distribution maps. 
# This is just an extra script and not really part of the workflow.
# 04d script automatically makes distribution maps per new feature (Province x Commercial spp.)

source("scripts/03a_AQM_Featuresfxn.R")

run_c <- features_pus(path = "outputs/commercial/04b_CommercialPredictions",
                      outdir = "outputs/commercial/04c_CommercialxPU/",
                      pu_shp = "inputs/rdsfiles/PacificABNJGrid_05deg.rds",
                      data = "global",
                      olayer = "surface")

run_d <- features_pus(path = "outputs/commercial/04b_CommercialPredictions",
                      outdir = "outputs/commercial/04c_CommercialxPU/",
                      pu_shp = "inputs/rdsfiles/PacificABNJGrid_05deg.rds",
                      data = "pacific",
                      olayer = "surface")

# testing it out
ggplot() +
  geom_sf(data = run_c, aes(color = feature_names))

ggplot() +
  geom_sf(data = run_d, aes(color = feature_names))
