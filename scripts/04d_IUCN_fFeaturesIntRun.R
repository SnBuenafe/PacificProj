# script by Tin Buenafe, 2021 (tinbuenafe@gmail.com)

# This code intersects the IUCN features with the planning units.

# The function fFeaturesInt() requires the following inputs:
# 1. path: folder's name where species conservation feature files are located
# 2. outdir: where to put the final sf-.rds object
# 3. pu_shp: .shp or .rds of the PUs
# 4. data: "global", "pacific", "AQM", "IUCN"

source("scripts/03_Features_fFeaturesInt.R")

#######################################
#### Running fFeaturesInt function ####
#######################################
#running with .rds
IUCN_FeatInt_run01 <- fFeaturesInt(path = "outputs/04_IUCN/04b_fIUCNIntersect/",
                                  outdir = "outputs/04_IUCN/04d_fFeaturesInt/",
                                  pu_shp = "outputs/01_StudyArea/01a_StudyArea/PacificABNJGrid_05deg.rds",
                                  data = "IUCN")
IUCN_FeatInt_run01
