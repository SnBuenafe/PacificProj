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

# Function is in 03a.
source("scripts/03a_IUCN_fIUCNIntersect.R")

############################
# RUNNING THE CODE
############################

IUCN_run01 <- fIUCNIntersect(input = "inputs/rasterfiles/IUCN/data_0.shp",
                  pu_file = "outputs/01_StudyArea/01a_StudyArea/PacificABNJGrid_05deg.rds",
                  outdir = "outputs/03_IUCN/03b_fIUCNIntersect/"
)
