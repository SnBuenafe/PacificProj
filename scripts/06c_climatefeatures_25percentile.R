# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.
# most are modified from IBM's code.

# This code filters only planning units that belong in the lower quartile of the climate features.
# Planning units that have values > lower quartile are replaced with NA values for "trans_value" (transformed values) and "rce_categ" (Category)
# It saves a new .rds file (sf object). "RCE_SSP126_25percentile.rds"

# The function filter_quartile() requires the following inputs:
# 1. input = "RCE" or "velocity"
# 2. scenario = e.g. "SSP126"
# 3. dir = directory to save the outputs
# 4. file = .rds file of the climate feature intersected with all the PUs (from 06b)

filter_quartile <- function(input, scenario, dir, file, ...) {
  
  library(dplyr)
  
  climate <- readRDS(file)
  qrt <- quantile(climate$trans_value)
  climate$trans_value[] <- ifelse(climate$trans_value > as.vector(qrt[2]), NA, climate$trans_value)
  
  if(input == "RCE") {
      climate$rce_categ[] <- ifelse(is.na(climate$trans_value), NA, climate$rce_categ)
  }else if(input == "velocity") {
      climate$velo_categ[] <- ifelse(is.na(climate$trans_value), NA, climate$velo_categ)
  }else {
    print("fail")
  }
  
  saveRDS(climate, paste0(dir, input,"_",scenario,"_25percentile.rds"))
  
  return(climate)
  
}

# Running function for RCE

run17 <- filter_quartile(input = "RCE",
                         scenario = "SSP126",
                         dir = "outputs/climate_features/RCE/",
                         file = "outputs/climate_features/RCE/RCESSP126.rds")

run18 <- filter_quartile(input = "RCE",
                         scenario = "SSP245",
                         dir = "outputs/climate_features/RCE/",
                         file = "outputs/climate_features/RCE/RCESSP245.rds")

run19 <- filter_quartile(input = "RCE",
                         scenario = "SSP585",
                         dir = "outputs/climate_features/RCE/",
                         file = "outputs/climate_features/RCE/RCESSP585.rds")

# Running function for Climate Velocity

run20 <- filter_quartile(input = "velocity",
                         scenario = "SSP126",
                         dir = "outputs/climate_features/velocity/",
                         file = "outputs/climate_features/velocity/velocitySSP126.rds")

run21 <- filter_quartile(input = "velocity",
                         scenario = "SSP245",
                         dir = "outputs/climate_features/velocity/",
                         file = "outputs/climate_features/velocity/velocitySSP245.rds")

run22 <- filter_quartile(input = "velocity",
                         scenario = "SSP585",
                         dir = "outputs/climate_features/velocity/",
                         file = "outputs/climate_features/velocity/velocitySSP585.rds")
