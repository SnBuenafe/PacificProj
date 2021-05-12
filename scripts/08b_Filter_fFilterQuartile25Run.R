# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.
# most are modified from IBM's code.

# This code filters the planning units whose conservation features (feature x Province) fall 
# under the lower quartile of RCE | climate velocity for the specific climate scenario
# It saves a new .rds file (sf object). commercialSSP126_25percentile.rds

# The function fFilterQuartile() requires the following inputs:
# 1. feature = "commercial" or "bycatch"
# 2. scenario = e.g. "SSP126"
# 3. velocity_file = .rds file for climate velocity of the scenario
# 4. RCE_file = .rds file for RCE of the scenario
# 5. feature_prov = .rds file for the features x province
# 6. outdir = path of the output
# 7. data = "smart" for climate-smart and NA for uninformed

# The function is in 08a.

source("scripts/08a_Filter_fFilterQuartile.R")

#########################################
# Commercial Features (Global - Fitted)
#########################################
# Scenario SSP126
FILTER_GLOBAL_run01 <- fFilterQuartile(velocity_file = "outputs/07_Climate/Velocity/velocitySSP126.rds",
          RCE_file = "outputs/07_Climate/RCE/RCESSP126.rds",
          feature_prov = "outputs/05_Commercial/05e_fProvIntersect/Global/commercial_features.rds",
          outdir = "outputs/08_Filter/08b_Filter25/",
          scenario = "SSP126",
          feature_n = "commercial",
          data = "smart"
)
FILTER_GLOBAL_run01
# Scenario SSP245
FILTER_GLOBAL_run02 <- fFilterQuartile(velocity_file = "outputs/07_Climate/Velocity/velocitySSP245.rds",
          RCE_file = "outputs/07_Climate/RCE/RCESSP245.rds",
          feature_prov = "outputs/05_Commercial/05e_fProvIntersect/Global/commercial_features.rds",
          outdir = "outputs/08_Filter/08b_Filter25/",
          scenario = "SSP245",
          feature_n = "commercial",
          data = "smart"
)
FILTER_GLOBAL_run02
# Scenario SSP585
FILTER_GLOBAL_run03 <- fFilterQuartile(velocity_file = "outputs/07_Climate/Velocity/velocitySSP585.rds",
          RCE_file = "outputs/07_Climate/RCE/RCESSP585.rds",
          feature_prov = "outputs/05_Commercial/05e_fProvIntersect/Global/commercial_features.rds",
          outdir = "outputs/08_Filter/08b_Filter25/",
          scenario = "SSP585",
          feature_n = "commercial",
          data = "smart"
)
FILTER_GLOBAL_run03
########################################
# Commercial Features (Pacific-Fitted)
########################################
# Scenario SSP126
FILTER_PACIFIC_run01 <- fFilterQuartile(velocity_file = "outputs/07_Climate/Velocity/velocitySSP126.rds",
          RCE_file = "outputs/07_Climate/RCE/RCESSP126.rds",
          feature_prov = "outputs/05_Commercial/05e_fProvIntersect/Pacific/commercial_features.rds",
          outdir = "outputs/08_Filter/08b_Filter25/",
          scenario = "SSP126",
          feature_n = "commercialpac",
          data = "smart"
)
FILTER_PACIFIC_run01
# Scenario SSP245
FILTER_PACIFIC_run02 <- fFilterQuartile(velocity_file = "outputs/07_Climate/Velocity/velocitySSP245.rds",
          RCE_file = "outputs/07_Climate/RCE/RCESSP245.rds",
          feature_prov = "outputs/05_Commercial/05e_fProvIntersect/Pacific/commercial_features.rds",
          outdir = "outputs/08_Filter/08b_Filter25/",
          scenario = "SSP245",
          feature_n = "commercialpac",
          data = "smart"
)
FILTER_PACIFIC_run02
# Scenario SSP585
FILTER_PACIFIC_run03 <- fFilterQuartile(velocity_file = "outputs/07_Climate/Velocity/velocitySSP585.rds",
          RCE_file = "outputs/07_Climate/RCE/RCESSP585.rds",
          feature_prov = "outputs/05_Commercial/05e_fProvIntersect/Pacific/commercial_features.rds",
          outdir = "outputs/08_Filter/08b_Filter25/",
          scenario = "SSP585",
          feature_n = "commercialpac",
          data = "smart"
)
FILTER_PACIFIC_run03
########################################
# AquaMaps Bycatch Features
########################################
# Scenario SSP126
FILTER_AQM_run01 <- fFilterQuartile(velocity_file = "outputs/07_Climate/Velocity/velocitySSP126.rds",
          RCE_file = "outputs/07_Climate/RCE/RCESSP126.rds",
          feature_prov = "outputs/03_AQM/03b_fProvIntersect/bycatch_features.rds",
          outdir = "outputs/08_Filter/08b_Filter25/",
          scenario = "SSP126",
          feature_n = "bycatch",
          data = "smart"
)
FILTER_AQM_run01
# Scenario SSP245
FILTER_AQM_run02 <- fFilterQuartile(velocity_file = "outputs/07_Climate/Velocity/velocitySSP245.rds",
          RCE_file = "outputs/07_Climate/RCE/RCESSP245.rds",
          feature_prov = "outputs/03_AQM/03b_fProvIntersect/bycatch_features.rds",
          outdir = "outputs/08_Filter/08b_Filter25/",
          scenario = "SSP245",
          feature_n = "bycatch",
          data = "smart"
)
FILTER_AQM_run02
# Scenario SSP585
FILTER_AQM_run03 <- fFilterQuartile(velocity_file = "outputs/07_Climate/Velocity/velocitySSP585.rds",
          RCE_file = "outputs/07_Climate/RCE/RCESSP585.rds",
          feature_prov = "outputs/03_AQM/03b_fProvIntersect/bycatch_features.rds",
          outdir = "outputs/08_Filter/08b_Filter25/",
          scenario = "SSP585",
          feature_n = "bycatch",
          data = "smart"
)
FILTER_AQM_run03
########################################
# IUCN Bycatch Features
########################################
# Scenario SSP126
FILTER_IUCN_run01 <- fFilterQuartile(velocity_file = "outputs/07_Climate/Velocity/velocitySSP126.rds",
          RCE_file = "outputs/07_Climate/RCE/RCESSP126.rds",
          feature_prov = "outputs/04_IUCN/04c_fProvIntersect/bycatch_features.rds",
          outdir = "outputs/08_Filter/08b_Filter25/",
          scenario = "SSP126",
          feature_n = "bycatchIUCN",
          data = "smart"
)
FILTER_IUCN_run01
# Scenario SSP245
FILTER_IUCN_run02 <- fFilterQuartile(velocity_file = "outputs/07_Climate/Velocity/velocitySSP245.rds",
          RCE_file = "outputs/07_Climate/RCE/RCESSP245.rds",
          feature_prov = "outputs/04_IUCN/04c_fProvIntersect/bycatch_features.rds",
          outdir = "outputs/08_Filter/08b_Filter25/",
          scenario = "SSP245",
          feature_n = "bycatchIUCN",
          data = "smart"
)
FILTER_IUCN_run02
# Scenario SSP585
FILTER_IUCN_run03 <- fFilterQuartile(velocity_file = "outputs/07_Climate/Velocity/velocitySSP585.rds",
          RCE_file = "outputs/07_Climate/RCE/RCESSP585.rds",
          feature_prov = "outputs/04_IUCN/04c_fProvIntersect/bycatch_features.rds",
          outdir = "outputs/08_Filter/08b_Filter25/",
          scenario = "SSP585",
          feature_n = "bycatchIUCN",
          data = "smart"
)
FILTER_IUCN_run03