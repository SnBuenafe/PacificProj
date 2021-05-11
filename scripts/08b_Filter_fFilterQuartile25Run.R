# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.
# most are modified from IBM's code.

# It is divided into 1) commercial features (global-fitted), 2) (pacific-fitted) and 3) bycatch and also by scenario.
# Ideally, after this, each feature should just be represented with the representation targets (depending on IUCN conservation status)

# Function is 07a.

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