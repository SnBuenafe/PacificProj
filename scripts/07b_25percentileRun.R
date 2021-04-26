# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.
# most are modified from IBM's code.

# This code runs the function of 07a.
# It is divided into 1) commercial features (global-fitted), 2) (pacific-fitted) and 3) bycatch
# and also by scenario.
# Ideally, after this, each feature should just be represented with the representation targets (depending on IUCN conservation status)

source("scripts/07a_climatefeatures_25percentile.R")

#########################################
# Commercial Features (Global - Fitted)
#########################################

# Scenario SSP126

FILTER_GLOBAL_run01 <- filter_quartile(velocity_file = "outputs/climate_features/velocity/velocitySSP126.rds",
          RCE_file = "outputs/climate_features/RCE/RCESSP126.rds",
          feature_prov = "outputs/commercial/04d_CommercialxProvince/global/commercial_features.rds",
          outdir = "outputs/final_features/07a_25perc/",
          scenario = "SSP126",
          feature_n = "commercial"
)

FILTER_GLOBAL_run01

# Scenario SSP245

FILTER_GLOBAL_run02 <- filter_quartile(velocity_file = "outputs/climate_features/velocity/velocitySSP245.rds",
          RCE_file = "outputs/climate_features/RCE/RCESSP245.rds",
          feature_prov = "outputs/commercial/04d_CommercialxProvince/global/commercial_features.rds",
          outdir = "outputs/final_features/07a_25perc/",
          scenario = "SSP245",
          feature_n = "commercial"
)

FILTER_GLOBAL_run02

# Scenario SSP585

FILTER_GLOBAL_run03 <- filter_quartile(velocity_file = "outputs/climate_features/velocity/velocitySSP585.rds",
          RCE_file = "outputs/climate_features/RCE/RCESSP585.rds",
          feature_prov = "outputs/commercial/04d_CommercialxProvince/global/commercial_features.rds",
          outdir = "outputs/final_features/07a_25perc/",
          scenario = "SSP585",
          feature_n = "commercial"
)

FILTER_GLOBAL_run03

########################################
# Commercial Features (Pacific-Fitted)
########################################

# Scenario SSP126

FILTER_PACIFIC_run01 <- filter_quartile(velocity_file = "outputs/climate_features/velocity/velocitySSP126.rds",
          RCE_file = "outputs/climate_features/RCE/RCESSP126.rds",
          feature_prov = "outputs/commercial/04d_CommercialxProvince/pacific/commercial_features.rds",
          outdir = "outputs/final_features/07a_25perc/",
          scenario = "SSP126",
          feature_n = "commercialpac"
)

FILTER_PACIFIC_run01

# Scenario SSP245

FILTER_PACIFIC_run02 <- filter_quartile(velocity_file = "outputs/climate_features/velocity/velocitySSP245.rds",
          RCE_file = "outputs/climate_features/RCE/RCESSP245.rds",
          feature_prov = "outputs/commercial/04d_CommercialxProvince/pacific/commercial_features.rds",
          outdir = "outputs/final_features/07a_25perc/",
          scenario = "SSP245",
          feature_n = "commercialpac"
)

FILTER_PACIFIC_run02

# Scenario SSP585

FILTER_PACIFIC_run03 <- filter_quartile(velocity_file = "outputs/climate_features/velocity/velocitySSP585.rds",
          RCE_file = "outputs/climate_features/RCE/RCESSP585.rds",
          feature_prov = "outputs/commercial/04d_CommercialxProvince/pacific/commercial_features.rds",
          outdir = "outputs/final_features/07a_25perc/",
          scenario = "SSP585",
          feature_n = "commercialpac"
)

FILTER_PACIFIC_run03

########################################
# AquaMaps Bycatch Features
########################################

# Scenario SSP126

FILTER_AQM_run01 <- filter_quartile(velocity_file = "outputs/climate_features/velocity/velocitySSP126.rds",
          RCE_file = "outputs/climate_features/RCE/RCESSP126.rds",
          feature_prov = "outputs/AQM_wflow/03d_AQMxProv/bycatch_features.rds",
          outdir = "outputs/final_features/07a_25perc/",
          scenario = "SSP126",
          feature_n = "bycatch"
)

FILTER_AQM_run01

# Scenario SSP245

FILTER_AQM_run02 <- filter_quartile(velocity_file = "outputs/climate_features/velocity/velocitySSP245.rds",
          RCE_file = "outputs/climate_features/RCE/RCESSP245.rds",
          feature_prov = "outputs/AQM_wflow/03d_AQMxProv/bycatch_features.rds",
          outdir = "outputs/final_features/07a_25perc/",
          scenario = "SSP245",
          feature_n = "bycatch"
)

FILTER_AQM_run02

# Scenario SSP585

FILTER_AQM_run03 <- filter_quartile(velocity_file = "outputs/climate_features/velocity/velocitySSP585.rds",
          RCE_file = "outputs/climate_features/RCE/RCESSP585.rds",
          feature_prov = "outputs/AQM_wflow/03d_AQMxProv/bycatch_features.rds",
          outdir = "outputs/final_features/07a_25perc/",
          scenario = "SSP585",
          feature_n = "bycatch"
)

########################################
# IUCN Bycatch Features
########################################

# Scenario SSP126

FILTER_IUCN_run01 <- filter_quartile(velocity_file = "outputs/climate_features/velocity/velocitySSP126.rds",
          RCE_file = "outputs/climate_features/RCE/RCESSP126.rds",
          feature_prov = "outputs/IUCN_wflow/09c_IUCNxProv/bycatch_features.rds",
          outdir = "outputs/final_features/07a_25perc/",
          scenario = "SSP126",
          feature_n = "bycatchIUCN"
)

# Scenario SSP245

FILTER_IUCN_run02 <- filter_quartile(velocity_file = "outputs/climate_features/velocity/velocitySSP245.rds",
          RCE_file = "outputs/climate_features/RCE/RCESSP245.rds",
          feature_prov = "outputs/IUCN_wflow/09c_IUCNxProv/bycatch_features.rds",
          outdir = "outputs/final_features/07a_25perc/",
          scenario = "SSP245",
          feature_n = "bycatchIUCN"
)

# Scenario SSP585

FILTER_IUCN_run03 <- filter_quartile(velocity_file = "outputs/climate_features/velocity/velocitySSP585.rds",
          RCE_file = "outputs/climate_features/RCE/RCESSP585.rds",
          feature_prov = "outputs/IUCN_wflow/09c_IUCNxProv/bycatch_features.rds",
          outdir = "outputs/final_features/07a_25perc/",
          scenario = "SSP585",
          feature_n = "bycatchIUCN"
)
