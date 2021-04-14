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

run17 <- filter_quartile(velocity_file = "outputs/climate_features/velocity/velocitySSP126.rds",
                         RCE_file = "outputs/climate_features/RCE/RCESSP126.rds",
                         feature_prov = "outputs/commercial/04d_CommercialxProvince/global/commercial_features.rds",
                         outdir = "outputs/final_features/07a_25perc/",
                         scenario = "SSP126",
                         feature_n = "commercial"
)

run17

# Scenario SSP245

run18 <- filter_quartile(velocity_file = "outputs/climate_features/velocity/velocitySSP245.rds",
                         RCE_file = "outputs/climate_features/RCE/RCESSP245.rds",
                         feature_prov = "outputs/commercial/04d_CommercialxProvince/global/commercial_features.rds",
                         outdir = "outputs/final_features/07a_25perc/",
                         scenario = "SSP245",
                         feature_n = "commercial"
)

# Scenario SSP585

run19 <- filter_quartile(velocity_file = "outputs/climate_features/velocity/velocitySSP585.rds",
                         RCE_file = "outputs/climate_features/RCE/RCESSP585.rds",
                         feature_prov = "outputs/commercial/04d_CommercialxProvince/global/commercial_features.rds",
                         outdir = "outputs/final_features/07a_25perc/",
                         scenario = "SSP585",
                         feature_n = "commercial"
)

########################################
# Commercial Features (Pacific-Fitted)
########################################

# Scenario SSP126

run20 <- filter_quartile(velocity_file = "outputs/climate_features/velocity/velocitySSP126.rds",
                         RCE_file = "outputs/climate_features/RCE/RCESSP126.rds",
                         feature_prov = "outputs/commercial/04d_CommercialxProvince/pacific/commercial_features.rds",
                         outdir = "outputs/final_features/07a_25perc/",
                         scenario = "SSP126",
                         feature_n = "commercialpac"
)

# Scenario SSP245

run21 <- filter_quartile(velocity_file = "outputs/climate_features/velocity/velocitySSP245.rds",
                         RCE_file = "outputs/climate_features/RCE/RCESSP245.rds",
                         feature_prov = "outputs/commercial/04d_CommercialxProvince/pacific/commercial_features.rds",
                         outdir = "outputs/final_features/07a_25perc/",
                         scenario = "SSP245",
                         feature_n = "commercialpac"
)

# Scenario SSP585

run22 <- filter_quartile(velocity_file = "outputs/climate_features/velocity/velocitySSP585.rds",
                         RCE_file = "outputs/climate_features/RCE/RCESSP585.rds",
                         feature_prov = "outputs/commercial/04d_CommercialxProvince/pacific/commercial_features.rds",
                         outdir = "outputs/final_features/07a_25perc/",
                         scenario = "SSP585",
                         feature_n = "commercialpac"
)

########################################
# Bycatch Features
########################################

# Scenario SSP126

run23 <- filter_quartile(velocity_file = "outputs/climate_features/velocity/velocitySSP126.rds",
                         RCE_file = "outputs/climate_features/RCE/RCESSP126.rds",
                         feature_prov = "outputs/AQM_wflow/03c_featprov/bycatch_provinces.rds",
                         outdir = "outputs/final_features/07a_25perc/",
                         scenario = "SSP126",
                         feature_n = "bycatch"
                         )

# Scenario SSP245

run24 <- filter_quartile(velocity_file = "outputs/climate_features/velocity/velocitySSP245.rds",
                         RCE_file = "outputs/climate_features/RCE/RCESSP245.rds",
                         feature_prov = "outputs/AQM_wflow/03c_featprov/bycatch_provinces.rds",
                         outdir = "outputs/final_features/07a_25perc/",
                         scenario = "SSP245",
                         feature_n = "bycatch"
)

# Scenario SSP585

run25 <- filter_quartile(velocity_file = "outputs/climate_features/velocity/velocitySSP585.rds",
                         RCE_file = "outputs/climate_features/RCE/RCESSP585.rds",
                         feature_prov = "outputs/AQM_wflow/03c_featprov/bycatch_provinces.rds",
                         outdir = "outputs/final_features/07a_25perc/",
                         scenario = "SSP585",
                         feature_n = "bycatch"
)
