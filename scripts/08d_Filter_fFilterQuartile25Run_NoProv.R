# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.
# most are modified from IBM's code.

# This code filters the planning units whose conservation features (not considering provinces) fall 
# under the lower quartile of RCE | climate velocity for the specific climate scenario
# It saves a new .rds file (sf object). commercialSSP126_25percentile.rds

# The function fFilterQuartile() requires the following inputs:
# 1. feature = "commercial" or "bycatch"
# 2. scenario = e.g. "SSP126"
# 3. velocity_file = .rds file for climate velocity of the scenario
# 4. RCE_file = .rds file for RCE of the scenario
# 5. feature_prov = .rds file for the features
# 6. outdir = path of the output
# 7. data = "smart" for climate-smart and NA for uninformed
# 8. prov = TRUE/FALSE (including provinces or not)

# The function is in 08a.

source("scripts/08a_Filter_fFilterQuartile.R")

#########################################
# Commercial Features (Global - Fitted)
#########################################
# Scenario SSP126
FILTER_noprov_GLOBAL_run01 <- fFilterQuartile(velocity_file = "outputs/07_Climate/Velocity/velocitySSP126.rds",
                                       RCE_file = "outputs/07_Climate/RCE/RCESSP126.rds",
                                       feature_prov = "outputs/05_Commercial/05d_fFeaturesInt/Global/commercial_features.rds",
                                       outdir = "outputs/08_Filter/08d_Filter25_NoProv/",
                                       scenario = "SSP126",
                                       feature_n = "commercial",
                                       data = "smart",
                                       prov = FALSE
)
FILTER_noprov_GLOBAL_run01
# Scenario SSP245
FILTER_noprov_GLOBAL_run02 <- fFilterQuartile(velocity_file = "outputs/07_Climate/Velocity/velocitySSP245.rds",
                                              RCE_file = "outputs/07_Climate/RCE/RCESSP245.rds",
                                              feature_prov = "outputs/05_Commercial/05d_fFeaturesInt/Global/commercial_features.rds",
                                              outdir = "outputs/08_Filter/08d_Filter25_NoProv/",
                                              scenario = "SSP245",
                                              feature_n = "commercial",
                                              data = "smart",
                                              prov = FALSE
)
FILTER_noprov_GLOBAL_run02
# Scenario SSP585
FILTER_noprov_GLOBAL_run03 <- fFilterQuartile(velocity_file = "outputs/07_Climate/Velocity/velocitySSP585.rds",
                                              RCE_file = "outputs/07_Climate/RCE/RCESSP585.rds",
                                              feature_prov = "outputs/05_Commercial/05d_fFeaturesInt/Global/commercial_features.rds",
                                              outdir = "outputs/08_Filter/08d_Filter25_NoProv/",
                                              scenario = "SSP585",
                                              feature_n = "commercial",
                                              data = "smart",
                                              prov = FALSE
)
FILTER_noprov_GLOBAL_run03
########################################
# Commercial Features (Pacific-Fitted)
########################################
# Scenario SSP126
FILTER_noprov_PACIFIC_run01 <- fFilterQuartile(velocity_file = "outputs/07_Climate/Velocity/velocitySSP126.rds",
                                        RCE_file = "outputs/07_Climate/RCE/RCESSP126.rds",
                                        feature_prov = "outputs/05_Commercial/05d_fFeaturesInt/Pacific/commercial_features.rds",
                                        outdir = "outputs/08_Filter/08d_Filter25_NoProv/",
                                        scenario = "SSP126",
                                        feature_n = "commercialpac",
                                        data = "smart",
                                        prov = FALSE
)
FILTER_noprov_PACIFIC_run01
# Scenario SSP245
FILTER_noprov_PACIFIC_run02 <- fFilterQuartile(velocity_file = "outputs/07_Climate/Velocity/velocitySSP245.rds",
                                               RCE_file = "outputs/07_Climate/RCE/RCESSP245.rds",
                                               feature_prov = "outputs/05_Commercial/05d_fFeaturesInt/Pacific/commercial_features.rds",
                                               outdir = "outputs/08_Filter/08d_Filter25_NoProv/",
                                               scenario = "SSP245",
                                               feature_n = "commercialpac",
                                               data = "smart",
                                               prov = FALSE
)
FILTER_noprov_PACIFIC_run02
# Scenario SSP585
FILTER_noprov_PACIFIC_run03 <- fFilterQuartile(velocity_file = "outputs/07_Climate/Velocity/velocitySSP585.rds",
                                               RCE_file = "outputs/07_Climate/RCE/RCESSP585.rds",
                                               feature_prov = "outputs/05_Commercial/05d_fFeaturesInt/Pacific/commercial_features.rds",
                                               outdir = "outputs/08_Filter/08d_Filter25_NoProv/",
                                               scenario = "SSP585",
                                               feature_n = "commercialpac",
                                               data = "smart",
                                               prov = FALSE
)
FILTER_noprov_PACIFIC_run03
########################################
# AquaMaps Bycatch Features
########################################
# Scenario SSP126
FILTER_noprov_AQM_run01 <- fFilterQuartile(velocity_file = "outputs/07_Climate/Velocity/velocitySSP126.rds",
                                    RCE_file = "outputs/07_Climate/RCE/RCESSP126.rds",
                                    feature_prov = "outputs/03_AQM/03a_fFeaturesInt/bycatch_features.rds",
                                    outdir = "outputs/08_Filter/08d_Filter25_NoProv/",
                                    scenario = "SSP126",
                                    feature_n = "bycatch",
                                    data = "smart",
                                    prov = FALSE
)
FILTER_noprov_AQM_run01
# Scenario SSP245
FILTER_noprov_AQM_run02 <- fFilterQuartile(velocity_file = "outputs/07_Climate/Velocity/velocitySSP245.rds",
                                           RCE_file = "outputs/07_Climate/RCE/RCESSP245.rds",
                                           feature_prov = "outputs/03_AQM/03a_fFeaturesInt/bycatch_features.rds",
                                           outdir = "outputs/08_Filter/08d_Filter25_NoProv/",
                                           scenario = "SSP245",
                                           feature_n = "bycatch",
                                           data = "smart",
                                           prov = FALSE
)
FILTER_noprov_AQM_run02
# Scenario SSP585
FILTER_noprov_AQM_run03 <- fFilterQuartile(velocity_file = "outputs/07_Climate/Velocity/velocitySSP585.rds",
                                           RCE_file = "outputs/07_Climate/RCE/RCESSP585.rds",
                                           feature_prov = "outputs/03_AQM/03a_fFeaturesInt/bycatch_features.rds",
                                           outdir = "outputs/08_Filter/08d_Filter25_NoProv/",
                                           scenario = "SSP585",
                                           feature_n = "bycatch",
                                           data = "smart",
                                           prov = FALSE
)
FILTER_noprov_AQM_run03
########################################
# IUCN Bycatch Features
########################################
# Scenario SSP126
FILTER_noprov_IUCN_run01 <- fFilterQuartile(velocity_file = "outputs/07_Climate/Velocity/velocitySSP126.rds",
                                     RCE_file = "outputs/07_Climate/RCE/RCESSP126.rds",
                                     feature_prov = "outputs/04_IUCN/04d_fFeaturesInt/bycatch_features.rds",
                                     outdir = "outputs/08_Filter/08d_Filter25_NoProv/",
                                     scenario = "SSP126",
                                     feature_n = "bycatchIUCN",
                                     data = "smart",
                                     prov = FALSE
)
FILTER_noprov_IUCN_run01
# Scenario SSP245
FILTER_noprov_IUCN_run02 <- fFilterQuartile(velocity_file = "outputs/07_Climate/Velocity/velocitySSP245.rds",
                                     RCE_file = "outputs/07_Climate/RCE/RCESSP245.rds",
                                     feature_prov = "outputs/04_IUCN/04d_fFeaturesInt/bycatch_features.rds",
                                     outdir = "outputs/08_Filter/08d_Filter25_NoProv/",
                                     scenario = "SSP245",
                                     feature_n = "bycatchIUCN",
                                     data = "smart",
                                     prov = FALSE
)
FILTER_noprov_IUCN_run02
# Scenario SSP585
FILTER_noprov_IUCN_run03 <- fFilterQuartile(velocity_file = "outputs/07_Climate/Velocity/velocitySSP585.rds",
                                     RCE_file = "outputs/07_Climate/RCE/RCESSP585.rds",
                                     feature_prov = "outputs/04_IUCN/04d_fFeaturesInt/bycatch_features.rds",
                                     outdir = "outputs/08_Filter/08d_Filter25_NoProv/",
                                     scenario = "SSP585",
                                     feature_n = "bycatchIUCN",
                                     data = "smart",
                                     prov = FALSE
)
FILTER_noprov_IUCN_run03