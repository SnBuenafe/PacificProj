# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.
# most are modified from IBM's code.

# This code runs the function of 07a.
# It is divided into 1) commercial features (global-fitted), 2) (pacific-fitted) and 3) bycatch
# CLIMATE-UNINFORMED
# Ideally, after this, each feature should just be represented with the representation targets (depending on IUCN conservation status)

source("scripts/07a_climatefeatures_25percentile.R")

#########################################
# Commercial Features (Global - Fitted)
#########################################

FILTER_GLOBALuninformed_run01 <- filter_quartile(feature_prov = "outputs/commercial/04d_CommercialxProvince/global/commercial_features.rds",
                                       outdir = "outputs/final_features/07c_100perc/",
                                       scenario = "uninformed",
                                       feature_n = "commercial",
                                       data = "uninformed"
)

FILTER_GLOBALuninformed_run01

########################################
# Commercial Features (Pacific-Fitted)
########################################

FILTER_PACIFICuninformed_run01 <- filter_quartile(feature_prov = "outputs/commercial/04d_CommercialxProvince/pacific/commercial_features.rds",
                                        outdir = "outputs/final_features/07c_100perc/",
                                        scenario = "uninformed",
                                        feature_n = "commercialpac",
                                        data = "uninformed"
)

FILTER_PACIFICuninformed_run01

########################################
# AquaMaps Bycatch Features
########################################

FILTER_AQMuninformed_run01 <- filter_quartile(feature_prov = "outputs/AQM_wflow/03d_AQMxProv/bycatch_features.rds",
                                    outdir = "outputs/final_features/07c_100perc/",
                                    scenario = "uninformed",
                                    feature_n = "bycatch",
                                    data = "uninformed"
)

FILTER_AQMuninformed_run01

########################################
# IUCN Bycatch Features
########################################

FILTER_IUCNuninformed_run01 <- filter_quartile(feature_prov = "outputs/IUCN_wflow/09c_IUCNxProv/bycatch_features.rds",
                                     outdir = "outputs/final_features/07c_100perc/",
                                     scenario = "uninformed",
                                     feature_n = "bycatchIUCN",
                                     data = "uninformed"
)

FILTER_IUCNuninformed_run01
