# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.
# most are modified from IBM's code.

# This code runs the function of 07a.
# It is divided into 1) commercial features (global-fitted), 2) (pacific-fitted) and 3) bycatch
# CLIMATE-UNINFORMED
# Ideally, after this, each feature should just be represented with the representation targets (depending on IUCN conservation status)

source("scripts/08a_Filter_fFilterQuartile.R")

#########################################
# Commercial Features (Global - Fitted)
#########################################
FILTER_GLOBALuninformed_run01 <- fFilterQuartile(feature_prov = "outputs/05_Commercial/05e_fProvIntersect/Global/commercial_features.rds",
                                       outdir = "outputs/08_Filter/08c_Filter100/",
                                       scenario = "uninformed",
                                       feature_n = "commercial",
                                       data = "uninformed"
)

FILTER_GLOBALuninformed_run01

########################################
# Commercial Features (Pacific-Fitted)
########################################
FILTER_PACIFICuninformed_run01 <- fFilterQuartile(feature_prov = "outputs/05_Commercial/05e_fProvIntersect/Pacific/commercial_features.rds",
                                        outdir = "outputs/08_Filter/08c_Filter100/",
                                        scenario = "uninformed",
                                        feature_n = "commercialpac",
                                        data = "uninformed"
)

FILTER_PACIFICuninformed_run01

########################################
# AquaMaps Bycatch Features
########################################
FILTER_AQMuninformed_run01 <- fFilterQuartile(feature_prov = "outputs/03_AQM/03b_fProvIntersect/bycatch_features.rds",
                                    outdir = "outputs/08_Filter/08c_Filter100/",
                                    scenario = "uninformed",
                                    feature_n = "bycatch",
                                    data = "uninformed"
)

FILTER_AQMuninformed_run01

########################################
# IUCN Bycatch Features
########################################
FILTER_IUCNuninformed_run01 <- fFilterQuartile(feature_prov = "outputs/04_IUCN/04c_fProvIntersect/bycatch_features.rds",
                                     outdir = "outputs/08_Filter/08c_Filter100/",
                                     scenario = "uninformed",
                                     feature_n = "bycatchIUCN",
                                     data = "uninformed"
)

FILTER_IUCNuninformed_run01
