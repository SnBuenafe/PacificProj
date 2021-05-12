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
