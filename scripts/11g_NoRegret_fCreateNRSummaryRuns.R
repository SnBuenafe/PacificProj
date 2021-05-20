# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.

# This function creates .csv summary of the features selected in no-regret plans, requiring the ff. inputs:
# NOTE: Make sure files correspond with value of "prov" (TRUE/FALSE)
# 1. commercial_file: commercial file (global/pacific)
# 2. bycatch_file: bycatch file (AQM/IUCN)
# 3. prov: TRUE/FALSE (including provinces or not)
# 4. noregret_file: no-regret .rds file for each target
# 5. outexcel: where to save .csv output
# 6. target_name: e.g. Target100

# Function is found in 11f.

source("scripts/11f_NoRegret_fCreateNRSummary.R")

##############################
########## AQM RUNS ##########
##############################
# Target 100
noregret_AQM_sum_target100 <- fCreateNRSummary(commercial_file = 'outputs/05_Commercial/05e_fProvIntersect/Global/commercial_features.rds',
                                               bycatch_file = 'outputs/03_AQM/03b_fProvIntersect/bycatch_features.rds',
                                               prov = TRUE,
                                               noregret_file = 'outputs/11_NoRegret/11b_AQMRuns/noregretclosures_Target100.rds',
                                               outexcel = 'excel/no_regret_AQM/',
                                               target_name = 'Target100')
# Target 90
noregret_AQM_sum_target90 <- fCreateNRSummary(commercial_file = 'outputs/05_Commercial/05e_fProvIntersect/Global/commercial_features.rds',
                                               bycatch_file = 'outputs/03_AQM/03b_fProvIntersect/bycatch_features.rds',
                                               prov = TRUE,
                                               noregret_file = 'outputs/11_NoRegret/11b_AQMRuns/noregretclosures_Target90.rds',
                                               outexcel = 'excel/no_regret_AQM/',
                                               target_name = 'Target90')
# Target 80
noregret_AQM_sum_target80 <- fCreateNRSummary(commercial_file = 'outputs/05_Commercial/05e_fProvIntersect/Global/commercial_features.rds',
                                               bycatch_file = 'outputs/03_AQM/03b_fProvIntersect/bycatch_features.rds',
                                               prov = TRUE,
                                               noregret_file = 'outputs/11_NoRegret/11b_AQMRuns/noregretclosures_Target80.rds',
                                               outexcel = 'excel/no_regret_AQM/',
                                               target_name = 'Target80')
# Target 70
noregret_AQM_sum_target70 <- fCreateNRSummary(commercial_file = 'outputs/05_Commercial/05e_fProvIntersect/Global/commercial_features.rds',
                                               bycatch_file = 'outputs/03_AQM/03b_fProvIntersect/bycatch_features.rds',
                                               prov = TRUE,
                                               noregret_file = 'outputs/11_NoRegret/11b_AQMRuns/noregretclosures_Target70.rds',
                                               outexcel = 'excel/no_regret_AQM/',
                                               target_name = 'Target70')
# Target 60
noregret_AQM_sum_target60 <- fCreateNRSummary(commercial_file = 'outputs/05_Commercial/05e_fProvIntersect/Global/commercial_features.rds',
                                               bycatch_file = 'outputs/03_AQM/03b_fProvIntersect/bycatch_features.rds',
                                               prov = TRUE,
                                               noregret_file = 'outputs/11_NoRegret/11b_AQMRuns/noregretclosures_Target60.rds',
                                               outexcel = 'excel/no_regret_AQM/',
                                               target_name = 'Target60')
# Target 50
noregret_AQM_sum_target50 <- fCreateNRSummary(commercial_file = 'outputs/05_Commercial/05e_fProvIntersect/Global/commercial_features.rds',
                                               bycatch_file = 'outputs/03_AQM/03b_fProvIntersect/bycatch_features.rds',
                                               prov = TRUE,
                                               noregret_file = 'outputs/11_NoRegret/11b_AQMRuns/noregretclosures_Target50.rds',
                                               outexcel = 'excel/no_regret_AQM/',
                                               target_name = 'Target50')
# Target 40
noregret_AQM_sum_target40 <- fCreateNRSummary(commercial_file = 'outputs/05_Commercial/05e_fProvIntersect/Global/commercial_features.rds',
                                               bycatch_file = 'outputs/03_AQM/03b_fProvIntersect/bycatch_features.rds',
                                               prov = TRUE,
                                               noregret_file = 'outputs/11_NoRegret/11b_AQMRuns/noregretclosures_Target40.rds',
                                               outexcel = 'excel/no_regret_AQM/',
                                               target_name = 'Target40')
# Target 30
noregret_AQM_sum_target30 <- fCreateNRSummary(commercial_file = 'outputs/05_Commercial/05e_fProvIntersect/Global/commercial_features.rds',
                                               bycatch_file = 'outputs/03_AQM/03b_fProvIntersect/bycatch_features.rds',
                                               prov = TRUE,
                                               noregret_file = 'outputs/11_NoRegret/11b_AQMRuns/noregretclosures_Target30.rds',
                                               outexcel = 'excel/no_regret_AQM/',
                                               target_name = 'Target30')
# Target 20
noregret_AQM_sum_target20 <- fCreateNRSummary(commercial_file = 'outputs/05_Commercial/05e_fProvIntersect/Global/commercial_features.rds',
                                               bycatch_file = 'outputs/03_AQM/03b_fProvIntersect/bycatch_features.rds',
                                               prov = TRUE,
                                               noregret_file = 'outputs/11_NoRegret/11b_AQMRuns/noregretclosures_Target20.rds',
                                               outexcel = 'excel/no_regret_AQM/',
                                               target_name = 'Target20')
# Target 10
noregret_AQM_sum_target10 <- fCreateNRSummary(commercial_file = 'outputs/05_Commercial/05e_fProvIntersect/Global/commercial_features.rds',
                                               bycatch_file = 'outputs/03_AQM/03b_fProvIntersect/bycatch_features.rds',
                                               prov = TRUE,
                                               noregret_file = 'outputs/11_NoRegret/11b_AQMRuns/noregretclosures_Target10.rds',
                                               outexcel = 'excel/no_regret_AQM/',
                                               target_name = 'Target10')

##############################
######### IUCN RUNS #########
##############################
# Target 100
noregret_IUCN_sum_target100 <- fCreateNRSummary(commercial_file = 'outputs/05_Commercial/05e_fProvIntersect/Global/commercial_features.rds',
                                                bycatch_file = 'outputs/04_IUCN/04c_fProvIntersect/bycatch_features.rds',
                                                prov = TRUE,
                                                noregret_file = 'outputs/11_NoRegret/11c_IUCNRuns/noregretclosures_Target100.rds',
                                                outexcel = 'excel/no_regret_IUCN/',
                                                target_name = 'Target100')
# Target 90
noregret_IUCN_sum_target90 <- fCreateNRSummary(commercial_file = 'outputs/05_Commercial/05e_fProvIntersect/Global/commercial_features.rds',
                                               bycatch_file = 'outputs/04_IUCN/04c_fProvIntersect/bycatch_features.rds',
                                               prov = TRUE,
                                               noregret_file = 'outputs/11_NoRegret/11c_IUCNRuns/noregretclosures_Target90.rds',
                                               outexcel = 'excel/no_regret_IUCN/',
                                               target_name = 'Target90')
# Target 80
noregret_IUCN_sum_target80 <- fCreateNRSummary(commercial_file = 'outputs/05_Commercial/05e_fProvIntersect/Global/commercial_features.rds',
                                               bycatch_file = 'outputs/04_IUCN/04c_fProvIntersect/bycatch_features.rds',
                                               prov = TRUE,
                                               noregret_file = 'outputs/11_NoRegret/11c_IUCNRuns/noregretclosures_Target80.rds',
                                               outexcel = 'excel/no_regret_IUCN/',
                                               target_name = 'Target80')
# Target 70
noregret_IUCN_sum_target70 <- fCreateNRSummary(commercial_file = 'outputs/05_Commercial/05e_fProvIntersect/Global/commercial_features.rds',
                                               bycatch_file = 'outputs/04_IUCN/04c_fProvIntersect/bycatch_features.rds',
                                               prov = TRUE,
                                               noregret_file = 'outputs/11_NoRegret/11c_IUCNRuns/noregretclosures_Target70.rds',
                                               outexcel = 'excel/no_regret_IUCN/',
                                               target_name = 'Target70')
# Target 60
noregret_IUCN_sum_target60 <- fCreateNRSummary(commercial_file = 'outputs/05_Commercial/05e_fProvIntersect/Global/commercial_features.rds',
                                               bycatch_file = 'outputs/04_IUCN/04c_fProvIntersect/bycatch_features.rds',
                                               prov = TRUE,
                                               noregret_file = 'outputs/11_NoRegret/11c_IUCNRuns/noregretclosures_Target60.rds',
                                               outexcel = 'excel/no_regret_IUCN/',
                                               target_name = 'Target60')
# Target 50
noregret_IUCN_sum_target50 <- fCreateNRSummary(commercial_file = 'outputs/05_Commercial/05e_fProvIntersect/Global/commercial_features.rds',
                                               bycatch_file = 'outputs/04_IUCN/04c_fProvIntersect/bycatch_features.rds',
                                               prov = TRUE,
                                               noregret_file = 'outputs/11_NoRegret/11c_IUCNRuns/noregretclosures_Target50.rds',
                                               outexcel = 'excel/no_regret_IUCN/',
                                               target_name = 'Target50')
# Target 40
noregret_IUCN_sum_target40 <- fCreateNRSummary(commercial_file = 'outputs/05_Commercial/05e_fProvIntersect/Global/commercial_features.rds',
                                               bycatch_file = 'outputs/04_IUCN/04c_fProvIntersect/bycatch_features.rds',
                                               prov = TRUE,
                                               noregret_file = 'outputs/11_NoRegret/11c_IUCNRuns/noregretclosures_Target40.rds',
                                               outexcel = 'excel/no_regret_IUCN/',
                                               target_name = 'Target40')
# Target 30
noregret_IUCN_sum_target30 <- fCreateNRSummary(commercial_file = 'outputs/05_Commercial/05e_fProvIntersect/Global/commercial_features.rds',
                                               bycatch_file = 'outputs/04_IUCN/04c_fProvIntersect/bycatch_features.rds',
                                               prov = TRUE,
                                               noregret_file = 'outputs/11_NoRegret/11c_IUCNRuns/noregretclosures_Target30.rds',
                                               outexcel = 'excel/no_regret_IUCN/',
                                               target_name = 'Target30')
# Target 20
noregret_IUCN_sum_target20 <- fCreateNRSummary(commercial_file = 'outputs/05_Commercial/05e_fProvIntersect/Global/commercial_features.rds',
                                               bycatch_file = 'outputs/04_IUCN/04c_fProvIntersect/bycatch_features.rds',
                                               prov = TRUE,
                                               noregret_file = 'outputs/11_NoRegret/11c_IUCNRuns/noregretclosures_Target20.rds',
                                               outexcel = 'excel/no_regret_IUCN/',
                                               target_name = 'Target20')
# Target 10
noregret_IUCN_sum_target10 <- fCreateNRSummary(commercial_file = 'outputs/05_Commercial/05e_fProvIntersect/Global/commercial_features.rds',
                                               bycatch_file = 'outputs/04_IUCN/04c_fProvIntersect/bycatch_features.rds',
                                               prov = TRUE,
                                               noregret_file = 'outputs/11_NoRegret/11c_IUCNRuns/noregretclosures_Target10.rds',
                                               outexcel = 'excel/no_regret_IUCN/',
                                               target_name = 'Target10')
#########################################
######### AQM No Provinces RUNS #########
#########################################
# Target 100
noregret_AQM_NoProv_sum_target100 <- fCreateNRSummary(commercial_file = 'outputs/05_Commercial/05d_fFeaturesInt/Global/commercial_features.rds',
                                                      bycatch_file = 'outputs/03_AQM/03a_fFeaturesInt/bycatch_features.rds',
                                                      prov = FALSE,
                                                      noregret_file = 'outputs/11_NoRegret/11d_AQMRuns_NoProv/noregretclosures_Target100.rds',
                                                      outexcel = 'excel/no_regret_AQM_NoProv/',
                                                      target_name = 'Target100')
# Target 90
noregret_AQM_NoProv_sum_target90 <- fCreateNRSummary(commercial_file = 'outputs/05_Commercial/05d_fFeaturesInt/Global/commercial_features.rds',
                                                     bycatch_file = 'outputs/03_AQM/03a_fFeaturesInt/bycatch_features.rds',
                                                     prov = FALSE,
                                                     noregret_file = 'outputs/11_NoRegret/11d_AQMRuns_NoProv/noregretclosures_Target90.rds',
                                                     outexcel = 'excel/no_regret_AQM_NoProv/',
                                                     target_name = 'Target90')
# Target 80
noregret_AQM_NoProv_sum_target80 <- fCreateNRSummary(commercial_file = 'outputs/05_Commercial/05d_fFeaturesInt/Global/commercial_features.rds',
                                                     bycatch_file = 'outputs/03_AQM/03a_fFeaturesInt/bycatch_features.rds',
                                                     prov = FALSE,
                                                     noregret_file = 'outputs/11_NoRegret/11d_AQMRuns_NoProv/noregretclosures_Target80.rds',
                                                     outexcel = 'excel/no_regret_AQM_NoProv/',
                                                     target_name = 'Target80')
# Target 70
noregret_AQM_NoProv_sum_target70 <- fCreateNRSummary(commercial_file = 'outputs/05_Commercial/05d_fFeaturesInt/Global/commercial_features.rds',
                                                     bycatch_file = 'outputs/03_AQM/03a_fFeaturesInt/bycatch_features.rds',
                                                     prov = FALSE,
                                                     noregret_file = 'outputs/11_NoRegret/11d_AQMRuns_NoProv/noregretclosures_Target70.rds',
                                                     outexcel = 'excel/no_regret_AQM_NoProv/',
                                                     target_name = 'Target70')
# Target 60
noregret_AQM_NoProv_sum_target60 <- fCreateNRSummary(commercial_file = 'outputs/05_Commercial/05d_fFeaturesInt/Global/commercial_features.rds',
                                                     bycatch_file = 'outputs/03_AQM/03a_fFeaturesInt/bycatch_features.rds',
                                                     prov = FALSE,
                                                     noregret_file = 'outputs/11_NoRegret/11d_AQMRuns_NoProv/noregretclosures_Target60.rds',
                                                     outexcel = 'excel/no_regret_AQM_NoProv/',
                                                     target_name = 'Target60')
# Target 50
noregret_AQM_NoProv_sum_target50 <- fCreateNRSummary(commercial_file = 'outputs/05_Commercial/05d_fFeaturesInt/Global/commercial_features.rds',
                                                     bycatch_file = 'outputs/03_AQM/03a_fFeaturesInt/bycatch_features.rds',
                                                     prov = FALSE,
                                                     noregret_file = 'outputs/11_NoRegret/11d_AQMRuns_NoProv/noregretclosures_Target50.rds',
                                                     outexcel = 'excel/no_regret_AQM_NoProv/',
                                                     target_name = 'Target50')
# Target 40
noregret_AQM_NoProv_sum_target40 <- fCreateNRSummary(commercial_file = 'outputs/05_Commercial/05d_fFeaturesInt/Global/commercial_features.rds',
                                                     bycatch_file = 'outputs/03_AQM/03a_fFeaturesInt/bycatch_features.rds',
                                                     prov = FALSE,
                                                     noregret_file = 'outputs/11_NoRegret/11d_AQMRuns_NoProv/noregretclosures_Target40.rds',
                                                     outexcel = 'excel/no_regret_AQM_NoProv/',
                                                     target_name = 'Target40')
# Target 30
noregret_AQM_NoProv_sum_target30 <- fCreateNRSummary(commercial_file = 'outputs/05_Commercial/05d_fFeaturesInt/Global/commercial_features.rds',
                                                     bycatch_file = 'outputs/03_AQM/03a_fFeaturesInt/bycatch_features.rds',
                                                     prov = FALSE,
                                                     noregret_file = 'outputs/11_NoRegret/11d_AQMRuns_NoProv/noregretclosures_Target30.rds',
                                                     outexcel = 'excel/no_regret_AQM_NoProv/',
                                                     target_name = 'Target30')
# Target 20
noregret_AQM_NoProv_sum_target20 <- fCreateNRSummary(commercial_file = 'outputs/05_Commercial/05d_fFeaturesInt/Global/commercial_features.rds',
                                                     bycatch_file = 'outputs/03_AQM/03a_fFeaturesInt/bycatch_features.rds',
                                                     prov = FALSE,
                                                     noregret_file = 'outputs/11_NoRegret/11d_AQMRuns_NoProv/noregretclosures_Target20.rds',
                                                     outexcel = 'excel/no_regret_AQM_NoProv/',
                                                     target_name = 'Target20')
# Target 10
noregret_AQM_NoProv_sum_target10 <- fCreateNRSummary(commercial_file = 'outputs/05_Commercial/05d_fFeaturesInt/Global/commercial_features.rds',
                                                     bycatch_file = 'outputs/03_AQM/03a_fFeaturesInt/bycatch_features.rds',
                                                     prov = FALSE,
                                                     noregret_file = 'outputs/11_NoRegret/11d_AQMRuns_NoProv/noregretclosures_Target10.rds',
                                                     outexcel = 'excel/no_regret_AQM_NoProv/',
                                                     target_name = 'Target10')
#########################################
######### IUCN No Provinces RUNS #########
#########################################
# Target 100
noregret_IUCN_NoProv_sum_target100 <- fCreateNRSummary(commercial_file = 'outputs/05_Commercial/05d_fFeaturesInt/Global/commercial_features.rds',
                                                       bycatch_file = 'outputs/04_IUCN/04d_fFeaturesInt/bycatch_features.rds',
                                                       prov = FALSE,
                                                       noregret_file = 'outputs/11_NoRegret/11e_IUCNRuns_NoProv/noregretclosures_Target100.rds',
                                                       outexcel = 'excel/no_regret_IUCN_NoProv/',
                                                       target_name = 'Target100')
# Target 90
noregret_IUCN_NoProv_sum_target90 <- fCreateNRSummary(commercial_file = 'outputs/05_Commercial/05d_fFeaturesInt/Global/commercial_features.rds',
                                                      bycatch_file = 'outputs/04_IUCN/04d_fFeaturesInt/bycatch_features.rds',
                                                      prov = FALSE,
                                                      noregret_file = 'outputs/11_NoRegret/11e_IUCNRuns_NoProv/noregretclosures_Target90.rds',
                                                      outexcel = 'excel/no_regret_IUCN_NoProv/',
                                                      target_name = 'Target90')
# Target 80
noregret_IUCN_NoProv_sum_target80 <- fCreateNRSummary(commercial_file = 'outputs/05_Commercial/05d_fFeaturesInt/Global/commercial_features.rds',
                                                      bycatch_file = 'outputs/04_IUCN/04d_fFeaturesInt/bycatch_features.rds',
                                                      prov = FALSE,
                                                      noregret_file = 'outputs/11_NoRegret/11e_IUCNRuns_NoProv/noregretclosures_Target80.rds',
                                                      outexcel = 'excel/no_regret_IUCN_NoProv/',
                                                      target_name = 'Target80')
# Target 70
noregret_IUCN_NoProv_sum_target70 <- fCreateNRSummary(commercial_file = 'outputs/05_Commercial/05d_fFeaturesInt/Global/commercial_features.rds',
                                                      bycatch_file = 'outputs/04_IUCN/04d_fFeaturesInt/bycatch_features.rds',
                                                      prov = FALSE,
                                                      noregret_file = 'outputs/11_NoRegret/11e_IUCNRuns_NoProv/noregretclosures_Target70.rds',
                                                      outexcel = 'excel/no_regret_IUCN_NoProv/',
                                                      target_name = 'Target70')
# Target 60
noregret_IUCN_NoProv_sum_target60 <- fCreateNRSummary(commercial_file = 'outputs/05_Commercial/05d_fFeaturesInt/Global/commercial_features.rds',
                                                      bycatch_file = 'outputs/04_IUCN/04d_fFeaturesInt/bycatch_features.rds',
                                                      prov = FALSE,
                                                      noregret_file = 'outputs/11_NoRegret/11e_IUCNRuns_NoProv/noregretclosures_Target60.rds',
                                                      outexcel = 'excel/no_regret_IUCN_NoProv/',
                                                      target_name = 'Target60')
# Target 50
noregret_IUCN_NoProv_sum_target50 <- fCreateNRSummary(commercial_file = 'outputs/05_Commercial/05d_fFeaturesInt/Global/commercial_features.rds',
                                                      bycatch_file = 'outputs/04_IUCN/04d_fFeaturesInt/bycatch_features.rds',
                                                      prov = FALSE,
                                                      noregret_file = 'outputs/11_NoRegret/11e_IUCNRuns_NoProv/noregretclosures_Target50.rds',
                                                      outexcel = 'excel/no_regret_IUCN_NoProv/',
                                                      target_name = 'Target50')
# Target 40
noregret_IUCN_NoProv_sum_target40 <- fCreateNRSummary(commercial_file = 'outputs/05_Commercial/05d_fFeaturesInt/Global/commercial_features.rds',
                                                      bycatch_file = 'outputs/04_IUCN/04d_fFeaturesInt/bycatch_features.rds',
                                                      prov = FALSE,
                                                      noregret_file = 'outputs/11_NoRegret/11e_IUCNRuns_NoProv/noregretclosures_Target40.rds',
                                                      outexcel = 'excel/no_regret_IUCN_NoProv/',
                                                      target_name = 'Target40')
# Target 30
noregret_IUCN_NoProv_sum_target30 <- fCreateNRSummary(commercial_file = 'outputs/05_Commercial/05d_fFeaturesInt/Global/commercial_features.rds',
                                                      bycatch_file = 'outputs/04_IUCN/04d_fFeaturesInt/bycatch_features.rds',
                                                      prov = FALSE,
                                                      noregret_file = 'outputs/11_NoRegret/11e_IUCNRuns_NoProv/noregretclosures_Target30.rds',
                                                      outexcel = 'excel/no_regret_IUCN_NoProv/',
                                                      target_name = 'Target30')
# Target 20
noregret_IUCN_NoProv_sum_target20 <- fCreateNRSummary(commercial_file = 'outputs/05_Commercial/05d_fFeaturesInt/Global/commercial_features.rds',
                                                      bycatch_file = 'outputs/04_IUCN/04d_fFeaturesInt/bycatch_features.rds',
                                                      prov = FALSE,
                                                      noregret_file = 'outputs/11_NoRegret/11e_IUCNRuns_NoProv/noregretclosures_Target20.rds',
                                                      outexcel = 'excel/no_regret_IUCN_NoProv/',
                                                      target_name = 'Target20')
# Target 10
noregret_IUCN_NoProv_sum_target10 <- fCreateNRSummary(commercial_file = 'outputs/05_Commercial/05d_fFeaturesInt/Global/commercial_features.rds',
                                                      bycatch_file = 'outputs/04_IUCN/04d_fFeaturesInt/bycatch_features.rds',
                                                      prov = FALSE,
                                                      noregret_file = 'outputs/11_NoRegret/11e_IUCNRuns_NoProv/noregretclosures_Target10.rds',
                                                      outexcel = 'excel/no_regret_IUCN_NoProv/',
                                                      target_name = 'Target10')
