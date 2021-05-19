# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.
# most are modified from IBM's code.

# This code assigns the representation targets for each feature depending on the IUCN category of the species.
# If the species belongs in a threatened category (i.e. "EX","EW","CR","EN","VU"), the target is 100%.
# If the species does not belong in a threatened category, the target is computed as follows:
# target_max * (PUs / PUtotal) * (target_max - target_min)
# Note that the areas of the features here are those belonging to the lower quartile of the RCE | velocity 
# It saves the summarized output as an .rds file 

# The function represent_target() requires the following inputs:
# 1. number_PU = total number of PUs in study area.
# 2. target_max = in percentage e.g. 100
# 3. target_min = in percentage e.g. 0
# 4. file_spec_info = .xlsx file of the species information (with the codes and the scientific names)
# 5. inpdir = directory where the .rds files of the conservation features for each scenario are
# 6. scenario = climate scenario (e.g. SSP126)
# 7. outdir = directory where the .rds files will be saved.
# 8. prov = TRUE/FALSE (including provinces or not)

# Function is found in 09a.
# These runs are for CLIMATE-UNINFORMED.

source("scripts/09a_Target_fRepresentTarget.R")

####################################
#### 100%, 0.1 - 1 RUN ####
####################################
TARGET100uninformed_run01 <- fRepresentTarget(number_PU <- 31917,
                                              target_max <- 1,
                                              target_min <- 0.1,
                                              file_spec_info <- "outputs/09_Target/spec_info.xlsx",
                                              inpdir <- "outputs/08_Filter/08e_Filter100_NoProv/",
                                              scenario <- "uninformed",
                                              outdir <- "outputs/09_Target/09d-e_TargetRuns/01_Target100/",
                                              prov = FALSE)
####################################
#### 90%, 0.1 - 0.9 RUN ####
####################################
TARGET90uninformed_run01 <- fRepresentTarget(number_PU <- 31917,
                                              target_max <- 0.9,
                                              target_min <- 0.1,
                                              file_spec_info <- "outputs/09_Target/spec_info.xlsx",
                                              inpdir <- "outputs/08_Filter/08e_Filter100_NoProv/",
                                              scenario <- "uninformed",
                                              outdir <- "outputs/09_Target/09d-e_TargetRuns/02_Target90/",
                                              prov = FALSE)
####################################
#### 80%, 0.1 - 0.8 RUN ####
####################################
TARGET80uninformed_run01 <- fRepresentTarget(number_PU <- 31917,
                                              target_max <- 0.8,
                                              target_min <- 0.1,
                                              file_spec_info <- "outputs/09_Target/spec_info.xlsx",
                                              inpdir <- "outputs/08_Filter/08e_Filter100_NoProv/",
                                              scenario <- "uninformed",
                                              outdir <- "outputs/09_Target/09d-e_TargetRuns/03_Target80/",
                                              prov = FALSE)
####################################
#### 70%, 0.1 - 0.7 RUN ####
####################################
TARGET70uninformed_run01 <- fRepresentTarget(number_PU <- 31917,
                                             target_max <- 0.7,
                                             target_min <- 0.1,
                                             file_spec_info <- "outputs/09_Target/spec_info.xlsx",
                                             inpdir <- "outputs/08_Filter/08e_Filter100_NoProv/",
                                             scenario <- "uninformed",
                                             outdir <- "outputs/09_Target/09d-e_TargetRuns/04_Target70/",
                                             prov = FALSE)
####################################
#### 60%, 0.1 - 0.6 RUN ####
####################################
TARGET60uninformed_run01 <- fRepresentTarget(number_PU <- 31917,
                                             target_max <- 0.6,
                                             target_min <- 0.1,
                                             file_spec_info <- "outputs/09_Target/spec_info.xlsx",
                                             inpdir <- "outputs/08_Filter/08e_Filter100_NoProv/",
                                             scenario <- "uninformed",
                                             outdir <- "outputs/09_Target/09d-e_TargetRuns/05_Target60/",
                                             prov = FALSE)
####################################
#### 50%, 0.1 - 0.5 RUN ####
####################################
TARGET50uninformed_run01 <- fRepresentTarget(number_PU <- 31917,
                                             target_max <- 0.5,
                                             target_min <- 0.1,
                                             file_spec_info <- "outputs/09_Target/spec_info.xlsx",
                                             inpdir <- "outputs/08_Filter/08e_Filter100_NoProv/",
                                             scenario <- "uninformed",
                                             outdir <- "outputs/09_Target/09d-e_TargetRuns/06_Target50/",
                                             prov = FALSE)
####################################
#### 40%, 0.1 - 0.4 RUN ####
####################################
TARGET40uninformed_run01 <- fRepresentTarget(number_PU <- 31917,
                                             target_max <- 0.4,
                                             target_min <- 0.1,
                                             file_spec_info <- "outputs/09_Target/spec_info.xlsx",
                                             inpdir <- "outputs/08_Filter/08e_Filter100_NoProv/",
                                             scenario <- "uninformed",
                                             outdir <- "outputs/09_Target/09d-e_TargetRuns/07_Target40/",
                                             prov = FALSE)
####################################
#### 30%, 0.1 - 0.3 RUN ####
####################################
TARGET30uninformed_run01 <- fRepresentTarget(number_PU <- 31917,
                                             target_max <- 0.3,
                                             target_min <- 0.1,
                                             file_spec_info <- "outputs/09_Target/spec_info.xlsx",
                                             inpdir <- "outputs/08_Filter/08e_Filter100_NoProv/",
                                             scenario <- "uninformed",
                                             outdir <- "outputs/09_Target/09d-e_TargetRuns/08_Target30/",
                                             prov = FALSE)
####################################
#### 20%, 0.1 - 0.2 RUN ####
####################################
TARGET20uninformed_run01 <- fRepresentTarget(number_PU <- 31917,
                                             target_max <- 0.2,
                                             target_min <- 0.1,
                                             file_spec_info <- "outputs/09_Target/spec_info.xlsx",
                                             inpdir <- "outputs/08_Filter/08e_Filter100_NoProv/",
                                             scenario <- "uninformed",
                                             outdir <- "outputs/09_Target/09d-e_TargetRuns/09_Target20/",
                                             prov = FALSE)
####################################
#### 10%, 0 - 0.1 RUN ####
####################################
TARGET10uninformed_run01 <- fRepresentTarget(number_PU <- 31917,
                                             target_max <- 0.1,
                                             target_min <- 0,
                                             file_spec_info <- "outputs/09_Target/spec_info.xlsx",
                                             inpdir <- "outputs/08_Filter/08e_Filter100_NoProv/",
                                             scenario <- "uninformed",
                                             outdir <- "outputs/09_Target/09d-e_TargetRuns/10_Target10/",
                                             prov = FALSE)
