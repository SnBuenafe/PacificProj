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
# 2. target_max = in proportion e.g. 0 - 1
# 3. target_min = in proportion e.g. 0 - 1
# 4. file_spec_info = .xlsx file of the species information (with the codes and the scientific names)
# 5. inpdir = directory where the .rds files of the conservation features for each scenario are
# 6. scenario = climate scenario (e.g. SSP126)
# 7. outdir = directory where the .rds files will be saved.

# Function is found in 08a.
# Note: .rds files saved without the scenario attached at the end of the filename are pacific-fitted commercial spp. distributions.

source("scripts/08a_targetfxn.R")

####################################
#### 100%, 0 - 1 RUN ####
####################################

# Run for SSP126
TARGET100_run01 <- represent_target(number_PU <- 31917,
                    target_max <- 1,
                    target_min <- 0.1,
                    file_spec_info <- "outputs/final_features/spec_info.xlsx",
                    inpdir <- "outputs/final_features/07a_25perc/",
                    scenario <- "SSP126",
                    outdir <- "outputs/final_features/08b_targets/01_Target100/")

# Run for SSP245
TARGET100_run02 <- represent_target(number_PU <- 31917,
                    target_max <- 1,
                    target_min <- 0.1,
                    file_spec_info <- "outputs/final_features/spec_info.xlsx",
                    inpdir <- "outputs/final_features/07a_25perc/",
                    scenario <- "SSP245",
                    outdir <- "outputs/final_features/08b_targets/01_Target100/")

# Run for SSP585
TARGET100_run03 <- represent_target(number_PU <- 31917,
                    target_max <- 1,
                    target_min <- 0.1,
                    file_spec_info <- "outputs/final_features/spec_info.xlsx",
                    inpdir <- "outputs/final_features/07a_25perc/",
                    scenario <- "SSP585",
                    outdir <- "outputs/final_features/08b_targets/01_Target100/")

####################################
#### 90%, 0.1 - 0.9 RUNS ####
####################################

# Run for SSP126
TARGET90_run01 <- represent_target(number_PU <- 31917,
                                    target_max <- 0.9,
                                    target_min <- 0.1,
                                    file_spec_info <- "outputs/final_features/spec_info.xlsx",
                                    inpdir <- "outputs/final_features/07a_25perc/",
                                    scenario <- "SSP126",
                                    outdir <- "outputs/final_features/08b_targets/02_Target90/")

# Run for SSP245
TARGET90_run02 <- represent_target(number_PU <- 31917,
                                    target_max <- 0.9,
                                    target_min <- 0.1,
                                    file_spec_info <- "outputs/final_features/spec_info.xlsx",
                                    inpdir <- "outputs/final_features/07a_25perc/",
                                    scenario <- "SSP245",
                                    outdir <- "outputs/final_features/08b_targets/02_Target90/")

# Run for SSP585
TARGET90_run03 <- represent_target(number_PU <- 31917,
                                    target_max <- 0.9,
                                    target_min <- 0.1,
                                    file_spec_info <- "outputs/final_features/spec_info.xlsx",
                                    inpdir <- "outputs/final_features/07a_25perc/",
                                    scenario <- "SSP585",
                                    outdir <- "outputs/final_features/08b_targets/02_Target90/")

####################################
#### 80%, 0.1 - 0.8 RUNS ####
####################################

# Run for SSP126
TARGET80_run01 <- represent_target(number_PU <- 31917,
                                   target_max <- 0.8,
                                   target_min <- 0.1,
                                   file_spec_info <- "outputs/final_features/spec_info.xlsx",
                                   inpdir <- "outputs/final_features/07a_25perc/",
                                   scenario <- "SSP126",
                                   outdir <- "outputs/final_features/08b_targets/03_Target80/")

# Run for SSP245
TARGET80_run02 <- represent_target(number_PU <- 31917,
                                   target_max <- 0.8,
                                   target_min <- 0.1,
                                   file_spec_info <- "outputs/final_features/spec_info.xlsx",
                                   inpdir <- "outputs/final_features/07a_25perc/",
                                   scenario <- "SSP245",
                                   outdir <- "outputs/final_features/08b_targets/03_Target80/")

# Run for SSP585
TARGET80_run03 <- represent_target(number_PU <- 31917,
                                   target_max <- 0.8,
                                   target_min <- 0.1,
                                   file_spec_info <- "outputs/final_features/spec_info.xlsx",
                                   inpdir <- "outputs/final_features/07a_25perc/",
                                   scenario <- "SSP585",
                                   outdir <- "outputs/final_features/08b_targets/03_Target80/")

####################################
#### 70%, 0.1 - 0.7 RUNS ####
####################################

# Run for SSP126
TARGET70_run01 <- represent_target(number_PU <- 31917,
                                   target_max <- 0.7,
                                   target_min <- 0.1,
                                   file_spec_info <- "outputs/final_features/spec_info.xlsx",
                                   inpdir <- "outputs/final_features/07a_25perc/",
                                   scenario <- "SSP126",
                                   outdir <- "outputs/final_features/08b_targets/04_Target70/")

# Run for SSP245
TARGET70_run02 <- represent_target(number_PU <- 31917,
                                   target_max <- 0.7,
                                   target_min <- 0.1,
                                   file_spec_info <- "outputs/final_features/spec_info.xlsx",
                                   inpdir <- "outputs/final_features/07a_25perc/",
                                   scenario <- "SSP245",
                                   outdir <- "outputs/final_features/08b_targets/04_Target70/")

 # Run for SSP585
TARGET70_run03 <- represent_target(number_PU <- 31917,
                                   target_max <- 0.7,
                                   target_min <- 0.1,
                                   file_spec_info <- "outputs/final_features/spec_info.xlsx",
                                   inpdir <- "outputs/final_features/07a_25perc/",
                                   scenario <- "SSP585",
                                   outdir <- "outputs/final_features/08b_targets/04_Target70/")

####################################
#### 60%, 0.1 - 0.6 RUNS ####
####################################

# Run for SSP126
TARGET60_run01 <- represent_target(number_PU <- 31917,
                                   target_max <- 0.6,
                                   target_min <- 0.1,
                                   file_spec_info <- "outputs/final_features/spec_info.xlsx",
                                   inpdir <- "outputs/final_features/07a_25perc/",
                                   scenario <- "SSP126",
                                   outdir <- "outputs/final_features/08b_targets/05_Target60/")

# Run for SSP245
TARGET60_run02 <- represent_target(number_PU <- 31917,
                                   target_max <- 0.6,
                                   target_min <- 0.1,
                                   file_spec_info <- "outputs/final_features/spec_info.xlsx",
                                   inpdir <- "outputs/final_features/07a_25perc/",
                                   scenario <- "SSP245",
                                   outdir <- "outputs/final_features/08b_targets/05_Target60/")

# Run for SSP585
TARGET60_run03 <- represent_target(number_PU <- 31917,
                                   target_max <- 0.6,
                                   target_min <- 0.1,
                                   file_spec_info <- "outputs/final_features/spec_info.xlsx",
                                   inpdir <- "outputs/final_features/07a_25perc/",
                                   scenario <- "SSP585",
                                   outdir <- "outputs/final_features/08b_targets/05_Target60/")

####################################
#### 50%, 0.1 - 0.5 RUNS ####
####################################

# Run for SSP126
TARGET50_run01 <- represent_target(number_PU <- 31917,
                                   target_max <- 0.5,
                                   target_min <- 0.1,
                                   file_spec_info <- "outputs/final_features/spec_info.xlsx",
                                   inpdir <- "outputs/final_features/07a_25perc/",
                                   scenario <- "SSP126",
                                   outdir <- "outputs/final_features/08b_targets/06_Target50/")

# Run for SSP245
TARGET50_run02 <- represent_target(number_PU <- 31917,
                                   target_max <- 0.5,
                                   target_min <- 0.1,
                                   file_spec_info <- "outputs/final_features/spec_info.xlsx",
                                   inpdir <- "outputs/final_features/07a_25perc/",
                                   scenario <- "SSP245",
                                   outdir <- "outputs/final_features/08b_targets/06_Target50/")

# Run for SSP585
TARGET50_run03 <- represent_target(number_PU <- 31917,
                                   target_max <- 0.5,
                                   target_min <- 0.1,
                                   file_spec_info <- "outputs/final_features/spec_info.xlsx",
                                   inpdir <- "outputs/final_features/07a_25perc/",
                                   scenario <- "SSP585",
                                   outdir <- "outputs/final_features/08b_targets/06_Target50/")

####################################
#### 40%, 0.1 - 0.4 RUNS ####
####################################

# Run for SSP126
TARGET40_run01 <- represent_target(number_PU <- 31917,
                                   target_max <- 0.4,
                                   target_min <- 0.1,
                                   file_spec_info <- "outputs/final_features/spec_info.xlsx",
                                   inpdir <- "outputs/final_features/07a_25perc/",
                                   scenario <- "SSP126",
                                   outdir <- "outputs/final_features/08b_targets/07_Target40/")

# Run for SSP245
TARGET40_run02 <- represent_target(number_PU <- 31917,
                                   target_max <- 0.4,
                                   target_min <- 0.1,
                                   file_spec_info <- "outputs/final_features/spec_info.xlsx",
                                   inpdir <- "outputs/final_features/07a_25perc/",
                                   scenario <- "SSP245",
                                   outdir <- "outputs/final_features/08b_targets/07_Target40/")

# Run for SSP585
TARGET40_run03 <- represent_target(number_PU <- 31917,
                                   target_max <- 0.4,
                                   target_min <- 0.1,
                                   file_spec_info <- "outputs/final_features/spec_info.xlsx",
                                   inpdir <- "outputs/final_features/07a_25perc/",
                                   scenario <- "SSP585",
                                   outdir <- "outputs/final_features/08b_targets/07_Target40/")

####################################
#### 30%, 0.1 - 0.3 RUNS ####
####################################

# Run for SSP126
TARGET30_run01 <- represent_target(number_PU <- 31917,
                                   target_max <- 0.3,
                                   target_min <- 0.1,
                                   file_spec_info <- "outputs/final_features/spec_info.xlsx",
                                   inpdir <- "outputs/final_features/07a_25perc/",
                                   scenario <- "SSP126",
                                   outdir <- "outputs/final_features/08b_targets/08_Target30/")

# Run for SSP245
TARGET30_run02 <- represent_target(number_PU <- 31917,
                                   target_max <- 0.3,
                                   target_min <- 0.1,
                                   file_spec_info <- "outputs/final_features/spec_info.xlsx",
                                   inpdir <- "outputs/final_features/07a_25perc/",
                                   scenario <- "SSP245",
                                   outdir <- "outputs/final_features/08b_targets/08_Target30/")

# Run for SSP585
TARGET30_run03 <- represent_target(number_PU <- 31917,
                                   target_max <- 0.3,
                                   target_min <- 0.1,
                                   file_spec_info <- "outputs/final_features/spec_info.xlsx",
                                   inpdir <- "outputs/final_features/07a_25perc/",
                                   scenario <- "SSP585",
                                   outdir <- "outputs/final_features/08b_targets/08_Target30/")

####################################
#### 20%, 0.1 - 0.2 RUNS ####
####################################

# Run for SSP126
TARGET20_run01 <- represent_target(number_PU <- 31917,
                                   target_max <- 0.2,
                                   target_min <- 0.1,
                                   file_spec_info <- "outputs/final_features/spec_info.xlsx",
                                   inpdir <- "outputs/final_features/07a_25perc/",
                                   scenario <- "SSP126",
                                   outdir <- "outputs/final_features/08b_targets/09_Target20/")

# Run for SSP245
TARGET20_run02 <- represent_target(number_PU <- 31917,
                                   target_max <- 0.2,
                                   target_min <- 0.1,
                                   file_spec_info <- "outputs/final_features/spec_info.xlsx",
                                   inpdir <- "outputs/final_features/07a_25perc/",
                                   scenario <- "SSP245",
                                   outdir <- "outputs/final_features/08b_targets/09_Target20/")

# Run for SSP585
TARGET20_run03 <- represent_target(number_PU <- 31917,
                                   target_max <- 0.2,
                                   target_min <- 0.1,
                                   file_spec_info <- "outputs/final_features/spec_info.xlsx",
                                   inpdir <- "outputs/final_features/07a_25perc/",
                                   scenario <- "SSP585",
                                   outdir <- "outputs/final_features/08b_targets/09_Target20/")

####################################
#### 10%, 0 - 0.1 RUNS ####
####################################

# Run for SSP126
TARGET10_run01 <- represent_target(number_PU <- 31917,
                                   target_max <- 0.1,
                                   target_min <- 0,
                                   file_spec_info <- "outputs/final_features/spec_info.xlsx",
                                   inpdir <- "outputs/final_features/07a_25perc/",
                                   scenario <- "SSP126",
                                   outdir <- "outputs/final_features/08b_targets/10_Target10/")

# Run for SSP245
TARGET10_run02 <- represent_target(number_PU <- 31917,
                                   target_max <- 0.1,
                                   target_min <- 0,
                                   file_spec_info <- "outputs/final_features/spec_info.xlsx",
                                   inpdir <- "outputs/final_features/07a_25perc/",
                                   scenario <- "SSP245",
                                   outdir <- "outputs/final_features/08b_targets/10_Target10/")

# Run for SSP585
TARGET10_run03 <- represent_target(number_PU <- 31917,
                                   target_max <- 0.1,
                                   target_min <- 0,
                                   file_spec_info <- "outputs/final_features/spec_info.xlsx",
                                   inpdir <- "outputs/final_features/07a_25perc/",
                                   scenario <- "SSP585",
                                   outdir <- "outputs/final_features/08b_targets/10_Target10/")
#############
## testing ##
#############

df_temp <- readRDS("outputs/final_features/08b_targets/04_Target70/SSP245/target_commercialSSP245.rds")


