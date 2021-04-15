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

# Function is found in 08a.
# Note: .rds files saved without the scenario attached at the end of the filename are pacific-fitted commercial spp. distributions.

source("scripts/08a_targetfxn.R")

# Run for SSP126

run26 <- represent_target(number_PU <- 31917,
                         target_max <- 1,
                         target_min <- 0,
                         file_spec_info <- "outputs/final_features/spec_info.xlsx",
                         inpdir <- "outputs/final_features/07a_25perc/",
                         scenario <- "SSP126",
                         outdir <- "outputs/final_features/08b_targets/",
                         target_max_perc <- 100)

# Run for SSP245

run27 <- represent_target(number_PU <- 31917,
                          target_max <- 1,
                          target_min <- 0,
                          file_spec_info <- "outputs/final_features/spec_info.xlsx",
                          inpdir <- "outputs/final_features/07a_25perc/",
                          scenario <- "SSP245",
                          outdir <- "outputs/final_features/08b_targets/",
                          target_max_perc <- 100)

# Run for SSP585

run28 <- represent_target(number_PU <- 31917,
                          target_max <- 1,
                          target_min <- 0,
                          file_spec_info <- "outputs/final_features/spec_info.xlsx",
                          inpdir <- "outputs/final_features/07a_25perc/",
                          scenario <- "SSP585",
                          outdir <- "outputs/final_features/08b_targets/",
                          target_max_perc <- 100)

# works for bycatch
df_temp <- readRDS("outputs/final_features/08b_targets/SSP585/target_bycatchSSP585.rds")
df_temp1 <- readRDS("outputs/final_features/08b_targets/SSP126/target_commercialSSP126.rds")
df_temp2 <- readRDS("outputs/final_features/08b_targets/SSP245/target_bycatchSSP245.rds")
