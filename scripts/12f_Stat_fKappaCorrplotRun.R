# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.

# This code creates corrplot of Cohen's Kappa Coefficient.
# Also saves the correlations and p-values in .csv file.

# This function, solution_summary() requres the following inputs:
# 1. inpdir = where the .rds (sf) files are located & where the .csv file will be saved.
# 2. name_cols = names of the columns. see run for example.

# Function is found in 12e.

source("scripts/12e_Stat_fKappaCorrplot.R")

# Run with everything @ 0.1 - 1
#kappa_run01 <- fKappaCorrplot(inpdir = "outputs/12_Stat/12e_Kappa/run1/",
#                              name_cols = c("No-regret", "SSP1-2.6", "SSP2-4.5", "SSP5-8.5", "Uninformed")) # make sure it follows the files' alphabetical order

# Run with 0.1 - 1 w/o No-regret.
#kappa_run02 <- fKappaCorrplot(inpdir = "outputs/12_Stat/12e_Kappa/run2/",
#                              name_cols = c("SSP1-2.6", "SSP2-4.5", "SSP5-8.5", "Uninformed"))

# Run with 0.1 - 0.9 w/o No-regret
kappa_run03 <- fKappaCorrplot(inpdir = "outputs/12_Stat/12e_Kappa/run3/",
                              name_cols = c("SSP1-2.6", "SSP2-4.5", "SSP5-8.5", "Uninformed"))

# Run with 0.1 - 0.6 w/o No-regret
kappa_run04 <- fKappaCorrplot(inpdir = "outputs/12_Stat/12e_Kappa/run4/",
                              name_cols = c("SSP1-2.6", "SSP2-4.5", "SSP5-8.5", "Uninformed"))

# Run with 0.1 - 0.3 w/o No-regret
kappa_run05 <- fKappaCorrplot(inpdir = "outputs/12_Stat/12e_Kappa/run5/",
                              name_cols = c("SSP1-2.6", "SSP2-4.5", "SSP5-8.5", "Uninformed"))

##########################################
#### IUCN: No Prov Runs w/o No-regret ####
##########################################
# Run with 0.1 - 0.9 w/o No-regret
iucn_kappa_run06 <- fKappaCorrplot(inpdir = "outputs/12_Stat/12e_Kappa/run6/",
                                   name_cols = c("SSP1-2.6", "SSP2-4.5", "SSP5-8.5", "Uninformed"))
# Run with 0.1 - 0.6 w/o No-regret
iucn_kappa_run07 <- fKappaCorrplot(inpdir = "outputs/12_Stat/12e_Kappa/run7/",
                                   name_cols = c("SSP1-2.6", "SSP2-4.5", "SSP5-8.5", "Uninformed"))
# Run with 0.1 - 0.3 w/o No-regret
iucn_kappa_run08 <- fKappaCorrplot(inpdir = "outputs/12_Stat/12e_Kappa/run8/",
                                  name_cols = c("SSP1-2.6", "SSP2-4.5", "SSP5-8.5", "Uninformed"))
