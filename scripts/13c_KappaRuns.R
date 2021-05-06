# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.

# This code creates corrplot of Cohen's Kappa Coefficient.
# Also saves the correlations and p-values in .csv file.

# This function, solution_summary() requres the following inputs:
# 1. inpdir = where the .rds (sf) files are located & where the .csv file will be saved.
# 2. name_cols = names of the columns. see run for example.

# Function is found in 13b.

source("scripts/13b_Kappa.R")

kappa_run01 <- kappa_corrplot(inpdir = "outputs/kappa_coefficient/run1/",
                              name_cols = c("No-regret", "SSP1-2.6", "SSP2-4.5", "SSP5-8.5", "Uninformed")) # make sure it follows the files' alphabetical order
