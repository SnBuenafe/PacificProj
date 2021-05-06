# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.

# This code saves .csv files of a summary of the metrics for each representation target.
# 1. for the variables of the solution itself, and 
# 2. for the relative representation of all of the features

# This function, solution_summary() requres the following inputs:
# 1. inpdir = where the .csv files of each solution are found.
# 2. outdir = where to save the final .csv files

# Function is found at 12a.

source("scripts/12a_SolutionSummaryFxn.R")

solution_summary(inpdir = "excel/AQM/",
                outdir = "excel/",
                plans = "climate-smart")

solution_summary(inpdir = "excel/no_regret/",
                 outdir = "excel/no_regret",
                 plans = "noregret")
