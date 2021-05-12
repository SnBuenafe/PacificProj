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

source("scripts/12a_Stat_fSolutionSummary.R")

AQM_run <- fSolutionSummary(inpdir = "excel/AQM/",
                            outdir = "excel/",
                            plans = "climate-smart")

AQM_noregret_run <- fSolutionSummary(inpdir = "excel/no_regret_AQM/",
                                    outdir = "excel/",
                                    plans = "noregret")

IUCN_run <- fSolutionSummary(inpdir = "excel/IUCN/",
                            outdir = "excel/",
                            plans = "climate-smart")

IUCN_noregret_run <- fSolutionSummary(inpdir = "excel/no_regret_IUCN/",
                                      outdir = "excel/",
                                      plans = "noregret")
