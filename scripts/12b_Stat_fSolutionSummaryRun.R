# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.

# This code saves .csv files of a summary of the metrics for each representation target.
# 1. for the variables of the solution itself, and 
# 2. for the relative representation of all of the features

# This function, solution_summary() requires the following inputs:
# 1. inpdir = where the .csv files of each solution are found.
# 2. outdir = where to save the final .csv files

# Function is found at 12a.

source("scripts/12a_Stat_fSolutionSummary.R")

############################
## Runs with provinces ##
############################

AQM_run1 <- fSolutionSummary(inpdir = "excel/AQM/",
                            outdir = "outputs/12_Stat/12b_fSolutionSummary/Prov/",
                            plans = 'climate-smart',
                            data = "AQM")

AQM_noregret_run1 <- fSolutionSummary(inpdir = "excel/no_regret_AQM/",
                                      outdir = "outputs/12_Stat/12b_fSolutionSummary/Prov/",
                                    plans = "noregret",
                                    data = "AQM_noregret")

IUCN_run1 <- fSolutionSummary(inpdir = "excel/IUCN/",
                              outdir = "outputs/12_Stat/12b_fSolutionSummary/Prov/",
                            plans = "climate-smart",
                            data = "IUCN")

IUCN_noregret_run1 <- fSolutionSummary(inpdir = "excel/no_regret_IUCN/",
                                       outdir = "outputs/12_Stat/12b_fSolutionSummary/Prov/",
                                      plans = "noregret",
                                      data = "IUCN_noregret")

############################
## Runs without provinces ##
############################

AQM_run2 <- fSolutionSummary(inpdir = "excel/AQM_NoProv/",
                             outdir = "outputs/12_Stat/12b_fSolutionSummary/NoProv/",
                            plans = "climate-smart",
                            data = "AQM")

AQM_noregret_run2 <- fSolutionSummary(inpdir = "excel/no_regret_AQM_NoProv/",
                                      outdir = "outputs/12_Stat/12b_fSolutionSummary/NoProv/",
                                     plans = "noregret",
                                     data = "AQM_noregret")

IUCN_run2 <- fSolutionSummary(inpdir = "excel/IUCN_NoProv/",
                              outdir = "outputs/12_Stat/12b_fSolutionSummary/NoProv/",
                              plans = "climate-smart",
                              data = "IUCN")

IUCN_noregret_run2 <- fSolutionSummary(inpdir = "excel/no_regret_IUCN_NoProv/",
                                       outdir = "outputs/12_Stat/12b_fSolutionSummary/NoProv/",
                                       plans = "noregret",
                                       data = "IUCN_noregret")
