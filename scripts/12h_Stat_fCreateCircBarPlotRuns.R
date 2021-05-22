# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.

# This function creates circular bar plots to study the representation of each features with respect to their targets
# To run, it requires the following inputs:
# 1. bycatch = 'bycatch' or 'bycatchIUCN' for AQM and IUCN datasets, respectively
# 2. commercial = 'commercial' or 'commercialpac' for global and pacific datasets, respectively
# 3. target_inpdir = directory of the targets for each max representation target
# 4. features.summary_csv = .csv file of the summary of the solutions of prioritizr (feat representation)
# 5. target = target name (must be consistent)

# Function is found in 12g.

source('scripts/12g_Stat_fCreateCircBarPlot.R')

##############################
## AQM (No Prov) Runs ##
##############################
# Target 100
#circplot_target100 <- fCreateCircBarPlot(bycatch = 'bycatch',
#                                         commercial = 'commercial',
#                                         target_inpdir = 'outputs/09_Target/09d-e_TargetRuns/01_Target100/',
#                                         features.summary_csv = 'outputs/12_Stat/12b_fSolutionSummary/NoProv/AQM_features_rep_summary.csv',
#                                         target = 'Target100')
# Target 90
#circplot_target90 <- fCreateCircBarPlot(bycatch = 'bycatch',
#                                        commercial = 'commercial',
#                                        target_inpdir = 'outputs/09_Target/09d-e_TargetRuns/02_Target90/',
#                                        features.summary_csv = 'outputs/12_Stat/12b_fSolutionSummary/NoProv/AQM_features_rep_summary.csv',
#                                        target = 'Target90')
# Target 80
#circplot_target80 <- fCreateCircBarPlot(bycatch = 'bycatch',
#                                        commercial = 'commercial',
#                                        target_inpdir = 'outputs/09_Target/09d-e_TargetRuns/03_Target80/',
#                                      features.summary_csv = 'outputs/12_Stat/12b_fSolutionSummary/NoProv/AQM_features_rep_summary.csv',
#                                        target = 'Target80')
# Target 70
#circplot_target70 <- fCreateCircBarPlot(bycatch = 'bycatch',
#                                        commercial = 'commercial',
#                                        target_inpdir = 'outputs/09_Target/09d-e_TargetRuns/04_Target70/',
#                                        features.summary_csv = 'outputs/12_Stat/12b_fSolutionSummary/NoProv/AQM_features_rep_summary.csv',
#                                        target = 'Target70')
# Target 60
#circplot_target60 <- fCreateCircBarPlot(bycatch = 'bycatch',
#                                        commercial = 'commercial',
#                                        target_inpdir = 'outputs/09_Target/09d-e_TargetRuns/05_Target60/',
#                                        features.summary_csv = 'outputs/12_Stat/12b_fSolutionSummary/NoProv/AQM_features_rep_summary.csv',
#                                        target = 'Target60')
# Target 50
#circplot_target50 <- fCreateCircBarPlot(bycatch = 'bycatch',
#                                        commercial = 'commercial',
#                                        target_inpdir = 'outputs/09_Target/09d-e_TargetRuns/06_Target50/',
#                                        features.summary_csv = 'outputs/12_Stat/12b_fSolutionSummary/NoProv/AQM_features_rep_summary.csv',
#                                        target = 'Target50')
# Target 40
#circplot_target40 <- fCreateCircBarPlot(bycatch = 'bycatch',
#                                        commercial = 'commercial',
#                                        target_inpdir = 'outputs/09_Target/09d-e_TargetRuns/07_Target40/',
#                                        features.summary_csv = 'outputs/12_Stat/12b_fSolutionSummary/NoProv/AQM_features_rep_summary.csv',
#                                        target = 'Target40')
# Target 30
#circplot_target30 <- fCreateCircBarPlot(bycatch = 'bycatch',
#                                        commercial = 'commercial',
#                                        target_inpdir = 'outputs/09_Target/09d-e_TargetRuns/08_Target30/',
#                                        features.summary_csv = 'outputs/12_Stat/12b_fSolutionSummary/NoProv/AQM_features_rep_summary.csv',
#                                        target = 'Target30')
# Target 20
#circplot_target20 <- fCreateCircBarPlot(bycatch = 'bycatch',
#                                        commercial = 'commercial',
#                                        target_inpdir = 'outputs/09_Target/09d-e_TargetRuns/09_Target20/',
#                                        features.summary_csv = 'outputs/12_Stat/12b_fSolutionSummary/NoProv/AQM_features_rep_summary.csv',
#                                        target = 'Target20')
# Target 10
#circplot_target10 <- fCreateCircBarPlot(bycatch = 'bycatch',
#                                        commercial = 'commercial',
#                                        target_inpdir = 'outputs/09_Target/09d-e_TargetRuns/10_Target10/',
#                                        features.summary_csv = 'outputs/12_Stat/12b_fSolutionSummary/NoProv/AQM_features_rep_summary.csv',
#                                        target = 'Target10')

####
#library(patchwork)

#plots <- (circplot_target100 + circplot_target90 + circplot_target80) /
#  (circplot_target70 + circplot_target60 + circplot_target50) /
#  (circplot_target40 + circplot_target30 + circplot_target20) /
#  (plot_spacer() + circplot_target10 + plot_spacer()) +
#  plot_layout(guides = 'collect') & theme(legend.position = 'bottom')
#plots
#ggsave('pdfs/12_Stat/representation_AQM.pdf', width = 21, height = 29.7)

##############################
## IUCN (No Prov) Runs ##
##############################
# Target 100
circplot_target100 <- fCreateCircBarPlot(bycatch = 'bycatchIUCN',
                                         commercial = 'commercial',
                                         target_inpdir = 'outputs/09_Target/09d-e_TargetRuns/01_Target100/',
                                         features.summary_csv = 'outputs/12_Stat/12b_fSolutionSummary/NoProv/IUCN_features_rep_summary.csv',
                                         target = 'Target100')
# Target 90
circplot_target90 <- fCreateCircBarPlot(bycatch = 'bycatchIUCN',
                                        commercial = 'commercial',
                                        target_inpdir = 'outputs/09_Target/09d-e_TargetRuns/02_Target90/',
                                        features.summary_csv = 'outputs/12_Stat/12b_fSolutionSummary/NoProv/IUCN_features_rep_summary.csv',
                                        target = 'Target90')
# Target 80
circplot_target80 <- fCreateCircBarPlot(bycatch = 'bycatchIUCN',
                                        commercial = 'commercial',
                                        target_inpdir = 'outputs/09_Target/09d-e_TargetRuns/03_Target80/',
                                        features.summary_csv = 'outputs/12_Stat/12b_fSolutionSummary/NoProv/IUCN_features_rep_summary.csv',
                                        target = 'Target80')
# Target 70
circplot_target70 <- fCreateCircBarPlot(bycatch = 'bycatchIUCN',
                                        commercial = 'commercial',
                                        target_inpdir = 'outputs/09_Target/09d-e_TargetRuns/04_Target70/',
                                        features.summary_csv = 'outputs/12_Stat/12b_fSolutionSummary/NoProv/IUCN_features_rep_summary.csv',
                                        target = 'Target70')
# Target 60
circplot_target60 <- fCreateCircBarPlot(bycatch = 'bycatchIUCN',
                                        commercial = 'commercial',
                                        target_inpdir = 'outputs/09_Target/09d-e_TargetRuns/05_Target60/',
                                        features.summary_csv = 'outputs/12_Stat/12b_fSolutionSummary/NoProv/IUCN_features_rep_summary.csv',
                                        target = 'Target60')
# Target 50
circplot_target50 <- fCreateCircBarPlot(bycatch = 'bycatchIUCN',
                                        commercial = 'commercial',
                                        target_inpdir = 'outputs/09_Target/09d-e_TargetRuns/06_Target50/',
                                        features.summary_csv = 'outputs/12_Stat/12b_fSolutionSummary/NoProv/IUCN_features_rep_summary.csv',
                                        target = 'Target50')
# Target 40
circplot_target40 <- fCreateCircBarPlot(bycatch = 'bycatchIUCN',
                                        commercial = 'commercial',
                                        target_inpdir = 'outputs/09_Target/09d-e_TargetRuns/07_Target40/',
                                        features.summary_csv = 'outputs/12_Stat/12b_fSolutionSummary/NoProv/IUCN_features_rep_summary.csv',
                                        target = 'Target40')
# Target 30
circplot_target30 <- fCreateCircBarPlot(bycatch = 'bycatchIUCN',
                                        commercial = 'commercial',
                                        target_inpdir = 'outputs/09_Target/09d-e_TargetRuns/08_Target30/',
                                        features.summary_csv = 'outputs/12_Stat/12b_fSolutionSummary/NoProv/IUCN_features_rep_summary.csv',
                                        target = 'Target30')
# Target 20
circplot_target20 <- fCreateCircBarPlot(bycatch = 'bycatchIUCN',
                                        commercial = 'commercial',
                                        target_inpdir = 'outputs/09_Target/09d-e_TargetRuns/09_Target20/',
                                        features.summary_csv = 'outputs/12_Stat/12b_fSolutionSummary/NoProv/IUCN_features_rep_summary.csv',
                                        target = 'Target20')
# Target 10
circplot_target10 <- fCreateCircBarPlot(bycatch = 'bycatchIUCN',
                                        commercial = 'commercial',
                                        target_inpdir = 'outputs/09_Target/09d-e_TargetRuns/10_Target10/',
                                        features.summary_csv = 'outputs/12_Stat/12b_fSolutionSummary/NoProv/IUCN_features_rep_summary.csv',
                                        target = 'Target10')
####
library(patchwork)

plot1 <- (circplot_target100 + circplot_target90 + circplot_target80) +
  plot_layout(guides = 'collect') & theme(legend.position = 'bottom')
plot2 <- (circplot_target70 + circplot_target60 + circplot_target50) +
  plot_layout(guides = 'collect') & theme(legend.position = 'bottom')
plot3 <- (circplot_target40 + circplot_target30 + circplot_target20) / 
  (plot_spacer() + circplot_target10 + plot_spacer()) +
  plot_layout(guides = 'collect') & theme(legend.position = 'bottom')
plot1 / plot2 / plot3

ggsave('pdfs/12_Stat/representation_IUCN.pdf', width = 21, height = 29.7)
