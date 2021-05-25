# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.

# This function further summarizes the statistics generated from the prioritizr() runs
# It requires the following inputs:
# 1. inpdir: where the .csv files of the summaries are
# 2. outdir: where the .csv files of the summaries should be saved (separate for w/ Provinces and w/o Provinces)
# 3. name: e.g. "AQM", "IUCN", "AQM_noregret", "IUCN_noregret"

# Function is found in 12c.

source('scripts/12c_Stat_fCreateSummary.R')

#######################################
#### Runs with Longhurst Provinces ####
#######################################

# AQM Run
summary_AQM <- fCreateSummary(inpdir = 'outputs/12_Stat/12b_fSolutionSummary/Prov/',
                               outdir = 'outputs/12_Stat/12d_fCreateSummary/Prov/',
                               name = "AQM")
# IUCN Run
summary_IUCN <- fCreateSummary(inpdir = 'outputs/12_Stat/12b_fSolutionSummary/Prov/',
                               outdir = 'outputs/12_Stat/12d_fCreateSummary/Prov/',
                               name = "IUCN")

# AQM No-regret
summary_AQM_noregret <- fCreateSummary(inpdir = 'outputs/12_Stat/12b_fSolutionSummary/Prov/',
                                       outdir = 'outputs/12_Stat/12d_fCreateSummary/Prov/',
                                       name = "AQM_noregret")
# IUCN No-regret
summary_IUCN_noregret <- fCreateSummary(inpdir = 'outputs/12_Stat/12b_fSolutionSummary/Prov/',
                                        outdir = 'outputs/12_Stat/12d_fCreateSummary/Prov/',
                                        name = "IUCN_noregret")

##########################################
#### Runs without Longhurst Provinces ####
##########################################

############
## Runs ##
############

# AQM Run
summary_noprov_AQM <- fCreateSummary(inpdir = 'outputs/12_Stat/12b_fSolutionSummary/NoProv/',
                              outdir = 'outputs/12_Stat/12d_fCreateSummary/NoProv/',
                              name = "AQM")
# IUCN Run
summary_noprov_IUCN <- fCreateSummary(inpdir = 'outputs/12_Stat/12b_fSolutionSummary/NoProv/',
                               outdir = 'outputs/12_Stat/12d_fCreateSummary/NoProv/',
                               name = "IUCN")

# AQM No-regret
summary_noprov_AQM_noregret <- fCreateSummary(inpdir = 'outputs/12_Stat/12b_fSolutionSummary/NoProv/',
                                       outdir = 'outputs/12_Stat/12d_fCreateSummary/NoProv/',
                                       name = "AQM_noregret")
# IUCN No-regret
summary_noprovIUCN_noregret <- fCreateSummary(inpdir = 'outputs/12_Stat/12b_fSolutionSummary/NoProv/',
                                        outdir = 'outputs/12_Stat/12d_fCreateSummary/NoProv/',
                                        name = "IUCN_noregret")

###########
## Tests ##
###########
library(tidyverse)
library(patchwork)
# Just testing IUCN here (w/o Longhurst Provinces)

#################################################
## Runs for climate-smart ##
#################################################
summary_df <- summary_noprov_IUCN

# RUNS FOR COST
# test for normality
shapiro.test(summary_df$total_cost) # not normal
# log-transform
shapiro.test(log10(summary_df$total_cost)) # normal! p-value > 0.05.
#hist(log10(summary_df$total_cost)) # looks normal in histogram plot!
#qqnorm(log10(summary_df$total_cost)) # looks normal in Q-Q plot!
# test for differences across targets, scenarios
cost_model <- aov(log10(total_cost) ~ as.factor(target) + as.factor(scenario), data = summary_df)
summary(cost_model)
# post-hoc
TukeyHSD(cost_model) # all are significant except SSP585-SSP245

# testing normality and homoscedasticity of residuals of model
cost_residuals <- rstandard(cost_model)
#qqnorm(cost_residuals)
#qqline(cost_residuals)
# not too bad

# plotting cost
cost_plot <- ggplot(data = summary_df, aes(x = reorder(target, total_cost), group = scenario)) +
                    geom_line(aes(color = scenario, y = log10(total_cost)), size = 0.5)+
                    geom_point(aes(shape = scenario, color = scenario,y = log10(total_cost)), size = 3) +
                    scale_color_brewer(palette = "Set2") +
                    xlab("Target (%)") + 
                    ylab("log10(cost)") +
                    theme(legend.position = "bottom") +
                    theme_classic()
#cost_plot

# RUNS FOR % area protected
# test for normality 
shapiro.test(summary_df$percent_area) # not normal
# try log transforming
hist(summary_df$percent_area)
qqnorm(summary_df$percent_area) # looks normal in Q-Q plot !
# test differences across different targets and scenarios:
percentarea_model <- aov(percent_area ~ as.factor(target) + as.factor(scenario), data = summary_df)
summary(percentarea_model)
# post-hoc
TukeyHSD(percentarea_model) # all are significant !

# testing normality and homoscedasticity of residuals of model
percentarea_residuals <- rstandard(percentarea_model)
#qqnorm(percentarea_residuals)
#qqline(percentarea_residuals)
# not too bad

# plotting percent area
percentarea_plot <- ggplot(data = summary_df, aes(x = reorder(target, percent_area), group = scenario)) +
  geom_line(aes(color = scenario, y = percent_area), size = 0.5)+
  geom_point(aes(shape = scenario, color = scenario,y = percent_area), size = 3) +
  scale_color_brewer(palette = "Set2") +
  xlab("Target (%)") + 
  ylab("Protected Area (%)") +
  theme(legend.position = "bottom") +
  theme_classic()
#percentarea_plot

# RUNS FOR PROTECTED PUs: Poisson distribution?
shapiro.test(summary_df$num_pu) # normal :O
#hist(summary_df$num_pu)

# create GLZs then !
protected_PUs_model <- glm(num_pu ~ as.factor(target) + as.factor(scenario), 
                            family = poisson(link = "log"), data = summary_df)
summary(protected_PUs_model)
residual_deviance <- protected_PUs_model$deviance
null_deviance <- protected_PUs_model$null.deviance
pseudo_r2 <- 1 - (residual_deviance/null_deviance)
pseudo_r2

# plotting protected PUs
protected_PUs_plot <- ggplot(data = summary_df, aes(x = reorder(target, percent_area), group = scenario)) +
  geom_line(aes(color = scenario, y = num_pu), size = 0.5)+
  geom_point(aes(shape = scenario, color = scenario,y = num_pu), size = 3) +
  scale_color_brewer(palette = "Set2") +
  xlab("Target (%)") + 
  ylab("protected planning units") +
  theme(legend.position = "bottom") +
  theme_classic()
#protected_PUs_plot

# RUNS FOR NUMBER OF REPRESENTED FEATURES
#shapiro.test(summary_df$represented_features) # not normal
#hist(summary_df$represented_features)

# create GLZs then !
#represented_features_model <- glm(represented_features ~ as.factor(target) + as.factor(scenario), 
#                            family = poisson(link = "log"), data = summary_df)
# not significant model
#summary(represented_features_model)
#residual_deviance <- represented_features_model$deviance
#null_deviance <- represented_features_model$null.deviance
#pseudo_r2 <- 1 - (residual_deviance/null_deviance)
#pseudo_r2

# plotting protected PUs
#represented_features_plot <- ggplot(data = summary_df, aes(x = reorder(target, percent_area), group = scenario)) +
#  geom_line(aes(color = scenario, y = represented_features), size = 0.5)+
#  geom_point(aes(shape = scenario, color = scenario,y = represented_features), size = 3) +
#  scale_color_brewer(palette = "Set2") +
#  xlab("Target (%)") + 
#  ylab("adequately represented features") +
#  theme(legend.position = "bottom") +
#  theme_classic()
#represented_features_plot

smart_plans <- (cost_plot | percentarea_plot) +
  plot_annotation(tag_levels = 'i', title = 'Results of climate-smart and -uninformed runs across targets') +
  plot_layout(guides = 'collect')
#smart_plans
#ggsave('pdfs/12_Stat/summary_IUCN_climate-smart+climate-uninformed.pdf', width = 21, height = 15)
#################################################
## Runs no-regret ##
#################################################
summary_df <- summary_noprovIUCN_noregret

# RUNS FOR COST
# no replications, so just plotting !
# plotting cost
cost_plot <- ggplot(data = summary_df, aes(x = reorder(target, total_cost), group = scenario)) +
  geom_line(aes(color = scenario, y = log10(total_cost)), size = 0.5)+
  geom_point(aes(shape = scenario, color = scenario,y = log10(total_cost)), size = 3) +
  scale_color_brewer(palette = "Dark2") +
  xlab("Target (%)") + 
  ylab("log10(cost)") +
  theme(legend.position = "bottom") +
  theme_classic()
#cost_plot

# RUNS FOR % area protected
# plotting percent area
percentarea_plot <- ggplot(data = summary_df, aes(x = reorder(target, percent_area), group = scenario)) +
  geom_line(aes(color = scenario, y = percent_area), size = 0.5)+
  geom_point(aes(shape = scenario, color = scenario,y = percent_area), size = 3) +
  scale_color_brewer(palette = "Dark2") +
  xlab("Target (%)") + 
  ylab("log10(percent protected area)") +
  theme(legend.position = "bottom") +
  theme_classic()
#percentarea_plot

# RUNS FOR PROTECTED PUs
# plotting protected PUs
protected_PUs_plot <- ggplot(data = summary_df, aes(x = reorder(target, percent_area), group = scenario)) +
  geom_line(aes(color = scenario, y = num_pu), size = 0.5)+
  geom_point(aes(shape = scenario, color = scenario,y = num_pu), size = 3) +
  scale_color_brewer(palette = "Dark2") +
  xlab("Target (%)") + 
  ylab("protected planning units") +
  theme(legend.position = "bottom") +
  theme_classic()
#protected_PUs_plot

# RUNS FOR ADEQUATELY REPRESENTED FEATURES
represented_features_plot <- ggplot(data = summary_df, aes(x = reorder(target, percent_area), group = scenario)) +
  geom_line(aes(color = scenario, y = represented_features), size = 0.5)+
  geom_point(aes(shape = scenario, color = scenario,y = represented_features), size = 3) +
  scale_color_brewer(palette = "Dark2") +
  xlab("Target (%)") + 
  ylab("adequately represented features") +
  theme(legend.position = "bottom") +
  theme_classic()
#represented_features_plot

noregret_plans <- (cost_plot | percentarea_plot) +
  plot_annotation(tag_levels = 'i', title = 'Results of no-regret plans across targets') +
  plot_layout(guides = 'collect')
#noregret_plans
#ggsave('pdfs/12_Stat/summary_IUCN_noregret.pdf', width = 21, height = 15)
