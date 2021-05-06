library(tidyverse)
library(stats)

stats <- read_csv("statistics/stats.csv")
stats

# cost is continuous
# perc_area_protected is constrained from 0 to 100 but considered continuous?
# protected PUs has a poisson distribution

# RUNS FOR COST
shapiro.test(log10(stats$cost))
hist(log10(stats$cost))
cost_model1 <- aov(log10(cost) ~ as.factor(target) + as.factor(scenario), data = stats)
cost_model1
summary(cost_model1)
TukeyHSD(cost_model1)

# RUNS FOR PERC_AREA_PROTECTED: binomially distributed
shapiro.test(log10(stats$perc_area_protected))
hist(log10(stats$perc_area_protected))

area_protected_model1 <- aov(log10(perc_area_protected) ~ as.factor(target) + as.factor(scenario),
                             data = stats)
area_protected_model1
summary(area_protected_model1)
TukeyHSD(area_protected_model1)

# RUNS FOR PROTECTED PUs
shapiro.test(log10(stats$protected_PU))
hist(stats$protected_PU)

protected_PU_model1 <- glm(protected_PU ~ as.factor(target) + as.factor(scenario), 
                           family = poisson(link = "log"), data = stats)
summary(protected_PU_model1)
