library(tidyverse)
library(stats)
library(grid)
library(patchwork)


stats <- read_csv("statistics/stats.csv")
stats

# cost is continuous
# perc_area_protected is constrained from 0 to 100 but considered continuous?
# protected PUs has a poisson distribution

# RUNS FOR COST
shapiro.test(log10(stats$cost))
#hist(log10(stats$cost))
cost_model1 <- aov(log10(cost) ~ as.factor(target) + as.factor(scenario), data = stats)
cost_model1
summary(cost_model1)
TukeyHSD(cost_model1)

# RUNS FOR PERC_AREA_PROTECTED: binomially distributed
shapiro.test(log10(stats$perc_area_protected))
#hist(log10(stats$perc_area_protected))

area_protected_model1 <- aov(log10(perc_area_protected) ~ as.factor(target) + as.factor(scenario),
                             data = stats)
area_protected_model1
summary(area_protected_model1)
TukeyHSD(area_protected_model1)

# RUNS FOR PROTECTED PUs
shapiro.test(log10(stats$protected_PU))
#hist(stats$protected_PU)

protected_PU_model1 <- glm(protected_PU ~ as.factor(target) + as.factor(scenario), 
                           family = poisson(link = "log"), data = stats)
summary(protected_PU_model1)

### PLOTTING
stats_filtered <- stats %>% 
  filter(scenario != "uninformed")

ggplot(data = stats_filtered, aes(x = as.factor(target), group = scenario)) +
  geom_line(aes(color = scenario, y = log10(cost)), size = 0.5)+
  geom_point(aes(shape = scenario, color = scenario,y = log10(cost)), size = 3) +
  scale_color_brewer(palette = "Reds") +
  xlab("Target (%)") + 
  ylab("log10(cost)") +
  theme(legend.position = "bottom") +
  theme_classic()

log_plot <- ggplot(data = stats_filtered, aes(x = as.factor(target), group = scenario)) +
  geom_line(aes(color = scenario, y = log10(cost)), size = 0.5)+
  geom_point(aes(shape = scenario, color = scenario,y = log10(cost)), size = 3) +
  scale_color_brewer(palette = "Reds") +
  xlab("Target (%)") + 
  ylab("log10(cost)") +
  theme(legend.position = "bottom") +
  theme_classic()
log_plot

area_plot <- ggplot(data = stats_filtered, aes(x = as.factor(target), group = scenario)) +
  geom_line(aes(color = scenario, y = perc_area_protected), size = 0.5) +
  geom_point(aes(shape = scenario, color = scenario,y = perc_area_protected), size = 3) +
  scale_color_brewer(palette = "Blues") +
  xlab("Target (%)") + 
  ylab("Protected Area (%)") +
  theme(legend.position = "bottom") +
  theme_classic()
area_plot

protected_features <- ggplot(data = stats_filtered, aes(x = as.factor(target), group = scenario)) +
  geom_line(aes(color = scenario, y = protected_PU), size = 0.5) +
  geom_point(aes(shape = scenario, color = scenario, y = protected_PU), size = 3) +
  scale_color_brewer(palette = "Purples") +
  xlab("Target (%)") + 
  ylab("Number of Features") +
  theme(legend.position = "bottom") +
  theme_classic()
protected_features

log_plot1 <- log_plot + theme(axis.text.x = element_blank(),
                  axis.title.x = element_blank() )

area_plot1 <- area_plot + theme(axis.text.x = element_blank(),
                           axis.title.x = element_blank() )

#log_plot1 / area_plot1 / protected_features +
#  plot_annotation(tag_levels = 'i')
#ggsave("pdfs/12_stat/stats.pdf", width = 21, height = 29.7, units = "cm")