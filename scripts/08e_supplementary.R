library(tidyverse)
library(sf)

# defining plot generalities
library(RColorBrewer)
library(patchwork)
pal_rich <- c("FALSE" = "lightsteelblue2", "TRUE" = "steelblue4")
solution <- c("Not selected PUs", "Selected PUs")
world_sf <- readRDS("outputs/01_StudyArea/01a_StudyArea/PacificCenterLand.rds")

source("scripts/study_area/fCreateRobinsonBoundary.R")
Bndry <- fCreateRobinsonBoundary(west = 78, east = 140, north = 51, south = 60)

# Creating spatial plans for supplementary

# creating ggplots for each climate-smart method and each target
target_list <- seq(20, 100, 10) # as k
method_list <- c('a_25perc','b_asfeat','c_penalty') # as j
scenario_list <- c('SSP126','SSP245','SSP585','uninformed') # as i

for(j in 1:length(method_list)) {
  for(k in 1:length(target_list)){
    for(i in 1:length(scenario_list)){
      s <- readRDS(paste0('outputs/08_Prioritizr/08',method_list[j],'/spatial_plans/solve_',scenario_list[i],'_target',target_list[k],'.rds')) %>% 
        mutate(solution_1 = as.logical(solution_1))
      
      gg <- ggplot() + 
        geom_sf(data = s, aes(fill = solution_1), color = "grey64", size = 0.02) +
        scale_fill_manual(name = "Solution",
                          values = pal_rich,
                          labels = solution) +
        geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
        coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
                 ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
                 expand = TRUE) +
        ggtitle(scenario_list[i]) +
        theme_bw() + 
        theme(legend.position = 'none', 
              title = element_blank(),
              axis.text.x = element_text(size = 25),
              axis.text.y = element_text(size = 25))
      assign(paste0('gg_',method_list[j],'_',scenario_list[i],'_target_',target_list[k]), gg)
      ggsave(paste0('outputs/08_Prioritizr/08e_supplementary/spatial_plans/',method_list[j],'/gg_',scenario_list[i],'_target_',target_list[k],'.png'),
             gg,
             width = 10, height = 10, dpi = 600)
      
      rm(s, gg)
    }
  }
}

# creating summary of cost
scenario.legend_color <- c('SSP126' = 'yellow3', 'SSP245' = 'orange2', 
                           'SSP585' = 'salmon4', 'uninformed' = 'lightslategray', 'NA' = NA)
temp_summary_25perc <- read.csv('outputs/08_Prioritizr/08a_25perc/summary_stat.csv')
temp_summary_asfeat <- read.csv('outputs/08_Prioritizr/08b_asfeat/summary_stat.csv')
temp_summary_penalty <- read.csv('outputs/08_Prioritizr/08c_penalty/summary_stat.csv')
# cost
ggplot(data = temp_summary_25perc, aes(x = target*0.25, color = scenario)) +
  scale_color_manual(name = 'Scenario',
                     values = scenario.legend_color) +
  geom_point(aes(y = log10(total_cost), shape = scenario), size = 3) +
  geom_line(aes(y = log10(total_cost)), size = 0.5) +
  theme_bw() +
  theme(legend.position = 'none',
        title = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25)) +
  ylim(c(4,6))
ggsave('outputs/08_Prioritizr/08e_supplementary/cost_25perc.png', width = 7, height = 5, dpi = 600)

ggplot(data = temp_summary_asfeat, aes(x = target*0.25, color = scenario)) +
  scale_color_manual(name = 'Scenario',
                     values = scenario.legend_color) +
  geom_point(aes(y = log10(total_cost), shape = scenario), size = 3) +
  geom_line(aes(y = log10(total_cost)), size = 0.5) +
  theme_bw() +
  theme(legend.position = 'none',
        title = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25)) +
  ylim(c(4,6))
ggsave('outputs/08_Prioritizr/08e_supplementary/cost_asfeat.png', width = 7, height = 5, dpi = 600)

ggplot(data = temp_summary_penalty, aes(x = target*0.25, color = scenario)) +
  scale_color_manual(name = 'Scenario',
                     values = scenario.legend_color) +
  geom_point(aes(y = log10(total_cost), shape = scenario), size = 3) +
  geom_line(aes(y = log10(total_cost)), size = 0.5) +
  theme_bw() +
  theme(legend.position = 'none',
        title = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25)) +
  ylim(c(4,6))
ggsave('outputs/08_Prioritizr/08e_supplementary/cost_penalty.png', width = 7, height = 5, dpi = 600)

# % area selected
ggplot(data = temp_summary_25perc, aes(x = target*0.25, color = scenario)) +
  scale_color_manual(name = 'Scenario',
                     values = scenario.legend_color) +
  geom_point(aes(y = percent_area, shape = scenario), size = 3) +
  geom_line(aes(y = percent_area), size = 0.5) +
  theme_bw() +
  theme(legend.position = 'none',
        title = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25)) +
  ylim(c(0,50))
ggsave('outputs/08_Prioritizr/08e_supplementary/area_25perc.png', width = 7, height = 5, dpi = 600)

ggplot(data = temp_summary_asfeat, aes(x = target*0.25, color = scenario)) +
  scale_color_manual(name = 'Scenario',
                     values = scenario.legend_color) +
  geom_point(aes(y = percent_area, shape = scenario), size = 3) +
  geom_line(aes(y = percent_area), size = 0.5) +
  theme_bw() +
  theme(legend.position = 'none',
        title = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25)) +
  ylim(c(0,50))
ggsave('outputs/08_Prioritizr/08e_supplementary/area_asfeat.png', width = 7, height = 5, dpi = 600)

ggplot(data = temp_summary_penalty, aes(x = target*0.25, color = scenario)) +
  scale_color_manual(name = 'Scenario',
                     values = scenario.legend_color) +
  geom_point(aes(y = percent_area, shape = scenario), size = 3) +
  geom_line(aes(y = percent_area), size = 0.5) +
  theme_bw() +
  theme(legend.position = 'none',
        title = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25)) +
  ylim(c(0,50))
ggsave('outputs/08_Prioritizr/08e_supplementary/area_penalty.png', width = 7, height = 5, dpi = 600)

# median RCE without uninformed
ggplot(data = temp_summary_25perc %>% filter(scenario != 'uninformed'), aes(x = target*0.25, color = scenario)) +
  scale_color_manual(name = 'Scenario',
                     values = scenario.legend_color) +
  geom_point(aes(y = median_RCE, shape = scenario), size = 3) +
  geom_line(aes(y = median_RCE), size = 0.5) +
  theme_bw() +
  theme(legend.position = 'none',
        title = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25)) +
  ylim(c(0,100))
ggsave('outputs/08_Prioritizr/08e_supplementary/RCE_25perc.png', width = 7, height = 5, dpi = 600)


ggplot(data = temp_summary_asfeat %>% filter(scenario != 'uninformed'), aes(x = target*0.25, color = scenario)) +
  scale_color_manual(name = 'Scenario',
                     values = scenario.legend_color) +
  geom_point(aes(y = median_RCE, shape = scenario), size = 3) +
  geom_line(aes(y = median_RCE), size = 0.5) +
  theme_bw() +
  theme(legend.position = 'none',
        title = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25)) +
  ylim(c(0,100))
ggsave('outputs/08_Prioritizr/08e_supplementary/RCE_asfeat.png', width = 7, height = 5, dpi = 600)


ggplot(data = temp_summary_penalty %>% filter(scenario != 'uninformed'), aes(x = target*0.25, color = scenario)) +
  scale_color_manual(name = 'Scenario',
                     values = scenario.legend_color) +
  geom_point(aes(y = median_RCE, shape = scenario), size = 3) +
  geom_line(aes(y = median_RCE), size = 0.5) +
  theme_bw() +
  theme(legend.position = 'none',
        title = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25)) +
  ylim(c(0,100))
ggsave('outputs/08_Prioritizr/08e_supplementary/RCE_penalty.png', width = 7, height = 5, dpi = 600)

# median velocity without uninformed
ggplot(data = temp_summary_25perc %>% filter(scenario != 'uninformed'), aes(x = target*0.25, color = scenario)) +
  scale_color_manual(name = 'Scenario',
                     values = scenario.legend_color) +
  geom_point(aes(y = median_velocity, shape = scenario), size = 3) +
  geom_line(aes(y = median_velocity), size = 0.5) +
  theme_bw() +
  theme(legend.position = 'none',
        title = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25))+
  ylim(c(0, 15))
ggsave('outputs/08_Prioritizr/08e_supplementary/velocity_25perc.png', width = 7, height = 5, dpi = 600)

ggplot(data = temp_summary_asfeat %>% filter(scenario != 'uninformed'), aes(x = target*0.25, color = scenario)) +
  scale_color_manual(name = 'Scenario',
                     values = scenario.legend_color) +
  geom_point(aes(y = median_velocity, shape = scenario), size = 3) +
  geom_line(aes(y = median_velocity), size = 0.5) +
  theme_bw() +
  theme(legend.position = 'none',
        title = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25))+
  ylim(c(-0.1, 15))
ggsave('outputs/08_Prioritizr/08e_supplementary/velocity_asfeat.png', width = 7, height = 5, dpi = 600)

ggplot(data = temp_summary_penalty %>% filter(scenario != 'uninformed'), aes(x = target*0.25, color = scenario)) +
  scale_color_manual(name = 'Scenario',
                     values = scenario.legend_color) +
  geom_point(aes(y = median_velocity, shape = scenario), size = 3) +
  geom_line(aes(y = median_velocity), size = 0.5) +
  theme_bw() +
  theme(legend.position = 'none',
        title = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25))+
  ylim(c(-0.1, 15))
ggsave('outputs/08_Prioritizr/08e_supplementary/velocity_penalty.png', width = 7, height = 5, dpi = 600)
