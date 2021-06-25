# A: 25 percentile
# B: penalty
# C: as feature

# run 08a - 08c first before running these

#######################
#### Spatial plans ####
#######################

# spatial plans for 25 percentile

gg_25perc_SSP126 <- gg_a_SSP126_90 + labs(y = '25 percentile', title = 'SSP 1-2.6')
gg_25perc_SSP245 <- gg_a_SSP245_90 + labs(title = 'SSP 2-4.5')
gg_25perc_SSP585 <- gg_a_SSP585_90 + labs(title = 'SSP 5-8.5')
gg_25perc_uninformed <- gg_a_uninformed_90 + labs(title = 'uninformed')

# spatial plans for penalty

gg_penalty_SSP126 <- gg_b_SSP126_90 + labs(y = 'penalty', title = element_blank())
gg_penalty_SSP245 <- gg_b_SSP245_90 + labs(title = element_blank())
gg_penalty_SSP585 <- gg_b_SSP585_90 + labs(title = element_blank())
gg_penalty_uninformed <- gg_b_uninformed_90 + labs(title = element_blank())

# spatial plans for features

gg_feat_SSP126 <- gg_c_SSP126_90 + labs(y = 'as feature', title = element_blank())
gg_feat_SSP245 <- gg_c_SSP245_90 + labs(title = element_blank())
gg_feat_SSP585 <- gg_c_SSP585_90 + labs(title = element_blank())
gg_feat_uninformed <- gg_c_uninformed_90 + labs(title = element_blank())

# plotting all the spatial plans (columns: climate scenarios, rows: methods)
gg_plots <- gg_25perc_SSP126 + gg_25perc_SSP245 + gg_25perc_SSP585 + gg_25perc_uninformed +
  gg_penalty_SSP126 + gg_penalty_SSP245 + gg_penalty_SSP585 + gg_penalty_uninformed +
  gg_feat_SSP126 + gg_feat_SSP245 + gg_feat_SSP585 + gg_feat_uninformed +
  plot_layout(nrow = 3, ncol = 4, guides = 'collect', widths = c(1, 1, 1, 1))
#ggsave('outputs/08_Prioritizr/08d_summary/gg_plots.png', gg_plots, width = 29.7, height = 21, dpi = 300)
#ggsave('outputs/08_Prioritizr/08d_summary/gg_plots.pdf', gg_plots, width = 29.7, height = 21, dpi = 300)

#######################
#### Summaries ####
#######################
# representation of features
circbplot_25perc <- circbplot_a_target_90 + labs(title = '25 percentile')
circbplot_penalty <- circbplot_b_target_90 + labs(title = 'penalty')
circbplot_feat <- circbplot_c_target_90 + labs (title = 'as feature')

circbplots <- circbplot_25perc + circbplot_penalty + circbplot_feat +
  plot_layout(nrow = 1, ncol = 3, guides = 'collect') +
  plot_annotation(tag_levels = 'A', tag_suffix = ')') & theme(legend.position = 'bottom')
ggsave('outputs/08_Prioritizr/08d_summary/circbplots.png', circbplots, width = 18, height = 15, dpi = 300)
ggsave('outputs/08_Prioritizr/08d_summary/circbplots.pdf', circbplots, width = 18, height = 15, dpi = 300)

# kappa (get solve stuff from outputs)
sol_a_SSP126 <- readRDS('outputs/08_Prioritizr/08a_25perc/spatial_plans/solve_SSP126_target90.rds')
sol_a_SSP245 <- readRDS('outputs/08_Prioritizr/08a_25perc/spatial_plans/solve_SSP245_target90.rds')
sol_a_SSP585 <- readRDS('outputs/08_Prioritizr/08a_25perc/spatial_plans/solve_SSP585_target90.rds')
sol_a_uninformed <- readRDS('outputs/08_Prioritizr/08a_25perc/spatial_plans/solve_uninformed_target90.rds')
dir <- 'outputs/08_Prioritizr/08d_summary/'

pdf('outputs/08_Prioritizr/08d_summary/kappa_25perc_target90.pdf', width = 8, height = 8)
kappa_90 <- create_kappacorrplot(sol_a_SSP126, sol_a_SSP245, sol_a_SSP585, sol_a_uninformed, dir)
dev.off()

sol_b_SSP126 <- readRDS('outputs/08_Prioritizr/08c_penalty/spatial_plans/solve_SSP126_target90.rds')
sol_b_SSP245 <- readRDS('outputs/08_Prioritizr/08c_penalty/spatial_plans/solve_SSP245_target90.rds')
sol_b_SSP585 <- readRDS('outputs/08_Prioritizr/08c_penalty/spatial_plans/solve_SSP585_target90.rds')
sol_b_uninformed <- readRDS('outputs/08_Prioritizr/08c_penalty/spatial_plans/solve_uninformed_target90.rds')

pdf('outputs/08_Prioritizr/08d_summary/kappa_penalty_target90.pdf', width = 8, height = 8)
kappa_90 <- create_kappacorrplot(sol_b_SSP126, sol_b_SSP245, sol_b_SSP585, sol_b_uninformed, dir)
dev.off()

sol_c_SSP126 <- readRDS('outputs/08_Prioritizr/08b_asfeat/spatial_plans/solve_SSP126_target90.rds')
sol_c_SSP245 <- readRDS('outputs/08_Prioritizr/08b_asfeat/spatial_plans/solve_SSP245_target90.rds')
sol_c_SSP585 <- readRDS('outputs/08_Prioritizr/08b_asfeat/spatial_plans/solve_SSP585_target90.rds')
sol_c_uninformed <- readRDS('outputs/08_Prioritizr/08b_asfeat/spatial_plans/solve_uninformed_target90.rds')

pdf('outputs/08_Prioritizr/08d_summary/kappa_feat_target90.pdf', width = 8, height = 8)
kappa_90 <- create_kappacorrplot(sol_c_SSP126, sol_c_SSP245, sol_c_SSP585, sol_c_uninformed, dir)
dev.off()

# cost, protected area, velocity, RCE
# for cost, % protected area:
a <- subset_a %>% 
  filter(target == 90) %>% 
  dplyr::mutate(method = '25 percentile')
b <- subset_b %>% 
  filter(target == 90) %>% 
  dplyr::mutate(method = 'penalty')
c <- subset_c %>% 
  filter(target == 90) %>% 
  dplyr::mutate(method = 'as feature')

joined <- full_join(a, b) %>% 
  full_join(c)
rm(a, b, c)

cost <- ggplot(data = joined, aes(x = as.factor(method), group = scenario)) +
  geom_bar(aes(y = log10(total_cost), fill = scenario), stat = 'identity', position = position_dodge()) +
  scale_fill_manual(name = 'Climate scenario',
                    values = scenario.legend_color) +
  xlab("Target (%)") + 
  ylab("log10(cost)") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(linetype = 'dashed', color = 'black'))

area <- ggplot(data = joined, aes(x = as.factor(method), group = scenario)) +
  geom_bar(aes(y = percent_area, fill = scenario), stat = 'identity', position = position_dodge()) +
  scale_fill_manual(name = 'Climate scenario',
                    values = scenario.legend_color) +
  xlab("Target (%)") + 
  ylab("Protected Area (%)") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(linetype = 'dashed', color = 'black'))

# for velocity and RCE
a <- stack_df.summary_a %>% 
  filter(target == 22.5) %>% 
  dplyr::mutate(method = '25 percentile')
b <- stack_df.summary_b %>% 
  filter(target == 22.5) %>% 
  dplyr::mutate(method = 'penalty')
c <- stack_df.summary_c %>% 
  filter(target == 22.5) %>% 
  dplyr::mutate(method = 'as feature')

joined_climate <- full_join(a, b) %>% 
  full_join(c)

rm(a, b, c)

velocity <- ggplot(data = joined_climate, aes(x = as.factor(uninformed), group = scenario)) +
  geom_bar(aes(y = median_velocity, fill = scenario), stat = 'identity', position = 'stack') +
  scale_fill_manual(name = 'Climate scenario',
                    values = scenario.legend_color) +
  xlab("Climate Scenario") + 
  ylab(expression('Median climate velocity (km yr'^"-1"*')')) +
  facet_grid(.~method) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(linetype = 'dashed', color = 'black'),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

RCE <- ggplot(data = joined_climate, aes(x = as.factor(uninformed), group = scenario)) +
  geom_bar(aes(y = median_RCE, fill = scenario), stat = 'identity', position = 'stack') +
  scale_fill_manual(name = 'Climate scenario',
                    values = scenario.legend_color) +
  xlab("Climate Scenario") + 
  ylab('Median RCE') +
  facet_grid(.~method) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(linetype = 'dashed', color = 'black'),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

summary <- cost + area + velocity + RCE +
  plot_layout(nrow = 1, guides = 'collect')
#ggsave('outputs/08_Prioritizr/08d_summary/summary.png', summary, width = 20, height = 5, dpi = 300)
#ggsave('outputs/08_Prioritizr/08d_summary/summary.pdf', summary, width = 20, height = 5, dpi = 300)

# no-regret plans
gg_25perc_noregret <- gg_a_no.regret_target_90 + labs(title = '25 percentile')
gg_penalty_noregret <- gg_b_no.regret_target_90 + labs(title = 'penalty')
gg_feat_noregret <- gg_c_no.regret_target_90 + labs(title = 'as feature')
noregret <- gg_25perc_noregret + gg_penalty_noregret + gg_feat_noregret +
  plot_layout(nrow = 1, guides = 'collect')

# frequency of PU selection
gg_25perc_freq <- ggfreq_a_no.regret_target_90 + labs(title = element_blank())
gg_penalty_freq <- ggfreq_b_no.regret_target_90 + labs(title = element_blank())
gg_feat_freq <- ggfreq_c_no.regret_target_90 + labs(title = element_blank())
freq <- gg_25perc_freq + gg_penalty_freq + gg_feat_freq +
  plot_layout(nrow = 1, guides = 'collect')

# summary of no-regret
a <- no.regret_summary_a %>% 
  filter(target == 90) %>% 
  dplyr::mutate(method = '25 percentile')
b <- no.regret_summary_b %>% 
  filter(target == 90) %>% 
  dplyr::mutate(method = 'penalty')
c <- no.regret_summary_c %>% 
  filter(target == 90) %>% 
  dplyr::mutate(method = 'as feature')

joined_noregret <- full_join(a, b) %>% 
  full_join(c)

rm(a, b, c)

cost_noregret <- ggplot(data = joined_noregret, aes(x = as.factor(method))) +
  geom_bar(aes(y = log10(total_cost)), stat = 'identity', position = position_dodge(), fill = 'lightskyblue3') +
  scale_fill_manual(name = 'Climate scenario',
                    values = scenario.legend_color) +
  xlab("Target (%)") + 
  ylab("log10(cost)") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(linetype = 'dashed', color = 'black'))

area_noregret <- ggplot(data = joined_noregret, aes(x = as.factor(method))) +
  geom_bar(aes(y = percent_area), stat = 'identity', position = position_dodge(), fill = 'lightskyblue3') +
  scale_fill_manual(name = 'Climate scenario',
                    values = scenario.legend_color) +
  xlab("Target (%)") + 
  ylab("Protected Area (%)") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(linetype = 'dashed', color = 'black'))

summary_noregret <- cost_noregret + plot_spacer() + area_noregret + plot_layout(widths = c(1,1), heights = c(1,1))
# include these plots above


summary_noregret <- (noregret + plot_layout(ncol = 3)) / (freq + plot_layout(ncol = 3)) / (cost_noregret + area_noregret + plot_spacer()) + 
  plot_layout(height = c(1,1,0.5), widths = c(1,1,0.5))
ggsave('outputs/08_Prioritizr/08d_summary/summary_noregret.png', summary_noregret, width = 20, height = 20, dpi = 600)
ggsave('outputs/08_Prioritizr/08d_summary/summary_noregret.pdf', summary_noregret, width = 20, height = 20, dpi = 300)
