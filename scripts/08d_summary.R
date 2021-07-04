# A: 25 percentile
# B: penalty
# C: as feature

# run 08a - 08c first before running these

#######################
#### Spatial plans ####
#######################

# spatial plans for 25 percentile

gg_25perc_SSP126 <- gg_a_SSP126_90 + 
  theme(legend.position = 'none', 
        title = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25))
ggsave('outputs/08_Prioritizr/08d_summary/25perc_SSP126.png', gg_25perc_SSP126, width = 10, height = 10, dpi = 600)
gg_25perc_SSP245 <- gg_a_SSP245_90 + 
  theme(legend.position = 'none', 
        title = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25))
ggsave('outputs/08_Prioritizr/08d_summary/25perc_SSP245.png', gg_25perc_SSP245, width = 10, height = 10, dpi = 600)
gg_25perc_SSP585 <- gg_a_SSP585_90 +
  theme(legend.position = 'none', 
        title = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25))
ggsave('outputs/08_Prioritizr/08d_summary/25perc_SSP585.png', gg_25perc_SSP585, width = 10, height = 10, dpi = 600)

# spatial plans for penalty

gg_penalty_SSP126 <- gg_b_SSP126_90 + 
  theme(legend.position = 'none', 
        title = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25))
ggsave('outputs/08_Prioritizr/08d_summary/penalty_SSP126.png', gg_penalty_SSP126, width = 10, height = 10, dpi = 600)
gg_penalty_SSP245 <- gg_b_SSP245_90 + 
  theme(legend.position = 'none', 
        title = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25))
ggsave('outputs/08_Prioritizr/08d_summary/penalty_SSP245.png', gg_penalty_SSP245, width = 10, height = 10, dpi = 600)
gg_penalty_SSP585 <- gg_b_SSP585_90 + 
  theme(legend.position = 'none', 
        title = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25))
ggsave('outputs/08_Prioritizr/08d_summary/penalty_SSP585.png', gg_penalty_SSP585, width = 10, height = 10, dpi = 600)

# spatial plans for features

gg_feat_SSP126 <- gg_c_SSP126_90 + 
  theme(legend.position = 'none', 
        title = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25))
ggsave('outputs/08_Prioritizr/08d_summary/feat_SSP126.png', gg_feat_SSP126, width = 10, height = 10, dpi = 600)
gg_feat_SSP245 <- gg_c_SSP245_90 + labs(title = element_blank()) + 
  theme(legend.position = 'none', 
        title = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25))
ggsave('outputs/08_Prioritizr/08d_summary/feat_SSP245.png', gg_feat_SSP245, width = 10, height = 10, dpi = 600)
gg_feat_SSP585 <- gg_c_SSP585_90 + labs(title = element_blank()) + 
  theme(legend.position = 'none', 
        title = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25))
ggsave('outputs/08_Prioritizr/08d_summary/feat_SSP585.png', gg_feat_SSP585, width = 10, height = 10, dpi = 600)

# spatial plan for uninformed (same for all methods)
gg_feat_uninformed <- gg_c_uninformed_90 +
  theme(legend.position = 'none', 
        title = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25))
ggsave('outputs/08_Prioritizr/08d_summary/uninformed.png', gg_feat_uninformed, width = 10, height = 10, dpi = 600)

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
circbplot_25perc <- circbplot_a_target_90 + theme(legend.position = 'none')
ggsave('outputs/08_Prioritizr/08d_summary/circbplots_25perc.png', circbplot_25perc, width = 10, height = 10, dpi = 600)
circbplot_penalty <- circbplot_b_target_90 + theme(legend.position = 'none')
ggsave('outputs/08_Prioritizr/08d_summary/circbplots_penalty.png', circbplot_penalty, width = 10, height = 10, dpi = 600)
circbplot_feat <- circbplot_c_target_90
ggsave('outputs/08_Prioritizr/08d_summary/circbplots_feat.png', circbplot_feat, width = 10, height = 10, dpi = 600)


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
  dplyr::mutate(method = 'B')
b <- subset_b %>% 
  filter(target == 90) %>% 
  dplyr::mutate(method = 'C')
c <- subset_c %>% 
  filter(target == 90) %>% 
  dplyr::mutate(method = 'A')

joined <- full_join(a, b) %>% 
  full_join(c)
rm(a, b, c)

scenario.legend_color <- c('SSP126' = 'darkseagreen', 'SSP245' = 'tan3', 
                           'SSP585' = 'salmon4', 'uninformed' = 'grey30', 'NA' = NA)
cost <- ggplot(data = joined, aes(x = as.factor(method), group = scenario), show.legend = FALSE) +
  geom_bar(aes(y = log10(total_cost), fill = scenario), stat = 'identity', position = position_dodge(), show.legend = FALSE) +
  scale_fill_manual(name = 'Climate scenario',
                    values = scenario.legend_color) +
  xlab("Target (%)") + 
  ylab("log10(cost)") +
  scale_y_continuous(limits = c(0,6), expand = c(0,0)) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(linetype = 'dashed', color = 'black'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 15))
cost
ggsave('outputs/08_Prioritizr/08d_summary/cost.png', cost, width = 10, height = 5, dpi = 600)

area <- ggplot(data = joined, aes(x = as.factor(method), group = scenario), show.legend = FALSE) +
  geom_bar(aes(y = percent_area, fill = scenario), stat = 'identity', position = position_dodge(), show.legend = FALSE) +
  scale_fill_manual(name = 'Climate scenario',
                    values = scenario.legend_color) +
  xlab("Target (%)") + 
  ylab("Protected Area (%)") +
  scale_y_continuous(limits = c(0,45), expand = c(0, 0)) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(linetype = 'dashed', color = 'black'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 15))
area
ggsave('outputs/08_Prioritizr/08d_summary/area.png', area, width = 10, height = 5, dpi = 600)


# for velocity and RCE
a <- stack_df.summary_a %>% 
  filter(target == 22.5) %>% 
  dplyr::mutate(method = 'B') %>% 

b <- stack_df.summary_b %>% 
  filter(target == 22.5) %>% 
  dplyr::mutate(method = 'C')
c <- stack_df.summary_c %>% 
  filter(target == 22.5) %>% 
  dplyr::mutate(method = 'A')

joined_climate <- full_join(a, b) %>% 
  full_join(c)

rm(a, b, c)
joined_climate
velocity <- ggplot(data = joined_climate, aes(x = as.factor(uninformed), group = scenario)) +
  geom_bar(aes(y = median_velocity, fill = scenario), stat = 'identity', position = position_dodge(), show.legend = FALSE) +
  scale_fill_manual(name = 'Climate scenario',
                    values = scenario.legend_color) +
  xlab("Climate Scenario") + 
  ylab(expression('Median climate velocity (km yr'^"-1"*')')) +
  facet_grid(.~method) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 18)) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(linetype = 'dashed', color = 'black'),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 15),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
velocity
ggsave('outputs/08_Prioritizr/08d_summary/velocity.png', velocity, bg = 'transparent', width = 10, height = 5, dpi = 600)

RCE <- ggplot(data = joined_climate, aes(x = as.factor(uninformed), group = scenario)) +
  geom_bar(aes(y = median_RCE, fill = scenario), stat = 'identity', position = position_dodge(), show.legend = FALSE) +
  scale_fill_manual(name = 'Climate scenario',
                    values = scenario.legend_color) +
  xlab("Climate Scenario") + 
  ylab('Median RCE') +
  facet_grid(.~method) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 110)) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(linetype = 'dashed', color = 'black'),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 15),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
RCE
ggsave('outputs/08_Prioritizr/08d_summary/RCE.png', RCE, bg = 'transparent', width = 10, height = 5, dpi = 600)


summary <- cost + area + velocity + RCE +
  plot_layout(nrow = 1, guides = 'collect')
#ggsave('outputs/08_Prioritizr/08d_summary/summary.png', summary, width = 20, height = 5, dpi = 300)
#ggsave('outputs/08_Prioritizr/08d_summary/summary.pdf', summary, width = 20, height = 5, dpi = 300)

# no-regret plans
gg_25perc_noregret <- gg_a_no.regret_target_90 +
  theme(legend.position = 'none', 
        title = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25))
ggsave('outputs/08_Prioritizr/08d_summary/no-regret_25perc.png', gg_25perc_noregret, width = 10, height = 10, dpi = 600)
gg_penalty_noregret <- gg_b_no.regret_target_90 +
  theme(legend.position = 'none', 
        title = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25))
ggsave('outputs/08_Prioritizr/08d_summary/no-regret_penalty.png', gg_penalty_noregret, width = 10, height = 10, dpi = 600)
gg_feat_noregret <- gg_c_no.regret_target_90 +
  theme(legend.position = 'none', 
        title = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25))
ggsave('outputs/08_Prioritizr/08d_summary/no-regret_feat.png', gg_feat_noregret, width = 10, height = 10, dpi = 600)
noregret <- gg_25perc_noregret + gg_penalty_noregret + gg_feat_noregret +
  plot_layout(nrow = 1, guides = 'collect')

# frequency of PU selection
gg_25perc_freq <- ggfreq_a_no.regret_target_90 + labs(title = element_blank()) +
  theme(legend.position = 'none', 
        title = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25))
ggsave('outputs/08_Prioritizr/08d_summary/freq_25perc.png', gg_25perc_freq, width = 10, height = 10, dpi = 600)
gg_penalty_freq <- ggfreq_b_no.regret_target_90 + labs(title = element_blank()) +
  theme(legend.position = 'none', 
        title = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25))
ggsave('outputs/08_Prioritizr/08d_summary/freq_penalty.png', gg_penalty_freq, width = 10, height = 10, dpi = 600)
gg_feat_freq <- ggfreq_c_no.regret_target_90 + labs(title = element_blank())+ 
  theme(legend.position = 'none', 
        title = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25))
ggsave('outputs/08_Prioritizr/08d_summary/freq_feat.png', gg_feat_freq, width = 10, height = 10, dpi = 600)
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
