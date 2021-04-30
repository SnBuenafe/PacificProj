# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.

# This function creates no-regret closures, requiring the ff. inputs:
# 1. inpdir: where the prioritizr_run results are found.
# 2. outdir: where to save the results of this function.
# 3. target: e.g. Target100

# Function is found in 11a_NoRegretFxn.R.

source("scripts/11a_NoRegretFxn.R")

# Plotting generalities
library(RColorBrewer)
library(patchwork)
pal_rich <- c("FALSE" = NA, "TRUE" = "sienna3")
solution <- c("FALSE", "TRUE")

########################################
## Runs using IUCN as bycatch + Plots ##
########################################

###############################
## Target 100% (0 - 1) ##
###############################
NOREGRET_IUCN_target100_run01 <- create_noregret(inpdir = "outputs/prioritizr_run/IUCN/01_Target100/",
                                                outdir = "outputs/noregret_closures/IUCN/",
                                                target = "Target100")
# To determine total cost
NOREGRET_IUCN_target100_run01 %>% 
  filter(!is.na(cost)) %>% 
  summarize(total_cost = sum(cost))

# Plotting
noregret_IUCNtarget100 <- ggplot() + 
  geom_sf(data = NOREGRET_IUCN_target100_run01, aes(fill = solution), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Target 100% (0 - 1)") +
  theme_bw()
noregret_IUCNtarget100
###############################
## Target 90% (0.1 - 0.9) ##
###############################
NOREGRET_IUCN_target90_run01 <- create_noregret(inpdir = "outputs/prioritizr_run/IUCN/02_Target90/",
                                                 outdir = "outputs/noregret_closures/IUCN/",
                                                 target = "Target90")
# To determine total cost
NOREGRET_IUCN_target90_run01 %>% 
  filter(!is.na(cost)) %>% 
  summarize(total_cost = sum(cost))

# Plotting
noregret_IUCNtarget90 <- ggplot() + 
  geom_sf(data = NOREGRET_IUCN_target90_run01, aes(fill = solution), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Target 90% (0.1 - 0.9)") +
  theme_bw()
noregret_IUCNtarget90
###############################
## Target 80% (0.1 - 0.8) ##
###############################
NOREGRET_IUCN_target80_run01 <- create_noregret(inpdir = "outputs/prioritizr_run/IUCN/03_Target80/",
                                                outdir = "outputs/noregret_closures/IUCN/",
                                                target = "Target80")
# To determine total cost
NOREGRET_IUCN_target80_run01 %>% 
  filter(!is.na(cost)) %>% 
  summarize(total_cost = sum(cost))

# Plotting
noregret_IUCNtarget80 <- ggplot() + 
  geom_sf(data = NOREGRET_IUCN_target80_run01, aes(fill = solution), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Target 80% (0.1 - 0.8)") +
  theme_bw()
noregret_IUCNtarget80
###############################
## Target 70% (0.1 - 0.7) ##
###############################
NOREGRET_IUCN_target70_run01 <- create_noregret(inpdir = "outputs/prioritizr_run/IUCN/04_Target70/",
                                                outdir = "outputs/noregret_closures/IUCN/",
                                                target = "Target70")
# To determine total cost
NOREGRET_IUCN_target70_run01 %>% 
  filter(!is.na(cost)) %>% 
  summarize(total_cost = sum(cost))

# Plotting
noregret_IUCNtarget70 <- ggplot() + 
  geom_sf(data = NOREGRET_IUCN_target70_run01, aes(fill = solution), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Target 70% (0.1 - 0.7)") +
  theme_bw()
noregret_IUCNtarget70
###############################
## Target 60% (0.1 - 0.6) ##
###############################
NOREGRET_IUCN_target60_run01 <- create_noregret(inpdir = "outputs/prioritizr_run/IUCN/05_Target60/",
                                                outdir = "outputs/noregret_closures/IUCN/",
                                                target = "Target60")
# To determine total cost
NOREGRET_IUCN_target60_run01 %>% 
  filter(!is.na(cost)) %>% 
  summarize(total_cost = sum(cost))

# Plotting
noregret_IUCNtarget60 <- ggplot() + 
  geom_sf(data = NOREGRET_IUCN_target60_run01, aes(fill = solution), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Target 60% (0.1 - 0.6)") +
  theme_bw()
noregret_IUCNtarget60
###############################
## Target 50% (0.1 - 0.5) ##
###############################
NOREGRET_IUCN_target50_run01 <- create_noregret(inpdir = "outputs/prioritizr_run/IUCN/06_Target50/",
                                                outdir = "outputs/noregret_closures/IUCN/",
                                                target = "Target50")
# To determine total cost
NOREGRET_IUCN_target50_run01 %>% 
  filter(!is.na(cost)) %>% 
  summarize(total_cost = sum(cost))

# Plotting
noregret_IUCNtarget50 <- ggplot() + 
  geom_sf(data = NOREGRET_IUCN_target50_run01, aes(fill = solution), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Target 50% (0.1 - 0.5)") +
  theme_bw()
noregret_IUCNtarget50
###############################
## Target 40% (0.1 - 0.4) ##
###############################
NOREGRET_IUCN_target40_run01 <- create_noregret(inpdir = "outputs/prioritizr_run/IUCN/07_Target40/",
                                                outdir = "outputs/noregret_closures/IUCN/",
                                                target = "Target40")
# To determine total cost
NOREGRET_IUCN_target40_run01 %>% 
  filter(!is.na(cost)) %>% 
  summarize(total_cost = sum(cost))

# Plotting
noregret_IUCNtarget40 <- ggplot() + 
  geom_sf(data = NOREGRET_IUCN_target40_run01, aes(fill = solution), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Target 40% (0.1 - 0.4)") +
  theme_bw()
noregret_IUCNtarget40
###############################
## Target 30% (0.1 - 0.3) ##
###############################
NOREGRET_IUCN_target30_run01 <- create_noregret(inpdir = "outputs/prioritizr_run/IUCN/08_Target30/",
                                                outdir = "outputs/noregret_closures/IUCN/",
                                                target = "Target30")
# To determine total cost
NOREGRET_IUCN_target30_run01 %>% 
  filter(!is.na(cost)) %>% 
  summarize(total_cost = sum(cost))

# Plotting
noregret_IUCNtarget30 <- ggplot() + 
  geom_sf(data = NOREGRET_IUCN_target30_run01, aes(fill = solution), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Target 30% (0.1 - 0.3)") +
  theme_bw()
noregret_IUCNtarget30
###############################
## Target 20% (0.1 - 0.2) ##
###############################
NOREGRET_IUCN_target20_run01 <- create_noregret(inpdir = "outputs/prioritizr_run/IUCN/09_Target20/",
                                                outdir = "outputs/noregret_closures/IUCN/",
                                                target = "Target20")
# To determine total cost
NOREGRET_IUCN_target20_run01 %>% 
  filter(!is.na(cost)) %>% 
  summarize(total_cost = sum(cost))

# Plotting
noregret_IUCNtarget20 <- ggplot() + 
  geom_sf(data = NOREGRET_IUCN_target20_run01, aes(fill = solution), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Target 20% (0.1 - 0.2)") +
  theme_bw()
noregret_IUCNtarget20
###############################
## Target 10% (0 - 0.1) ##
###############################
NOREGRET_IUCN_target10_run01 <- create_noregret(inpdir = "outputs/prioritizr_run/IUCN/10_Target10/",
                                                outdir = "outputs/noregret_closures/IUCN/",
                                                target = "Target10")
# To determine total cost
NOREGRET_IUCN_target10_run01 %>% 
  filter(!is.na(cost)) %>% 
  summarize(total_cost = sum(cost))

# Plotting
noregret_IUCNtarget10 <- ggplot() + 
  geom_sf(data = NOREGRET_IUCN_target10_run01, aes(fill = solution), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Target 10% (0 - 0.1)") +
  theme_bw()
noregret_IUCNtarget10
###############################
## Plot Everything ##
###############################
IUCN_noregret <- (noregret_IUCNtarget100 + noregret_IUCNtarget90 + noregret_IUCNtarget80 + noregret_IUCNtarget70 + noregret_IUCNtarget60 + noregret_IUCNtarget50 + 
                   noregret_IUCNtarget40 + noregret_IUCNtarget30 + noregret_IUCNtarget20 + plot_spacer() + noregret_IUCNtarget10 + plot_spacer()) +
  plot_layout(ncol = 3, nrow = 4, guides = "collect") +
  plot_annotation(tag_levels = 'i',
                  title = 'No-Regret Closures',
                  subtitle = 'Using IUCN Bycatch & Global-fitted Commercial Data')
IUCN_noregret
ggsave("pdfs/solutions/IUCN/NoRegret_IUCN.pdf")
