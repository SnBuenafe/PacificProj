# RUN 1: make a 0 cost layer run
library(tidyverse)
library(sf)
library(prioritizr)
library(gurobi)

rob_pacific <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" # Best to define these first so you don't make mistakes below

test <- cbind(c('180','0'))

CnR <- proj4::project(test, proj = rob_pacific)

# inputs
velocity_ssp126 <- readRDS('outputs/07_Climate/Velocity/velocitySSP126.rds')
feature1 <- readRDS('outputs/04_IUCN/04d_fFeaturesInt/bycatch_features.rds')
feature1 <- st_intersection(feature1, velocity_ssp126) %>% 
  filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) %>% 
  as_tibble() %>% 
  filter(feature_names %in% c('Chelonia_mydas_IUCN')) %>%  #, )) %>% 'Dermochelys_coriacea_IUCN'
  rename(new_features = feature_names) %>% 
#  filter(value <= quantile(velocity_ssp126$value, 0.25)) %>% 
  dplyr::select(-geometry)

planning_region <- readRDS('outputs/01_StudyArea/01a_StudyArea/PacificABNJGrid_05deg.rds')
coordinates <- sf::st_point_on_surface(planning_region) %>% 
  st_coordinates() %>% 
  as_tibble()
planning_region$x <- coordinates$X
planning_region$y <- coordinates$Y

planning_region %<>%
  mutate(cellsID = 1:nrow(planning_region), 
         area_km2 = as.numeric(st_area(planning_region)/1e+06)) %>% 
  mutate(cost = case_when((y > CnR[1,1]) ~ 100,
                          (y < CnR[1,1]) ~ 0)
         ) %>% 
  as_tibble()

# rotating the df
species <- feature1 %>% 
  dplyr::select(c(new_features, cellsID)) %>% 
  mutate(Presence = 1) %>% # if the feature has the particular cellsID, it is present
  pivot_wider(names_from = new_features, values_from = Presence) %>% 
  replace(is.na(.), 0)

features_list = unique(feature1$new_features)

x <- feature1 %>%
  dplyr::select(-new_features) %>% 
  distinct(cellsID, .keep_all = TRUE) %>% 
  left_join(species, by = "cellsID") %>% 
  right_join(planning_region, by = "cellsID") %>% 
  st_as_sf(sf_column_name = "geometry")

p1 <- prioritizr::problem(x, features_list, 'cost') %>% 
  add_min_set_objective() %>%
  add_relative_targets(0.5) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0.1, verbose = TRUE) # using Gurobi Solver

s1 <- solve(p1) %>% 
  st_as_sf(sf_column_name = "geometry") # Output changes from sf to df so we change it back
solution <- s1 %>% 
  mutate(solution_1 = as.logical(solution_1))

ggplot() + 
  geom_sf(data = solution, aes(fill = solution_1), color = "grey64", size = 0.02) +
  scale_fill_manual(name = "Solution",
                    values = pal_rich,
                    labels = solution) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  theme_bw()

############
library(stats)
library(Hmisc)
RCE_SSP126_temp <- RCE_SSP126 %>% 
  as_tibble() %>% 
  select(cellsID, value) %>% 
  rename(RCE_126 = value)
RCE_SSP126_temp

RCE_SSP245_temp <- RCE_SSP245 %>% 
  as_tibble() %>% 
  select(cellsID, value) %>% 
  rename(RCE_245 = value)
RCE_SSP245_temp

RCE_SSP585_temp <- RCE_SSP585 %>% 
  as_tibble() %>% 
  select(cellsID, value) %>% 
  rename(RCE_585 = value)
RCE_SSP585_temp

velo_SSP126_temp <- velo_SSP126 %>% 
  as_tibble() %>% 
  select(cellsID, value) %>% 
  rename(velo_126 = value)
velo_SSP126_temp

velo_SSP245_temp <- velo_SSP245 %>% 
  as_tibble() %>% 
  select(cellsID, value) %>% 
  rename(velo_245 = value)
velo_SSP245_temp

velo_SSP585_temp <- velo_SSP585 %>% 
  as_tibble() %>% 
  select(cellsID, value) %>% 
  rename(velo_585 = value)
velo_SSP585_temp

df <- left_join(RCE_SSP126_temp, RCE_SSP245_temp, by = 'cellsID') %>% 
  left_join(RCE_SSP585_temp) %>% 
  left_join(velo_SSP126_temp) %>% 
  left_join(velo_SSP245_temp) %>% 
  left_join(velo_SSP585_temp)
df[,2:7]

# subsample for shapiro test
sub_RCE126 <- sample(df$RCE_126, size = 5000, replace = FALSE)
shapiro.test(sub_RCE126)

sub_RCE245 <- sample(df$RCE_245, size = 5000, replace = FALSE)
shapiro.test(sub_RCE245)

sub_RCE585 <- sample(df$RCE_585, size = 5000, replace = FALSE)
shapiro.test(sub_RCE585)

sub_velo126 <- sample(df$velo_126, size = 5000, replace = FALSE)
shapiro.test(sub_velo126)

sub_velo245 <- sample(df$velo_245, size = 5000, replace = FALSE)
shapiro.test(sub_velo245)

sub_velo585 <- sample(df$velo_585, size = 5000, replace = FALSE)
shapiro.test(sub_velo585)
# all are not normal ! therefore use spearman

cor_df <- cor(df[,2:7], method = 'spearman')
cor_df

rcorr_df <- rcorr(as.matrix(df[,2:7]), type = 'pearson')
rcorr_df
