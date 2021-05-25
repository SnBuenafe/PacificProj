# RUN 1: make a 0 cost layer run
library(tidyverse)
library(sf)
library(prioritizr)
library(gurobi)

rob_pacific <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" # Best to define these first so you don't make mistakes below

test <- cbind(c('180','0'))

CnR <- proj4::project(test, proj = rob_pacific)

# inputs
feature1 <- readRDS('outputs/04_IUCN/04d_fFeaturesInt/bycatch_features.rds') %>% 
  as_tibble() %>% 
  filter(feature_names %in% c('Dermochelys_coriacea_IUCN', 'Chelonia_mydas_IUCN')) %>% 
  rename(new_features = feature_names) %>% 
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
                          (y < CnR[1,1]) ~ 1)
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
  add_relative_targets(c(0.5,0.5)) %>%
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



