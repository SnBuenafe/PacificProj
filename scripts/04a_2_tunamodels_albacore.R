# All models (global-fitted) are from James Mercer's code.
# 04a creates the GAMs for each of the species.
# It saves the predictions (with the environmental variables and the coordinates) as a .csv file. (dir: input/mercer/)
# Saves the visreg plots and maps. (dir: outputs/commercial/GAM_plots/)
# There are 4 parts to 04a:
# 1. 04a_1: yellowfin
# 2. 04a_2: albacore
# 3. 04a_3: swordfish
# 4. 04a_4: skipjack
# The code must be run one after the other.

library(tidyverse)
library(effects)
library(splines)
library(ggplot2)
library(devtools)
library(ggiraphExtra)
library(ggiraph)
library(mgcv)
library(visreg)
library(patchwork)
library(MBA) # Does bilinear interpolation
library(reshape2) # For melt
library(colorRamps) # for Matlab like colour scheme
library(ggthemes) # for theme_minimal()
library(patchwork)

###################################
# Albacore
###################################

# Creating GAM for all variables
m6 <- gam(pa ~ s(SST) + Season2 + s(MLD) + s(Latitude, Longitude) + s(Bathymetry) + s(Dist2Coast) + s(Nitrate) + s(Chl), data = alba, family = "binomial")
summary(m6) # Everything significant except Bathymetry and Distance to Coast

# Plotting response of all variables
fullplot_alb1 <- visreg(m6, "SST", partial = FALSE, ylab = " ", xlab = "SST", gg = TRUE) + theme_bw()
fullplot_alb2 <- visreg(m6, "Season2", partial = FALSE, ylab = " ", xlab = "Seasons", gg = TRUE) + theme_bw()
fullplot_alb3 <- visreg(m6, "MLD", partial = FALSE, ylab = " ", xlab = "MLD", gg = TRUE) + theme_bw()
fullplot_alb4 <- visreg(m6, "Latitude", partial = FALSE, ylab = " ", xlab = "Latitude", gg = TRUE) + theme_bw()
fullplot_alb5 <- visreg(m6, "Longitude", partial = FALSE, ylab = " ", xlab = "Longitude", gg = TRUE) + theme_bw()
fullplot_alb6 <- visreg(m6, "Bathymetry", partial = FALSE, ylab = " ", xlab = "Bathymetry", gg = TRUE) + theme_bw()
fullplot_alb7 <- visreg(m6, "Dist2Coast", partial = FALSE, ylab = " ", xlab = "Dist2Coast", gg = TRUE) + theme_bw() 
fullplot_alb8 <- visreg(m6, "Nitrate", partial = FALSE, ylab = " ", xlab = "Nitrate", gg = TRUE) + theme_bw()
fullplot_alb9 <- visreg(m6, "Chl", partial = FALSE, ylab = " ", xlab = "Chl", gg = TRUE) + theme_bw()
# Some of the relationships look too wiggly, and might not make sense

ALB_FullModel <- (fullplot_alb1 | fullplot_alb2 | fullplot_alb3) / (fullplot_alb4 | fullplot_alb5 | fullplot_alb6) / (fullplot_alb7 | fullplot_alb8 | fullplot_alb9) +
  plot_annotation(title = "Response of Variables for Full Model", subtitle = "Albacore Tuna", tag_levels = "i")
ALB_FullModel
ggsave("outputs/commercial/GAM_plots/ALB/ALB_FullModel.pdf", width = 20, height = 20, dpi = 320)
# Some of the relationships look too wiggly, and might not make sense

# First, let's see what we can drop using BIC
summary(m6) # Bathymetry and Dist2Coast are not significant.
# Deviance explained = 30.9%, R-squared (adjusted) = 0.185

# Removing Bathymetry
m7 <- update(m6, ~ . -s(Bathymetry))
BIC(m7, m6) # m7 has lower BIC (i.e. Bathymetry n.s.)
summary(m7)

# Dist2Coast n.s. but looks important in plot - let's see
m8 <- update(m7, ~ . -s(Dist2Coast))
BIC(m7, m8) # m8 has lower BIC (Dist2Coast n.s.)
summary(m8)

# Removing Nitrate
m9 <- update(m8, ~ . -s(Nitrate))
BIC(m9, m8) # m9 has lower BIC (Nitrate n.s.)
summary(m9)

# Removing MLD
m10 <- update(m9, ~ . -s(MLD))
BIC(m10, m9) # m10 has lower BIC (MLD n.s.)
summary(m10)

# Try removing SST
m11 <- update(m10, ~ . -s(SST))
BIC(m11, m10) # m10 has lower BIC (i.e. SST should be retained)
summary(m11)

# Try removing Chl
m12 <- update(m10, ~ . -s(Chl))
BIC(m10, m12) # m10 has lower BIC (i.e. Chl should be retained)
summary(m12)

# Best Model
ALBA_BestModel <- m10

# Saving predictions
alba$Preds <- predict.gam(ALBA_BestModel, type = "response")
median(alba$Preds)
# Writing the data and predictions into a .csv
write_csv(alba, file = "inputs/mercer/alba.csv")

bestplot_alba1 <- visreg(ALBA_BestModel, "SST", partial = FALSE, ylab = "s(SST, 5.53)", xlab = "SST (°C)", gg = TRUE) + theme_bw() + ylim(-100, 50)
bestplot_alba2 <- visreg(ALBA_BestModel, "Season2", partial = FALSE, ylab = "f(Season)", xlab = "Season", gg = TRUE) + theme_bw() 
bestplot_alba3 <- visreg(ALBA_BestModel, "Chl", partial = FALSE, ylab = "s(Chl, 4.29)", xlab = "Chlorophyll A (mg/m^3)", gg = TRUE) + theme_bw()  

bestplot_alba <- (bestplot_alba1 | bestplot_alba2) / (bestplot_alba3) +
  plot_annotation(title = "Response of Variables for Best Model", subtitle = "Albacore Tuna", tag_levels = "i")
bestplot_alba
ggsave("outputs/commercial/GAM_plots/ALB/ALBA_BestModel.pdf", width = 20, height = 20, dpi = 320)

vis.gam(ALBA_BestModel, c("Latitude", "Longitude"), type = "response", ticktype = "detailed", xlab = "\nLatitude (°)", 
        ylab = "Longitude (°)", zlab = "Presence", color = "cm", theta = 30, phi = 30, r = 100)
dev.copy2pdf(file = "outputs/commercial/GAM_plots/ALB/ALBA_BestModelLatLong.pdf", paper = "A4r")

#######################################
# Plotting best model as a map
#######################################

Surface <- mba.surf(alba[, c("Longitude", "Latitude", "Preds")], 1000, 1000)

# This is just to organise dataframe for plotting
dimnames(Surface$xyz.est$z) <- list(Surface$xyz.est$x, Surface$xyz.est$y)
df3 <- melt(Surface$xyz.est$z, varnames = c('Longitude', 'Latitude'), value.name = 'Preds')

# Plot the map
x11(width = 14, height = 7)
p <- ggplot(data = df3, aes(Longitude, Latitude)) +
  geom_raster(aes(fill = Preds)) +
  scale_fill_gradientn(colours = matlab.like(7), na.value = "white") +
  theme_minimal() +
  theme(legend.position="right")

p <- p + geom_map(data = WorldData, map = WorldData,
                  aes(x = long, y = lat, group = group, map_id = region),
                  fill = "grey", colour = "grey", size = 0.5)
p 
ggsave("outputs/commercial/GAM_plots/ALB/ALB_map.png", p, dpi = 1200)

###################################
# Albacore Tuna (Pacific-fitted)
###################################

m06 <- gam(pa ~ s(SST) + Season2 + s(MLD) + s(Latitude, Longitude) + s(Bathymetry) + s(Dist2Coast) + s(Nitrate) + s(Chl), data = alba_pacific, family = "binomial")
summary(m06)

# Plotting response of all variables
fullplot_albpac1 <- visreg(m06, "SST", partial = FALSE, ylab = " ", xlab = "SST", gg = TRUE) + theme_bw()
fullplot_albpac2 <- visreg(m06, "Season2", partial = FALSE, ylab = " ", xlab = "Seasons", gg = TRUE) + theme_bw()
fullplot_albpac3 <- visreg(m06, "MLD", partial = FALSE, ylab = " ", xlab = "MLD", gg = TRUE) + theme_bw()
fullplot_albpac4 <- visreg(m06, "Latitude", partial = FALSE, ylab = " ", xlab = "Latitude", gg = TRUE) + theme_bw()
fullplot_albpac5 <- visreg(m06, "Longitude", partial = FALSE, ylab = " ", xlab = "Longitude", gg = TRUE) + theme_bw()
fullplot_albpac6 <- visreg(m06, "Bathymetry", partial = FALSE, ylab = " ", xlab = "Bathymetry", gg = TRUE) + theme_bw()
fullplot_albpac7 <- visreg(m06, "Dist2Coast", partial = FALSE, ylab = " ", xlab = "Dist2Coast", gg = TRUE) + theme_bw() 
# Dist2Coast seems to make more sense that Bathymetry
fullplot_albpac8 <- visreg(m06, "Nitrate", partial = FALSE, ylab = " ", xlab = "Nitrate", gg = TRUE) + theme_bw()
fullplot_albpac9 <- visreg(m06, "Chl", partial = FALSE, ylab = " ", xlab = "Chl", gg = TRUE) + theme_bw()
# Some of the relationships look too wiggly, and might not make sense

ALBPAC_FullModel <- (fullplot_albpac1 | fullplot_albpac2 | fullplot_albpac3) / (fullplot_albpac4 | fullplot_albpac5 | fullplot_albpac6) / (fullplot_albpac7 | fullplot_albpac8 | fullplot_albpac9) +
  plot_annotation(title = "Response of Variables for Full Model", subtitle = "Albacore Tuna (Pacific-fitted)", tag_levels = "i")
ALBPAC_FullModel
ggsave("outputs/commercial/GAM_plots/ALB/ALBPAC_FullModel.pdf", width = 20, height = 20, dpi = 320)

# First, let's see what we can drop using BIC
summary(m06) # MLD, Bathymetry, Dist2Coast, Nitrate are not significant.
# Deviance explained = 33.3%, R-squared (adjusted) = 0.206

# Remove MLD.
m07 <- update(m06, ~. -s(MLD))
BIC(m07, m06) # BIC is lower. (i.e. MLD is not significant)
summary(m07)

# Remove Bathymetry.
m08 <- update(m07, ~. -s(Bathymetry))
BIC(m08, m07) # BIC is lower. (i.e. Bathymetry is not significant)
summary(m08)

# Remove Dist2Coast.
m09 <- update(m08, ~. -s(Dist2Coast))
BIC(m09, m08) # BIC is lower. (i.e. Dist2Coast is not significant)
summary(m09)

# Remove Nitrate.
m010 <- update(m09, ~. -s(Nitrate))
BIC(m010, m09) # BIC is lower. (i.e. Nitrate is not significant)
summary(m010)

# Best model
ALBPAC_BestModel <- m010

# Saving predictions
alba_pacific$Preds <- predict.gam(ALBPAC_BestModel, type = "response")
median(alba_pacific$Preds)
# Writing the data and predictions into a .csv
write_csv(alba_pacific, file = "inputs/mercer/alba_pacific.csv")

bestplot_albpac1 <- visreg(ALBPAC_BestModel, "SST", partial = FALSE, ylab = "s(SST, 5.69)", xlab = "SST (°C)", gg = TRUE) + theme_bw() + ylim(-100, 50)
bestplot_albpac2 <- visreg(ALBPAC_BestModel, "Season2", partial = FALSE, ylab = "f(Season)", xlab = "Season", gg = TRUE) + theme_bw() 
bestplot_albpac3 <- visreg(ALBPAC_BestModel, "Chl", partial = FALSE, ylab = "s(Chl, 3.76)", xlab = "Chlorophyll A (mg/m^3)", gg = TRUE) + theme_bw()  

bestplot_albpac <- (bestplot_albpac1 | bestplot_albpac2) + (bestplot_albpac3)  +
  plot_annotation(title = "Response of Variables for Best Model", subtitle = "Albacore Tuna (Pacific-fitted)", tag_levels = "i")
bestplot_albpac
ggsave("outputs/commercial/GAM_plots/ALB/ALBPAC_BestModel.pdf", width = 20, height = 20, dpi = 320)

vis.gam(ALBPAC_BestModel, c("Latitude", "Longitude"), type = "response", ticktype = "detailed", xlab = "\nLatitude (°)", 
        ylab = "Longitude (°)", zlab = "Presence", color = "cm", theta = 30, phi = 30, r = 100)
dev.copy2pdf(file = "outputs/commercial/GAM_plots/ALB/ALBPAC_BestModelLatLong.pdf", paper = "A4r")

#######################################
# Plotting best model as a map
#######################################

Surface <- mba.surf(alba_pacific[, c("Longitude", "Latitude", "Preds")], 1000, 1000)

# This is just to organise dataframe for plotting
dimnames(Surface$xyz.est$z) <- list(Surface$xyz.est$x, Surface$xyz.est$y)
df3 <- melt(Surface$xyz.est$z, varnames = c('Longitude', 'Latitude'), value.name = 'Preds')

# Plot the map
x11(width = 14, height = 7)
p <- ggplot(data = df3, aes(Longitude, Latitude)) +
  geom_raster(aes(fill = Preds)) +
  scale_fill_gradientn(colours = matlab.like(7), na.value = "white") +
  theme_minimal() +
  theme(legend.position="right")

p <- p + geom_map(data = WorldData, map = WorldData,
                  aes(x = long, y = lat, group = group, map_id = region),
                  fill = "grey", colour = "grey", size = 0.5)
p 
ggsave("outputs/commercial/GAM_plots/ALB/ALBPAC_map.png", p, dpi = 1200)
