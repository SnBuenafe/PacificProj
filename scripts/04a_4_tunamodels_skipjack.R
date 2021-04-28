# All models (global-fitted) are from James Mercer's code.
# 04a creates the GAMs for each of the species.
# It saves the predictions (with the environmental variables and the coordinates) as a .csv file. (dir: input/mercer/)
# Saves the visreg plots and maps. (dir: outputs/commercial/GAM_plots/)
# There are 4 parts to 04a:
# 1. 04a_1: yellowfin
# 2. 04a_2: albacore
# 3. 04a_3: skipjack
# 4. 04a_4: swordfish
# The code must be run one after the other.

####################################################################################
####### Defining packages needed
####################################################################################
# List of pacakges that we will use
list.of.packages <- c("tidyverse", "effects", "splines", "devtools", "ggiraphExtra",
                      "ggiraph", "mgcv", "visreg", "patchwork", "MBA", "reshape2",
                      "colorRamps", "ggthemes")
# MBA: for bilinear interpolation; reshape2: for melt
# If is not installed, install the pacakge
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
# Load packages
lapply(list.of.packages, require, character.only = TRUE)

###################################
# Skipjack Tuna
###################################

# Look at all variables
m21 <- gam(pa ~ s(SST) + Season2 + s(MLD) + s(Latitude, Longitude) + s(Bathymetry) + s(Dist2Coast) + s(Nitrate) + s(Chl), data = skip, family = "binomial")
summary(m21) # Everything significant except Bathymetry

# Plotting response of all variables
fullplot_skp1 <- visreg(m21, "SST", partial = FALSE, ylab = " ", xlab = "SST", gg = TRUE) + theme_bw()
fullplot_skp2 <- visreg(m21, "Season2", partial = FALSE, ylab = " ", xlab = "Seasons", gg = TRUE) + theme_bw()
fullplot_skp3 <- visreg(m21, "MLD", partial = FALSE, ylab = " ", xlab = "MLD", gg = TRUE) + theme_bw()
fullplot_skp4 <- visreg(m21, "Latitude", partial = FALSE, ylab = " ", xlab = "Latitude", gg = TRUE) + theme_bw()
fullplot_skp5 <- visreg(m21, "Longitude", partial = FALSE, ylab = " ", xlab = "Longitude", gg = TRUE) + theme_bw()
fullplot_skp6 <- visreg(m21, "Bathymetry", partial = FALSE, ylab = " ", xlab = "Bathymetry", gg = TRUE) + theme_bw()
fullplot_skp7 <- visreg(m21, "Dist2Coast", partial = FALSE, ylab = " ", xlab = "Dist2Coast", gg = TRUE) + theme_bw() 
fullplot_skp8 <- visreg(m21, "Nitrate", partial = FALSE, ylab = " ", xlab = "Nitrate", gg = TRUE) + theme_bw()
fullplot_skp9 <- visreg(m21, "Chl", partial = FALSE, ylab = " ", xlab = "Chl", gg = TRUE) + theme_bw()
# Some of the relationships look too wiggly, and might not make sense

SKP_FullModel <- (fullplot_skp1 | fullplot_skp2 | fullplot_skp3) / (fullplot_skp4 | fullplot_skp5 | fullplot_skp6) / (fullplot_skp7 | fullplot_skp8 | fullplot_skp9) +
  plot_annotation(title = "Response of Variables for Full Model", subtitle = "Skipjack tuna", tag_levels = "i")
SKP_FullModel
ggsave("outputs/commercial/GAM_plots/SKP/SKP_FullModel.pdf", width = 20, height = 20, dpi = 320)

# JAMES EDIT: Left in Nitrate as a linear effect
m22 <- gam(pa ~ s(SST) + Season2 + s(MLD) + s(Latitude, Longitude) + s(Bathymetry) + s(Dist2Coast) + Nitrate + s(Chl), data = skip, family = "binomial")
summary(m22) # Everything significant except Bathymetry

# Removing Bathymetry
m23 <- update(m22, ~ . -s(Bathymetry))
BIC(m22, m23) # m23 has lower BIC (removed Bathymetry)
summary(m23)

# Try Removing Dist2Coast
m24 <- update(m23, ~ . -s(Dist2Coast))
BIC(m24, m23) # m24 has higher BIC (Dist2Coast retained)
summary(m24)

# Try Removing Nitrate
m25 <- update(m23, ~ . -Nitrate)
BIC(m25, m23) # m25 has higher BIC (Nitrate retained)
summary(m25)

# Removing MLD
m26 <- update(m23, ~ . -s(MLD))
BIC(m26, m23) # m26 has lower BIC (MLD n.s.)
summary(m26)

# Try removing SST
m27 <- update(m26, ~ . -s(SST))
BIC(m27, m26) # m27 has higher BIC (SST retained)

# Best Model
SKP_BestModel <- m26
# James' best model
SKP_BestModel <- gam(pa ~ s(SST) + Season2 + s(Latitude, Longitude)+ Dist2Coast + Nitrate + s(Chl, k = 4), data = skip, family = "binomial")
summary(SKP_BestModel)

# Saving predictions
skip$Preds <- predict.gam(SKP_BestModel, type = "response")
median(skip$Preds)
# Writing the data and predictions into a .csv
write_csv(sword, file = "inputs/mercer/skip.csv")

bestplot_skp1 <- visreg(SKP_BestModel, "SST", partial = FALSE, ylab = "s(SST, 4.82)", xlab = "SST (°C)", gg = TRUE) + theme_bw() 
bestplot_skp2 <- visreg(SKP_BestModel, "Season2", partial = FALSE, ylab = "f(Season)", xlab = "Season", gg = TRUE) + theme_bw() 
bestplot_skp3 <- visreg(SKP_BestModel, "Nitrate", partial = FALSE, ylab = "f(Nitrate, -0.20)", xlab = "Nitrate (µmol/l)", gg = TRUE) + theme_bw()
bestplot_skp4 <- visreg(SKP_BestModel, "Dist2Coast", partial = FALSE, ylab = "f(DistCoast, -1.02x10^-3)", xlab = "Distance to Coast (km)", gg = TRUE) + theme_bw() 
bestplot_skp5 <- visreg(SKP_BestModel, "Chl", partial = FALSE, ylab = "s(Chl, 1.89)", xlab = "Chlorophyll A (mg/m^3)", gg = TRUE) + theme_bw()  

bestplot_skp <- (bestplot_skp1 | bestplot_skp2 | bestplot_skp3) / ( bestplot_skp4 | bestplot_skp5) +
  plot_annotation(title = "Response of Variables for Best Model", subtitle = "Skipjack Tuna", tag_levels = "i")
bestplot_skp
ggsave("outputs/commercial/GAM_plots/SKP/SKP_BestModel.pdf", width = 20, height = 20, dpi = 320)

vis.gam(SKP_BestModel, c("Latitude", "Longitude"), ticktype = "detailed", xlab = "\nLatitude (oC)", 
        ylab = "Longitude", zlab = "\nPresence/Absence", color = "cm", theta = 45, phi = 10, r = 100)
dev.copy2pdf(file = "outputs/commercial/GAM_plots/SKP/SKP_BestModelLatLong.pdf", paper = "A4r")

#######################################
# Plotting best model as a map
#######################################

Surface <- mba.surf(skip[, c("Longitude", "Latitude", "Preds")], 1000, 1000)

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
ggsave("outputs/commercial/GAM_plots/SKP/SKP_map.png", p, dpi = 1200)

###################################
# Skipjack Tuna (Pacific-fitted)
###################################

m018 <- gam(pa ~ s(SST) + Season2 + s(MLD) + s(Latitude, Longitude) + s(Bathymetry) + s(Dist2Coast) + s(Nitrate) + s(Chl), data = skip_pacific, family = "binomial")
summary(m018)

# Plotting response of all variables
fullplot_skppac1 <- visreg(m018, "SST", partial = FALSE, ylab = " ", xlab = "SST", gg = TRUE) + theme_bw()
fullplot_skppac2 <- visreg(m018, "Season2", partial = FALSE, ylab = " ", xlab = "Seasons", gg = TRUE) + theme_bw()
fullplot_skppac3 <- visreg(m018, "MLD", partial = FALSE, ylab = " ", xlab = "MLD", gg = TRUE) + theme_bw()
fullplot_skppac4 <- visreg(m018, "Latitude", partial = FALSE, ylab = " ", xlab = "Latitude", gg = TRUE) + theme_bw()
fullplot_skppac5 <- visreg(m018, "Longitude", partial = FALSE, ylab = " ", xlab = "Longitude", gg = TRUE) + theme_bw()
fullplot_skppac6 <- visreg(m018, "Bathymetry", partial = FALSE, ylab = " ", xlab = "Bathymetry", gg = TRUE) + theme_bw()
fullplot_skppac7 <- visreg(m018, "Dist2Coast", partial = FALSE, ylab = " ", xlab = "Dist2Coast", gg = TRUE) + theme_bw() 
# Dist2Coast seems to make more sense that Bathymetry
fullplot_skppac8 <- visreg(m018, "Nitrate", partial = FALSE, ylab = " ", xlab = "Nitrate", gg = TRUE) + theme_bw()
fullplot_skppac9 <- visreg(m018, "Chl", partial = FALSE, ylab = " ", xlab = "Chl", gg = TRUE) + theme_bw()
# Some of the relationships look too wiggly, and might not make sense

SKPPAC_FullModel <- (fullplot_skppac1 | fullplot_skppac2 | fullplot_skppac3) / (fullplot_skppac4 | fullplot_skppac5 | fullplot_skppac6) / (fullplot_skppac7 | fullplot_skppac8 | fullplot_skppac9) +
  plot_annotation(title = "Response of Variables for Full Model", subtitle = "Skipjack Tuna (Pacific-fitted)", tag_levels = "i")
SKPPAC_FullModel
ggsave("outputs/commercial/GAM_plots/SKP/SKPPAC_FullModel.pdf", width = 20, height = 20, dpi = 320)

# First, let's see what we can drop using BIC
summary(m018) # Dist2Coast is not significant.
# Deviance explained = 18.3%, R-squared (adjusted) = 0.149

# Remove Dist2Coast.
m019 <- update(m018, ~. -s(Dist2Coast))
BIC(m019, m018) # BIC is larger for m19. Retain Dist2Coast
summary(m019)

# Try to make Dist2Coast linear instead.
m020 <- update(m019, ~. +Dist2Coast)
BIC(m020, m018)
summary(m020)

# Best model is m020.
SKPPAC_BestModel <- m020
BIC(m020, m018)

# Saving predictions
skip_pacific$Preds <- predict.gam(SKPPAC_BestModel, type = "response")
median(skip_pacific$Preds)
# Writing the data and predictions into a .csv
write_csv(skip_pacific, file = "inputs/mercer/skip_pacific.csv")

bestplot_skppac1 <- visreg(SKPPAC_BestModel, "SST", partial = FALSE, ylab = "s(SST, 3.86)", xlab = "SST (°C)", gg = TRUE) + theme_bw() + ylim(-100, 50)
bestplot_skppac2 <- visreg(SKPPAC_BestModel, "Season2", partial = FALSE, ylab = "f(Season)", xlab = "Season", gg = TRUE) + theme_bw() 
bestplot_skppac3 <- visreg(SKPPAC_BestModel, "Chl", partial = FALSE, ylab = "s(Chl, 8.78)", xlab = "Chlorophyll A (mg/m^3)", gg = TRUE) + theme_bw()  
bestplot_skppac4 <- visreg(SKPPAC_BestModel, "Nitrate", partial = FALSE, ylab = "s(Nitrate, 1.00)", xlab = "Nitrate (µmol/l)", gg = TRUE) + theme_bw()  
bestplot_skppac5 <- visreg(SKPPAC_BestModel, "MLD", partial = FALSE, ylab = "s(MLD, 4.72)", xlab = "MLD (m)", gg = TRUE) + theme_bw()  
bestplot_skppac6 <- visreg(SKPPAC_BestModel, "Bathymetry", partial = FALSE, ylab = "s(Bathymetry, 3.48)", xlab = "Bathymetry (m)", gg = TRUE) + theme_bw()  
bestplot_skppac7 <- visreg(SKPPAC_BestModel, "Dist2Coast", partial = FALSE, ylab = "f(Dist2Coast, -3.92x10^-4)", xlab = "Dist2Coast (km)", gg = TRUE) + theme_bw()  

bestplot_skppac <- (bestplot_skppac1 | bestplot_skppac2 | bestplot_skppac3 | bestplot_skppac4) / (bestplot_skppac5 | bestplot_skppac6 | bestplot_skppac7) + 
  plot_annotation(title = "Response of Variables for Best Model", subtitle = "Skipjack Tuna (Pacific-fitted)", tag_levels = "i")
bestplot_skppac
ggsave("outputs/commercial/GAM_plots/SKP/SKPPAC_BestModel.pdf", width = 20, height = 20, dpi = 320)

vis.gam(SKPPAC_BestModel, c("Latitude", "Longitude"), type = "response", ticktype = "detailed", xlab = "\nLatitude (°)", 
        ylab = "Longitude (°)", zlab = "Presence", color = "cm", theta = 30, phi = 30, r = 100)
dev.copy2pdf(file = "outputs/commercial/GAM_plots/SKP/SKPPAC_BestModelLatLong.pdf", paper = "A4r")

#######################################
# Plotting best model as a map
#######################################

Surface <- mba.surf(skip_pacific[, c("Longitude", "Latitude", "Preds")], 1000, 1000)

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
ggsave("outputs/commercial/GAM_plots/SKP/SKPPAC_map.png", p, dpi = 1200)
