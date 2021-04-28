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
# Swordfish
###################################

# Look at all variables
m13 <- gam(pa ~ s(SST) + Season2 + s(MLD) + s(Latitude, Longitude) + s(Bathymetry) + s(Dist2Coast) + s(Nitrate) + s(Chl), data = sword, family = "binomial")
summary(m13) # Everything significant except Bathymetry

# Plotting response of all variables
fullplot_swo1 <- visreg(m13, "SST", partial = FALSE, ylab = " ", xlab = "SST", gg = TRUE) + theme_bw()
fullplot_swo2 <- visreg(m13, "Season2", partial = FALSE, ylab = " ", xlab = "Seasons", gg = TRUE) + theme_bw()
fullplot_swo3 <- visreg(m13, "MLD", partial = FALSE, ylab = " ", xlab = "MLD", gg = TRUE) + theme_bw()
fullplot_swo4 <- visreg(m13, "Latitude", partial = FALSE, ylab = " ", xlab = "Latitude", gg = TRUE) + theme_bw()
fullplot_swo5 <- visreg(m13, "Longitude", partial = FALSE, ylab = " ", xlab = "Longitude", gg = TRUE) + theme_bw()
fullplot_swo6 <- visreg(m13, "Bathymetry", partial = FALSE, ylab = " ", xlab = "Bathymetry", gg = TRUE) + theme_bw()
fullplot_swo7 <- visreg(m13, "Dist2Coast", partial = FALSE, ylab = " ", xlab = "Dist2Coast", gg = TRUE) + theme_bw() 
fullplot_swo8 <- visreg(m13, "Nitrate", partial = FALSE, ylab = " ", xlab = "Nitrate", gg = TRUE) + theme_bw()
fullplot_swo9 <- visreg(m13, "Chl", partial = FALSE, ylab = " ", xlab = "Chl", gg = TRUE) + theme_bw()
# Some of the relationships look too wiggly, and might not make sense

SWO_FullModel <- (fullplot_swo1 | fullplot_swo2 | fullplot_swo3) / (fullplot_swo4 | fullplot_swo5 | fullplot_swo6) / (fullplot_swo7 | fullplot_swo8 | fullplot_swo9) +
  plot_annotation(title = "Response of Variables for Full Model", subtitle = "Swordfish", tag_levels = "i")
SWO_FullModel
ggsave("outputs/commercial/GAM_plots/SWO/SWO_FullModel.pdf", width = 20, height = 20, dpi = 320)

# First, let's see what we can drop using BIC
# JAMES EDIT: Left in Chl + Dist2Coast as a linear effect
m14 <- gam(pa ~ s(SST) + Season2 + s(MLD) + s(Latitude, Longitude) + s(Bathymetry) + Dist2Coast + s(Nitrate) + Chl, data = sword, family = "binomial")
summary(m14) # Everything significant except Bathymetry

# Removing Bathymetry
m15 <- update(m14, ~ . -s(Bathymetry))
BIC(m15, m14) # m15 has lower BIC (i.e. Bathymetry n.s.)
summary(m15)

# Try Removing Dist2Coast
m16 <- update(m15, ~ . -Dist2Coast)
BIC(m16, m15) # m16 has lower BIC (Dist2Coast removed)
summary(m16)

# Try Removing Nitrate
m17 <- update(m16, ~ . -s(Nitrate))
BIC(m17, m16) # m17 has higher BIC (Nitrate retained)
summary(m17)

# Removing MLD
m18 <- update(m16, ~ . -s(MLD))
BIC(m18, m16) # m18 has lower BIC (MLD n.s.)
summary(m18)

# Try removing SST
m19 <- update(m18, ~ . -s(SST))
BIC(m19, m18) # m19 has higher BIC (i.e. SST should be retained)
summary(m19)

# Try removing Chl
m20 <- update(m18, ~ . -Chl)
BIC(m20, m18) # m20 has higher BIC (i.e. Chl should be retained)
summary(m20)

# Best Model
SWO_BestModel <- m18
# James' Best Model
SWO_BestModel <- gam(pa ~ s(SST) + Season2 + s(Latitude, Longitude) + Dist2Coast + s(Nitrate) + Chl, data = sword, family = "binomial")
summary(SWO_BestModel)
BIC(SWO_BestModel)

# Saving predictions
sword$Preds <- predict.gam(SWO_BestModel, type = "response")
median(sword$Preds)
# Writing the data and predictions into a .csv
write_csv(sword, file = "inputs/mercer/sword.csv")

bestplot_swo1 <- visreg(SWO_BestModel, "SST", partial = FALSE, ylab = "s(SST, 4.36)", xlab = "SST (°C)", gg = TRUE) + theme_bw() 
bestplot_swo2 <- visreg(SWO_BestModel, "Season2", partial = FALSE, ylab = "f(Season)", xlab = "Season", gg = TRUE) + theme_bw() 
bestplot_swo3 <- visreg(SWO_BestModel, "Nitrate", partial = FALSE, ylab = "s(Nitrate, 5.81)", xlab = "Nitrate (µmol/l)", gg = TRUE) + theme_bw()
bestplot_swo4 <- visreg(SWO_BestModel, "Chl", partial = FALSE, ylab = "f(Chl, -5.45)", xlab = "Chlorophyll A (mg/m^3)", gg = TRUE) + theme_bw()  
bestplot_swo5 <- visreg(SWO_BestModel, "Dist2Coast", partial = FALSE, ylab = "f(Dist2Coast, -5.75x10^-3)", xlab = "Dist2Coast (km)", gg = TRUE) + theme_bw()  

bestplot_swo <- (bestplot_swo1 | bestplot_swo2 | bestplot_swo3) / ( bestplot_swo4 | bestplot_swo5) +
  plot_annotation(title = "Response of Variables for Best Model", subtitle = "Swordfish Tuna", tag_levels = "i")
bestplot_swo
ggsave("outputs/commercial/GAM_plots/SWO/SWO_BestModel.pdf", width = 20, height = 20, dpi = 320)

vis.gam(SWO_BestModel, c("Latitude", "Longitude"), ticktype = "detailed", xlab = "\nLatitude (oC)", 
        ylab = "Longitude", zlab = "\nPresence/Absence", color = "cm", theta = 45, phi = 10, r = 100)
dev.copy2pdf(file = "outputs/commercial/GAM_plots/SWO/SWO_BestModelLatLong.pdf", paper = "A4r")

#######################################
# Plotting best model as a map
#######################################

Surface <- mba.surf(sword[, c("Longitude", "Latitude", "Preds")], 1000, 1000)

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
ggsave("outputs/commercial/GAM_plots/SWO/SWO_map.png", p, dpi = 1200)

###################################
# Swordfish (Pacific-fitted)
###################################

m011 <- gam(pa ~ s(SST) + Season2 + s(MLD) + s(Latitude, Longitude) + s(Bathymetry) + s(Dist2Coast) + s(Nitrate) + s(Chl), data = sword_pacific, family = "binomial")
summary(m011)

# Plotting response of all variables
fullplot_swopac1 <- visreg(m011, "SST", partial = FALSE, ylab = " ", xlab = "SST", gg = TRUE) + theme_bw()
fullplot_swopac2 <- visreg(m011, "Season2", partial = FALSE, ylab = " ", xlab = "Seasons", gg = TRUE) + theme_bw()
fullplot_swopac3 <- visreg(m011, "MLD", partial = FALSE, ylab = " ", xlab = "MLD", gg = TRUE) + theme_bw()
fullplot_swopac4 <- visreg(m011, "Latitude", partial = FALSE, ylab = " ", xlab = "Latitude", gg = TRUE) + theme_bw()
fullplot_swopac5 <- visreg(m011, "Longitude", partial = FALSE, ylab = " ", xlab = "Longitude", gg = TRUE) + theme_bw()
fullplot_swopac6 <- visreg(m011, "Bathymetry", partial = FALSE, ylab = " ", xlab = "Bathymetry", gg = TRUE) + theme_bw()
fullplot_swopac7 <- visreg(m011, "Dist2Coast", partial = FALSE, ylab = " ", xlab = "Dist2Coast", gg = TRUE) + theme_bw() 
# Dist2Coast seems to make more sense that Bathymetry
fullplot_swopac8 <- visreg(m011, "Nitrate", partial = FALSE, ylab = " ", xlab = "Nitrate", gg = TRUE) + theme_bw()
fullplot_swopac9 <- visreg(m011, "Chl", partial = FALSE, ylab = " ", xlab = "Chl", gg = TRUE) + theme_bw()
# Some of the relationships look too wiggly, and might not make sense

SWOPAC_FullModel <- (fullplot_swopac1 | fullplot_swopac2 | fullplot_swopac3) / (fullplot_swopac4 | fullplot_swopac5 | fullplot_swopac6) / (fullplot_swopac7 | fullplot_swopac8 | fullplot_swopac9) +
  plot_annotation(title = "Response of Variables for Full Model", subtitle = "Swordfish (Pacific-fitted)", tag_levels = "i")
SWOPAC_FullModel
ggsave("outputs/commercial/GAM_plots/SWO/SWOPAC_FullModel.pdf", width = 20, height = 20, dpi = 320)

# First, let's see what we can drop using BIC
summary(m011) # Bathymetry and Dist2Coast are not significant.
# Chl seems linear.
# Deviance explained = 20.1%, R-squared (adjusted) = 0.0898

# Remove Bathymetry.
m012 <- update(m011, ~. -s(Bathymetry))
BIC(m012, m011) # BIC is lower. (i.e. Bathymetry n.s.)
summary(m012)

# Remove Dist2Coast.
m013 <- update(m012, ~. -s(Dist2Coast))
BIC(m013, m012) # BIC is lower. (i.e. Dist2Coast n.s.)
summary(m013)

# Try removing MLD.
m014 <- update(m013, ~. -s(MLD))
BIC(m014, m013) # BIC is lower. (i.e. MLD n.s.)
summary(m015)

# Try making MLD linear instead.
m015 <- update(m013, ~. -s(MLD) + MLD)
BIC(m015, m014, m013) # BIC of m014 is lower. (remove MLD)

# Try removing Chl.
m016 <- update(m014, ~. -s(Chl))
BIC(m016, m014) # BIC does not change much. Try making Chl linear instead.
summary(m015)

m017 <- update(m014, ~. -s(Chl) + Chl)
BIC(m017, m016, m014) # BIC does not change much as well.
# Keep Chl as a spline then.

# Best model is m014.
SWOPAC_BestModel <- m014
BIC(m014, m011)

# Saving predictions
sword_pacific$Preds <- predict.gam(SWOPAC_BestModel, type = "response")
median(sword_pacific$Preds)
# Writing the data and predictions into a .csv
write_csv(sword_pacific, file = "inputs/mercer/sword_pacific.csv")

bestplot_swopac1 <- visreg(SWOPAC_BestModel, "SST", partial = FALSE, ylab = "s(SST, 3.16)", xlab = "SST (°C)", gg = TRUE) + theme_bw() + ylim(-100, 50)
bestplot_swopac2 <- visreg(SWOPAC_BestModel, "Season2", partial = FALSE, ylab = "f(Season)", xlab = "Season", gg = TRUE) + theme_bw() 
bestplot_swopac3 <- visreg(SWOPAC_BestModel, "Chl", partial = FALSE, ylab = "s(Chl, 1.00)", xlab = "Chlorophyll A (mg/m^3)", gg = TRUE) + theme_bw()  
bestplot_swopac4 <- visreg(SWOPAC_BestModel, "Nitrate", partial = FALSE, ylab = "s(Nitrate, 4.99)", xlab = "Nitrate (µmol/l)", gg = TRUE) + theme_bw()  

bestplot_swopac <- (bestplot_swopac1 | bestplot_swopac2) / (bestplot_swopac3 | bestplot_swopac4)  +
  plot_annotation(title = "Response of Variables for Best Model", subtitle = "Swordfish (Pacific-fitted)", tag_levels = "i")
bestplot_swopac
ggsave("outputs/commercial/GAM_plots/SWO/SWOPAC_BestModel.pdf", width = 20, height = 20, dpi = 320)

vis.gam(SWOPAC_BestModel, c("Latitude", "Longitude"), type = "response", ticktype = "detailed", xlab = "\nLatitude (°)", 
        ylab = "Longitude (°)", zlab = "Presence", color = "cm", theta = 30, phi = 30, r = 100)
dev.copy2pdf(file = "outputs/commercial/GAM_plots/SWO/SWOPAC_BestModelLatLong.pdf", paper = "A4r")

#######################################
# Plotting best model as a map
#######################################

Surface <- mba.surf(sword_pacific[, c("Longitude", "Latitude", "Preds")], 1000, 1000)

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
ggsave("outputs/commercial/GAM_plots/SWO/SWOPAC_map.png", p, dpi = 1200)
