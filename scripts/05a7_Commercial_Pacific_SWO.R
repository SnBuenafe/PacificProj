# All models (global-fitted) are from James Mercer's code.
# 05a creates the GAMs for each of the species.
# It saves the predictions (with the environmental variables and the coordinates) as a .csv file. (dir: input/mercer/)
# Saves the visreg plots and maps. (dir: outputs/05_Commercial/05a_GAMPlots)
# There are 8 parts to 04a; the last 4 of which are pacific-fitted:
# 1. 05a5: yellowfin
# 2. 05a6: albacore
# 3. 05a7: swordfish
# 4. 05a8: skipjack
# The code must be run one after the other.

####################################
##### Defining packages needed #####
####################################
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
# Swordfish (Pacific-fitted)
###################################

m09 <- gam(pa ~ s(SST) + Season2 + s(MLD) + s(Latitude, Longitude) + s(Bathymetry) + s(Dist2Coast) + s(Nitrate) + s(Chl), data = sword_pacific, family = "binomial")
summary(m09)

# Plotting response of all variables
fullplot_swopac1 <- visreg(m09, "SST", partial = FALSE, ylab = " ", xlab = "SST", gg = TRUE) + theme_bw()
fullplot_swopac2 <- visreg(m09, "Season2", partial = FALSE, ylab = " ", xlab = "Seasons", gg = TRUE) + theme_bw()
fullplot_swopac3 <- visreg(m09, "MLD", partial = FALSE, ylab = " ", xlab = "MLD", gg = TRUE) + theme_bw()
fullplot_swopac4 <- visreg(m09, "Latitude", partial = FALSE, ylab = " ", xlab = "Latitude", gg = TRUE) + theme_bw()
fullplot_swopac5 <- visreg(m09, "Longitude", partial = FALSE, ylab = " ", xlab = "Longitude", gg = TRUE) + theme_bw()
fullplot_swopac6 <- visreg(m09, "Bathymetry", partial = FALSE, ylab = " ", xlab = "Bathymetry", gg = TRUE) + theme_bw()
fullplot_swopac7 <- visreg(m09, "Dist2Coast", partial = FALSE, ylab = " ", xlab = "Dist2Coast", gg = TRUE) + theme_bw() 
# Dist2Coast seems to make more sense that Bathymetry
fullplot_swopac8 <- visreg(m09, "Nitrate", partial = FALSE, ylab = " ", xlab = "Nitrate", gg = TRUE) + theme_bw()
fullplot_swopac9 <- visreg(m09, "Chl", partial = FALSE, ylab = " ", xlab = "Chl", gg = TRUE) + theme_bw()
# Some of the relationships look too wiggly, and might not make sense

SWOPAC_FullModel <- (fullplot_swopac1 | fullplot_swopac2 | fullplot_swopac3) / (fullplot_swopac4 | fullplot_swopac5 | fullplot_swopac6) / (fullplot_swopac7 | fullplot_swopac8 | fullplot_swopac9) +
  plot_annotation(title = "Response of Variables for Full Model", subtitle = "Swordfish (Pacific-fitted)", tag_levels = "i")
SWOPAC_FullModel
ggsave("outputs/05_Commercial/05a_GAMPlots/05a7_SWO/SWOPAC_FullModel.pdf", width = 20, height = 20, dpi = 320)

# First, let's see what we can drop using BIC
summary(m09) # Bathymetry and Dist2Coast are not significant.
# Deviance explained = 20.1%, R-squared (adjusted) = 0.0897

# Remove Bathymetry.
m010 <- update(m09, ~. -s(Bathymetry))
BIC(m010, m09) # BIC is lower. (i.e. Bathymetry n.s.)
summary(m010)

# Remove Dist2Coast.
m011 <- update(m010, ~. -s(Dist2Coast))
BIC(m011, m010) # BIC is lower. (i.e. Dist2Coast n.s.)
summary(m011)

# Try removing MLD.
m012 <- update(m011, ~. -s(MLD))
BIC(m012, m011) # BIC is lower. (i.e. MLD n.s.)
summary(m012)

# Try removing Chl.
m013 <- update(m012, ~. -s(Chl))
BIC(m013, m012) # BIC does not change much, but is a little bit lower.
summary(m013)
# Keep Chl.

# Best model is m012.
SWOPAC_BestModel <- m012
BIC(m012, m09)

# Saving predictions
sword_pacific$Preds <- predict.gam(SWOPAC_BestModel, type = "response")
median(sword_pacific$Preds)
# Writing the data and predictions into a .csv
write_csv(sword_pacific, file = "inputs/mercer/sword_pacific.csv")

bestplot_swopac1 <- visreg(SWOPAC_BestModel, "SST", partial = FALSE, ylab = "s(SST, 3.16)", xlab = "SST (°C)", gg = TRUE) + theme_bw() + ylim(-100, 50)
bestplot_swopac2 <- visreg(SWOPAC_BestModel, "Season2", partial = FALSE, ylab = "f(Season)", xlab = "Season", gg = TRUE) + theme_bw() 
bestplot_swopac3 <- visreg(SWOPAC_BestModel, "Chl", partial = FALSE, ylab = "s(Chl, 1.00)", xlab = "Chlorophyll A (mg/m^3)", gg = TRUE) + theme_bw()  
bestplot_swopac4 <- visreg(SWOPAC_BestModel, "Nitrate", partial = FALSE, ylab = "s(Nitrate, 4.83)", xlab = "Nitrate (µmol/l)", gg = TRUE) + theme_bw()  

bestplot_swopac <- (bestplot_swopac1 | bestplot_swopac2) / (bestplot_swopac3 | bestplot_swopac4)  +
  plot_annotation(title = "Response of Variables for Best Model", subtitle = "Swordfish (Pacific-fitted)", tag_levels = "i")
bestplot_swopac
ggsave("outputs/05_Commercial/05a_GAMPlots/05a7_SWO/SWOPAC_BestModel.pdf", width = 20, height = 20, dpi = 320)

vis.gam(SWOPAC_BestModel, c("Latitude", "Longitude"), type = "response", ticktype = "detailed", xlab = "\nLatitude (°)", 
        ylab = "Longitude (°)", zlab = "Presence", color = "cm", theta = 30, phi = 30, r = 100)
dev.copy2pdf(file = "outputs/05_Commercial/05a_GAMPlots/05a7_SWO/SWOPAC_BestModelLatLong.pdf", paper = "A4r")

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
ggsave("outputs/05_Commercial/05a_GAMPlots/05a7_SWO/SWOPAC_map.png", p, dpi = 1200)
