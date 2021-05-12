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
# Skipjack Tuna (Pacific-fitted)
###################################

m014 <- gam(pa ~ s(SST) + Season2 + s(MLD) + s(Latitude, Longitude) + s(Bathymetry) + s(Dist2Coast) + s(Nitrate) + s(Chl), data = skip_pacific, family = "binomial")
summary(m014)

# Plotting response of all variables
fullplot_skppac1 <- visreg(m014, "SST", partial = FALSE, ylab = " ", xlab = "SST", gg = TRUE) + theme_bw()
fullplot_skppac2 <- visreg(m014, "Season2", partial = FALSE, ylab = " ", xlab = "Seasons", gg = TRUE) + theme_bw()
fullplot_skppac3 <- visreg(m014, "MLD", partial = FALSE, ylab = " ", xlab = "MLD", gg = TRUE) + theme_bw()
fullplot_skppac4 <- visreg(m014, "Latitude", partial = FALSE, ylab = " ", xlab = "Latitude", gg = TRUE) + theme_bw()
fullplot_skppac5 <- visreg(m014, "Longitude", partial = FALSE, ylab = " ", xlab = "Longitude", gg = TRUE) + theme_bw()
fullplot_skppac6 <- visreg(m014, "Bathymetry", partial = FALSE, ylab = " ", xlab = "Bathymetry", gg = TRUE) + theme_bw()
fullplot_skppac7 <- visreg(m014, "Dist2Coast", partial = FALSE, ylab = " ", xlab = "Dist2Coast", gg = TRUE) + theme_bw() 
# Dist2Coast seems to make more sense that Bathymetry
fullplot_skppac8 <- visreg(m014, "Nitrate", partial = FALSE, ylab = " ", xlab = "Nitrate", gg = TRUE) + theme_bw()
fullplot_skppac9 <- visreg(m014, "Chl", partial = FALSE, ylab = " ", xlab = "Chl", gg = TRUE) + theme_bw()
# Some of the relationships look too wiggly, and might not make sense

SKPPAC_FullModel <- (fullplot_skppac1 | fullplot_skppac2 | fullplot_skppac3) / (fullplot_skppac4 | fullplot_skppac5 | fullplot_skppac6) / (fullplot_skppac7 | fullplot_skppac8 | fullplot_skppac9) +
  plot_annotation(title = "Response of Variables for Full Model", subtitle = "Skipjack Tuna (Pacific-fitted)", tag_levels = "i")
SKPPAC_FullModel
ggsave("outputs/05_Commercial/05a_GAMPlots/05a8_SKP/SKPPAC_FullModel.pdf", width = 20, height = 20, dpi = 320)

# First, let's see what we can drop using BIC
summary(m014) # Dist2Coast is not significant.
# Deviance explained = 18.2%, R-squared (adjusted) = 0.149

# Remove Dist2Coast.
m015 <- update(m014, ~. -s(Dist2Coast))
BIC(m015, m014) # BIC is larger for m15. Retain Dist2Coast
summary(m015)

# Best model is m014. Same as full model.
SKPPAC_BestModel <- m014

# Saving predictions
skip_pacific$Preds <- predict.gam(SKPPAC_BestModel, type = "response")
median(skip_pacific$Preds)
# Writing the data and predictions into a .csv
write_csv(skip_pacific, file = "inputs/mercer/skip_pacific.csv")

bestplot_skppac1 <- visreg(SKPPAC_BestModel, "SST", partial = FALSE, ylab = "s(SST, 3.85)", xlab = "SST (°C)", gg = TRUE) + theme_bw() + ylim(-100, 50)
bestplot_skppac2 <- visreg(SKPPAC_BestModel, "Season2", partial = FALSE, ylab = "f(Season)", xlab = "Season", gg = TRUE) + theme_bw() 
bestplot_skppac3 <- visreg(SKPPAC_BestModel, "Chl", partial = FALSE, ylab = "s(Chl, 7.90)", xlab = "Chlorophyll A (mg/m^3)", gg = TRUE) + theme_bw()  
bestplot_skppac4 <- visreg(SKPPAC_BestModel, "Nitrate", partial = FALSE, ylab = "s(Nitrate, 1.00)", xlab = "Nitrate (µmol/l)", gg = TRUE) + theme_bw()  
bestplot_skppac5 <- visreg(SKPPAC_BestModel, "MLD", partial = FALSE, ylab = "s(MLD, 4.73)", xlab = "MLD (m)", gg = TRUE) + theme_bw()  
bestplot_skppac6 <- visreg(SKPPAC_BestModel, "Bathymetry", partial = FALSE, ylab = "s(Bathymetry, 3.54)", xlab = "Bathymetry (m)", gg = TRUE) + theme_bw()  
bestplot_skppac7 <- visreg(SKPPAC_BestModel, "Dist2Coast", partial = FALSE, ylab = "s(Dist2Coast, 1.00)", xlab = "Dist2Coast (km)", gg = TRUE) + theme_bw()  

bestplot_skppac <- (bestplot_skppac1 | bestplot_skppac2 | bestplot_skppac3 | bestplot_skppac4) / (bestplot_skppac5 | bestplot_skppac6 | bestplot_skppac7) + 
  plot_annotation(title = "Response of Variables for Best Model", subtitle = "Skipjack Tuna (Pacific-fitted)", tag_levels = "i")
bestplot_skppac
ggsave("outputs/05_Commercial/05a_GAMPlots/05a8_SKP/SKPPAC_BestModel.pdf", width = 20, height = 20, dpi = 320)

vis.gam(SKPPAC_BestModel, c("Latitude", "Longitude"), type = "response", ticktype = "detailed", xlab = "\nLatitude (°)", 
        ylab = "Longitude (°)", zlab = "Presence", color = "cm", theta = 30, phi = 30, r = 100)
dev.copy2pdf(file = "outputs/05_Commercial/05a_GAMPlots/05a8_SKP/SKPPAC_BestModelLatLong.pdf", paper = "A4r")

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
ggsave("outputs/05_Commercial/05a_GAMPlots/05a8_SKP/SKPPAC_map.png", p, dpi = 1200)
