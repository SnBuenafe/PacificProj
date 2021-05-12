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
# Albacore Tuna (Pacific-fitted)
###################################

m04 <- gam(pa ~ s(SST) + Season2 + s(MLD) + s(Latitude, Longitude) + s(Bathymetry) + s(Dist2Coast) + s(Nitrate) + s(Chl), data = alba_pacific, family = "binomial")
summary(m04)

# Plotting response of all variables
fullplot_albpac1 <- visreg(m04, "SST", partial = FALSE, ylab = " ", xlab = "SST", gg = TRUE) + theme_bw()
fullplot_albpac2 <- visreg(m04, "Season2", partial = FALSE, ylab = " ", xlab = "Seasons", gg = TRUE) + theme_bw()
fullplot_albpac3 <- visreg(m04, "MLD", partial = FALSE, ylab = " ", xlab = "MLD", gg = TRUE) + theme_bw()
fullplot_albpac4 <- visreg(m04, "Latitude", partial = FALSE, ylab = " ", xlab = "Latitude", gg = TRUE) + theme_bw()
fullplot_albpac5 <- visreg(m04, "Longitude", partial = FALSE, ylab = " ", xlab = "Longitude", gg = TRUE) + theme_bw()
fullplot_albpac6 <- visreg(m04, "Bathymetry", partial = FALSE, ylab = " ", xlab = "Bathymetry", gg = TRUE) + theme_bw()
fullplot_albpac7 <- visreg(m04, "Dist2Coast", partial = FALSE, ylab = " ", xlab = "Dist2Coast", gg = TRUE) + theme_bw() 
# Dist2Coast seems to make more sense that Bathymetry
fullplot_albpac8 <- visreg(m04, "Nitrate", partial = FALSE, ylab = " ", xlab = "Nitrate", gg = TRUE) + theme_bw()
fullplot_albpac9 <- visreg(m04, "Chl", partial = FALSE, ylab = " ", xlab = "Chl", gg = TRUE) + theme_bw()
# Some of the relationships look too wiggly, and might not make sense

ALBPAC_FullModel <- (fullplot_albpac1 | fullplot_albpac2 | fullplot_albpac3) / (fullplot_albpac4 | fullplot_albpac5 | fullplot_albpac6) / (fullplot_albpac7 | fullplot_albpac8 | fullplot_albpac9) +
  plot_annotation(title = "Response of Variables for Full Model", subtitle = "Albacore Tuna (Pacific-fitted)", tag_levels = "i")
ALBPAC_FullModel
ggsave("outputs/05_Commercial/05a_GAMPlots/05a6_ALB/ALBPAC_FullModel.pdf", width = 20, height = 20, dpi = 320)

# First, let's see what we can drop using BIC
summary(m04) # MLD, Bathymetry, Dist2Coast, Nitrate are not significant.
# Deviance explained = 33.3%, R-squared (adjusted) = 0.206

# Remove MLD.
m05 <- update(m04, ~. -s(MLD))
BIC(m05, m04) # BIC is lower. (i.e. MLD is not significant)
summary(m05)

# Remove Bathymetry.
m06 <- update(m05, ~. -s(Bathymetry))
BIC(m06, m05) # BIC is lower. (i.e. Bathymetry is not significant)
summary(m06)

# Remove Dist2Coast.
m07 <- update(m06, ~. -s(Dist2Coast))
BIC(m07, m06) # BIC is lower. (i.e. Dist2Coast is not significant)
summary(m07)

# Remove Nitrate.
m08 <- update(m07, ~. -s(Nitrate))
BIC(m08, m07) # BIC is lower. (i.e. Nitrate is not significant)
summary(m08)

# Best model
ALBPAC_BestModel <- m08

# Saving predictions
alba_pacific$Preds <- predict.gam(ALBPAC_BestModel, type = "response")
median(alba_pacific$Preds)
# Writing the data and predictions into a .csv
write_csv(alba_pacific, file = "inputs/mercer/alba_pacific.csv")

bestplot_albpac1 <- visreg(ALBPAC_BestModel, "SST", partial = FALSE, ylab = "s(SST, 5.69)", xlab = "SST (°C)", gg = TRUE) + theme_bw() + ylim(-100, 50)
bestplot_albpac2 <- visreg(ALBPAC_BestModel, "Season2", partial = FALSE, ylab = "f(Season)", xlab = "Season", gg = TRUE) + theme_bw() 
bestplot_albpac3 <- visreg(ALBPAC_BestModel, "Chl", partial = FALSE, ylab = "s(Chl, 3.79)", xlab = "Chlorophyll A (mg/m^3)", gg = TRUE) + theme_bw()  

bestplot_albpac <- (bestplot_albpac1 | bestplot_albpac2) + (bestplot_albpac3)  +
  plot_annotation(title = "Response of Variables for Best Model", subtitle = "Albacore Tuna (Pacific-fitted)", tag_levels = "i")
bestplot_albpac
ggsave("outputs/05_Commercial/05a_GAMPlots/05a6_ALB/ALBPAC_BestModel.pdf", width = 20, height = 20, dpi = 320)

vis.gam(ALBPAC_BestModel, c("Latitude", "Longitude"), type = "response", ticktype = "detailed", xlab = "\nLatitude (°)", 
        ylab = "Longitude (°)", zlab = "Presence", color = "cm", theta = 30, phi = 30, r = 100)
dev.copy2pdf(file = "outputs/05_Commercial/05a_GAMPlots/05a6_ALB/ALBPAC_BestModelLatLong.pdf", paper = "A4r")

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
ggsave("outputs/05_Commercial/05a_GAMPlots/05a6_ALB/ALBPAC_map.png", p, dpi = 1200)
