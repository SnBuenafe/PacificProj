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

######################
#### Calling Data ####
######################
tuna_data <- read.csv("inputs/mercer/TunaData_final.txt", sep="")
tuna_data <- na.omit(tuna_data)

tuna_data <- tuna_data %>% mutate(Season2 = replace(Season2, Season2 == "summer", 2)) %>% 
  mutate(Season2 = replace(Season2, Season2 == "winter", 4)) %>% 
  mutate(Season2 = replace(Season2, Season2 == "autumn", 3)) %>% 
  mutate(Season2 = replace(Season2, Season2 == "spring", 1))

#subset based on species
yft_pacific <- subset(tuna_data, species == "yellowfin tuna") %>% 
  subset(ocean == "pacific") %>% 
  select(-ocean, -species)
alba_pacific <- subset(tuna_data, species == "albacore") %>% 
  subset(ocean == "pacific") %>% 
  select(-ocean, -species)
sword_pacific <- subset(tuna_data, species == "swordfish") %>% 
  subset(ocean == "pacific") %>% 
  select(-ocean, -species)
skip_pacific <- subset(tuna_data, species == "skipjack tuna") %>% 
  subset(ocean == "pacific") %>% 
  select(-ocean, -species)
bigeye_pacific <- subset(tuna_data, species == "bigeye tuna") %>% 
  subset(ocean == "pacific") %>% 
  select(-ocean, -species)

###################################
# Yellowfin Tuna (Pacific-fitted) #
###################################

m01 <- gam(pa ~ s(SST) + Season2 + s(MLD) + s(Latitude, Longitude) + s(Bathymetry) + s(Dist2Coast) + s(Nitrate) + s(Chl), data = yft_pacific, family = "binomial")
summary(m01)

# Plotting response of all variables
fullplot_yftpac1 <- visreg(m01, "SST", partial = FALSE, ylab = " ", xlab = "SST", gg = TRUE) + theme_bw()
fullplot_yftpac2 <- visreg(m01, "Season2", partial = FALSE, ylab = " ", xlab = "Seasons", gg = TRUE) + theme_bw()
fullplot_yftpac3 <- visreg(m01, "MLD", partial = FALSE, ylab = " ", xlab = "MLD", gg = TRUE) + theme_bw()
fullplot_yftpac4 <- visreg(m01, "Latitude", partial = FALSE, ylab = " ", xlab = "Latitude", gg = TRUE) + theme_bw()
fullplot_yftpac5 <- visreg(m01, "Longitude", partial = FALSE, ylab = " ", xlab = "Longitude", gg = TRUE) + theme_bw()
fullplot_yftpac6 <- visreg(m01, "Bathymetry", partial = FALSE, ylab = " ", xlab = "Bathymetry", gg = TRUE) + theme_bw()
fullplot_yftpac7 <- visreg(m01, "Dist2Coast", partial = FALSE, ylab = " ", xlab = "Dist2Coast", gg = TRUE) + theme_bw() 
# Dist2Coast seems to make more sense that Bathymetry
fullplot_yftpac8 <- visreg(m01, "Nitrate", partial = FALSE, ylab = " ", xlab = "Nitrate", gg = TRUE) + theme_bw()
fullplot_yftpac9 <- visreg(m01, "Chl", partial = FALSE, ylab = " ", xlab = "Chl", gg = TRUE) + theme_bw()
# Some of the relationships look too wiggly, and might not make sense

YFTPAC_FullModel <- (fullplot_yftpac1 | fullplot_yftpac2 | fullplot_yftpac3) / (fullplot_yftpac4 | fullplot_yftpac5 | fullplot_yftpac6) / (fullplot_yftpac7 | fullplot_yftpac8 | fullplot_yftpac9) +
  plot_annotation(title = "Response of Variables for Full Model", subtitle = "Yellowfin Tuna (Pacific-fitted)", tag_levels = "i")
YFTPAC_FullModel
ggsave("outputs/05_Commercial/05a_GAMPlots/05a5_YFT/YFTPAC_FullModel.pdf", width = 20, height = 20, dpi = 320)

# First, let's see what we can drop using BIC
summary(m01) # Nitrate is not significant.
# Dist2Coast and MLD looks kind of linear.
# Deviance explained = 17.1%, R-squared (adjusted) = 0.129

# Remove Nitrate as it is not significant.
m02 <- update(m01, ~. -s(Nitrate))
BIC(m01, m02)
summary(m02) # BIC of m02 is lower (Nitrate is n.s.)

# Try dropping Bathymetry.
m03 <- update(m02, ~. -s(Bathymetry))
BIC(m03, m02) #BIC of m05 is lower (remove Bathymetry)
summary(m03)

# Best model
YFTPAC_BestModel <- m03

# Saving predictions
yft_pacific$Preds <- predict.gam(YFTPAC_BestModel, type = "response")
median(yft_pacific$Preds)
# Writing the data and predictions into a .csv
write_csv(yft_pacific, file = "inputs/mercer/yft_pacific.csv")

bestplot_yftpac1 <- visreg(YFTPAC_BestModel, "SST", partial = FALSE, ylab = "s(SST, 4.75)", xlab = "SST (°C)", gg = TRUE) + theme_bw() + ylim(-100, 50)
bestplot_yftpac2 <- visreg(YFTPAC_BestModel, "Season2", partial = FALSE, ylab = "f(Season)", xlab = "Season", gg = TRUE) + theme_bw() 
bestplot_yftpac3 <- visreg(YFTPAC_BestModel, "Chl", partial = FALSE, ylab = "s(Chl, 7.47)", xlab = "Chlorophyll A (mg/m^3)", gg = TRUE) + theme_bw()  
bestplot_yftpac4 <- visreg(YFTPAC_BestModel, "MLD", partial = FALSE, ylab = "s(MLD, 6.69)", xlab = "MLD (m)", gg = TRUE) + theme_bw()  
bestplot_yftpac5 <- visreg(YFTPAC_BestModel, "Dist2Coast", partial = FALSE, ylab = "s(Dist2Coast, 1.01)", xlab = "Dist2Coast (km)", gg = TRUE) + theme_bw()  

bestplot_yftpac <- (bestplot_yftpac1 | bestplot_yftpac2 | bestplot_yftpac3) / (bestplot_yftpac4 | bestplot_yftpac5) +
  plot_annotation(title = "Response of Variables for Best Model", subtitle = "Yellowfin Tuna (Pacific-fitted)", tag_levels = "i")
bestplot_yftpac
ggsave("outputs/05_Commercial/05a_GAMPlots/05a5_YFT/YFTPAC_BestModel.pdf", width = 20, height = 20, dpi = 320)

vis.gam(YFTPAC_BestModel, c("Latitude", "Longitude"), type = "response", ticktype = "detailed", xlab = "\nLatitude (°)", 
        ylab = "Longitude (°)", zlab = "Presence", color = "cm", theta = 30, phi = 30, r = 100)
dev.copy2pdf(file = "outputs/05_Commercial/05a_GAMPlots/05a5_YFT/YFTPAC_BestModelLatLong.pdf", paper = "A4r")

#######################################
# Plotting best model as a map
#######################################

Surface <- mba.surf(yft_pacific[, c("Longitude", "Latitude", "Preds")], 1000, 1000)

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
ggsave("outputs/05_Commercial/05a_GAMPlots/05a5_YFT/YFTPAC_map.png", p, dpi = 1200)
