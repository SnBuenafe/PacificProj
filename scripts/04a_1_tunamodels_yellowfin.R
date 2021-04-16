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

# calling data

tuna_data <- read.csv("inputs/mercer/TunaData_final.txt", sep="")
tuna_data <- na.omit(tuna_data)

# Very high chlorophyll max

#Very high chlorophyll max
hist(tuna_data$Chl)
# tuna_data <- tuna_data %>% mutate(Chl = replace(Chl, Chl > 5, 5)) # Set max Chl to 5
# the data was already pre-processed to set max Chl to 5

# Reorder Seasons so they are in order
tuna_data <- tuna_data %>% mutate(Season2 = replace(Season2, Season2 == "summer", 2)) %>% 
  mutate(Season2 = replace(Season2, Season2 == "winter", 4)) %>% 
  mutate(Season2 = replace(Season2, Season2 == "autumn", 3)) %>% 
  mutate(Season2 = replace(Season2, Season2 == "spring", 1))

#subset based on species
yft <- subset(tuna_data, species == "yellowfin tuna")
alba <- subset(tuna_data, species == "albacore")
sword <- subset(tuna_data, species == "swordfish")
skip <- subset(tuna_data, species == "skipjack tuna")
bigeye <- subset(tuna_data, species == "bigeye tuna") # there's also bigeye?!

###################################################################
# Pre-processing before GAMs to reduce wide SEs (max of the data)
##################################################################

yft$SST[yft$SST<10] <- 10 # Few data <10oC, so make them 10oC
yft$Chl[yft$Chl>2] <- 2 # Few data >2, so make them 2

alba$SST[alba$SST<10] <- 10 # Few data <10oC, so make them 10oC
alba$Chl[alba$Chl>2] <- 2 # Few data >2, so make them 2

sword$SST[sword$SST<10] <- 10 # Few data <10oC, so make them 10oC
sword$Chl[sword$Chl>2] <- 2 # Few data >2, so make them 2
sword$Nitrate[sword$Nitrate>10] <- 10 # Few data >2, so make them 2

skip$SST[skip$SST<10] <- 10 # Few data <10oC, so make them 10oC
skip$Chl[skip$Chl>2] <- 2 # Few data >2, so make them 2

bigeye$SST[bigeye$SST<10] <- 10 # Few data <10oC, so make them 10oC
bigeye$Chl[bigeye$Chl>2] <- 2 # Few data >2, so make them 2

# create subsets of the Pacific data
yft_pacific <- subset(yft, ocean == "pacific") %>% 
  select(-ocean, -species)
alba_pacific <- subset(alba, ocean == "pacific") %>% 
  select(-ocean, -species)
sword_pacific <- subset(sword, ocean == "pacific") %>% 
  select(-ocean, -species)
skip_pacific <- subset(skip, ocean == "pacific") %>% 
  select(-ocean, -species)

###################################
# Doing GAMs for each subset
###################################

###################################
# Yellowfin Tuna
###################################

#James edit: Change MLD to a linear effect
m1 <- gam(pa ~ s(SST) + Season2 + MLD + s(Latitude, Longitude) + s(Bathymetry) + s(Dist2Coast) + s(Nitrate) + s(Chl), data = yft, family = "binomial")
summary(m1)

# Plotting response of all variables
fullplot_yft1 <- visreg(m1, "SST", partial = FALSE, ylab = " ", xlab = "SST", gg = TRUE) + theme_bw()
fullplot_yft2 <- visreg(m1, "Season2", partial = FALSE, ylab = " ", xlab = "Seasons", gg = TRUE) + theme_bw()
fullplot_yft3 <- visreg(m1, "MLD", partial = FALSE, ylab = " ", xlab = "MLD", gg = TRUE) + theme_bw()
fullplot_yft4 <- visreg(m1, "Latitude", partial = FALSE, ylab = " ", xlab = "Latitude", gg = TRUE) + theme_bw()
fullplot_yft5 <- visreg(m1, "Longitude", partial = FALSE, ylab = " ", xlab = "Longitude", gg = TRUE) + theme_bw()
fullplot_yft6 <- visreg(m1, "Bathymetry", partial = FALSE, ylab = " ", xlab = "Bathymetry", gg = TRUE) + theme_bw()
fullplot_yft7 <- visreg(m1, "Dist2Coast", partial = FALSE, ylab = " ", xlab = "Dist2Coast", gg = TRUE) + theme_bw() 
# Dist2Coast seems to make more sense that Bathymetry
fullplot_yft8 <- visreg(m1, "Nitrate", partial = FALSE, ylab = " ", xlab = "Nitrate", gg = TRUE) + theme_bw()
fullplot_yft9 <- visreg(m1, "Chl", partial = FALSE, ylab = " ", xlab = "Chl", gg = TRUE) + theme_bw()
# Some of the relationships look too wiggly, and might not make sense

YFT_FullModel <- (fullplot_yft1 | fullplot_yft2 | fullplot_yft3) / (fullplot_yft4 | fullplot_yft5 | fullplot_yft6) / (fullplot_yft7 | fullplot_yft8 | fullplot_yft9) +
  plot_annotation(title = "Response of Variables for Full Model", subtitle = "Yellowfin Tuna", tag_levels = "i")
YFT_FullModel
ggsave("outputs/commercial/GAM_plots/YFT/YFT_FullModel.pdf", width = 20, height = 20, dpi = 320)

# First, let's see what we can drop using BIC
summary(m1) # Bathymetry is not significant.
# Deviance explained = 14.4%, R-squared (adjusted) = 0.101

# Removing Bathymetry
m2 <- update(m1, ~ . -s(Bathymetry))
BIC(m1, m2) # m2 has lower BIC (i.e. Bathymetry n.s.)

# Removing Nitrate
m3 <- update(m2, ~ . -s(Nitrate))
BIC(m2, m3) # m3 has lower BIC (i.e. Nitrate n.s.)

# Removing Chl
m4 <- update(m3, ~ . -s(Chl))
BIC(m3, m4) # m3 has lower BIC (i.e. Chl should be retained)

# Let's use Chl with fewer dfs though, so not so wiggly
m5 <- gam(pa ~ s(SST) + Season2 + MLD + s(Latitude, Longitude) + s(Dist2Coast) + s(Chl, k = 4), data = yft, family = "binomial")
# NOTE: m5 will have higher BIC (less wiggly), but probably more realistic
summary(m5) # 13.5% deviance explained. R-sq(adj) = 0.0938

# Best Model
YFT_BestModel <- m5

# Saving predictions
yft$Preds <- predict.gam(YFT_BestModel, type = "response")
median(yft$Preds)
# Writing the data and predictions into a .csv
write_csv(yft, file = "inputs/mercer/yft.csv")

bestplot_yft1 <- visreg(YFT_BestModel, "SST", partial = FALSE, ylab = "s(SST, 2.88)", xlab = "SST (°C)", gg = TRUE) + theme_bw() 
bestplot_yft2 <- visreg(YFT_BestModel, "Season2", partial = FALSE, ylab = "f(Season)", xlab = "Season", gg = TRUE) + theme_bw() 
bestplot_yft3 <- visreg(YFT_BestModel, "MLD", partial = FALSE, ylab = "f(MLD, -0.02)", xlab = "MLD (m)", gg = TRUE) + theme_bw() 
bestplot_yft4 <- visreg(YFT_BestModel, "Dist2Coast", partial = FALSE, ylab = "s(DistCoast, 3.58)", xlab = "Distance to Coast (km)", gg = TRUE) + theme_bw() 
bestplot_yft5 <- visreg(YFT_BestModel, "Chl", partial = FALSE, ylab = "s(Chl, 2.14)", xlab = "Chlorophyll A (mg/m^3)", gg = TRUE) + theme_bw()  

bestplot_yft <- (bestplot_yft1 | bestplot_yft2 | bestplot_yft3) / (bestplot_yft4 | bestplot_yft5) + 
  plot_annotation(title = "Response of Variables for Best Model", subtitle = "Yellowfin Tuna", tag_levels = "i")
bestplot_yft
ggsave("outputs/commercial/GAM_plots/YFT/YFT_BestModel.pdf", width = 20, height = 20, dpi = 320)

vis.gam(YFT_BestModel, c("Latitude", "Longitude"), type = "response", ticktype = "detailed", xlab = "\nLatitude (°)", 
        ylab = "Longitude (°)", zlab = "Presence", color = "cm", theta = 30, phi = 30, r = 100)
dev.copy2pdf(file = "outputs/commercial/GAM_plots/YFT/YFT_BestModelLatLong.pdf", paper = "A4r")

#######################################
# Plotting best model as a map
#######################################

Surface <- mba.surf(yft[, c("Longitude", "Latitude", "Preds")], 1000, 1000)

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
ggsave("outputs/commercial/GAM_plots/YFT/YFT_map.png", p, dpi = 1200)

###### delete after this

###################################
# Yellowfin Tuna (Pacific-fitted)
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
ggsave("outputs/commercial/GAM_plots/YFT/YFTPAC_FullModel.pdf", width = 20, height = 20, dpi = 320)

# First, let's see what we can drop using BIC
summary(m01) # Nitrate is not significant.
# Dist2Coast and MLD looks kind of linear.
# Deviance explained = 17.1%, R-squared (adjusted) = 0.129

# Remove Nitrate as it is not significant.
m02 <- update(m01, ~. -s(Nitrate))
BIC(m01, m02)
summary(m02) # BIC of m02 is lower (Nitrate is n.s.)

# Try to make Dist2Coast linear.
m03 <- update(m02, ~. -s(Dist2Coast) +Dist2Coast)
BIC(m02, m03) # BIC of m03 is lower (retain linearity)

# Try to make MLD linear?
m04 <- update(m03, ~. -s(MLD) +MLD)
BIC(m04, m03) # BIC of m04 is lower (retain linearity)
summary(m04)

# Try dropping Bathymetry.
m05 <- update(m04, ~. -s(Bathymetry))
BIC(m05, m04) #BIC of m05 is lower (remove Bathymetry)
summary(m05)

# Best model
YFTPAC_BestModel <- m05

# Saving predictions
yft_pacific$Preds <- predict.gam(YFTPAC_BestModel, type = "response")
median(yft_pacific$Preds)
# Writing the data and predictions into a .csv
write_csv(yft_pacific, file = "inputs/mercer/yft_pacific.csv")

bestplot_yftpac1 <- visreg(YFTPAC_BestModel, "SST", partial = FALSE, ylab = "s(SST, 4.71)", xlab = "SST (°C)", gg = TRUE) + theme_bw() + ylim(-100, 50)
bestplot_yftpac2 <- visreg(YFTPAC_BestModel, "Season2", partial = FALSE, ylab = "f(Season)", xlab = "Season", gg = TRUE) + theme_bw() 
bestplot_yftpac3 <- visreg(YFTPAC_BestModel, "Chl", partial = FALSE, ylab = "s(Chl, 8.70)", xlab = "Chlorophyll A (mg/m^3)", gg = TRUE) + theme_bw()  
bestplot_yftpac4 <- visreg(YFTPAC_BestModel, "MLD", partial = FALSE, ylab = "f(MLD, -0.02)", xlab = "MLD (m)", gg = TRUE) + theme_bw()  
bestplot_yftpac5 <- visreg(YFTPAC_BestModel, "Dist2Coast", partial = FALSE, ylab = "f(Chl, -1.03x10^-3)", xlab = "Dist2Coast (km)", gg = TRUE) + theme_bw()  

bestplot_yftpac <- (bestplot_yftpac1 | bestplot_yftpac2 | bestplot_yftpac3) / (bestplot_yftpac4 | bestplot_yftpac5) +
  plot_annotation(title = "Response of Variables for Best Model", subtitle = "Yellowfin Tuna (Pacific-fitted)", tag_levels = "i")
bestplot_yftpac
ggsave("outputs/commercial/GAM_plots/YFT/YFTPAC_BestModel.pdf", width = 20, height = 20, dpi = 320)

vis.gam(YFTPAC_BestModel, c("Latitude", "Longitude"), type = "response", ticktype = "detailed", xlab = "\nLatitude (°)", 
        ylab = "Longitude (°)", zlab = "Presence", color = "cm", theta = 30, phi = 30, r = 100)
dev.copy2pdf(file = "outputs/commercial/GAM_plots/YFT/YFTPAC_BestModelLatLong.pdf", paper = "A4r")

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
ggsave("outputs/commercial/GAM_plots/YFT/YFTPAC_map.png", p, dpi = 1200)
