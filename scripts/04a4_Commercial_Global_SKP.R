# All models (global-fitted) are from James Mercer's code.
# 04a creates the GAMs for each of the species.
# It saves the predictions (with the environmental variables and the coordinates) as a .csv file. (dir: input/mercer/)
# Saves the visreg plots and maps. (dir: outputs/04_Commercial/04a_GAMPlots/)
# There are 8 parts to 04a; the first 4 of which are global-fitted data:
# 1. 04a1: yellowfin (where the data is preprocessed & packages are defined)
# 2. 04a2: albacore
# 3. 04a3: swordfish
# 4. 04a4: skipjack
# The code must be run one after the other.

###################################
# Skipjack
###################################

model1 <- gam(PA ~ s(SST) + Season2 + s(MLD) + s(Latitude, Longitude) + 
            s(Bathymetry) + s(Dist2Coast) + s(Nitrate) + log10(Chl), 
          data = dat %>% filter(Species == "skipjack tuna"), family = "binomial")
summary(model1)

# Try removing Bathymetry (it's got the lowest p-level)
model2 <- update(model1, ~ . -s(Bathymetry))
BIC(model1, model2) # model2 has lower BIC (i.e. Bathymetry n.s.)
summary(model2)

# Try removing Chl- it's now got the lowest p-level
model3 <- update(model2, ~ . -log10(Chl))
BIC(model2, model3) # model3 has lower BIC (i.e. drop Chl)
summary(model3)

# Try removing MLD - it's now got the lowest p-level
model4 <- update(model3, ~ . -s(MLD))
BIC(model3, model4) # model4 has lower BIC (i.e. Remove MLD)
summary(model4)

# Try removing Nitrate - it's now got the lowest p-level
model5 <- update(model4, ~ . -s(Nitrate))
BIC(model4, model5) # model4 has lower BIC (i.e. Keep Nitrate)
summary(model4) # model4 is the best model

# Saving predictions
skp_preds <- as.numeric(predict.gam(model4, type = "response"))
median(skp_preds)
# Writing the data and predictions into a .csv
skp <- dat %>% filter(Species == 'skipjack tuna')
skp$Preds <- skp_preds
write_csv(skp, file = "inputs/mercer/skip.csv")

#######################################
# Plotting best model as a map
#######################################

MaxPA <- 0.20
plot1 <- PlotVisreg(model4, "SST", Ylab = " Probability occurrence", Xlab = "SST", MaxPA) +
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
plot1
ggsave('outputs/04_Commercial/04a_GAMPlots/04a4_SKP/bestmodel_SST.png', plot1, width = 10, height = 10, dpi = 600)


plot2 <- PlotVisreg(model4, "Season2", Ylab = " ", Xlab = "Season", MaxPA) +
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
plot2
ggsave('outputs/04_Commercial/04a_GAMPlots/04a4_SKP/bestmodel_season.png', plot2, width = 10, height = 10, dpi = 600)


plot3 <- PlotVisreg(model4, "Dist2Coast", Ylab = " ", Xlab = "Distance to coast", MaxPA) +
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
plot3
ggsave('outputs/04_Commercial/04a_GAMPlots/04a4_SKP/bestmodel_dist2coast.png', plot3, width = 10, height = 10, dpi = 600)


plot4 <- PlotVisreg(model4, "Nitrate", Ylab = " ", Xlab = "Nitrate", MaxPA) +
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
plot4
ggsave('outputs/04_Commercial/04a_GAMPlots/04a4_SKP/bestmodel_nitrate.png', plot4, width = 10, height = 10, dpi = 600)


plot5 <- visreg2d(model4, yvar = "Latitude", xvar = "Longitude", scale = "response", plot.type = "gg")+
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.key.width = unit(1,"cm"),
        legend.text = element_text(size = 20),
        legend.title = element_blank()) 
plot5
ggsave('outputs/04_Commercial/04a_GAMPlots/04a4_SKP/bestmodel_longlat.png', plot5, width = 10, height = 10, dpi = 600)


# Combine plots for albacore
(plot1 | plot2 | plot3) / (plot4 | plot_spacer() | plot5) +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') & theme_bw(base_size = 18)
ggsave("outputs/04_Commercial/04a_GAMPlots/04a4_SKP/SKP_BestModel.pdf", width = 15, height = 10, dpi = 600)

# Plot the map
x11(width = 14, height = 7)

df <- fOrganizedf('skipjack tuna', skp_preds)
p <- PlotMap(df, "Preds")
p
ggsave("outputs/04_Commercial/04a_GAMPlots/04a4_SKP/SKP_map.png", p, dpi = 1200)

p <- PlotMap(df, "Preds2") +
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.key.width = unit(1,"cm"),
        legend.text = element_text(size = 15),
        legend.title = element_blank()) 
p
ggsave("outputs/04_Commercial/04a_GAMPlots/04a4_SKP/SKP_map_presence.png", p, dpi = 600)

rm(model1, model2, model3, model4, model5, skp_preds, skp, plot1, plot2, plot3, plot4, plot5, df, p)
