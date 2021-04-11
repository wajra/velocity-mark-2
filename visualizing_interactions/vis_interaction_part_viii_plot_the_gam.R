# Visualizing Interactions - Part VIII
# Here we will plot the GAM
# Author - Jeewantha Bandara (mailto:jeewantha.bandara@rutgers.edu) 
# Research Group - Pinsky Lab, Rutgers University (https://pinsky.marine.rutgers.edu)
# Following Advanced R style guide - http://adv-r.had.co.nz/Style.html

library(mgcv)
library(tidyverse)

# Read the GAM for presence/absence
mi_model_gam_1 <- readRDS(file="data/mi_model_gam_1.rds")
# Read the GAM for biomass/abundance
mi_model_gam_2 <- readRDS(file="data/mi_model_gam_2.rds")

summary(mi_model_gam_2)

png(filename="roms_figures_output_ver_1/mi_model_gam_smooths_ver_2.png", width=8, height=7, units="in", res=300)
# Setting up a plot of 3x3 grids
par(mfrow=c(4,3))
# First SBT.seasonal
plot(mi_model_gam_2, select=1, xlab="Seasonal Bottom Temp.(C)",
     ylab=paste("s(Seasonal Bottom Temp,",
                round(summary(mi_model_gam_2)$edf[1],3),")"),
     shade=T)
# Then Dissolved Oxygen
plot(mi_model_gam_2, select=2, xlab="Dissolved Oxygen (mol/kg)",
     ylab=paste("s(Dissolved Oxygen,",
                round(summary(mi_model_gam_2)$edf[2],3),")"),
     shade=T)
# Then metabolic index
plot(mi_model_gam_2, select=3, xlab="MI",
     ylab=paste("s(MI,",
                round(summary(mi_model_gam_2)$edf[3],3),")"),
     shade=T)
# Then zooplankton class mean sum
plot(mi_model_gam_2, select=4, xlab="Seasonal Zooplankton Density (mg/kg)",
     ylab=paste("s(Seas. Zooplank. Dens.),",
                round(summary(mi_model_gam_2)$edf[4],3),")"),
     shade=T)
# Then Seasonal Surface Temp
plot(mi_model_gam_2, select=5, xlab="Seasonal Surface Temp. (C)",
     ylab=paste("s(Seasonal Surface Temp,",
                round(summary(mi_model_gam_2)$edf[5],3),")"),
     shade=T)
# Then Sea bottom temp minimum
plot(mi_model_gam_2, select=6, xlab="Minimum Sea Bottom Temp. (C)",
     ylab=paste("s(Minimum Sea Bottom Temp,",
                round(summary(mi_model_gam_2)$edf[6],3),")"),
     shade=T)
# Then Sea bottom temp maximum
plot(mi_model_gam_2, select=7, xlab="Maximum Sea Bottom Temp. (C)",
     ylab=paste("s(Maximum Sea Bottom Temp,",
                round(summary(mi_model_gam_2)$edf[7],3),")"),
     shade=T)
# Then Sea surface temp maximum
plot(mi_model_gam_2, select=8, xlab="Maximum Sea Surface Temp (C)",
     ylab=paste("s(Maximum Sea Surface Temp,",
                round(summary(mi_model_gam_2)$edf[8],3),")"),
     shade=T)
# Then Rugosity
plot(mi_model_gam_2, select=9, xlab="Rugosity",
     ylab=paste("s(Rugosity,",
                round(summary(mi_model_gam_2)$edf[9],3),")"),
     shade=T)
# Then Grainsize
plot(mi_model_gam_2, select=10, xlab="Grainsize",
     ylab=paste("s(Grainsize,",
                round(summary(mi_model_gam_2)$edf[10],3),")"),
     shade=T)

dev.off()



#################### ONLY WITH PARTIAL EFFECT AS THE Y AXIS ##################

png(filename="roms_figures_output_ver_1/mi_model_gam_smooths_ver_11.png",
    width=6, height=8, units="in", res=300)
# Setting up a plot of 3x3 grids
par(mfrow=c(4,3))
# First SBT.seasonal
par(mai=c(0.62,0.55,0.1,0.1))
plot(mi_model_gam_2, select=1, xlab="Seasonal Bottom Temp",
     ylab="Partial effect",
     shade=T, cex.lab=1.2, cex.axis=1.1)
# Then Dissolved Oxygen
plot(mi_model_gam_2, select=2, xlab="Dissolved Oxygen",
     ylab="",shade=T, cex.lab=1.2)
# Then metabolic index
plot(mi_model_gam_2, select=3, xlab="MI",
     ylab="",shade=T, cex.lab=1.2)
# Then zooplankton class mean sum
plot(mi_model_gam_2, select=4, xlab="Seasonal Zooplankton Density",
     ylab="Partial effect",
     shade=T, cex.lab=1.2, cex.axis=1.1)
# Then Seasonal Surface Temp
plot(mi_model_gam_2, select=5, xlab="Seasonal Surface Temp.",
     ylab="",
     shade=T, cex.lab=1.2, cex.axis=1.1)
# Then Sea bottom temp minimum
plot(mi_model_gam_2, select=6, xlab="Minimum Sea Bottom Temp.",
     ylab="",
     shade=T, cex.lab=1.2, cex.axis=1.1)
# Then Sea bottom temp maximum
plot(mi_model_gam_2, select=7, xlab="Maximum Sea Bottom Temp.",
     ylab="Partial effect",
     shade=T, cex.lab=1.2, cex.axis=1.1)
# Then Sea surface temp maximum
plot(mi_model_gam_2, select=8, xlab="Maximum Sea Surface Temp.",
     ylab="",
     shade=T, cex.lab=1.2, cex.axis=1.1)
# Then Rugosity
plot(mi_model_gam_2, select=9, xlab="Rugosity",
     ylab="",
     shade=T, cex.lab=1.2, cex.axis=1.1)
# Then Grainsize
plot(mi_model_gam_2, select=10, xlab="Grainsize",
     ylab="Partial effect",
     shade=T, cex.lab=1.2, cex.axis=1.1)

dev.off()


###################### Plotting only the six most significant curves #######

png(filename="roms_figures_output_ver_1/mi_model_gam_smooths_ver_12.png",
    width=6, height=4, units="in", res=300)
# Setting up a plot of 3x3 grids
par(mfrow=c(2,3))
# First SBT.seasonal
par(mai=c(0.62,0.55,0.1,0.1))
plot(mi_model_gam_2, select=1, xlab="Seasonal Bottom Temp",
     ylab="Partial effect",
     shade=T, cex.lab=1.2, cex.axis=1.1)
# Then Dissolved Oxygen
plot(mi_model_gam_2, select=2, xlab="Dissolved Oxygen",
     ylab="",shade=T, cex.lab=1.2)
# Then metabolic index
plot(mi_model_gam_2, select=3, xlab="MI",
     ylab="",shade=T, cex.lab=1.2)
plot(mi_model_gam_2, select=8, xlab="Maximum Sea Surface Temp.",
     ylab="",
     shade=T, cex.lab=1.2, cex.axis=1.1)
plot(mi_model_gam_2, select=5, xlab="Seasonal Surface Temp.",
     ylab="",
     shade=T, cex.lab=1.2, cex.axis=1.1)
# Then Grainsize
plot(mi_model_gam_2, select=10, xlab="Grainsize",
     ylab="Partial effect",
     shade=T, cex.lab=1.2, cex.axis=1.1)

dev.off()


######################################################################

png(filename="roms_figures_output_ver_1/mi_model_gam_pres_abs_ver_1.png",
    width=6, height=8, units="in", res=300)
# Setting up a plot of 3x3 grids
par(mfrow=c(4,3))
# First SBT.seasonal
par(mai=c(0.62,0.55,0.1,0.1))
plot(mi_model_gam_1, select=1, xlab="Seasonal Bottom Temp",
     ylab="Partial effect",
     shade=T, cex.lab=1.2, cex.axis=1.1)
# Then Dissolved Oxygen
plot(mi_model_gam_1, select=2, xlab="Dissolved Oxygen",
     ylab="",shade=T, cex.lab=1.2)
# Then metabolic index
plot(mi_model_gam_1, select=3, xlab="MI",
     ylab="",shade=T, cex.lab=1.2)
# Then zooplankton class mean sum
plot(mi_model_gam_1, select=4, xlab="Seasonal Zooplankton Density",
     ylab="Partial effect",
     shade=T, cex.lab=1.2, cex.axis=1.1)
# Then Seasonal Surface Temp
plot(mi_model_gam_1, select=5, xlab="Seasonal Surface Temp.",
     ylab="",
     shade=T, cex.lab=1.2, cex.axis=1.1)
# Then Sea bottom temp minimum
plot(mi_model_gam_1, select=6, xlab="Minimum Sea Bottom Temp.",
     ylab="",
     shade=T, cex.lab=1.2, cex.axis=1.1)
# Then Sea bottom temp maximum
plot(mi_model_gam_1, select=7, xlab="Maximum Sea Bottom Temp.",
     ylab="Partial effect",
     shade=T, cex.lab=1.2, cex.axis=1.1)
# Then Sea surface temp maximum
plot(mi_model_gam_1, select=8, xlab="Maximum Sea Surface Temp.",
     ylab="",
     shade=T, cex.lab=1.2, cex.axis=1.1)
# Then Rugosity
plot(mi_model_gam_1, select=9, xlab="Rugosity",
     ylab="",
     shade=T, cex.lab=1.2, cex.axis=1.1)
# Then Grainsize
plot(mi_model_gam_1, select=10, xlab="Grainsize",
     ylab="Partial effect",
     shade=T, cex.lab=1.2, cex.axis=1.1)

dev.off()


