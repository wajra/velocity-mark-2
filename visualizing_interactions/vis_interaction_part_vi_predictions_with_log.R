# Visualizing Interactions - Part IV
# Here we will plot the abundance and presence data from within sample hindcasting
# Same to iv. But with log units
# Author - Jeewantha Bandara (mailto:jeewantha.bandara@rutgers.edu) 
# Research Group - Pinsky Lab, Rutgers University (https://pinsky.marine.rutgers.edu)
# Following Advanced R style guide - http://adv-r.had.co.nz/Style.html

library("ggplot2")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library(tidyverse)
library(ggspatial)
library(scales)
# Things to be done
# 1. Read the predictions from a well performing model
# 2. Plot the actual abundance data vs. predicted abundance
# 3. Set the minima and maxima for xlim and ylim using the dataset itself
# 4. Insert the titles, north arrow and scales

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

theme_set(theme_bw())

ggplot(data = world) +
  geom_sf()



hindcast <- read.csv(file="roms_model_output_ver_1/classic_model_full_hindcast.csv")
# Worst performing model - roms_model_output_ver_1/full_dataset_hindcast.csv
# roms_model_output_ver_1/classic_model_full_hindcast.csv

# 2021-02-28 -> Originally from 1980 to 1990
hindcast_1985 <- hindcast[(hindcast$year>=1980 & hindcast$year<1990),]


"""
ggplot(sum, aes(lon, lat, width = 0.25, fill = x)) + geom_tile()

ggplot(hindcast_1985, aes(longrid, latgrid, width = 0.25, fill = logwtcpue)) + geom_tile()

ggplot(hindcast_1985, aes(longrid, latgrid, width = 0.25, fill = preds)) + geom_tile()

"""
# Here I need to basically write a couple of 'if' statements for the max and min
# values
plot_min <- NA
plot_max <- NA

# Get the max and min values for logwtcpue and log(preds)
logwtcpue_max <- max(hindcast_1985$logwtcpue, na.rm=TRUE)
log_preds_max <- max(log(hindcast_1985$preds), na.rm=TRUE)
logwtcpue_min <- min(hindcast_1985$logwtcpue, na.rm=TRUE)
log_preds_min <- min(log(hindcast_1985$preds), na.rm=TRUE)

# make plot_min == log_preds_min
# Then compare using 'if'
plot_min = log_preds_min
if (logwtcpue_min < plot_min){
  plot_min=logwtcpue_min
}
# Then max
plot_max = log_preds_max
if (logwtcpue_min < plot_min){
  plot_max=logwtcpue_max
}

# ggplot(hindcast_1985, aes(longrid, latgrid, width = 0.25, fill = logwtcpue)) + 
#  geom_tile() + scale_fill_gradient(limits = range(plot_min, plot_max))



# Plotting on the map itself
# Here we need to get the minimum and maximum for latitude and longitude
lat_max <- max(hindcast_1985$lat, na.rm=TRUE) + 1
lat_min <- min(hindcast_1985$lat, na.rm=TRUE) - 1
lon_max <- max(hindcast_1985$lon, na.rm=TRUE) + 1
lon_min <- min(hindcast_1985$lon, na.rm=TRUE) - 1

hindcast_1985$lat_grid025 <- floor(hindcast_1985$lat*4)/4 + 0.125
hindcast_1985$lon_grid025 <- floor(hindcast_1985$lon*4)/4 + 0.125

sum <- aggregate(log(hindcast_1985$preds+1), 
                 by = list(lat = hindcast_1985$lat_grid025,
                           lon = hindcast_1985$lon_grid025),
                 FUN = mean, na.rm= TRUE)

sum_abundance <- aggregate(log(hindcast_1985$wtcpue+1), 
                           by = list(lat = hindcast_1985$lat_grid025,
                                     lon = hindcast_1985$lon_grid025),
                           FUN = mean, na.rm= TRUE)

plot_1 <- ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max), expand = FALSE) +
  labs(fill="log(Biomass+1)") + ylab("Latitude") + xlab("") + 
  geom_tile(data=sum_abundance, aes(lon, lat, width = 0.25, fill = x)) +
  scale_fill_gradient2()

# Note to self - 2021-02-17
# We kept scale_fill_gradient2() just as it is before
# Now we are trying to actually implement actual colors
test_plot <- ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max), expand = FALSE) +
  labs(fill="log(Biomass+1)") + ylab("Latitude") + xlab("") + 
  geom_tile(data=sum_abundance, aes(lon, lat, width = 0.25, fill = x)) +
#  scale_fill_gradient2(low = "#e7e1ef",
#                       mid = "#c994c7",
#                       high = "#dd1c77",
#                       midpoint = 0.5)
  scale_fill_gradientn(colors=c("#ffffcc","#a1dab4","#41b6c4", "#225ea8"),
                       values = scales::rescale(c(0,.05,0.15, max(sum_abundance$x))))

# https://stackoverflow.com/questions/41985921/specify-manual-values-for-scale-gradientn-with-transformed-color-fill-variable

plot_2 <- ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max), expand = FALSE) +
  geom_tile(data=sum, aes(lon, lat, width=0.25, fill=x)) + 
  # scale_fill_gradientn(colors=c("#ffffd9", "#edf8b1","#c7e9b4", "#7fcdbb",
  #                              "#41b6c4", "#1d91c0","#225ea8", "#0c2c84")) + 
  scale_fill_gradientn(colors=c("#ffffcc","#a1dab4","#41b6c4", "#225ea8"),
                       values = scales::rescale(c(0,.05,0.15, max(sum$x)))) + 
  labs(fill="log(Biomass+1)") + xlab("Longitude") + ylab("Latitude") + 
  annotation_north_arrow(location = 'br', which_north = 'true', 
                         pad_x = unit(0.1, 'in'), pad_y = unit(0.3, 'in'), 
                         style = north_arrow_fancy_orienteering,
                         height=unit(1, "cm"),
                         width=unit(1, "cm"))

# low = "blue", mid = "green", high = "red"
png("roms_figures_output_ver_1/classic_model_predictions_log_ver_4.png", width=6, height=6, units="in", res=300)
gridExtra::grid.arrange(test_plot, plot_2, nrow=2)
dev.off()
