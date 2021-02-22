# Visualizing Interactions - Part IV
# Here we will plot the abundance and presence data from within sample hindcasting
# Author - Jeewantha Bandara (mailto:jeewantha.bandara@rutgers.edu) 
# Research Group - Pinsky Lab, Rutgers University (https://pinsky.marine.rutgers.edu)
# Following Advanced R style guide - http://adv-r.had.co.nz/Style.html

library("ggplot2")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library(tidyverse)
library(ggspatial)

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


ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97), expand = FALSE) +
  geom_tile(data=sum_95, aes(lon, lat, width=0.25, fill=x))



hindcast <- read.csv(file="roms_model_output_ver_1/classic_model_full_hindcast.csv")
# Worst performing model - roms_model_output_ver_1/full_dataset_hindcast.csv
# roms_model_output_ver_1/classic_model_full_hindcast.csv

hindcast_1980 <- hindcast[(hindcast$year>=1980 & hindcast$year<1990),]
hindcast_2000 <- hindcast[(hindcast$year>=2000 & hindcast$year<2010),]


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
logwtcpue_max <- max(hindcast_2000$logwtcpue, na.rm=TRUE)
log_preds_max <- max(log(hindcast_2000$preds), na.rm=TRUE)
logwtcpue_min <- min(hindcast_2000$logwtcpue, na.rm=TRUE)
log_preds_min <- min(log(hindcast_2000$preds), na.rm=TRUE)

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
lat_max <- max(hindcast_2000$lat, na.rm=TRUE) + 1
lat_min <- min(hindcast_2000$lat, na.rm=TRUE) - 1
lon_max <- max(hindcast_2000$lon, na.rm=TRUE) + 1
lon_min <- min(hindcast_2000$lon, na.rm=TRUE) - 1

hindcast_1980$lat_grid025 <- floor(hindcast_1980$lat*4)/4 + 0.125
hindcast_1980$lon_grid025 <- floor(hindcast_1980$lon*4)/4 + 0.125

hindcast_2000$lat_grid025 <- floor(hindcast_2000$lat*4)/4 + 0.125
hindcast_2000$lon_grid025 <- floor(hindcast_2000$lon*4)/4 + 0.125

sum_1980 <- aggregate(hindcast_1980$wtcpue, 
                 by = list(lat = hindcast_1980$lat_grid025,
                           lon = hindcast_1980$lon_grid025),
                 FUN = mean, na.rm= TRUE)

sum_2000 <- aggregate(hindcast_2000$wtcpue, 
                           by = list(lat = hindcast_2000$lat_grid025,
                                     lon = hindcast_2000$lon_grid025),
                           FUN = mean, na.rm= TRUE)

plot_1 <- ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max), expand = FALSE) +
  labs(fill="Biomass (kg/tow)") + ylab("Latitude") + xlab("") + 
  geom_tile(data=sum_1980, aes(lon, lat, width = 0.25, fill = x)) + 
  scale_fill_gradientn(colors=c("#ffffcc","#a1dab4","#41b6c4", "#225ea8")) +  
  annotate(geom = "text", x = lon_min + 4, y = lat_max - 3, label = "1980-1990", 
            size = 3)

# Previously used 'scale_fill_gradient2()' here for coloring the geom_tile

plot_2 <- ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max), expand = FALSE) +
  geom_tile(data=sum_2000, aes(lon, lat, width=0.25, fill=x)) + 
  scale_fill_gradientn(colors=c("#ffffcc","#a1dab4","#41b6c4", "#225ea8")) + 
  labs(fill="Biomass (kg/tow)") + xlab("Longitude") + ylab("Latitude") + 
  annotation_north_arrow(location = 'br', which_north = 'true', 
                         pad_x = unit(0.1, 'in'), pad_y = unit(0.3, 'in'), 
                         style = north_arrow_fancy_orienteering,
                         height=unit(1, "cm"),
                         width=unit(1, "cm")) +
  annotate(geom = "text", x = lon_min + 4, y = lat_max - 3 , label = "2000-2010", 
            size = 3)

# scale_fill_gradient(low = "#56B1F7", high = "#132B43"s)

png("roms_figures_output_ver_1/bsb_dist_1990_to_2010_ver_3.png", width=6, height=6, units="in", res=300)
gridExtra::grid.arrange(plot_1, plot_2, nrow=2)
dev.off()
