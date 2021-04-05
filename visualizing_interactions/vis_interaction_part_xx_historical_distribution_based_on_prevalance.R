# Visualizing Interactions - Part XX
# Here we will plot black sea bass presence/absence historically in terms of 
# prevalance
# Author - Jeewantha Bandara (mailto:jeewantha.bandara@rutgers.edu) 
# Research Group - Pinsky Lab, Rutgers University (https://pinsky.marine.rutgers.edu)
# Following Advanced R style guide - http://adv-r.had.co.nz/Style.html


# Visualizing Interactions - Part XVIIII
# Plot abundance and presence in the form of prevalance
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




hindcast <- read.csv(file="roms_model_output_ver_1/classic_model_full_hindcast.csv")
# Worst performing model - roms_model_output_ver_1/full_dataset_hindcast.csv
# roms_model_output_ver_1/classic_model_full_hindcast.csv


hindcast$lat_grid025 <- floor(hindcast$lat*4)/4 + 0.125
hindcast$lon_grid025 <- floor(hindcast$lon*4)/4 + 0.125

hindcast$latlon <- paste(hindcast$lon_grid025,"-", hindcast$lat_grid025)

# 2021-02-28 - 
hindcast_1980 <- hindcast[(hindcast$year>=1980 & hindcast$year<1990),]
hindcast_2000 <- hindcast[(hindcast$year>=2000 & hindcast$year<2010),]


# Group and count
group_1980 <- hindcast_1980 %>% dplyr::count(lon_grid025, lat_grid025)
group_2000 <- hindcast_2000 %>% dplyr::count(lon_grid025, lat_grid025)

# Now take just the black sea bass observations
bsb_observations <- hindcast %>% dplyr::filter(sppocean=="centropristis striata_Atl")
bsb_observations_1980 <- bsb_observations[(bsb_observations$year>=1980 & bsb_observations$year<1990),]
bsb_observations_2000 <- bsb_observations[(bsb_observations$year>=2000 & bsb_observations$year<2010),]

# Now group the black sea bass observations by their latlon
bsb_obs_group_1980 <- bsb_observations_1980 %>% dplyr::count(lon_grid025, lat_grid025)
bsb_obs_group_2000 <- bsb_observations_2000 %>% dplyr::count(lon_grid025, lat_grid025)

# Create a new column using paste combining lat and lon
group_1980$latlon <- paste(group_1980$lon_grid025,"-", group_1980$lat_grid025)
group_2000$latlon <- paste(group_2000$lon_grid025,"-", group_2000$lat_grid025)
bsb_obs_group_1980$latlon <- paste(bsb_obs_group_1980$lon_grid025, "-", bsb_obs_group_1980$lat_grid025)
bsb_obs_group_2000$latlon <- paste(bsb_obs_group_2000$lon_grid025, "-", bsb_obs_group_2000$lat_grid025)



# Now we will rename some of the columns in all four dataframes of interest
# 1. group_1980
# 2. group_2000
# 3. bsb_obs_group_1980
# 4. bsb_obs_group_2000

group_1980 <- group_1980 %>% dplyr::rename(total_obs=n)
group_2000 <- group_2000 %>% dplyr::rename(total_obs=n)
bsb_obs_group_1980 <- bsb_obs_group_1980 %>% dplyr::rename(bsb_obs=n)
bsb_obs_group_2000 <- bsb_obs_group_2000 %>% dplyr::rename(bsb_obs=n)

# Merge for 1980s
merge_1980s <- merge(group_1980, bsb_obs_group_1980, by='latlon', all.x=TRUE)
# Merge for 2000s
merge_2000s <- merge(group_2000, bsb_obs_group_2000, by='latlon', all.x=TRUE)
# Replace NA's in the bsb_obs column by 0
merge_1980s <- merge_1980s %>% dplyr::mutate(bsb_obs = replace_na(bsb_obs, 0))
merge_2000s <- merge_2000s %>% dplyr::mutate(bsb_obs = replace_na(bsb_obs, 0))
# Now create a new column for the actual prevalance
merge_1980s <- merge_1980s %>% dplyr::mutate(prev=bsb_obs/total_obs)
merge_2000s <- merge_2000s %>% dplyr::mutate(prev=bsb_obs/total_obs)


# Set the plot/map boundaries

# Plotting on the map itself
# Here we need to get the minimum and maximum for latitude and longitude
lat_max <- max(hindcast_2000$lat, na.rm=TRUE) + 1
lat_min <- min(hindcast_2000$lat, na.rm=TRUE) - 1
lon_max <- max(hindcast_2000$lon, na.rm=TRUE) + 1
lon_min <- min(hindcast_2000$lon, na.rm=TRUE) - 1


# Now we plot only the common grids
unique_1980s <- unique(merge_1980s$latlon)
unique_2000s <- unique(merge_2000s$latlon)

intersect_two <- intersect(unique_1980s, unique_2000s)

merge_1980s <- merge_1980s[(merge_1980s$latlon %in% intersect_two), ]
merge_2000s <- merge_2000s[(merge_2000s$latlon %in% intersect_two), ]


plot_1 <- ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max), expand = FALSE) +
  labs(fill="Prevalance") + ylab("Latitude") + xlab("") + 
  geom_tile(data=merge_1980s, aes(lon_grid025.x, lat_grid025.x, width = 0.25, fill = prev)) +  
  scale_fill_gradientn(colors=c("#ffffcc","#a1dab4","#41b6c4", "#225ea8")) +
  annotate(geom = "text", x = lon_min + 4, y = lat_max - 3, label = "1980-1990", 
           size = 3)




# Setting the scale for plot 1 - values = scales::rescale(c(0,0.5,1.5, max(sum_1980$x)))
# Previously used 'scale_fill_gradient2()' here for coloring the geom_tile

plot_2 <- ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max), expand = FALSE) +
  geom_tile(data=merge_2000s, aes(lon_grid025.x, lat_grid025.x, width=0.25, fill=prev)) + 
  scale_fill_gradientn(colors=c("#ffffcc","#a1dab4","#41b6c4", "#225ea8")) + 
  labs(fill="Prevalance") + xlab("Longitude") + ylab("Latitude") + 
  annotation_north_arrow(location = 'br', which_north = 'true', 
                         pad_x = unit(0.1, 'in'), pad_y = unit(0.3, 'in'), 
                         style = north_arrow_fancy_orienteering,
                         height=unit(1, "cm"),
                         width=unit(1, "cm")) +
  annotate(geom = "text", x = lon_min + 4, y = lat_max - 3 , label = "2000-2010", 
           size = 3)

# scale_fill_gradient(low = "#56B1F7", high = "#132B43"s)

# Fix the color scaling
# low = "blue", mid = "green", high = "red"
png("roms_figures_output_ver_1/bsb_prevalance_1990_2010_version_1.png", width=6, height=6, units="in", res=300)
gridExtra::grid.arrange(plot_1, plot_2, nrow=2)
dev.off()



