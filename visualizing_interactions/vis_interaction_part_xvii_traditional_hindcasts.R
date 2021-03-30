# Visualizing Interactions - Part XVII
# Merge the hindcasts with each other based on common grids
# Author - Jeewantha Bandara (mailto:jeewantha.bandara@rutgers.edu) 
# Research Group - Pinsky Lab, Rutgers University (https://pinsky.marine.rutgers.edu)
# Following Advanced R style guide - http://adv-r.had.co.nz/Style.html

# Let's do this for the 'Classical model hindcasts'

library("ggplot2")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library(tidyverse)
library(ggspatial)
library(scales)

# First, let's see if this theoretically works
# Join 2 splits by their lat-long grids

hindcast <- read.csv(file="roms_model_output_ver_1/classic_model_full_hindcast.csv")
# Worst performing model - roms_model_output_ver_1/full_dataset_hindcast.csv
# roms_model_output_ver_1/classic_model_full_hindcast.csv

# Make a new column joining the latgrid and longrid
hindcast$lonandlat <- paste(hindcast$longrid, "-", hindcast$latgrid)


hindcast_pre_2000 <- hindcast[hindcast$year<2000,]
hindcast_post_2000 <- hindcast[hindcast$year>=2000,]

# Now we should get the unique combinations between longrid and latgrid
unique_pre_2000 <- unique(hindcast_post_2000$lonandlat)
unique_post_2000 <- unique(hindcast_pre_2000$lonandlat)

intersect_two <- intersect(unique_pre_2000, unique_post_2000)

hindcast_pre_2000 <- hindcast_pre_2000[(hindcast_pre_2000$lonandlat %in% intersect_two), ]
hindcast_post_2000 <- hindcast_post_2000[(hindcast_post_2000$lonandlat %in% intersect_two), ]

ggplot() +
  geom_tile(data=hindcast_pre_2000, aes(longrid, latgrid, width=0.5, fill=preds))

ggplot() +
  geom_tile(data=hindcast_post_2000, aes(longrid, latgrid, width=0.5, fill=preds))
