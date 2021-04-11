# Visualizing Interactions - Part IX
# Here we will plot the abundance histogram
# Author - Jeewantha Bandara (mailto:jeewantha.bandara@rutgers.edu) 
# Research Group - Pinsky Lab, Rutgers University (https://pinsky.marine.rutgers.edu)
# Following Advanced R style guide - http://adv-r.had.co.nz/Style.html

library(mgcv)
library(dismo)
library(respirometry)
library(tidyverse)


# Load the hauls file
load('data/master_hauls_March7_2017.RData') # import master hauls file


filtered_hauls_roms <- read.csv('data/transformed_haul_data_NWA-SZ_HCob10T_including_column_ver_4.csv',stringsAsFactors = FALSE)
# Drop some of the unncessary columns
filtered_hauls_roms <- subset(filtered_hauls_roms, select=-c(sppocean,year,month,lon,lat))
# Now we load the 
hauls <- merge(filtered_hauls_roms,hauls,by='haulid',all.x=T,sort=F)

# Now let's see the correlation for only BSB observations
# Load the species file
# This is the dataset named 'hauls'
load('data/dat_selectedspp_Feb_1_2017.Rdata')# load species catch data

# Filter out the BSB
# We'll use a function for this
bsb_filter <- function(x) startsWith(x, "centropristis striata")

# Then using 'sapply' we'll apply the function to 'sppocean' column in dat
dat <- dat[sapply(dat$sppocean,bsb_filter),]

merged_obs <- merge(hauls,dat,by='haulid',all.y=T,sort=F)

# Drop the NAs
merged_obs <- merged_obs %>% drop_na(o2_seasonal)

# Insert the cuts
merged_obs <- merged_obs %>% mutate(year_intervals = cut_interval(year.x, n = 5))

group_surveys <- merged_obs %>% group_by(regionfact, year_intervals)  %>% summarise(n = n())

survey_name_group <- merged_obs %>% dplyr::count(regionfact)

# Drop the NAs
