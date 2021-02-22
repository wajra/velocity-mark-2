# Visualizing Interactions - Part VII
# Here we will plot DO vs haul depth
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

# Now let's plot depth vs. DO
do_vs_depth_plot <- ggplot(data=hauls, aes(x=depth, y=o2_seasonal)) + geom_point()

# Let's see the correlation
do_depth_cor <- cor(x=hauls$o2_seasonal, y=hauls$depth, use="complete.obs", method="pearson")
print(do_depth_cor)

# Now let's see the correlation for only BSB observations
# Load the species file
# This is the dataset named 'hauls'
load('data/dat_selectedspp_Feb_1_2017.Rdata')# load species catch data

# Filter out the BSB
# We'll use a function for this
bsb_filter <- function(x) startsWith(x, "centropristis striata")

# Then using 'sapply' we'll apply the function to 'sppocean' column in dat
dat <- dat[sapply(dat$sppocean,bsb_filter),]

# Merge the hauls and dat
bsb_obs <- merge(filtered_hauls_roms,dat,by='haulid',all.y=T,sort=F)
# Then drop the NA rows based on NA DO rows
bsb_obs <- bsb_obs %>% drop_na(o2_seasonal)

# Now do the correlation
do_depth_cor_bsb <- cor.test(x=bsb_obs$o2_seasonal, y=bsb_obs$depth, use="complete.obs", method="pearson")
print(do_depth_cor_bsb)
# It's still pretty low and not significant

# Now run a linear model for 
do_depth_plot <- ggplot(data=bsb_obs, aes(x=depth, y=o2_seasonal)) + 
  geom_point() + 
  stat_smooth(method = "lm", col = "red") +
  xlab("Depth (m)") + ylab("Seasonal Dissolved Oxygen (mol/kg)")

ggsave("roms_figures_output_ver_1/do_vs_depth.png", 
       width=4, height=4, units=c("in"))
