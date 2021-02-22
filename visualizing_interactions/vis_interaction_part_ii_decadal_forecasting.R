# Visualizing Interactions - Part II
# Make decadal forecasting based on the best performing models
# Author - Jeewantha Bandara (mailto:jeewantha.bandara@rutgers.edu) 
# Research Group - Pinsky Lab, Rutgers University (https://pinsky.marine.rutgers.edu)
# Following Advanced R style guide - http://adv-r.had.co.nz/Style.html


library(ggplot2)
library(tidyr)
library(tibble)
# library(hrbrthemes)
library(dplyr)

library(RColorBrewer)
pal <- colorRampPalette(brewer.pal(11, "RdYlGn"))(100)

# Read the values
hindcast <- read.csv(file="roms_model_output_ver_1/full_dataset_hindcast.csv")
# roms_model_output_ver_1/classic_model_full_hindcast.csv

hindcast_1985 <- hindcast[(hindcast$year>=1985 & hindcast$year<1995),]

hindcast_1985$lat_grid025 <- floor(hindcast_1985$lat*4)/4 + 0.125
hindcast_1985$lon_grid025 <- floor(hindcast_1985$lon*4)/4 + 0.125

sum <- aggregate(hindcast_1985$preds, by = list(lat = hindcast_1985$lat_grid025, lon = hindcast_1985$lon_grid025), FUN = mean, na.rm= TRUE)

ggplot(sum, aes(lon, lat, width = 0.25, fill = x)) + geom_tile()

ggplot(hindcast_1985, aes(longrid, latgrid, width = 0.25, fill = logwtcpue)) + geom_tile()
# Because log(0) is undefined, perhaps add a very small value. 10^-10 to wtcpue and then multiply??

# To draw maps - https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html

# hindcast_1985 <- hindcast_1985 %>% 
#  dplyr::filter(sppocean=='centropristis striata_Atl')

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

ggplot(hindcast_1985, aes(longrid, latgrid, width = 0.25, fill = logwtcpue)) + 
  geom_tile() + scale_fill_gradient(limits = range(plot_min, plot_max))

# Seems to be a problem with the max values
# Histogram time
hist_1985 <- ggplot(data=hindcast_1985, aes(x=logwtcpue)) + 
  geom_histogram(color='black', fill='white')

ggplot(hindcast_1985, aes(longrid, latgrid, fill= logwtcpue)) + 
  geom_tile() + scale_fill_gradient(low="blue", high="red")

# scale_fill_gradient(limits = range(df1$z, df2$z))


ggplot(hindcast_1985, aes(longrid, latgrid, fill= log(preds))) + 
  geom_tile() + scale_fill_gradient(low="blue", high="red")



## Same for 1995 to 2005

hindcast_1995 <- hindcast[(hindcast$year>=1995 & hindcast$year<2005),]

hindcast_1995$lat_grid025 <- floor(hindcast_1995$lat*4)/4 + 0.125
hindcast_1995$lon_grid025 <- floor(hindcast_1995$lon*4)/4 + 0.125

sum_95 <- aggregate(hindcast_1995$preds, by = list(lat = hindcast_1995$lat_grid025,
                                                lon = hindcast_1995$lon_grid025), FUN = mean, na.rm= TRUE)

ggplot(sum_95, aes(lon, lat, width = 0.25, fill = x)) + geom_tile()

ggplot(hindcast_1995, aes(longrid, latgrid, width = 0.25, fill = logwtcpue)) + geom_tile()
# Because log(0) is undefined, perhaps add a very small value. 10^-10 to wtcpue and then multiply??
