# What needs to be done in this section

# Create a bar chart for the number of NA's across each longitude/latitude interval
# Create a bar chart for the number of negative numbers across each longitude/latitude interval

library(tidyverse)
library(gridExtra)

# Load the data in
# Then check for incorrect and null values
# Remove those values
# Then plot the values

# Read the data in as a tibble
diag_data <- read_csv('data/full_haul_data_ver_4.csv')

# Getting the average seasonal values for the parameters over
# each haul month
mean_data <- diag_data %>% group_by(month) %>% summary()

# First for Oxygen
mean_o2 <- diag_data %>% group_by(month) %>% 
  summarise(mean_o2 = mean(o2_seasonal, na.rm=TRUE))

# To make things nicer, let's convert the month column to a factor
mean_o2 <- mean_o2 %>% mutate(month=as.factor(month))

# Now to make a grid and average across that
# Let's see what the minimum and maximum values are for latitude and longitude
lon_min <- floor(min(diag_data$lon))
lon_max <- ceiling(max(diag_data$lon))
lat_min <- floor(min(diag_data$lat))
lat_max <- ceiling(max(diag_data$lat))

# For longitude create a sequence from minimum to maximum
lon_intervals <- seq(lon_min, lon_max, 1)
# Create groups based on these intervals and assign them to each haul
diag_data$lon_groups <- cut(diag_data$lon, breaks=lon_intervals)
# Do the same for latitudes
lat_intervals <- seq(lat_min, lat_max, 1)
diag_data$lat_groups <- cut(diag_data$lat, breaks=lat_intervals)

# Group NA's for the three zooplankton classes
finding_nas_lg_zplk <- diag_data %>% group_by(lon_groups) %>% summarize(sum(is.na(lg_zplk_seasonal)))
finding_nas_sm_zplk <- diag_data %>% group_by(lon_groups) %>% summarize(sum(is.na(sm_zplk_seasonal)))
finding_nas_me_zplk <- diag_data %>% group_by(lon_groups) %>% summarize(sum(is.na(me_zplk_seasonal)))

# Filter this anomaly out. Where is it?
location <- diag_data %>% filter(is.na(lg_zplk_seasonal))
write.csv(location,"data/NA_location.csv")
# I looked at it. It's a pretty isolate point inbetween a few islets. Probably why we had all nulls around it.


# Now shall we trying to do this for negative values. Coverting them to 'zero' is suggested
# First lets' see the number of negative values for [O2] in latitudes and longitudes
# o2_negs_longitudes <- diag_data %>% filter(o2_seasonal<0) %>% group_by(lon_groups) %>% tally()
# o2_negs_latitudes <- diag_data %>% filter(o2_seasonal<0) %>% group_by(lat_groups) %>% tally()

o2_negs_longitudes <- diag_data %>% 
                      group_by(lon_groups) %>% 
                      summarize(neg_prop = (sum(o2_seasonal<0,na.rm=TRUE)/n())*100, hauls=n(), bsb_hauls=sum(sppocean=='centropristis striata_Atl',na.rm=TRUE)) %>% 
                      mutate(lon_groups_str = str_replace(as.character(lon_groups),"]",")"))

o2_negs_latitudes <- diag_data %>%
                      group_by(lat_groups) %>%
                      summarize(neg_prop = (sum(o2_seasonal<0,na.rm=TRUE)/n())*100) %>% 
                      mutate(lat_groups_str = str_replace(as.character(lat_groups),"]",")"))

# Now let's see how negative values are spread for small zooplankton
sm_zplk_longitudes <- diag_data %>% 
                      group_by(lon_groups) %>% 
                      summarise(neg_prop = (sum(sm_zplk_seasonal<0,na.rm=TRUE)/n())*100) %>% 
                      mutate(lon_groups_str = str_replace(as.character(lon_groups),"]",")"))
# Along the latitudes
sm_zplk_latitudes <- diag_data %>%
                      group_by(lat_groups) %>%
                      summarize(neg_prop = (sum(sm_zplk_seasonal<0,na.rm=TRUE)/n())*100) %>% 
                      mutate(lat_groups_str = str_replace(as.character(lat_groups),"]",")"))

# Do the same for the medium and large zooplankton
# Medium zooplankton
me_zplk_longitudes <- diag_data %>%
                      group_by(lon_groups) %>%
                      summarize(neg_prop = (sum(me_zplk_seasonal<0,na.rm=TRUE)/n())*100) %>% 
                      mutate(lon_groups_str = str_replace(as.character(lon_groups),"]",")"))

me_zplk_latitudes <- diag_data %>% 
                      group_by(lat_groups) %>%
                      summarize(neg_prop = (sum(me_zplk_seasonal<0,na.rm=TRUE)/n())*100) %>% 
                      mutate(lat_groups_str = str_replace(as.character(lat_groups),"]",")"))

# Large zooplankton
lg_zplk_longitudes <- diag_data %>%
                      group_by(lon_groups) %>% 
                      summarize(neg_prop = (sum(lg_zplk_seasonal<0,na.rm=TRUE)/n())*100) %>% 
                      mutate(lon_groups_str = str_replace(as.character(lon_groups),"]",")"))

lg_zplk_latitudes <- diag_data %>% 
                      group_by(lat_groups) %>% 
                      summarize(neg_prop = (sum(lg_zplk_seasonal<0,na.rm=TRUE)/n())*100) %>% 
                      mutate(lat_groups_str = str_replace(as.character(lat_groups),"]",")"))

# Start plotting
# Let's use gridExtra library for multiple plots
# For longitude
# Let's plot the proportions of negative values for [O2] and the size classes of zooplankton

lon_neg_o2_plot <- ggplot(o2_negs_longitudes, aes(x=lon_groups, y=neg_prop)) +
                  geom_bar(stat="identity") +
                  labs(x="Longitude intervals", y="%") +
                  scale_x_discrete(labels=o2_negs_longitudes$lon_groups_str) +
                  theme(axis.text.x=element_text(angle = -90, vjust=0.5), plot.title = element_text(hjust = 0.5)) 

lon_neg_smzplk_plot <- ggplot(sm_zplk_longitudes, aes(x=lon_groups, y=neg_prop)) +
                      geom_bar(stat="identity") +
                      labs(x="Longitude intervals", y="%") +
                      scale_x_discrete(labels=sm_zplk_longitudes$lon_groups_str) +
                      theme(axis.text.x=element_text(angle = -90, vjust=0.5), plot.title = element_text(hjust = 0.5)) 

lon_neg_mezplk_plot <- ggplot(me_zplk_longitudes, aes(x=lon_groups, y=neg_prop)) +
                      geom_bar(stat="identity") +
                      labs(x="Longitude intervals", y="%") +
                      scale_x_discrete(labels=me_zplk_longitudes$lon_groups_str) +
                      theme(axis.text.x=element_text(angle = -90, vjust=0.5), plot.title = element_text(hjust = 0.5)) 

lon_neg_lgzplk_plot <- ggplot(lg_zplk_longitudes, aes(x=lon_groups, y=neg_prop)) +
                      geom_bar(stat="identity") +
                      labs(x="Longitude intervals", y="%") +
                      scale_x_discrete(labels=lg_zplk_longitudes$lon_groups_str) +
                      theme(axis.text.x=element_text(angle = -90, size=6, vjust=0.5), plot.title = element_text(hjust = 0.5)) 

# Use gridExtra to gather them together
grid.arrange(lon_neg_o2_plot, lon_neg_smzplk_plot, lon_neg_mezplk_plot, lon_neg_lgzplk_plot,nrow=2, ncol=2)



# Now we do the same for latitudes
lat_neg_o2_plot <- ggplot(o2_negs_latitudes, aes(x=lat_groups, y=neg_prop)) +
                  geom_bar(stat="identity") +
                  labs(x="Latitude intervals", y="%") +
                  scale_x_discrete(labels=o2_negs_latitudes$lat_groups_str) +
                  theme(axis.text.x=element_text(angle = -90, vjust=0.5), plot.title = element_text(hjust = 0.5)) 

lat_neg_smzplk_plot <- ggplot(sm_zplk_latitudes, aes(x=lat_groups, y=neg_prop)) +
                      geom_bar(stat="identity") +
                      labs(x="Latitude intervals", y="%") +
                      scale_x_discrete(labels=sm_zplk_latitudes$lat_groups_str) +
                      theme(axis.text.x=element_text(angle = -90, vjust=0.5), plot.title = element_text(hjust = 0.5)) 

lat_neg_mezplk_plot <- ggplot(me_zplk_latitudes, aes(x=lat_groups, y=neg_prop)) +
                      geom_bar(stat="identity") +
                      labs(x="Latitude intervals", y="%") +
                      scale_x_discrete(labels=me_zplk_latitudes$lat_groups_str) +
                      theme(axis.text.x=element_text(angle = -90, vjust=0.5), plot.title = element_text(hjust = 0.5)) 

lat_neg_lgzplk_plot <- ggplot(lg_zplk_latitudes, aes(x=lat_groups, y=neg_prop)) +
                      geom_bar(stat="identity") +
                      labs(x="Latitude intervals", y="%") +
                      scale_x_discrete(labels=lg_zplk_latitudes$lat_groups_str) +
                      theme(axis.text.x=element_text(angle = -90, size=6, vjust=0.5), plot.title = element_text(hjust = 0.5)) 

# Use gridExtra to gather them together
grid.arrange(lat_neg_o2_plot, lat_neg_smzplk_plot, lat_neg_mezplk_plot, lat_neg_lgzplk_plot,nrow=2, ncol=2)


# So based on this, what we can say is that there is a sort of 'edge effect' that causes a lot of negative values
# So it would be advisable that these values be dropped
# Other negative values will be converted to zero

#-----------------------------------------------END---------------------------------------------------------------#