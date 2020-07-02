library(tidyverse)

# Load the data in
# Then check for incorrect and null values
# Remove those values
# Then plot the values

# Read the data in as a tibble
diag_data <- read_csv('data/full_haul_data_ver_4.csv')


# Plot histogram for O2
# bar outlines are black and white fill
ggplot(diag_data, aes(x=o2_seasonal)) + 
  geom_histogram(color="black", fill="white") +
  labs(title="Seasonal O2 (mol/kg) across all hauls", x="[O2]",
       y="Count")
# Save the figure
ggsave("figures/diagnostics/o2_histogram.png")

# Plot histogram for Small Zooplankton
ggplot(diag_data, aes(x=sm_zplk_seasonal)) + 
  geom_histogram(color="black", fill="white") +
  labs(title="NO-3 (mol/kg) for Small Zooplankton", x="[NO-3]",
       y="Count")
# Save the figure
ggsave("figures/diagnostics/small_NO-3_histogram.png")

# Plot histogram for Medium Zooplankton
ggplot(diag_data, aes(x=me_zplk_seasonal)) + 
  geom_histogram(color="black", fill="white") +
  labs(title="NO-3 (mol/kg) for Medium Zooplankton", x="[NO-3]",
       y="Count")
# Save the figure
ggsave("figures/diagnostics/medium_NO-3_histogram.png")

# Plot histogram for Large Zooplankton
ggplot(diag_data, aes(x=lg_zplk_seasonal)) + 
  geom_histogram(color="black", fill="white") +
  labs(title="NO-3 (mol/kg) for Large Zooplankton", x="[NO-3]",
       y="Count")
# Save the figure
ggsave("figures/diagnostics/large_NO-3_histogram.png")

# Getting the average seasonal values for the parameters over
# each haul month
mean_data <- diag_data %>% group_by(month) %>% summary()

# First for Oxygen
mean_o2 <- diag_data %>% group_by(month) %>% 
  summarise(mean_o2 = mean(o2_seasonal, na.rm=TRUE))

# To make things nicer, let's convert the month column to a factor
mean_o2 <- mean_o2 %>% mutate(month=as.factor(month))

# Make a bar chart across all the months
ggplot(data=mean_o2, aes(x=month, y=mean_o2)) + 
  geom_bar(stat = "identity") + 
  labs(title="Mean [O2] across each month", x="Month", y="[O2] (mol/kg)")
# Save the figure
ggsave("figures/diagnostics/o2_months.png")

# Now to make a grid and average across that
# Let's see what the minimum and maximum values are for latitude and longitude
lon_min <- floor(min(diag_data$lon))
lon_max <- ceiling(max(diag_data$lon))
lat_min <- floor(min(diag_data$lat))
lat_max <- ceiling(max(diag_data$lat))

# Now to make intervals along longitude and latitude
# Then average [O2] across these intervals
# Generate the intervals
# For longitude create a sequence from minimum to maximum
lon_intervals <- seq(lon_min, lon_max, 1)
# Create groups based on these intervals and assign them to each haul
diag_data$lon_groups <- cut(diag_data$lon, breaks=lon_intervals)
# Create a table for each group and mean [O2] for each group
lon_o2_means <- diag_data %>% group_by(lon_groups) %>% 
  summarize(mean_o2=mean(o2_seasonal, na.rm=TRUE))
lon_o2_means$lon_groups_str <- str_replace(as.character(lon_o2_means$lon_groups), "]", ")")
ggplot(data=lon_o2_means, aes(x=lon_groups, y=mean_o2)) + 
  geom_bar(stat="identity") +
  labs(title="Mean [O2] across longitudes", x="Longitude intervals", y="Seasonal [02] (mol/kg)") +
  scale_x_discrete(labels=lon_o2_means$lon_groups_str) +
  theme(axis.text.x=element_text(angle = -90, vjust=0.5), plot.title = element_text(hjust = 0.5)) 
# Save the figure
ggsave("figures/diagnostics/o2_longitude.png",scale=1.3)

# For latitude we do the same process
lat_intervals <- seq(lat_min, lat_max, 1)
# Create groups based on these intervals and assign them to each haul
diag_data$lat_groups <- cut(diag_data$lat, breaks=lat_intervals)
# Create a table for each group and mean [O2] for each group
lat_o2_means <- diag_data %>% group_by(lat_groups) %>% 
  summarize(mean_o2=mean(o2_seasonal, na.rm=TRUE))
lat_o2_means$lat_groups_str <- str_replace(as.character(lat_o2_means$lat_groups), "]", ")")
ggplot(data=lat_o2_means, aes(x=lat_groups, y=mean_o2)) + 
  geom_bar(stat="identity") +
  labs(title="Mean [O2] across latitudes", x="Latitude intervals", y="Seasonal [02] (mol/kg)") +
  scale_x_discrete(labels=lat_o2_means$lat_groups_str) +
  theme(axis.text.x=element_text(angle = -90, vjust=0.5), plot.title = element_text(hjust = 0.5)) 
# Save the figure
ggsave("figures/diagnostics/o2_latitudes.png")

# Now for small zooplankton Nitrogen
mean_sm_zplk <- diag_data %>% group_by(month) %>% 
  summarise(mean_sm_zplk = mean(sm_zplk_seasonal, na.rm=TRUE))

# To make things nicer, let's convert the month column to a factor
mean_sm_zplk <- mean_sm_zplk %>% mutate(month=as.factor(month))

# Make a bar chart across all the months
ggplot(data=mean_sm_zplk, aes(x=month, y=mean_sm_zplk)) + 
  geom_bar(stat = "identity") + 
  labs(title="Mean [NO-3] for small zooplankton across each month", x="Month", y="[NO-3] (mol/kg)")
# Save the figure
ggsave("figures/diagnostics/small_NO-3_months.png")


# Now to make intervals along longitude and latitude
# Then average [NO-3] across these intervals
# Create a table for each group and mean [NO-3] for each group
lon_sm_zplk_means <- diag_data %>% group_by(lon_groups) %>% 
  summarize(mean_sm_zplk=mean(sm_zplk_seasonal, na.rm=TRUE))
# Continue from here
lon_sm_zplk_means$lon_groups_str <- str_replace(as.character(lon_sm_zplk_means$lon_groups), "]", ")")
ggplot(data=lon_sm_zplk_means, aes(x=lon_groups, y=mean_sm_zplk)) + 
  geom_bar(stat="identity") +
  labs(title="Mean [NO-3] for small zooplankton across longitudes", x="Longitude intervals", y="Seasonal [NO-3] (mol/kg)") +
  scale_x_discrete(labels=lon_sm_zplk_means$lon_groups_str) +
  theme(axis.text.x=element_text(angle = -90, vjust=0.5), plot.title = element_text(hjust = 0.5)) 
# Save the figure
ggsave("figures/diagnostics/small_NO-3_longitude.png",scale=1.3)

# Now for latitude
lat_sm_zplk_means <- diag_data %>% group_by(lat_groups) %>% 
  summarize(mean_sm_zplk=mean(sm_zplk_seasonal, na.rm=TRUE))
lat_sm_zplk_means$lat_groups_str <- str_replace(as.character(lat_sm_zplk_means$lat_groups), "]", ")")
ggplot(data=lat_sm_zplk_means, aes(x=lat_groups, y=mean_sm_zplk)) + 
  geom_bar(stat="identity") +
  labs(title="Mean [NO-3] for small zooplankton across latitudes", x="Latitude intervals", y="Seasonal [NO-3] (mol/kg)") +
  scale_x_discrete(labels=lat_sm_zplk_means$lat_groups_str) +
  theme(axis.text.x=element_text(angle = -90, vjust=0.5), plot.title = element_text(hjust = 0.5)) 
# Save the figure
ggsave("figures/diagnostics/small_NO-3_latitude.png")

# Now for medium zooplankton Nitrogen
mean_me_zplk <- diag_data %>% group_by(month) %>% 
  summarise(mean_me_zplk = mean(me_zplk_seasonal, na.rm=TRUE))

# To make things nicer, let's convert the month column to a factor
mean_me_zplk <- mean_me_zplk %>% mutate(month=as.factor(month))

# Make a bar chart across all the months
ggplot(data=mean_me_zplk, aes(x=month, y=mean_me_zplk)) + 
  geom_bar(stat = "identity") + 
  labs(title="Mean [NO-3] for medium zooplankton across each month", x="Month", y="[NO-3] (mol/kg)")
# Save the figure
ggsave("figures/diagnostics/medium_NO-3_months.png")


# Now to make intervals along longitude and latitude
# Then average [NO-3] across these intervals
# Create a table for each group and mean [NO-3] for each group
lon_me_zplk_means <- diag_data %>% group_by(lon_groups) %>% 
  summarize(mean_me_zplk=mean(me_zplk_seasonal, na.rm=TRUE))
# Continue from here
lon_me_zplk_means$lon_groups_str <- str_replace(as.character(lon_me_zplk_means$lon_groups), "]", ")")
ggplot(data=lon_me_zplk_means, aes(x=lon_groups, y=mean_me_zplk)) + 
  geom_bar(stat="identity") +
  labs(title="Mean [NO-3] for medium zooplankton across longitudes", x="Longitude intervals", y="Seasonal [NO-3] (mol/kg)") +
  scale_x_discrete(labels=lon_me_zplk_means$lon_groups_str) +
  theme(axis.text.x=element_text(angle = -90, vjust=0.5), plot.title = element_text(hjust = 0.5)) 
# Save the figure
ggsave("figures/diagnostics/medium_NO-3_longitude.png",scale=1.3)

# Now for latitude
lat_me_zplk_means <- diag_data %>% group_by(lat_groups) %>% 
  summarize(mean_me_zplk=mean(me_zplk_seasonal, na.rm=TRUE))
lat_me_zplk_means$lat_groups_str <- str_replace(as.character(lat_me_zplk_means$lat_groups), "]", ")")
ggplot(data=lat_me_zplk_means, aes(x=lat_groups, y=mean_me_zplk)) + 
  geom_bar(stat="identity") +
  labs(title="Mean [NO-3] for medium zooplankton across latitudes", x="Latitude intervals", y="Seasonal [NO-3] (mol/kg)") +
  scale_x_discrete(labels=lat_me_zplk_means$lat_groups_str) +
  theme(axis.text.x=element_text(angle = -90, vjust=0.5), plot.title = element_text(hjust = 0.5)) 
# Save the figure
ggsave("figures/diagnostics/medium_NO-3_latitude.png")


# Now for large zooplankton Nitrogen
mean_lg_zplk <- diag_data %>% group_by(month) %>% 
  summarise(mean_lg_zplk = mean(lg_zplk_seasonal, na.rm=TRUE))

# To make things nicer, let's convert the month column to a factor
mean_lg_zplk <- mean_lg_zplk %>% mutate(month=as.factor(month))

# Make a bar chart across all the months
ggplot(data=mean_lg_zplk, aes(x=month, y=mean_lg_zplk)) + 
  geom_bar(stat = "identity") + 
  labs(title="Mean [NO-3] for large zooplankton across each month", x="Month", y="[NO-3] (mol/kg)")
# Save the figure
ggsave("figures/diagnostics/large_NO-3_months.png")


# Now to make intervals along longitude and latitude
# Then average [NO-3] across these intervals
# Create a table for each group and mean [NO-3] for each group
lon_lg_zplk_means <- diag_data %>% group_by(lon_groups) %>% 
  summarize(mean_lg_zplk=mean(lg_zplk_seasonal, na.rm=TRUE))
# Continue from here
lon_lg_zplk_means$lon_groups_str <- str_replace(as.character(lon_lg_zplk_means$lon_groups), "]", ")")
ggplot(data=lon_lg_zplk_means, aes(x=lon_groups, y=mean_lg_zplk)) + 
  geom_bar(stat="identity") +
  labs(title="Mean [NO-3] for large zooplankton across longitudes", x="Longitude intervals", y="Seasonal [NO-3] (mol/kg)") +
  scale_x_discrete(labels=lon_lg_zplk_means$lon_groups_str) +
  theme(axis.text.x=element_text(angle = -90, vjust=0.5), plot.title = element_text(hjust = 0.5)) 
# Save the figure
ggsave("figures/diagnostics/large_NO-3_longitude.png",scale=1.3)


# Now for latitude
lat_lg_zplk_means <- diag_data %>% group_by(lat_groups) %>% 
  summarize(mean_lg_zplk=mean(lg_zplk_seasonal, na.rm=TRUE))
lat_lg_zplk_means$lat_groups_str <- str_replace(as.character(lat_lg_zplk_means$lat_groups), "]", ")")
ggplot(data=lat_lg_zplk_means, aes(x=lat_groups, y=mean_lg_zplk)) + 
  geom_bar(stat="identity") +
  labs(title="Mean [NO-3] for large zooplankton across latitudes", x="Latitude intervals", y="Seasonal [NO-3] (mol/kg)") +
  scale_x_discrete(labels=lat_lg_zplk_means$lat_groups_str) +
  theme(axis.text.x=element_text(angle = -90, vjust=0.5), plot.title = element_text(hjust = 0.5)) 
# Save the figure
ggsave("figures/diagnostics/large_NO-3_latitude.png")
