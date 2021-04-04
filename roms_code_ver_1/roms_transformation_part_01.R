library(tidyverse)

# Load the data in
# Then check for incorrect and null values
# Remove those values
# Then plot the values

# Read the data in as a tibble
diag_data <- read_csv('data/full_haul_data_ver_8.csv')


# Plot histogram for O2
# bar outlines are black and white fill
ggplot(diag_data, aes(x=o2_seasonal)) + 
  geom_histogram(color="black", fill="white") +
  labs(title="Seasonal O2 (mol/kg) across all hauls", x="[O2]",
       y="Count")
# Save the figure
# ggsave("figures/diagnostics/o2_histogram.png")

# Plot histogram for Small Zooplankton
ggplot(diag_data, aes(x=sm_zplk_seasonal)) + 
  geom_histogram(color="black", fill="white") +
  labs(title="NO-3 (mol/kg) for Small Zooplankton", x="[NO-3]",
       y="Count")
# Save the figure
# ggsave("figures/diagnostics/small_NO-3_histogram.png")

# Plot histogram for Medium Zooplankton
ggplot(diag_data, aes(x=me_zplk_seasonal)) + 
  geom_histogram(color="black", fill="white") +
  labs(title="NO-3 (mol/kg) for Medium Zooplankton", x="[NO-3]",
       y="Count")
# Save the figure
# ggsave("figures/diagnostics/medium_NO-3_histogram.png")

# Plot histogram for Large Zooplankton
ggplot(diag_data, aes(x=lg_zplk_seasonal)) + 
  geom_histogram(color="black", fill="white") +
  labs(title="NO-3 (mol/kg) for Large Zooplankton", x="[NO-3]",
       y="Count")
# Save the figure
# ggsave("figures/diagnostics/large_NO-3_histogram.png")


# Now, we have to read in the data for hauls
# This is the dataset named 'dat'
load('data/master_hauls_March7_2017.RData') # import master hauls file
# This is the dataset named 'hauls'
load('data/dat_selectedspp_Feb_1_2017.Rdata')# load species catch data

# Remove the duplicate columns
filtered_hauls_roms <- subset(diag_data, select=-c(sppocean,year,month,lon,lat))
# Now we merge the hauls dataset and our new data by 'haulid'
hauls <- merge(filtered_hauls_roms,hauls,by='haulid',all.x=T,sort=F)

# Plot the validated SBT data vs. the model SBT data
sbt_scatterplot <- ggplot(hauls, aes(x=SBT.seasonal, y=sbt_temp_seasonal)) + 
  geom_point(colour="black", fill="white", size=0.5) +
  labs(x="SBT from SODA 3.3.1 (Celsius)",
       y="SBT from ROMS (Celsius)")

ggsave("figures/sbt_correlation.png", plot=sbt_scatterplot, dpi=300)
# Perform a correlation analysis
cor(x= hauls$SBT.seasonal, y= hauls$sbt_temp_seasonal, use='complete.obs')