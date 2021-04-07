# We'll use this script to apply what we learned in
# roms_data_diagnostics.R
# roms_data_diagnostics_part_ii.R
# to clean up the edge values and convert the remaining values to negatives

library(tidyverse)
library(gridExtra)

# Load the data in
# Read the data in as a tibble
diag_data <- read_csv('data/full_haul_data_ver_8.csv')


# Remove any haul with [O2] or [NO-3] with NAs
# We'll use the dplyr 'filter_at' function for this
filter_vars <- vars(o2_seasonal, sm_zplk_seasonal, me_zplk_seasonal, lg_zplk_seasonal, column_sm_zplk_seasonal,
                    column_me_zplk_seasonal, column_lg_zplk_seasonal)
diag_data <- diag_data %>% 
  filter_at(filter_vars, all_vars(!is.na(.)))

# Remove the edge values
# These would be the hauls lying between 
# i. longitudes -57 and -56
# ii. latitudes 49 and 50

# First the latitudes
# We can use the dplyr 'between' function in conjunction with 'filter' for this
diag_data <- diag_data %>% filter(!between(lat,49,50))

# Then the longitudes
diag_data <- diag_data %>% filter(!between(lon,-57,-56))


# Then convert the negative values in other haul locations to 'zero'
# First [O2]
# Then [NO-3] for zooplankton class sizes
# Basically we'll use the 'mutate' function 
# If value is less than zero, replace it with zero, else keep it the same
diag_data <- diag_data %>% 
  mutate(o2_seasonal = ifelse(o2_seasonal<0,0,o2_seasonal)) %>% 
  mutate(sm_zplk_seasonal = ifelse(sm_zplk_seasonal<0,0,sm_zplk_seasonal)) %>%
  mutate(me_zplk_seasonal = ifelse(me_zplk_seasonal<0,0,me_zplk_seasonal)) %>%
  mutate(lg_zplk_seasonal = ifelse(lg_zplk_seasonal<0,0,lg_zplk_seasonal)) %>%
  mutate(column_sm_zplk_seasonal = ifelse(column_sm_zplk_seasonal<0,0,column_sm_zplk_seasonal)) %>%
  mutate(column_me_zplk_seasonal = ifelse(column_me_zplk_seasonal<0,0,column_me_zplk_seasonal)) %>%
  mutate(column_lg_zplk_seasonal = ifelse(column_lg_zplk_seasonal<0,0,column_lg_zplk_seasonal))

# Now let's just plot the small zooplankton [NO-3] and see if negative values are set to zero

# Plot histogram for Small Zooplankton [NO-3]
ggplot(diag_data, aes(x=sm_zplk_seasonal)) + 
  geom_histogram(color="black", fill="white") +
  labs(title="NO-3 (mol/kg) for Small Zooplankton", x="[NO-3]",
       y="Count")
# Save the figure

# Plot histogram for Medium zooplankton [NO-3]
ggplot(diag_data, aes(x=me_zplk_seasonal)) + 
  geom_histogram(color="black", fill="white") +
  labs(title="NO-3 (mol/kg) for Medium Zooplankton", x="[NO-3]",
       y="Count")
# Looks about right.


# Plot histogram for Small Zooplankton [NO-3] across the column
ggplot(diag_data, aes(x=column_sm_zplk_seasonal)) + 
  geom_histogram(color="black", fill="white") +
  labs(title="NO-3 (mol/kg) for Small Zooplankton across column", x="[NO-3]",
       y="Count")
# Save the figure

# Now let's make a plot and see
plot_1 <- ggplot(diag_data, aes(x=o2_seasonal)) + 
  geom_histogram(color="black", fill="white") +
  labs(title="Dissolved oxygen", x="[O2]",
       y="Count")
plot_2 <- ggplot(diag_data, aes(x=sm_zplk_seasonal)) + 
  geom_histogram(color="black", fill="white") +
  labs(title="Small Zooplankton", x="[NO-3]",
       y="Count")
plot_3 <- ggplot(diag_data, aes(x=me_zplk_seasonal)) + 
  geom_histogram(color="black", fill="white") +
  labs(title="Medium Zooplankton", x="[NO-3]",
       y="Count")
plot_4 <- ggplot(diag_data, aes(x=lg_zplk_seasonal)) + 
  geom_histogram(color="black", fill="white") +
  labs(title="Large Zooplankton", x="[NO-3]",
       y="Count")
plot_5 <- ggplot(diag_data, aes(x=column_sm_zplk_seasonal)) + 
  geom_histogram(color="black", fill="white") +
  labs(title="Small Zooplankton across Column", x="[NO-3]",
       y="Count")
plot_6 <- ggplot(diag_data, aes(x=column_me_zplk_seasonal)) + 
  geom_histogram(color="black", fill="white") +
  labs(title="Medium Zooplankton across Column", x="[NO-3]",
       y="Count")
plot_7 <- ggplot(diag_data, aes(x=column_lg_zplk_seasonal)) + 
  geom_histogram(color="black", fill="white") +
  labs(title="Large Zooplankton across Column", x="[NO-3]",
       y="Count")

collective_plot <- grid.arrange(plot_1, plot_2,plot_3, plot_4, plot_5, plot_6, plot_7, nrow=4, ncol=2)
ggsave("figures/adjusted_parameter_histograms_including_column.png",plot=collective_plot,dpi=300, width=8, height=6, units="in")

# Let's write to the disk
write_csv(diag_data, 'data/haul_data_for_transformation_4.csv')

# And that's that. Since we have
# i. Removed all NA's
# ii. Set all negative values to 'zero' for the variables of interest
# We now have to focus on getting some transformation which will make the data appear more normal

# Next file is roms_data_transformation_part_iv.R

#-----------------------------------------------END---------------------------------------------------------------#