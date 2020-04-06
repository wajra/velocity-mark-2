# We'll use this script to apply what we learned in
# roms_data_diagnostics.R
# roms_data_diagnostics_part_ii.R
# to clean up the edge values and convert the remaining values to negatives

library(tidyverse)
library(gridExtra)

# Load the data in
# Read the data in as a tibble
diag_data <- read_csv('data/full_haul_data_ver_4.csv')


# Remove any haul with [O2] or [NO-3] with NAs
# We'll use the dplyr 'filter_at' function for this
diag_data <- diag_data %>% 
            filter_at(vars(o2_seasonal, sm_zplk_seasonal, me_zplk_seasonal,lg_zplk_seasonal), all_vars(!is.na(.)))

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
            mutate(lg_zplk_seasonal = ifelse(lg_zplk_seasonal<0,0,lg_zplk_seasonal))

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


# And that's that. Since we have
# i. Removed all NA's
# ii. Set all negative values to 'zero' for the variables of interest
# We now have to focus on getting some transformation which will make the data appear more normal

# Next file is roms_data_transformation_part_iv.R

#-----------------------------------------------END---------------------------------------------------------------#