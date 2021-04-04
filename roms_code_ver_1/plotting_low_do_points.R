# Part III - Plotting the spatial distribution of the dissolved oxygen points
# Author - Jeewantha Bandara (jeewantha.bandara@rutgers.edu)
# Date - 2021/02/16


library(respirometry)
library(tidyverse)
library(gridExtra)

############################ Read the data ####################################

bsb_environment <- readr::read_csv("data/merged_bsb_observations.csv")

bsb_environment <- bsb_environment %>% drop_na(o2_seasonal)

