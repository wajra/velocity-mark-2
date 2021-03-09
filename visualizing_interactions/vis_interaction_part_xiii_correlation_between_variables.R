# Visualizing Interactions - Part XIII
# Plot the correlation between variables for all observations
# Author - Jeewantha Bandara (mailto:jeewantha.bandara@rutgers.edu) 
# Research Group - Pinsky Lab, Rutgers University (https://pinsky.marine.rutgers.edu)
# Following Advanced R style guide - http://adv-r.had.co.nz/Style.html

library(tidyverse)
library(Hmisc)
library(corrplot)

# Read the dataframe of black sea bass presences merged with trawl surveys
# Only for true black sea bass presences

# Read the data
bsb_hauls <- readr::read_csv(file="roms_model_output_ver_1/classic_model_full_hindcast.csv")

# Select only the relevant columns
# These would be
# i. SBT.min
# ii. SBT.max
# iii. SST.max
# iv. rugosity
# v. GRAINSIZE
# vi. SST.seasonal.mean 
# vii. SBT.seasonal -> Renamed to 'SBT.seasonal.mean'
# viii. o2_seasonal -> Renamed to 'O2.seasonal'
# ix. salinity_seasonal -> Renamed to 'salinity.seasonal'
# x. column_sm_zplk_seasonal -> Renamed to 'zplk.seasonal'

# Rename the columns for niceness
bsb_hauls <- bsb_hauls %>% 
  dplyr::rename(SBT=SBT.seasonal,
                DO=o2_seasonal,
                S=salinity_seasonal,
                Z=zplk_class_mean_seasonal,
                G=GRAINSIZE,
                R=rugosity,
                SST=SST.seasonal.mean,
                )

# Select only the relevant columns
bsb_hauls <- bsb_hauls %>% 
  dplyr::select(SBT.min, SBT.max, SST.max, R, G,
                SST, SBT, DO, 
                S, Z)

# Now let's see the correlation between the different variables
# We will check the 'Pearson correlation' and use 'complete.observations'
# Using methods illustrated here 
# http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software

bsb_cor_matrix <- cor(bsb_hauls, method=c("pearson"), use="complete.obs")

# Now get the correlation levels with significant levels using Hmisc package
bsb_cor_p_matrix <- Hmisc::rcorr(as.matrix(bsb_hauls), type=c("pearson"))

# Since this outputs 3 separate lists, we should reformat it into a table
# Writing a custom function for this
flattenListObject <- function(cormat, pmat){
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

# Run the function
bsb_result_matrix <- flattenListObject(bsb_cor_p_matrix$r, bsb_cor_p_matrix$P)

# Create a column for significance of p-value
bsb_result_matrix <- bsb_result_matrix %>% 
  dplyr::mutate(p_percent_5 = dplyr::case_when(
    p <= 0.05 ~ TRUE,
    p > 0.05 ~ FALSE
  ))

# Create a column of significant for correlation value
# If over 0.6 in either direction, we'll deem it as been significant
bsb_result_matrix <- bsb_result_matrix %>%
  dplyr::mutate(cor_high = dplyr::case_when(
    cor >= 0.6 | cor <= -0.6 ~ TRUE,
    cor < 0.6 | cor > -0.6 ~ FALSE
  ))


# Use corrplot to illustrate these relationships
# Arguments : type -> Show only upper half of the correlation matrix
# order -> How to order the correlation matrix
# tl.col -> Text color
# tl.srt -> Rotation of the text
# Get the correlation matrix again
# Save it to a PNG file
png(filename = "exploratory_figures/correlation_plot_all_observations.png",
    width = 5, height = 5, units = "in", pointsize = 12,
    bg = "white", res = 300)
corrplot::corrplot(bsb_cor_matrix, type = "upper", 
                   order = "hclust", tl.col = "black", tl.srt = 90)
dev.off()


# End of file
# Proceed to 'data_exploration_part_02_basic_statistics.R'