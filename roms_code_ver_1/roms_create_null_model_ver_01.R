# ROMS - Create null parameter
# We will attempt to create a null parameter that's better than using
# the ocean floor rugosity and grainsize
# Author - Jeewantha Bandara (mailto:jeewantha.bandara@rutgers.edu) 
# Research Group - Pinsky Lab, Rutgers University (https://pinsky.marine.rutgers.edu)
# Following Tidyverse style guide - https://style.tidyverse.org

# First let's create a dataframe with a random variable

# A dataframe of 100 rows with random numbers generated from a 
# normal distribution of mean=10 and standard deviation=3
null_dataframe <- data.frame(row_id=seq(1,100), 
  rnorm_val=rnorm(100, mean=10, sd=3))

# Let's do the same for a uniform distribution
# We'll simply add it to the data frame
null_dataframe$unif_val <- runif(100, min=0, max=10)

# Plot the distributions
hist(null_dataframe$random_val)
hist(null_dataframe$unif_val)

# So the plan is that we'll create a similar thing for the 

# Get the size of the dataframe
dim(null_dataframe)[1]
# Let's try to create a new dataframe using the 'dim' argument
null_dataframe$random_val_2 <- rnorm(dim(null_dataframe)[1], mean=10, sd=3)
