# We'll use this script to make transformations of our right-skewed (lots of zero values)
# data and try to get them looking more like a normal distribution

library(tidyverse)
library(gridExtra)

# Load the data in
# Read the data in as a tibble
roms_data <- read_csv('data/haul_data_for_transformation_4.csv')


# We'll use Prof. James Kirchner's guide to transform data
# http://seismo.berkeley.edu/~kirchner/eps_120/Toolkits/Toolkit_03.pdf

# Following his advise, we'll start at
# i. square root : x**0.5
# ii. cube root : x**0.33
# iii. logarithmic : log(x + k)
# iv. reciprocal root : -1/(x**0.5)
# v. reciprocal : -1/x
# v. reciprocal square : -1/(x**2)


# We'll have to adapt these methods a little. Add a small constant (k) which would be the minimum value of 
# a variable other than zero
min_o2_val <- min(roms_data %>% filter(o2_seasonal!=0) %>% dplyr::select(o2_seasonal))
min_sm_zplk_val <- min(roms_data %>% filter(sm_zplk_seasonal!=0) %>% dplyr::select(sm_zplk_seasonal))
min_me_zplk_val <- min(roms_data %>% filter(me_zplk_seasonal!=0) %>% dplyr::select(me_zplk_seasonal))
min_lg_zplk_val <- min(roms_data %>% filter(lg_zplk_seasonal!=0) %>% dplyr::select(lg_zplk_seasonal))

# If we want to use a global value for the transformation's minimum values
overall_min_val <- 1

# And now we move on to the next stage

# i. square root : x**0.5 transformation
roms_data <- roms_data %>% mutate(sqrt_o2_seasonal = (o2_seasonal+min_o2_val)**0.5) %>%
  mutate(sqrt_sm_zplk_seasonal = (sm_zplk_seasonal+min_sm_zplk_val)**0.5) %>%
  mutate(sqrt_me_zplk_seasonal = (me_zplk_seasonal+min_me_zplk_val)**0.5) %>% 
  mutate(sqrt_lg_zplk_seasonal = (lg_zplk_seasonal+min_lg_zplk_val)**0.5)

# ii. cube root : x**1/3
roms_data <- roms_data %>% mutate(cbrt_o2_seasonal = (o2_seasonal+min_o2_val)**(1/3)) %>%
  mutate(cbrt_sm_zplk_seasonal = (sm_zplk_seasonal+min_sm_zplk_val)**(1/3)) %>%
  mutate(cbrt_me_zplk_seasonal = (me_zplk_seasonal+min_me_zplk_val)**(1/3)) %>% 
  mutate(cbrt_lg_zplk_seasonal = (lg_zplk_seasonal+min_lg_zplk_val)**(1/3))

# iii. logarithmic : log(x + k)
roms_data <- roms_data %>% mutate(log_o2_seasonal = log(o2_seasonal+min_o2_val)) %>%
  mutate(log_sm_zplk_seasonal = log(sm_zplk_seasonal+min_sm_zplk_val)) %>%
  mutate(log_me_zplk_seasonal = log(me_zplk_seasonal+min_me_zplk_val)) %>% 
  mutate(log_lg_zplk_seasonal = log(lg_zplk_seasonal+min_lg_zplk_val))

# iv. reciprocal root. Not done yet
roms_data <- roms_data %>% mutate(rert_o2_seasonal = -1/(o2_seasonal+min_o2_val)**0.5) %>%
  mutate(rert_sm_zplk_seasonal = -1/(sm_zplk_seasonal+min_sm_zplk_val)**0.5) %>%
  mutate(rert_me_zplk_seasonal = -1/(me_zplk_seasonal+min_me_zplk_val)**0.5) %>% 
  mutate(rert_lg_zplk_seasonal = -1/(lg_zplk_seasonal+min_lg_zplk_val)**0.5)


# Let's see how the transformation looks vs. normal data
# For small zooplankton
plot_1 <- ggplot(roms_data, aes(x=sm_zplk_seasonal)) + 
  geom_histogram(color="black", fill="white")
plot_2 <- ggplot(roms_data, aes(x=sqrt_sm_zplk_seasonal)) + 
  geom_histogram(color="white", fill="black")
plot_3 <- ggplot(roms_data, aes(x=cbrt_sm_zplk_seasonal)) + 
  geom_histogram(color="red", fill="white")
plot_4 <- ggplot(roms_data, aes(x=log_sm_zplk_seasonal)) + 
  geom_histogram(color="blue", fill="white")
plot_5 <- ggplot(roms_data, aes(x=rert_sm_zplk_seasonal)) +
  geom_histogram(color="White", fill="gray")



collective_plot <- grid.arrange(plot_1, plot_2,plot_3, plot_4, plot_5, nrow=3, ncol=2)

ggsave('figures/transformations_plot_sm_zplk.png',collective_plot,dpi=300, width=8, height=6, units="in")


# Approach #2
# OK. Now let's see how we do when we add a larger constant

# i. square root : x**0.5 transformation
roms_data <- roms_data %>% mutate(sqrt_o2_seasonal = (o2_seasonal+overall_min_val)**0.5) %>%
  mutate(sqrt_sm_zplk_seasonal = (sm_zplk_seasonal+overall_min_val)**0.5) %>%
  mutate(sqrt_me_zplk_seasonal = (me_zplk_seasonal+overall_min_val)**0.5) %>% 
  mutate(sqrt_lg_zplk_seasonal = (lg_zplk_seasonal+overall_min_val)**0.5)

# ii. cube root : x**1/3
roms_data <- roms_data %>% mutate(cbrt_o2_seasonal = (o2_seasonal+overall_min_val)**(1/3)) %>%
  mutate(cbrt_sm_zplk_seasonal = (sm_zplk_seasonal+overall_min_val)**(1/3)) %>%
  mutate(cbrt_me_zplk_seasonal = (me_zplk_seasonal+overall_min_val)**(1/3)) %>% 
  mutate(cbrt_lg_zplk_seasonal = (lg_zplk_seasonal+overall_min_val)**(1/3))

# iii. logarithmic : log(x + k)
roms_data <- roms_data %>% mutate(log_o2_seasonal = log(o2_seasonal+overall_min_val)) %>%
  mutate(log_sm_zplk_seasonal = log(sm_zplk_seasonal+overall_min_val)) %>%
  mutate(log_me_zplk_seasonal = log(me_zplk_seasonal+overall_min_val)) %>% 
  mutate(log_lg_zplk_seasonal = log(lg_zplk_seasonal+overall_min_val))

# iv. reciprocal root. Not done yet
roms_data <- roms_data %>% mutate(rert_o2_seasonal = -1/(o2_seasonal+overall_min_val)**0.5) %>%
  mutate(rert_sm_zplk_seasonal = -1/(sm_zplk_seasonal+overall_min_val)**0.5) %>%
  mutate(rert_me_zplk_seasonal = -1/(me_zplk_seasonal+overall_min_val)**0.5) %>% 
  mutate(rert_lg_zplk_seasonal = -1/(lg_zplk_seasonal+overall_min_val)**0.5)

# Let's see how the transformation looks vs. normal data
# For small zooplankton
plot_1 <- ggplot(roms_data, aes(x=sm_zplk_seasonal)) + 
  geom_histogram(color="black", fill="white")
plot_2 <- ggplot(roms_data, aes(x=sqrt_sm_zplk_seasonal)) + 
  geom_histogram(color="white", fill="black")
plot_3 <- ggplot(roms_data, aes(x=cbrt_sm_zplk_seasonal)) + 
  geom_histogram(color="red", fill="white")
plot_4 <- ggplot(roms_data, aes(x=log_sm_zplk_seasonal)) + 
  geom_histogram(color="blue", fill="white")
plot_5 <- ggplot(roms_data, aes(x=rert_sm_zplk_seasonal)) +
  geom_histogram(color="White", fill="gray")



collective_plot <- grid.arrange(plot_1, plot_2,plot_3, plot_4, plot_5, nrow=3, ncol=2)

write_csv(roms_data, 'data/transformed_haul_data_NWA-SZ_HCob10T_including_column_ver_3.csv')

# OK. So following Malin's advise, I will proceed without transforming the data at first.

# Next up is 6_ver_5_bsb_modeling_o2

#-----------------------------------------------END---------------------------------------------------------------#