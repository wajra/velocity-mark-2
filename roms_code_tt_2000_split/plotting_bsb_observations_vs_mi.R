# Part II - Metabolic index for aquatic animals
# Author - Jeewantha Bandara (jeewantha.bandara@rutgers.edu)
# Date - 2020/11/25

# Metabolic index (phi) was defined by Deutch et al. (2015) as follows
# phi = Ao * Bn * Po2 / exp(-Eo/kB * T)
# Where
# phi -> Metabolic index [unitless]
# Ao -> Ratio of rate coefficients for O2 supply and metabolic rate [unitless]
# Bn -> Body mass scaling. This can be equated to 10^-2 for black sea bass[unitless]
# Po2 -> Partial pressure of dissolved oxygen [units: kPa]
# Eo -> Temperature dependence of metabolic activity [eV]
# kB -> Boltzmann's constant: 8.617E-05 [units: eVK^-1]
# T -> Temperature [units: Kelvin]

# Relationship between population growth rate and metabolic index
# It can be said as 'linear'
# Population growth rate : r
# 'c' would be the slope, 'k' would be the intercept
# Linear regression would be : r = (c * phi) + k

# Now let's convert the mol/kg to mg/L
# Then using 'respirometry' we'll convert the mg/L to kPa

library(respirometry)
library(tidyverse)
library(gridExtra)

# This works
respirometry::conv_o2(o2=9.1, from="mg_per_l", to="kPa", temp=9.2, 
                      sal=34, atm_pres=1013.25)
# Now converting Dissolved Oxygen
# ROMS has DO is mol/kg
# mol/kg -> mg/L -> kPa
# 1 L of seawater = 1.024 kg
# 1 mol/0.9766 L
# Example = 0.0003 mol/kg
# This is : 32g/mol of O2
# So.... :
# Molar mass * [mol/kg] * [g to mg| 1000]
32*0.0003*1000
# So the value in mg/L is
32*0.0003*1000/0.9766


############################ Read the data ####################################

bsb_environment <- readr::read_csv("data/merged_bsb_observations.csv")

# Convert the oxygen to mg/L
bsb_environment$o2_mgl <- bsb_environment$o2_seasonal*32*1000/0.9766

# Get the minimum and maximum for 'o2_seasonal'
o2_seasonal_max <- max(bsb_environment$o2_seasonal, na.rm=TRUE)
# Get the minimum value
o2_seasonal_min <- min(bsb_environment$o2_seasonal, na.rm=TRUE)
# We should convert these from mol/kg to mg/L of seawater
o2_seasonal_max <- o2_seasonal_max*32*1000/0.9766
o2_seasonal_min <- o2_seasonal_min*32*1000/0.9766

# Do the same for 'SBT.seasonal'
sbt_seasonal_max <- max(bsb_environment$SBT.seasonal, na.rm=TRUE)
# Get the minimum value
sbt_seasonal_min <- min(bsb_environment$SBT.seasonal, na.rm=TRUE)

# Now let's create the matrix
# First get a sequence of values
sbt_seasonal_seq <- seq(sbt_seasonal_min, sbt_seasonal_max, 0.5)

o2_seasonal_seq <- seq(o2_seasonal_min, o2_seasonal_max, 0.5)

# Now make the matrix
sbt_o2_comb <- expand.grid(sbt_seasonal = sbt_seasonal_seq,
                           o2_seasonal=o2_seasonal_seq)

# Insert an empty column to the matrix for the partial pressure of O2
sbt_o2_comb$po2 <- NA

# Insert a value to the first row of the third column
# Notation is [row,column] for insertions
# We'll loop along the length of the matrix
# We'll use 'NROW' function to get the number of rows
matrix_seq <- seq(1, NROW(sbt_o2_comb))

# Now we loop through
for (loc in matrix_seq){
  sbt_o2_comb[loc,3] <- respirometry::conv_o2(o2=sbt_o2_comb[loc,2], 
                                              from="mg_per_l", 
                                              to="kPa", temp=sbt_o2_comb[loc,1], 
                                              sal=34, atm_pres=1013.25)
}

# Now convert Temperature units from Celsius to Kelvin
sbt_o2_comb[,1] <- sbt_o2_comb[,1] + 273.15

# Now do the computations for 'Metabolic index' : phi
# Metabolic index (phi) was defined by Deutch et al. (2015) as follows
# phi = Ao * Bn * Po2 / exp(-Eo/kB * T)
# Where
# phi -> Metabolic index [unitless]
# Ao -> 0.00040728 [unitless]
# Bn -> 1 [unitless]
# Po2 -> [units: kPa]
# Eo -> 0.27 [unitless]
# kB -> Boltzmann's constant: 8.617E-05 [units: eVK^-1]
# T -> [units: Kelvin]
# Species specific values for these constants were gathered from Saba et al, and
# Slesinger et al.
Ao <- 0.00040728
Bn <- 1E-02
Eo <- 0.27
kB <- 8.617E-05

# Insert an empty column to the matrix for the partial pressure of O2
sbt_o2_comb$phi <- NA

# For this operation we have to multiply the numerator by 10^-2 to equalize the terms
# So, we have the values for the metabolic index
for (loc in matrix_seq){
  sbt_o2_comb[loc,4] <- (Ao * sbt_o2_comb[loc,3] *Bn)/exp(-Eo/(kB*sbt_o2_comb[loc,1]))
}


# So, what should we do now??
# We should plot the variation of 'phi' with temperature and oxygen
# Then we should plot the variation of 'alpha' with temperature and oxygen
# Then we can start producing the interesting ecological states

# Date - 2020/11/29

# Now, change the first column back to Celsius and drop the third column
sbt_o2_comb <- sbt_o2_comb[,-3]
sbt_o2_comb[,1] <- sbt_o2_comb[,1] - 273.15

# theme_minimal()
# theme_classic()

phi_heatmap <- ggplot(sbt_o2_comb, aes(sbt_seasonal,
                                       o2_seasonal)) + 
  theme_classic() + 
  theme(axis.title = element_text(size = 14),
        axis.text=element_text(size=10),
        legend.title = element_text(size=12),
        legend.text = element_text(size=10)) +
  geom_tile(aes(fill=phi)) +
  scale_fill_gradient(low="white", high="blue") + 
  labs(fill="  \u03d5  ") + 
  xlab("Temperature (C)") + ylab("Dissolved Oxygen (mg/L)") +
  geom_point(data = bsb_environment, 
             mapping = aes(x = SBT.seasonal, y = o2_mgl),
             alpha=0.3, size=1.2)


phi_heatmap
ggsave("roms_figures_output_ver_1/phi_variation_vs_bsb_observations_ver_2.png", phi_heatmap)

# Save the 2 plots at once
# Now time to produce interesting ecological states
# First let's calculate the alpha
# Relationship (with theoretical realistic parameters) between 
# metabolic index (phi) and population growth rate (alpha)
# In the form of 'y = mx + c'
# alpha = 0.2 * phi - 0.2

# Now add the column for 'alpha'
sbt_o2_comb$alpha <- NA
# Now fill it up
sbt_o2_comb$alpha <- (sbt_o2_comb$phi * 0.2) - 0.2

alpha_heatmap <- ggplot(sbt_o2_comb, aes(sbt_seasonal,
                                         o2_seasonal, fill= alpha)) + 
  geom_tile() +
  scale_fill_gradient(low="white", high="red") +
  xlab("Temperature (C)") + ylab("Dissolved Oxygen (mg/L)")


# Now we should circle the ecologically realistic values in these graphs
alpha_heatmap
# ggsave("output/alpha_variation.png", alpha_heatmap)


############### 2021-02-02 ###########################
# Now to add the line for MI=1

# Filter out the values close to 1
# Make a copy of sbt_o2_comb
phi_df <- sbt_o2_comb[(sbt_o2_comb$phi<1.1) & (sbt_o2_comb$phi>0.90000),]
phi_df$phi <- 1.0
ggplot(phi_df, aes(x=sbt_seasonal, y=o2_seasonal)) + 
  geom_point()

'''
# Save the SBT and O2 and Phi dataset
png(filename="output/alpha_and_phi.png",  width=6, height=6, units=c("in"), res=300)
gridExtra::grid.arrange(phi_heatmap, alpha_heatmap, nrow=1)

# Do the same for MSY
sbt_o2_comb$msy <- NA
sbt_o2_comb$msy <- 30*sbt_o2_comb$alpha/4

msy_heatmap <- ggplot(sbt_o2_comb, aes(sbt_seasonal,
                                       o2_seasonal, fill= msy)) + 
  geom_tile() +
  scale_fill_gradient(low="white", high="black") +
  xlab("Temperature (C)") + ylab("Dissolved Oxygen (mg/L)")

ggsave("output/msy_variation.png", msy_heatmap)

'''