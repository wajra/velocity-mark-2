# My first proper script in Project Velocity
# Here we go

# Objectives
# 1. Get only the data for black sea bass (Centropristis striata) from species catch data
# 2. Clean it up
# 3. Merge it with haul data


# Loop through species and fit models.
library(mgcv)
library(dismo)
library(tidyverse)

# This is the dataset named 'dat'
load('data/master_hauls_March7_2017.RData') # import master hauls file
# This is the dataset named 'hauls'
load('data/dat_selectedspp_Feb_1_2017.Rdata')# load species catch data

runname <- "fitallreg_2017"

# Before all other data clean ups we can actually filter out only black sea bass columns
# This will lessen the load on my poor laptop

# We'll use a function for this
bsb_filter <- function(x) startsWith(x, "centropristis striata")

# Then using 'sapply' we'll apply the function to 'sppocean' column in dat
dat <- dat[sapply(dat$sppocean,bsb_filter),]

# This dataframe contains only observations for black sea bass
# write.csv(dat, 'data/black_sea_bass_observations.csv')
# print('Done')

# 11/14/2019
# Since the ROMS model for hindcasts starts from 1980
# it would be useful to see where the observations starting
# from 1980 are from
dat_1980 <- dat %>% filter(dat$year>=1980)
# Sorting
head(arrange(dat_1980, dat_1980$year), 10)

arrange(dat_1980, dat_1980$year)

arrange(dat_1980, dat_1980$year)[c(dat_1980$year, dat_1980$month), ]

dat_pre_1980 <- dat %>% filter(dat$year<1980)