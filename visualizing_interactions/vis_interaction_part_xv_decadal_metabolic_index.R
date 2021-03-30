library("tseries")
library('plot.matrix')


######################## 2021-03-24 ###############################

# Now let's do this for 1980s Fall seasons

do_1980s_files <- Sys.glob(file.path("/Volumes/JB_DRIVE/RESEARCH/ROMS/1980s/fall/do", "*.txt"))
temp_1980s_files <- Sys.glob(file.path("/Volumes/JB_DRIVE/RESEARCH/ROMS/1980s/fall/temperature", "*.txt"))
salinity_1980s_files <- Sys.glob(file.path("/Volumes/JB_DRIVE/RESEARCH/ROMS/1980s/fall/salinity", "*.txt"))

calc_files <- data.frame(do=do_1980s_files,
                         temp=temp_1980s_files,
                         salinity=salinity_1980s_files,
                         stringsAsFactors = FALSE)

# Get the dimensions and set up the empty (sort of) 3D matrix

length <- dim(calc_files)[1]
rowr <- 362
colr <- 722

mi_calc_array = array(0, dim=c(length,rowr,colr))


do_test <- array(c(9.7,8.4, 6.8, 9.1), dim=c(2,2))

array(respirometry::conv_o2(o2=do_test, from="mg_per_l", to="kPa", temp=9.2, 
                            sal=34, atm_pres=1013.25), dim=c(2,2))

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


# Now we need to loop through 'calc_files' and then also convert DO
# First convert DO to mg/L then to kPa through Respirometry

for(i in 1:nrow(calc_files)) {
  # First convert mol/kg to mg/L
  do_first <- as.matrix(read.table(calc_files$do[i]))
  do_mgl <- do_first*32*1000/0.9766
  # Get the temperature and salinity out
  temp_day <- as.matrix(read.table(calc_files$temp[i]))
  salinity_day <- as.matrix(read.table(calc_files$salinity[i]))
  # Now convert from mg/L to kPa
  do_kpa <- array(respirometry::conv_o2(o2=c(do_mgl), 
                                        from="mg_per_l", to="kPa", temp=c(temp_day), 
                                        sal=c(salinity_day), atm_pres=1013.25),
                  dim=c(rowr, colr))
  # Convert temp from Celsius to Kelvin
  temp_day <- temp_day + 273.15
  mi_day <- (Ao * do_kpa *Bn)/exp(-Eo/(kB*temp_day))
  
  # Then put it into mi_calc_array
  mi_calc_array[i,,] <- mi_day
  print(paste('Done for date:',i))
}

print('Done')

mi_mean <- apply(mi_calc_array, c(2,3), mean)

# Now write this out
# Write this matrix
write.table(mi_mean, "/Users/jeewantha/Downloads/mi_1980s_fall.txt", col.names=F, row.names=F)

# Now scp this to Proteus and plot it out



# Now do the same for 1980s Spring #####################################

do_2000_files <- Sys.glob(file.path("/Volumes/JB_DRIVE/RESEARCH/ROMS/1980s/spring/do", "*.txt"))
temp_2000_files <- Sys.glob(file.path("/Volumes/JB_DRIVE/RESEARCH/ROMS/1980s/spring/temperature", "*.txt"))
salinity_2000_files <- Sys.glob(file.path("/Volumes/JB_DRIVE/RESEARCH/ROMS/1980s/spring/salinity", "*.txt"))

calc_files <- data.frame(do=do_2000_files,
                         temp=temp_2000_files,
                         salinity=salinity_2000_files,
                         stringsAsFactors = FALSE)

# Get the dimensions and set up the empty (sort of) 3D matrix

length <- dim(calc_files)[1]
rowr <- 362
colr <- 722

mi_calc_array = array(0, dim=c(length,rowr,colr))


do_test <- array(c(9.7,8.4, 6.8, 9.1), dim=c(2,2))

array(respirometry::conv_o2(o2=do_test, from="mg_per_l", to="kPa", temp=9.2, 
                            sal=34, atm_pres=1013.25), dim=c(2,2))

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


# Now we need to loop through 'calc_files' and then also convert DO
# First convert DO to mg/L then to kPa through Respirometry

for(i in 1:nrow(calc_files)) {
  # First convert mol/kg to mg/L
  do_first <- as.matrix(read.table(calc_files$do[i]))
  do_mgl <- do_first*32*1000/0.9766
  # Get the temperature and salinity out
  temp_day <- as.matrix(read.table(calc_files$temp[i]))
  salinity_day <- as.matrix(read.table(calc_files$salinity[i]))
  # Now convert from mg/L to kPa
  do_kpa <- array(respirometry::conv_o2(o2=c(do_mgl), 
                                        from="mg_per_l", to="kPa", temp=c(temp_day), 
                                        sal=c(salinity_day), atm_pres=1013.25),
                  dim=c(rowr, colr))
  # Convert temp from Celsius to Kelvin
  temp_day <- temp_day + 273.15
  mi_day <- (Ao * do_kpa *Bn)/exp(-Eo/(kB*temp_day))
  
  # Then put it into mi_calc_array
  mi_calc_array[i,,] <- mi_day
  print(paste('Done for date:',i))
}

print('Done')

mi_mean <- apply(mi_calc_array, c(2,3), mean)

# Now write this out
# Write this matrix
write.table(mi_mean, "/Users/jeewantha/Downloads/mi_1980s_spring.txt", col.names=F, row.names=F)


############################################################################################################
# Now we'll do it for 2000s Fall

do_2000s_fall_files <- Sys.glob(file.path("/Volumes/JB_DRIVE/RESEARCH/ROMS/2000s/fall/do", "*.txt"))
temp_2000s_fall_files <- Sys.glob(file.path("/Volumes/JB_DRIVE/RESEARCH/ROMS/2000s/fall/temperature", "*.txt"))
salinity_2000s_fall_files <- Sys.glob(file.path("/Volumes/JB_DRIVE/RESEARCH/ROMS/2000s/fall/salinity", "*.txt"))

calc_files <- data.frame(do=do_2000s_fall_files,
                         temp=temp_2000s_fall_files,
                         salinity=salinity_2000s_fall_files,
                         stringsAsFactors = FALSE)

# Get the dimensions and set up the empty (sort of) 3D matrix

length <- dim(calc_files)[1]
rowr <- 362
colr <- 722

mi_calc_array = array(0, dim=c(length,rowr,colr))


do_test <- array(c(9.7,8.4, 6.8, 9.1), dim=c(2,2))

array(respirometry::conv_o2(o2=do_test, from="mg_per_l", to="kPa", temp=9.2, 
                            sal=34, atm_pres=1013.25), dim=c(2,2))

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


# Now we need to loop through 'calc_files' and then also convert DO
# First convert DO to mg/L then to kPa through Respirometry

for(i in 1:nrow(calc_files)) {
  # First convert mol/kg to mg/L
  do_first <- as.matrix(read.table(calc_files$do[i]))
  do_mgl <- do_first*32*1000/0.9766
  # Get the temperature and salinity out
  temp_day <- as.matrix(read.table(calc_files$temp[i]))
  salinity_day <- as.matrix(read.table(calc_files$salinity[i]))
  # Now convert from mg/L to kPa
  do_kpa <- array(respirometry::conv_o2(o2=c(do_mgl), 
                                        from="mg_per_l", to="kPa", temp=c(temp_day), 
                                        sal=c(salinity_day), atm_pres=1013.25),
                  dim=c(rowr, colr))
  # Convert temp from Celsius to Kelvin
  temp_day <- temp_day + 273.15
  mi_day <- (Ao * do_kpa *Bn)/exp(-Eo/(kB*temp_day))
  
  # Then put it into mi_calc_array
  mi_calc_array[i,,] <- mi_day
  print(paste('Done for date:',i))
}

print('Done')

mi_mean <- apply(mi_calc_array, c(2,3), mean)

# Now write this out
# Write this matrix
write.table(mi_mean, "/Users/jeewantha/Downloads/mi_2000s_fall.txt", col.names=F, row.names=F)



############################################################################################################
# Now we'll do it for 2000s Spring

do_2000s_spring_files <- Sys.glob(file.path("/Volumes/JB_DRIVE/RESEARCH/ROMS/2000s/spring/do", "*.txt"))
temp_2000s_spring_files <- Sys.glob(file.path("/Volumes/JB_DRIVE/RESEARCH/ROMS/2000s/spring/temperature", "*.txt"))
salinity_2000s_spring_files <- Sys.glob(file.path("/Volumes/JB_DRIVE/RESEARCH/ROMS/2000s/spring/salinity", "*.txt"))

calc_files <- data.frame(do=do_2000s_spring_files,
                         temp=temp_2000s_spring_files,
                         salinity=salinity_2000s_spring_files,
                         stringsAsFactors = FALSE)

# Get the dimensions and set up the empty (sort of) 3D matrix

length <- dim(calc_files)[1]
rowr <- 362
colr <- 722

mi_calc_array = array(0, dim=c(length,rowr,colr))


do_test <- array(c(9.7,8.4, 6.8, 9.1), dim=c(2,2))

array(respirometry::conv_o2(o2=do_test, from="mg_per_l", to="kPa", temp=9.2, 
                            sal=34, atm_pres=1013.25), dim=c(2,2))

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


# Now we need to loop through 'calc_files' and then also convert DO
# First convert DO to mg/L then to kPa through Respirometry

for(i in 1:nrow(calc_files)) {
  # First convert mol/kg to mg/L
  do_first <- as.matrix(read.table(calc_files$do[i]))
  do_mgl <- do_first*32*1000/0.9766
  # Get the temperature and salinity out
  temp_day <- as.matrix(read.table(calc_files$temp[i]))
  salinity_day <- as.matrix(read.table(calc_files$salinity[i]))
  # Now convert from mg/L to kPa
  do_kpa <- array(respirometry::conv_o2(o2=c(do_mgl), 
                                        from="mg_per_l", to="kPa", temp=c(temp_day), 
                                        sal=c(salinity_day), atm_pres=1013.25),
                  dim=c(rowr, colr))
  # Convert temp from Celsius to Kelvin
  temp_day <- temp_day + 273.15
  mi_day <- (Ao * do_kpa *Bn)/exp(-Eo/(kB*temp_day))
  
  # Then put it into mi_calc_array
  mi_calc_array[i,,] <- mi_day
  print(paste('Done for date:',i))
}

print('Done')

mi_mean <- apply(mi_calc_array, c(2,3), mean)

# Now write this out
# Write this matrix
write.table(mi_mean, "/Users/jeewantha/Downloads/mi_2000s_spring.txt", col.names=F, row.names=F)

