library("tseries")
library('plot.matrix')

table_1 <- read.table("data/1981-05-03.txt")

table_2 <- read.table("data/1981-05-02.txt")

t_3 <- table_1 * table_2

# Now write 't_3' into downloads
write.table(t_3, "/Users/jeewantha/Downloads/t_3_output.txt",col.names = F, row.names = F)

read_in <- read.table("/Users/jeewantha/Downloads/t_3_output.txt")

aa <- as.matrix(read.table("data/1981-05-02.txt"))

bb <- as.matrix(read.table("data/1981-05-03.txt"))


cc <- aa * bb

# Write this matrix
write.table(cc, "/Users/jeewantha/Downloads/cc_output.txt", col.names=F, row.names=F)
# Read it in
read_cc <- read.table("/Users/jeewantha/Downloads/cc_output.txt")

image(aa)


# Read the folder contents

# my_files <- Sys.glob(file.path("/Volumes/JB_DRIVE/RESEARCH/ROMS/DATA/", "do", "*.txt"))
my_files <- Sys.glob(file.path("/Volumes/JB_DRIVE/RESEARCH/ROMS/DATA/do", "*.txt"))

df <- data.frame(filename=my_files, stringsAsFactors = FALSE)

table_4 <- read.table(df$filename[1])



######################## 2021-03-08 ###############################

do_1981_files <- Sys.glob(file.path("/Volumes/JB_DRIVE/RESEARCH/ROMS/DATA/do", "*.txt"))
temp_1981_files <- Sys.glob(file.path("/Volumes/JB_DRIVE/RESEARCH/ROMS/DATA/temperature", "*.txt"))
salinity_1981_files <- Sys.glob(file.path("/Volumes/JB_DRIVE/RESEARCH/ROMS/DATA/salinity", "*.txt"))

calc_files <- data.frame(do=do_1981_files,
                         temp=temp_1981_files,
                         salinity=salinity_1981_files,
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



# Now put 
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
write.table(mi_mean, "/Users/jeewantha/Downloads/mi_1981_spring.txt", col.names=F, row.names=F)

# Now scp this to Proteus and plot it out