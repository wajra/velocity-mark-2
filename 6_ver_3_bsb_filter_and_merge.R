# Objectives
# 1. Get only the data for black sea bass (Centropristis striata) from species catch data
# 2. Clean it up
# 3. Merge it with haul data
# 4. Save it as a CSV file

# Loop through species and fit models.
library(mgcv)
library(dismo)

# This is the dataset named 'dat'
load('data/master_hauls_March7_2017.RData') # import master hauls file
# This is the dataset named 'hauls'
load('data/dat_selectedspp_Feb_1_2017.Rdata')# load species catch data

# Changed the runname for my own diagnostics
runname <- "fitallreg_2019_ver_2"

# Before all other data clean ups we can actually filter out only black sea bass columns
# This will lessen the load on my poor laptop

# We'll use a function for this
bsb_filter <- function(x) startsWith(x, "centropristis striata")

# Then using 'sapply' we'll apply the function to 'sppocean' column in dat
dat <- dat[sapply(dat$sppocean,bsb_filter),]

# Here they simply say that all other rows apart from those having a 'wtcpue' value of 0 and 'region'
# value of 'DFO_SoGulf' are actual absences therefore can be removed from the dataframe
dat <- dat[!(dat$wtcpue == 0 & dat$region == 'DFO_SoGulf'),] # the zeros in SoGulf are actual zeros (ie not just a scale issue) and thus are true absences
# And then the remaining zero values of 'wtcpue' are set to 0.0002 because they do have a value. They are not absences.
# They are barely there
dat$wtcpue[dat$wtcpue == 0] <- 0.0002 # 'zeros' in dat are now species too light to register on scales_here a value below the lowest non-zero value is assigned_for transforming data
# Then we check for the species ('sppocean' column) named 'calappa sulcata in Atlantic Ocean' and
# check if there 'wtcpue' is missing. If so we set it to 0.13.
dat$wtcpue[dat$sppocean=="calappa sulcata_Atl" & is.na(dat$wtcpue)] <- 0.13 # the median weight of this species when observed
# Then a new column is created named 'logwtcpue' and log values of 'wtcpue' is set to it
dat$logwtcpue <- log(dat$wtcpue)

# drop dubious GMex observations of temperate species or species that have a Gulf of Mexico endemic congener that closely resembles the Atlantic coast species
# So what we have here is a list or vector of a set of species that are in both Gulf of Mexico and Atlantic Ocean
drops <- c('alosa pseudoharengus_Gmex', 'brevoortia tyrannus_Gmex', 'clupea harengus_Gmex', 'dipturus laevis_Gmex', 'paralichthys dentatus_Gmex',
           'hippoglossoides platessoides_Gmex', 'pseudopleuronectes americanus_Gmex', 'scomber scombrus_Gmex', 'cryptacanthodes maculatus_Gmex',
           'echinarachnius parma_Gmex', 'illex illecebrosus_Gmex', 'melanostigma atlanticum_Gmex', 'menidia menidia_Gmex', 'ovalipes ocellatus_Gmex','placopecten magellanicus_Gmex')
# 'gsub' is a function that simply substitutes a substring for another substring in a string or vector
# USAGE
# gsub(pattern, replacement, x, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
drops <- gsub('_Gmex', '_Atl', drops)

# OK. So here we need to understand some context. 'SEFSC_GOMexFall' is the Gulf of Mexico survey
# This survey went from 1982-2014 and occured in the Summer and Fall.
# So what needs to be done is see whether a row has 'region' belonging to these surveys and seasons and
# see whether the 'sppocean' value is in the 'drops' vector
# Rows that fall into this classification are filtered out using !
dat <- dat[!(dat$region=='SEFSC_GOMexFall' & dat$sppocean %in% drops),]
# Same for Summery surveys
dat <- dat[!(dat$region=='SEFSC_GOMexSummer' & dat$sppocean %in% drops),]

# trim columns that are already in master hauls file, which will be merged in below with the hauls data
# Creating a new data frame using columns and data from 'dat' data frame
dat <- data.frame(haulid = dat$haulid, sppocean = dat$sppocean, Freq = dat$Freq, wtcpue = dat$wtcpue, logwtcpue = dat$logwtcpue, presfit = TRUE, stringsAsFactors = F)
# Here the rows that have'sppocean'  value of 'NO CATCH' will be filtered out
dat <- dat[!dat$sppocean=='NO CATCH',]
#dat <- dat[!is.na(dat$wtcpue),] # drop NAs as it creates errors for some species. May want to go back and manually do 'oncorhynchus tshawytscha_Pac' as almost 10% of presence records have NA for wtcpue (tagging study?)
# Found a species naming error_correcting here
dat$sppocean[dat$sppocean=='heppasteria phygiana_Atl'] <- 'hippasteria phrygiana_Atl' # no need to aggregate as they never overlapped w/n hauls (only found in Newfoundland and they called it one way or the other)
# save(dat, hauls, file='data/hauls_catch_Dec2017_2019_09_14.RData') # For distribution to other people

######################
# Start the big loop #
######################

# Create table to store model diagnostics
# Making a sorted vector of all unique species
# USAGE
# sort(x, decreasing = FALSE, …)
# 'unique' simply gives you a vector of unique values in a vector
allspp = sort(unique(dat$sppocean))
#allspp <- gsub('_Gmex', '_Atl', drops) # taken from MS_figures script
#allspp <- sort(sample(allspp, 30))
# 'rep' is simply a function for repeating a certain value over a number of times
# USAGE
# rep(x, times = 1, length.out = NA, each = 1)
n = rep(NA, length(allspp))
# Model diagnostics for each species will be stored
# in this data frame
# modeldiag = data.frame(sppocean=n, npres=n, fakepres=n, npres.tr=n, npres.te=n, ntot=n, thresh.kappa=n, thresh=n, thresh.trKap=n, thresh.tr=n, auc=n, auc.tt=n, tss=n, tss.tt=n, tssmax=n, tssmax.tt=n, acc=n, acc.tt=n, accmax=n, accmax.tt=n, sens=n, sens.tt=n, spec=n, spec.tt=n, kappa=n, kappa.tt=n, kappamax=n, kappamax.tt=n, rpb=n, prev.obs=n, prev.pred.prev=n, prev.pred.kap=n, smear=n, pred_obsMedian=n, pred_obsMean=n, r2.biomass=n, r2.biomass.tt=n, r2.all=n, r2.all.tt=n, r2.pres.surv_year=n, r2.pres.surv_year.kap=n, r2.abun.surv_year=n, r2.predPATT.surv_year=n, r2.predPATTkap.surv_year=n, r2.predTT.surv_year=n, dev.pres=n, dev.biomass=n, dev.pres.null=n, dev.biomass.null=n, stringsAsFactors=FALSE) # pred_obsMedianNosmear=n, pred_obsMeanNosmear=n, # tt is for training/testing model
# redos <- c(40, 159)# I deleted some of these already that are non-salvagable
# redos2 <- c(40, 115, 159, 198, 201, 335, 361, 558, 651) # this group adds 6 other species that wasn't able to calculate an r2 value, but not one of our criteria so not going to look into it now

# 'options' function
# Allow the user to set and examine a variety of global options 
# which affect the way in which R computes and displays its results.
options(warn=1) # print warnings as they occur
allwarnings = NULL
# 'paste' function
# Used to concatenate vectors together
print(paste(length(allspp), 'models to fit'))

print('We are here now')
# 'merge' is a useful function in R
# https://www.rdocumentation.org/packages/base/versions/3.6.1/topics/merge
# USAGE
# merge(first_dataframe, second_dataframe, by = 'common_column or field', all.x = if 'TRUE' then for each row in x that has no matching value in y
# a new row will be added in the new dataframe, sort='T if you want to sort the data')
# Here we will merge the species data to the hauls by 'haulid' column
spdata <- merge(hauls, dat, by='haulid', all.x = T, sort=F)
print(nrow(spdata))
myocean <- head(spdata$ocean[spdata$presfit == TRUE])[1] # identify if this is a Pacific or Atlantic species
spdata <- spdata[spdata$ocean == myocean,] # trim master hauls file to the ocean of interest
write.csv(spdata, 'catch_data_hauls_merge_3.csv')