## Set working directories
if(Sys.info()["nodename"] == "pinsky-macbookair"){
	setwd('~/Documents/Rutgers/Range projections/proj_ranges/')
	projfolder <- '../CEmodels_proj' # holds model projections (outside Git)
	modfolder <- '../CEModels' # holds the models (outside Git)
	}
if(Sys.info()["nodename"] == "amphiprion.deenr.rutgers.edu"){
	setwd('~/Documents/range_projections/')
	projfolder <- 'CEmodels_proj'
	modfolder <- 'CEmodels'
	.libPaths(new='~/R/x86_64-redhat-linux-gnu-library/3.1/') # so that it can find my old packages (chron and ncdf4)
	}
# could add code for Lauren's working directory here
 

#######################
## Script-wide flags ##
#######################

## choose which run and time periods to use
#runtype <- 'test'
#runtype <- 'testK6noSeas'; projtype <- ''
#runtype <- 'testK6noSeas'; projtype <- '_xreg' # with cross-region projections
runtype <- 'fitallreg'; projtype <- '_xreg' # with cross-region projections

# choose the RCP
rcp <- 85
#rcp <- 45

# whether to plot observations next to projections in the maps
#plotobs <- FALSE
plotobs <- TRUE


###########################
## Script-wide functions ##
###########################
#require(mgcv)
require(Hmisc)
require(lattice)
require(gridExtra)

# weighted mean function for summarize()
wmean <- function(x){
	i <- !is.na(x[,1]) & !is.na(x[,2])
	if(sum(i) == 0) return(NA)
	else return(weighted.mean(x[i,1], x[i,2]))
}


#############################################################
## Plot time-series of summarized spp biomass and latitude ##
## within each region                                      ##
## use common biomass y-axis if projtype=='_xreg'          ##
#############################################################

## load the data
load(paste('data/meanlat,lon,biomass_', runtype, projtype, '_rcp', rcp, '.RData', sep='')) # biomasssum, meanlat, meanlon: projected biomass by year for each taxon in each region

# plots by region (whether xreg or not)
## plots of change in biomass
	sppregions <- sort(unique(paste(biomasssum$region, biomasssum$sppocean)))
	length(sppregions)

	# quartz(width=10, height=8)
	pdf(file=paste('figures/biomasssum_proj_', runtype, projtype, '_rcp', rcp, '.pdf', sep=''), width=10, height=8)
	par(mfrow = c(6,6), mai=c(0.3, 0.3, 0.2, 0.05), cex.main=0.7, cex.axis=0.8, omi=c(0,0.2,0.1,0), mgp=c(2.8, 0.7, 0), font.main=3)

	rc <- 1 # row counter
	cc <- 0 # column counter

	options(warn=1) # print warnings as they occur
	for(i in 1:length(sppregions)){
		if(i %% 100 == 0) print(i)
		inds <- paste(biomasssum$region, biomasssum$sppocean) == sppregions[i]
		thisreg <- unique(biomasssum$region[inds])
		thisspp <- unique(biomasssum$sppocean[inds])

		# increment row and column counters as needed
		cc <- cc+1
		if(cc == 7){ cc <- 1; rc <- rc + 1}
		if(rc == 7){ cc <- 1; rc <- 1}

		if(i>1){
			 if(thisreg != oldreg){  # switch to a new page when we get to a new region
				par(mfrow = c(6,6), mai=c(0.3, 0.3, 0.2, 0.05), cex.main=0.7, cex.axis=0.8, omi=c(0,0.2,0.1,0), mgp=c(2.8, 0.7, 0), font.main=3)
				rc <- 1; cc <- 1
			}
		}
	
		# ylims
		ylims <- c(0, max(biomasssum[inds, grep('summwtcpue', names(biomasssum))], na.rm=TRUE)) # will warn if all values are NA
		if(projtype=='_xreg'){
			inds2 <- biomasssum$sppocean == thisspp # if cross-regional, then look across all regions to set y-axis
			ylims <- c(0, max(biomasssum[inds2, grep('summwtcpue', names(biomasssum))], na.rm=TRUE)) # will warn if all values are NA
		}
		if(is.infinite(ylims[2])) ylims <- c(0,1)

		# plot data for each GCM
		plot(biomasssum$year[inds], biomasssum$summwtcpue_1[inds], col='grey', las=1, type='l', ylim=ylims, main=thisspp)
		for(j in 2:13) lines(biomasssum$year[inds], biomasssum[[paste('summwtcpue', j, sep='_')]][inds], col='grey')
	
		# plot ensemble mean
		agg <- cbind(biomasssum$year[inds], rowMeans(biomasssum[inds, grep('summwtcpue', names(biomasssum))]))
		lines(agg[,1], agg[,2], col='black')

		if(cc==1) mtext(text='Biomass index', side=2, line=2.3, cex=0.6) # add y label on left of each row
		if(rc==1) mtext(text=thisreg, side=3, line=1.3, cex=0.6) # add region header on top of page

		oldreg <- thisreg # save the previous region to see if we need a new page on the next round
	}

	dev.off()


## plots of change in latitude
	sppregions <- sort(unique(paste(meanlat$region, meanlat$sppocean)))
	length(sppregions)

	# quartz(width=10, height=8)
	pdf(file=paste('figures/meanlat_proj_', runtype, projtype, '_rcp', rcp, '.pdf', sep=''), width=10, height=8)
	par(mfrow = c(6,6), mai=c(0.3, 0.3, 0.2, 0.05), cex.main=0.7, cex.axis=0.8, omi=c(0,0.2,0.1,0), mgp=c(2.8, 0.7, 0), font.main=3)

	rc <- 1 # row counter
	cc <- 0 # column counter

	options(warn=1) # print warnings as they occur
	for(i in 1:length(sppregions)){
		if(i %% 100 == 0) print(i)
		inds <- paste(meanlat$region, meanlat$sppocean) == sppregions[i]
		thisreg <- unique(meanlat$region[inds])
		thisspp <- unique(meanlat$sppocean[inds])

		# increment row and column counters as needed
		cc <- cc+1
		if(cc == 7){ cc <- 1; rc <- rc + 1}
		if(rc == 7){ cc <- 1; rc <- 1}

		if(i>1){ if(thisreg != oldreg){  # switch to a new page when I get to a new region
				par(mfrow = c(6,6), mai=c(0.3, 0.3, 0.2, 0.05), cex.main=0.7, cex.axis=0.8, omi=c(0,0.2,0.1,0), mgp=c(2.8, 0.7, 0), font.main=3)
				rc <- 1; cc <- 1
		}}
	
		# ylims
		ylims <- range(meanlat[inds, grep('lat', names(meanlat))], na.rm=TRUE) # set ylims for this species in this region
#		ylims <- range(meanlat[meanlat$region == thisreg, grep('lat', names(meanlat))])	# set ylims for the whole region
		if(is.infinite(ylims[1])) ylims <- c(0,1)

		# plot data for each GCM
		plot(meanlat$year[inds], meanlat$lat_1[inds], col='grey', las=1, type='l', ylim=ylims, main=thisspp)
		for(j in 2:13) lines(meanlat$year[inds], meanlat[[paste('lat', j, sep='_')]][inds], col='grey')
	
		# plot ensemble mean
		agg <- cbind(meanlat$year[inds], rowMeans(meanlat[inds, grep('lat', names(meanlat))]))
		lines(agg[,1], agg[,2], col='black')

		if(cc==1) mtext(text='Mean latitude (°N)', side=2, line=2.3, cex=0.6) # add y label on left of each row
		if(rc==1) mtext(text=thisreg, side=3, line=1.3, cex=0.6) # add region header on top of page

		oldreg <- thisreg # save the previous region to see if we need a new page on the next round
	}

	dev.off()


#############################################################
## Plot time-series of summarized spp biomass and latitude ##
## within regions                                          ##
## for example species                                     ##
#############################################################

## load the data
load(paste('data/meanlat,lon,biomass_', runtype, projtype, '_rcp', rcp, '.RData', sep='')) # biomasssum, meanlat, meanlon: projected biomass by year for each taxon in each region

# plots by region (whether xreg or not)
## plots of change in biomass
	thisspp <- 'gadus morhua_Atl'; div <- 34000
#	thisspp <- 'loligo pealeii_Atl'; div <- 580000
	thisreg <- 'NEFSC_NEUSFall'
	inds <- biomasssum$sppocean==thisspp & biomasssum$region==thisreg

	thisbiomass <- biomasssum[inds, grep('summwtcpue', names(biomasssum))]
	thisbiomass <- thisbiomass/div # divide by constant to make the y-axis more readable


	# quartz(width=3, height=2.5)
	pdf(file=paste('figures/biomasssum_projexample_', thisspp, '_', thisreg, '_', runtype, projtype, '_rcp', rcp, '.pdf', sep=''), width=3, height=2.5)
	par(mai=c(0.65, 0.65, 0.2, 0.15), cex.main=0.7, cex.axis=0.8, mgp=c(2.8, 0.7, 0), font.main=3)
	
	# ylims
	ylims <- c(0, max(thisbiomass, na.rm=TRUE)) # will warn if all values are NA
	if(is.infinite(ylims[2])) ylims <- c(0,1)

	# plot data for each GCM
	plot(biomasssum$year[inds], thisbiomass$summwtcpue_1, col='grey', las=1, type='l', ylim=ylims, main=paste(thisreg, thisspp), xlab='', ylab='')
	for(j in 2:13) lines(biomasssum$year[inds], thisbiomass[[paste('summwtcpue', j, sep='_')]], col='grey')

	# plot ensemble mean
	agg <- cbind(biomasssum$year[inds], rowMeans(thisbiomass[, names(thisbiomass)]))
	lines(agg[,1], agg[,2], col='black', lwd=2)
	
	mtext('Year', side=1, line=2)
	mtext('Biomass index', side=2, line=2)

	dev.off()


#############################################################
## Plot time-series of summarized spp biomass and latitude ##
## across regions (if xreg)                                ##
## use common biomass y-axis if projtype=='_xreg'          ##
#############################################################

# plots by spp overall if projtype is xreg
if(projtype=='_xreg'){
	load(paste('data/meanlat,lon,biomassbyspp_', runtype, projtype, '_rcp', rcp, '.RData', sep='')) # biomasssumbyspp, meanlatbyspp, meanlonbyspp: projected biomass by year for each taxon across all regions


	## plots of change in biomass
		spps <- sort(unique(biomasssumbyspp$sppocean))
		length(spps)

		# quartz(width=10, height=8)
		pdf(file=paste('figures/biomasssumbyspp_proj_', runtype, projtype, '_rcp', rcp, '.pdf', sep=''), width=10, height=8)
		par(mfrow = c(6,6), mai=c(0.3, 0.3, 0.2, 0.05), cex.main=0.7, cex.axis=0.8, omi=c(0,0.2,0.1,0), mgp=c(2.8, 0.7, 0), font.main=3)

		rc <- 1 # row counter
		cc <- 0 # column counter

		options(warn=1) # print warnings as they occur
		for(i in 1:length(spps)){
			if(i %% 100 == 0) print(i)
			inds <- biomasssumbyspp$sppocean == spps[i]
			thisspp <- unique(biomasssumbyspp$sppocean[inds])

			# increment row and column counters as needed
			cc <- cc+1
			if(cc == 7){ cc <- 1; rc <- rc + 1}
			if(rc == 7){ cc <- 1; rc <- 1}
			
			# ylims
			ylims <- c(0, max(biomasssumbyspp[inds, grep('summwtcpue', names(biomasssumbyspp))], na.rm=TRUE)) # will warn if all values are NA
			if(is.infinite(ylims[2])) ylims <- c(0,1)

			# plot data for each GCM
			plot(biomasssumbyspp$year[inds], biomasssumbyspp$summwtcpue_1[inds], col='grey', las=1, type='l', ylim=ylims, main=thisspp)
			for(j in 2:13) lines(biomasssumbyspp$year[inds], biomasssumbyspp[[paste('summwtcpue', j, sep='_')]][inds], col='grey')
		
			# plot ensemble mean
			agg <- cbind(biomasssumbyspp$year[inds], rowMeans(biomasssumbyspp[inds, grep('summwtcpue', names(biomasssumbyspp))]))
			lines(agg[,1], agg[,2], col='black')
	
			if(cc==1) mtext(text='Biomass index', side=2, line=2.3, cex=0.6) # add y label on left of each row
		}

		dev.off()


	## plots of change in latitude
		spps <- sort(unique(meanlatbyspp$sppocean))
		length(spps)

		# quartz(width=10, height=8)
		pdf(file=paste('figures/meanlatbyspp_proj_', runtype, projtype, '_rcp', rcp, '.pdf', sep=''), width=10, height=8)
		par(mfrow = c(6,6), mai=c(0.3, 0.3, 0.2, 0.05), cex.main=0.7, cex.axis=0.8, omi=c(0,0.2,0.1,0), mgp=c(2.8, 0.7, 0), font.main=3)

		rc <- 1 # row counter
		cc <- 0 # column counter

		options(warn=1) # print warnings as they occur
		for(i in 1:length(spps)){
			if(i %% 100 == 0) print(i)
			inds <- meanlatbyspp$sppocean == spps[i]
			thisspp <- unique(meanlatbyspp$sppocean[inds])

			# increment row and column counters as needed
			cc <- cc+1
			if(cc == 7){ cc <- 1; rc <- rc + 1}
			if(rc == 7){ cc <- 1; rc <- 1}
			
			# ylims
			ylims <- range(meanlatbyspp[inds, grep('lat', names(meanlatbyspp))], na.rm=TRUE) # set ylims for this species in this region
			if(is.infinite(ylims[1])) ylims <- c(0,1)
	
			# plot data for each GCM
			plot(meanlatbyspp$year[inds], meanlatbyspp$lat_1[inds], col='grey', las=1, type='l', ylim=ylims, main=thisspp)
			for(j in 2:13) lines(meanlatbyspp$year[inds], meanlatbyspp[[paste('lat', j, sep='_')]][inds], col='grey')
		
			# plot ensemble mean
			agg <- cbind(meanlatbyspp$year[inds], rowMeans(meanlatbyspp[inds, grep('lat', names(meanlatbyspp))]))
			lines(agg[,1], agg[,2], col='black')
	
			if(cc==1) mtext(text='Mean latitude (°N)', side=2, line=2.3, cex=0.6) # add y label on left of each row

		}

		dev.off()
}




#################################################################
## Plot ensemble mean maps of spp projections by 20-year block ##
## each region separately                                      ##
#################################################################

# Summer flounder for rcp45####
load(paste('data/biomassavemap_', runtype, projtype, '_rcp', rcp, '.RData', sep=''))
	
sum45 <- biomassavemap[biomassavemap$sppocean=="paralichthys dentatus_Atl" & biomassavemap$region=="NEFSC_NEUSSpring",]

sum45$lonadj <- sum45$lon - 360

sum452000 <- sum45[sum45$period=="2006-2020",]
sum452041 <- sum45[sum45$period=="2041-2060",]
sum452081 <- sum45[sum45$period=="2081-2100",]

sum452000 <- data.frame(cbind(lat=sum452000$lat, lon=sum452000$lonadj, z=sum452000$wtcpue.proj))
sum452041 <- data.frame(cbind(lat=sum452041$lat, lon=sum452041$lonadj, z=sum452041$wtcpue.proj))
sum452081 <- data.frame(cbind(lat=sum452081$lat, lon=sum452081$lonadj, z=sum452081$wtcpue.proj))

sum452000 <- sum452000[!is.na(sum452000$z),]
sum452041 <- sum452041[!is.na(sum452041$z),]
sum452081 <- sum452081[!is.na(sum452081$z),]

coordinates(sum452000) <- ~ lon + lat
coordinates(sum452041) <- ~ lon + lat
coordinates(sum452081) <- ~ lon + lat

gridded(sum452041) <- TRUE; gridded(sum452000) <- TRUE; gridded(sum452081) <- TRUE

sum45rast00 <- raster(sum452000)
sum45rast41 <- raster(sum452041)
sum45rast81 <- raster(sum452081)

colfunc <- colorRampPalette(c('grey90', 'blue', 'purple', 'red1'))

png("~/Desktop/fluke_rcp45_2006-2020.png", width=4.2, height=3.5, units="in", res=150)
par(mfrow=c(1,1), mar=c(2.2,2.2,0,0), cex=1.2, oma=c(0.5,0.1,0.2,0.1), ps=8, mgp=c(1.2,.25,0), tcl=NA)
plot(sum45rast00, col=colfunc(20))
map(database="world", fill=T, col="gray", xlim=c(-78, -65), ylim=c(34, 50), mar=c(2,3,0,0), add=TRUE)
dev.off()

png("~/Desktop/fluke_rcp45_2041-2060.png", width=4.2, height=3.5, units="in", res=150)
par(mfrow=c(1,1), mar=c(2.2,2.2,0,0), cex=1.2, oma=c(0.5,0.1,0.2,0.1), ps=8, mgp=c(1.2,.25,0), tcl=NA)
plot(sum45rast41, col=colfunc(20))
map(database="world", fill=T, col="gray", xlim=c(-78, -65), ylim=c(34, 50), mar=c(2,3,0,0), add=TRUE)
dev.off()

png("~/Desktop/fluke_rcp45_2081-2100.png", width=4.2, height=3.5, units="in", res=150)
par(mfrow=c(1,1), mar=c(2.2,2.2,0,0), cex=1.2, oma=c(0.5,0.1,0.2,0.1), ps=8, mgp=c(1.2,.25,0), tcl=NA)
plot(sum45rast81, col=colfunc(20))
map(database="world", fill=T, col="gray", xlim=c(-78, -65), ylim=c(34, 50), mar=c(2,3,0,0), add=TRUE)
dev.off()

# Black sea bass for rcp45####
bsb45 <- biomassavemap[biomassavemap$sppocean=="centropristis striata_Atl" & biomassavemap$region=="NEFSC_NEUSSpring",]

bsb45$lonadj <- bsb45$lon - 360

bsb452000 <- bsb45[bsb45$period=="2006-2020",]
bsb452041 <- bsb45[bsb45$period=="2041-2060",]
bsb452081 <- bsb45[bsb45$period=="2081-2100",]

bsb452000 <- data.frame(cbind(lat=bsb452000$lat, lon=bsb452000$lonadj, z=bsb452000$wtcpue.proj))
bsb452041 <- data.frame(cbind(lat=bsb452041$lat, lon=bsb452041$lonadj, z=bsb452041$wtcpue.proj))
bsb452081 <- data.frame(cbind(lat=bsb452081$lat, lon=bsb452081$lonadj, z=bsb452081$wtcpue.proj))

bsb452000 <- bsb452000[!is.na(bsb452000$z),]
bsb452041 <- bsb452041[!is.na(bsb452041$z),]
bsb452081 <- bsb452081[!is.na(bsb452081$z),]

coordinates(bsb452000) <- ~ lon + lat
coordinates(bsb452041) <- ~ lon + lat
coordinates(bsb452081) <- ~ lon + lat

gridded(bsb452041) <- TRUE; gridded(bsb452000) <- TRUE; gridded(bsb452081) <- TRUE

bsb45rast00 <- raster(bsb452000)
bsb45rast41 <- raster(bsb452041)
bsb45rast81 <- raster(bsb452081)

colfunc <- colorRampPalette(c('grey90', 'blue', 'purple', 'red1'))

png("~/Desktop/bsb_rcp45_2006-2020.png", width=4.2, height=3.5, units="in", res=150)
par(mfrow=c(1,1), mar=c(2.2,2.2,0,0), cex=1.2, oma=c(0.5,0.1,0.2,0.1), ps=8, mgp=c(1.2,.25,0), tcl=NA)
plot(bsb45rast00, col=colfunc(20))
map(database="world", fill=T, col="gray", xlim=c(-78, -65), ylim=c(34, 50), mar=c(2,3,0,0), add=TRUE)
dev.off()

png("~/Desktop/bsb_rcp45_2041-2060.png", width=4.2, height=3.5, units="in", res=150)
par(mfrow=c(1,1), mar=c(2.2,2.2,0,0), cex=1.2, oma=c(0.5,0.1,0.2,0.1), ps=8, mgp=c(1.2,.25,0), tcl=NA)
plot(bsb45rast41, col=colfunc(20))
map(database="world", fill=T, col="gray", xlim=c(-78, -65), ylim=c(34, 50), mar=c(2,3,0,0), add=TRUE)
dev.off()

png("~/Desktop/bsb_rcp45_2081-2100.png", width=4.2, height=3.5, units="in", res=150)
par(mfrow=c(1,1), mar=c(2.2,2.2,0,0), cex=1.2, oma=c(0.5,0.1,0.2,0.1), ps=8, mgp=c(1.2,.25,0), tcl=NA)
plot(bsb45rast81, col=colfunc(20))
map(database="world", fill=T, col="gray", xlim=c(-78, -65), ylim=c(34, 50), mar=c(2,3,0,0), add=TRUE)
dev.off()

# Scup for rcp45####
scup45 <- biomassavemap[biomassavemap$sppocean=="stenotomus chrysops_Atl" & biomassavemap$region=="NEFSC_NEUSSpring",]

scup45$lonadj <- scup45$lon - 360

scup452000 <- scup45[scup45$period=="2006-2020",]
scup452041 <- scup45[scup45$period=="2041-2060",]
scup452081 <- scup45[scup45$period=="2081-2100",]

scup452000 <- data.frame(cbind(lat=scup452000$lat, lon=scup452000$lonadj, z=scup452000$wtcpue.proj))
scup452041 <- data.frame(cbind(lat=scup452041$lat, lon=scup452041$lonadj, z=scup452041$wtcpue.proj))
scup452081 <- data.frame(cbind(lat=scup452081$lat, lon=scup452081$lonadj, z=scup452081$wtcpue.proj))

scup452000 <- scup452000[!is.na(scup452000$z),]
scup452041 <- scup452041[!is.na(scup452041$z),]
scup452081 <- scup452081[!is.na(scup452081$z),]

coordinates(scup452000) <- ~ lon + lat
coordinates(scup452041) <- ~ lon + lat
coordinates(scup452081) <- ~ lon + lat

gridded(scup452041) <- TRUE; gridded(scup452000) <- TRUE; gridded(scup452081) <- TRUE

scup45rast00 <- raster(scup452000)
scup45rast41 <- raster(scup452041)
scup45rast81 <- raster(scup452081)

colfunc <- colorRampPalette(c('grey90', 'blue', 'purple', 'red1'))

png("~/Desktop/scup_rcp45_2006-2020.png", width=4.2, height=3.5, units="in", res=150)
par(mfrow=c(1,1), mar=c(2.2,2.2,0,0), cex=1.2, oma=c(0.5,0.1,0.2,0.1), ps=8, mgp=c(1.2,.25,0), tcl=NA)
plot(scup45rast00, col=colfunc(20))
map(database="world", fill=T, col="gray", xlim=c(-78, -65), ylim=c(34, 50), mar=c(2,3,0,0), add=TRUE)
dev.off()

png("~/Desktop/scup_rcp45_2041-2060.png", width=4.2, height=3.5, units="in", res=150)
par(mfrow=c(1,1), mar=c(2.2,2.2,0,0), cex=1.2, oma=c(0.5,0.1,0.2,0.1), ps=8, mgp=c(1.2,.25,0), tcl=NA)
plot(scup45rast41, col=colfunc(20))
map(database="world", fill=T, col="gray", xlim=c(-78, -65), ylim=c(34, 50), mar=c(2,3,0,0), add=TRUE)
dev.off()

png("~/Desktop/scup_rcp45_2081-2100.png", width=4.2, height=3.5, units="in", res=150)
par(mfrow=c(1,1), mar=c(2.2,2.2,0,0), cex=1.2, oma=c(0.5,0.1,0.2,0.1), ps=8, mgp=c(1.2,.25,0), tcl=NA)
plot(scup45rast81, col=colfunc(20))
map(database="world", fill=T, col="gray", xlim=c(-78, -65), ylim=c(34, 50), mar=c(2,3,0,0), add=TRUE)
dev.off()

#################################################
#Now for rcp85
#################################################
load(paste('data/biomassavemap_', runtype, projtype, '_rcp', rcp, '.RData', sep=''))

# Summer flounder for rcp85####
sum45 <- biomassavemap[biomassavemap$sppocean=="paralichthys dentatus_Atl" & biomassavemap$region=="NEFSC_NEUSSpring",]

sum45$lonadj <- sum45$lon - 360

sum452000 <- sum45[sum45$period=="2006-2020",]
sum452041 <- sum45[sum45$period=="2041-2060",]
sum452081 <- sum45[sum45$period=="2081-2100",]

sum452000 <- data.frame(cbind(lat=sum452000$lat, lon=sum452000$lonadj, z=sum452000$wtcpue.proj))
sum452041 <- data.frame(cbind(lat=sum452041$lat, lon=sum452041$lonadj, z=sum452041$wtcpue.proj))
sum452081 <- data.frame(cbind(lat=sum452081$lat, lon=sum452081$lonadj, z=sum452081$wtcpue.proj))

sum452000 <- sum452000[!is.na(sum452000$z),]
sum452041 <- sum452041[!is.na(sum452041$z),]
sum452081 <- sum452081[!is.na(sum452081$z),]

coordinates(sum452000) <- ~ lon + lat
coordinates(sum452041) <- ~ lon + lat
coordinates(sum452081) <- ~ lon + lat

gridded(sum452041) <- TRUE; gridded(sum452000) <- TRUE; gridded(sum452081) <- TRUE

sum45rast00 <- raster(sum452000)
sum45rast41 <- raster(sum452041)
sum45rast81 <- raster(sum452081)

colfunc <- colorRampPalette(c('grey90', 'blue', 'purple', 'red1'))

png("~/Desktop/fluke_rcp85_2006-2020.png", width=4.2, height=3.5, units="in", res=150)
par(mfrow=c(1,1), mar=c(2.2,2.2,0,0), cex=1.2, oma=c(0.5,0.1,0.2,0.1), ps=8, mgp=c(1.2,.25,0), tcl=NA)
plot(sum45rast00, col=colfunc(20))
map(database="world", fill=T, col="gray", xlim=c(-78, -65), ylim=c(34, 50), mar=c(2,3,0,0), add=TRUE)
dev.off()

png("~/Desktop/fluke_rcp85_2041-2060.png", width=4.2, height=3.5, units="in", res=150)
par(mfrow=c(1,1), mar=c(2.2,2.2,0,0), cex=1.2, oma=c(0.5,0.1,0.2,0.1), ps=8, mgp=c(1.2,.25,0), tcl=NA)
plot(sum45rast41, col=colfunc(20))
map(database="world", fill=T, col="gray", xlim=c(-78, -65), ylim=c(34, 50), mar=c(2,3,0,0), add=TRUE)
dev.off()

png("~/Desktop/fluke_rcp85_2081-2100.png", width=4.2, height=3.5, units="in", res=150)
par(mfrow=c(1,1), mar=c(2.2,2.2,0,0), cex=1.2, oma=c(0.5,0.1,0.2,0.1), ps=8, mgp=c(1.2,.25,0), tcl=NA)
plot(sum45rast81, col=colfunc(20))
map(database="world", fill=T, col="gray", xlim=c(-78, -65), ylim=c(34, 50), mar=c(2,3,0,0), add=TRUE)
dev.off()

# Black sea bass for rcp85####
bsb45 <- biomassavemap[biomassavemap$sppocean=="centropristis striata_Atl" & biomassavemap$region=="NEFSC_NEUSSpring",]

bsb45$lonadj <- bsb45$lon - 360

bsb452000 <- bsb45[bsb45$period=="2006-2020",]
bsb452041 <- bsb45[bsb45$period=="2041-2060",]
bsb452081 <- bsb45[bsb45$period=="2081-2100",]

bsb452000 <- data.frame(cbind(lat=bsb452000$lat, lon=bsb452000$lonadj, z=bsb452000$wtcpue.proj))
bsb452041 <- data.frame(cbind(lat=bsb452041$lat, lon=bsb452041$lonadj, z=bsb452041$wtcpue.proj))
bsb452081 <- data.frame(cbind(lat=bsb452081$lat, lon=bsb452081$lonadj, z=bsb452081$wtcpue.proj))

bsb452000 <- bsb452000[!is.na(bsb452000$z),]
bsb452041 <- bsb452041[!is.na(bsb452041$z),]
bsb452081 <- bsb452081[!is.na(bsb452081$z),]

coordinates(bsb452000) <- ~ lon + lat
coordinates(bsb452041) <- ~ lon + lat
coordinates(bsb452081) <- ~ lon + lat

gridded(bsb452041) <- TRUE; gridded(bsb452000) <- TRUE; gridded(bsb452081) <- TRUE

bsb45rast00 <- raster(bsb452000)
bsb45rast41 <- raster(bsb452041)
bsb45rast81 <- raster(bsb452081)

colfunc <- colorRampPalette(c('grey90', 'blue', 'purple', 'red1'))

png("~/Desktop/bsb_rcp85_2006-2020.png", width=4.2, height=3.5, units="in", res=150)
par(mfrow=c(1,1), mar=c(2.2,2.2,0,0), cex=1.2, oma=c(0.5,0.1,0.2,0.1), ps=8, mgp=c(1.2,.25,0), tcl=NA)
plot(bsb45rast00, col=colfunc(20))
map(database="world", fill=T, col="gray", xlim=c(-78, -65), ylim=c(34, 50), mar=c(2,3,0,0), add=TRUE)
dev.off()

png("~/Desktop/bsb_rcp85_2041-2060.png", width=4.2, height=3.5, units="in", res=150)
par(mfrow=c(1,1), mar=c(2.2,2.2,0,0), cex=1.2, oma=c(0.5,0.1,0.2,0.1), ps=8, mgp=c(1.2,.25,0), tcl=NA)
plot(bsb45rast41, col=colfunc(20))
map(database="world", fill=T, col="gray", xlim=c(-78, -65), ylim=c(34, 50), mar=c(2,3,0,0), add=TRUE)
dev.off()

png("~/Desktop/bsb_rcp85_2081-2100.png", width=4.2, height=3.5, units="in", res=150)
par(mfrow=c(1,1), mar=c(2.2,2.2,0,0), cex=1.2, oma=c(0.5,0.1,0.2,0.1), ps=8, mgp=c(1.2,.25,0), tcl=NA)
plot(bsb45rast81, col=colfunc(20))
map(database="world", fill=T, col="gray", xlim=c(-78, -65), ylim=c(34, 50), mar=c(2,3,0,0), add=TRUE)
dev.off()

# Scup for rcp85####
scup45 <- biomassavemap[biomassavemap$sppocean=="stenotomus chrysops_Atl" & biomassavemap$region=="NEFSC_NEUSSpring",]

scup45$lonadj <- scup45$lon - 360

scup452000 <- scup45[scup45$period=="2006-2020",]
scup452041 <- scup45[scup45$period=="2041-2060",]
scup452081 <- scup45[scup45$period=="2081-2100",]

scup452000 <- data.frame(cbind(lat=scup452000$lat, lon=scup452000$lonadj, z=scup452000$wtcpue.proj))
scup452041 <- data.frame(cbind(lat=scup452041$lat, lon=scup452041$lonadj, z=scup452041$wtcpue.proj))
scup452081 <- data.frame(cbind(lat=scup452081$lat, lon=scup452081$lonadj, z=scup452081$wtcpue.proj))

scup452000 <- scup452000[!is.na(scup452000$z),]
scup452041 <- scup452041[!is.na(scup452041$z),]
scup452081 <- scup452081[!is.na(scup452081$z),]

coordinates(scup452000) <- ~ lon + lat
coordinates(scup452041) <- ~ lon + lat
coordinates(scup452081) <- ~ lon + lat

gridded(scup452041) <- TRUE; gridded(scup452000) <- TRUE; gridded(scup452081) <- TRUE

scup45rast00 <- raster(scup452000)
scup45rast41 <- raster(scup452041)
scup45rast81 <- raster(scup452081)

colfunc <- colorRampPalette(c('grey90', 'blue', 'purple', 'red1'))

png("~/Desktop/scup_rcp85_2006-2020.png", width=4.2, height=3.5, units="in", res=150)
par(mfrow=c(1,1), mar=c(2.2,2.2,0,0), cex=1.2, oma=c(0.5,0.1,0.2,0.1), ps=8, mgp=c(1.2,.25,0), tcl=NA)
plot(scup45rast00, col=colfunc(20))
map(database="world", fill=T, col="gray", xlim=c(-78, -65), ylim=c(34, 50), mar=c(2,3,0,0), add=TRUE)
dev.off()

png("~/Desktop/scup_rcp85_2041-2060.png", width=4.2, height=3.5, units="in", res=150)
par(mfrow=c(1,1), mar=c(2.2,2.2,0,0), cex=1.2, oma=c(0.5,0.1,0.2,0.1), ps=8, mgp=c(1.2,.25,0), tcl=NA)
plot(scup45rast41, col=colfunc(20))
map(database="world", fill=T, col="gray", xlim=c(-78, -65), ylim=c(34, 50), mar=c(2,3,0,0), add=TRUE)
dev.off()

png("~/Desktop/scup_rcp85_2081-2100.png", width=4.2, height=3.5, units="in", res=150)
par(mfrow=c(1,1), mar=c(2.2,2.2,0,0), cex=1.2, oma=c(0.5,0.1,0.2,0.1), ps=8, mgp=c(1.2,.25,0), tcl=NA)
plot(scup45rast81, col=colfunc(20))
map(database="world", fill=T, col="gray", xlim=c(-78, -65), ylim=c(34, 50), mar=c(2,3,0,0), add=TRUE)
dev.off()



#sumFl45 = SpatialPixelsDataFrame(points=sum452041[c("lon", "lat")], data=sum452041)
#sumFl45 <- rasterFromXYZ(sumFl45)
#sumFL45B <- raster(sumFl45)

colfunc <- colorRampPalette(c('grey85', 'blue', 'red1'))


contour(abc, maxpixels=1000)

png("~/Desktop/sum45.png", width=4.2, height=3.5, units="in", res=150)
par(mfrow=c(1,1), mar=c(2.2,2.2,0,0), cex=1.2, oma=c(0.5,0.1,0.2,0.1), ps=8, mgp=c(1.2,.25,0), tcl=NA)
image(sum45[sum45$period=="2041-2060",], "wtcpue.proj", col=colfunc(20), xlim=c(-78, -65), ylim=c(35,45))
box()

map(database="state", fill=T, col="gray", xlim=c(-78, -65), ylim=c(34, 50), mar=c(2,3,0,0), add=TRUE)



spplot(sum45, "wtcpue.proj", col.regions=brewer.pal(9, "Spectral"), cuts=8)

cols <- colorRampPalette(c('grey80', 'blue', 'purple', 'red1'), interpolate='linear')

 


rng <- c(0, 1.01*max(sum45$wtcpue.proj, na.rm=TRUE)) # slightly expanded to capture all values

levelplot(wtcpue.proj ~ lonadj*lat, data=sum45[sum45$period=="2041-2060",], at=seq(rng[1], rng[2], length.out=20), colorkey=list(axis.text=list(cex=0.5)), col.regions=cols(100), main=list(label="Black sea bass, spring, 2041-2060", cex=1), ylab=list(label='Latitude', cex=0.5), xlab=list(label='Longitude', cex=0.5), scales=list(cex=0.5),1)# observed averaged biomass
map(database="world", fill=T, col="gray", xlim=c(-78, -60), ylim=c(34, 50), mar=c(2,3,0,0), add=TRUE)
map(database="state", fill=T, col="gray", xlim=c(-82.5, -74.5), ylim=c(27.3, 36.7), mar=c(2,3,0,0), add=TRUE)
axis(side=1, padj=-1.8, cex.axis=.7, tcl=-.2); axis(side=2, padj=1.8, cex.axis=.7, tcl=-.2)




sppreg <- biomassavemap[!duplicated(biomassavemap[,c('sppocean', 'region')]), c('sppocean', 'region')] # each spp/region combination to make maps for
	sppreg <- sppreg[order(sppreg$region, sppreg$sppocean),]
	nrow(sppreg)

# Make a set of plots on separate pages, one for each spp/region combination
options(warn=1) # print warnings as they occur
cols <- colorRampPalette(c('grey80', 'blue', 'purple', 'red1'), interpolate='linear')
periods <- sort(unique(biomassavemap$period))

pdf(width=10, height=3, file=paste('figures/biomass_proj_maps_', runtype, projtype, '_rcp', rcp, '.pdf', sep=''))

for(i in 1:nrow(sppreg)){
	print(paste(i, 'of', nrow(sppreg), Sys.time()))
	inds <- biomassavemap$sppocean == sppreg$sppocean[i] & biomassavemap$region == sppreg$region[i]
	maintitle <- paste(sppreg$region[i], sppreg$sppocean[i])

	if(!all(is.na(biomassavemap$wtcpue.proj[inds]))){
		rng <- c(0, 1.01*max(biomassavemap$wtcpue.proj[inds], na.rm=TRUE)) # slightly expanded to capture all values
		if(projtype=='_xreg'){
			inds2 <- biomassavemap$sppocean == sppreg$sppocean[i] # use all regions if projections were cross-region
			rng <- c(0, 1.01*max(biomassavemap$wtcpue.proj[inds2], na.rm=TRUE)) # slightly expanded to capture all values
		}

		thisplot <- levelplot(wtcpue.proj ~ lon*lat|period, data=biomassavemap[inds,], at=seq(rng[1], rng[2], length.out=20), colorkey=list(axis.text=list(cex=0.5)), col.regions=cols(100), main=list(label=maintitle, cex=1), ylab=list(label='lat', cex=0.5), xlab=list(label='lon', cex=0.5), scales=list(cex=0.5), layout = c(length(periods), 1)) # observed averaged biomass

		grid.arrange(thisplot) # plot on one page
	}	
}

dev.off()


################################################################
## Plot ensemble mean maps of spp projections by 20-year block
## all regions together (by ocean)
## used if projtype == '_xreg'
################################################################

if(projtype=='_xreg'){
	if(plotobs){
		load('data/dat_selectedspp.Rdata') # load dat data.frame. Has all trawl observations from all regions. wtcpue
		dat$lon[dat$lon<0] <- dat$lon[dat$lon<0] + 360 # reformat to match projections
	}

	load(paste('data/biomassavemap_', runtype, projtype, '_rcp', rcp, '.RData', sep=''))

	# drop one of the west coast regions, since they overlap
	#biomassavemap <- biomassavemap[biomassavemap$region != 'NWFSC_WCAnn',]
	biomassavemap <- biomassavemap[biomassavemap$region != 'AFSC_WCTri',]
	biomassavemap <- droplevels(biomassavemap)
	
	spps <- sort(unique(biomassavemap$sppocean)) # each spp to make maps for
		length(spps)

	# Make a set of ocean-scale plots on separate pages, one for each spp
	options(warn=1) # print warnings as they occur
	cols <- colorRampPalette(c('grey80', 'blue', 'purple', 'red1'), interpolate='linear')
	periods <- sort(unique(biomassavemap$period))

	pdf(width=10, height=3, file=paste('figures/biomass_proj_mapscontinent_', runtype, projtype, '_rcp', rcp, '.pdf', sep=''))
	#quartz(width=10, height=3)

	for(i in 1:length(spps)){
		print(paste(i, 'of', length(spps), Sys.time()))
		maintitle <- spps[i]

		inds <- biomassavemap$sppocean == spps[i]
		mydat <- biomassavemap[inds,c('lat', 'lon', 'period', 'wtcpue.proj')]

		# expand with NAs
		zs <- expand.grid(list(lat=seq(min(mydat$lat), max(mydat$lat), by=0.25), lon=seq(min(mydat$lon), max(mydat$lon), by=0.25), period=sort(unique(mydat$period))))
		matches <- paste(zs$lat, zs$lon) %in% paste(mydat$lat, mydat$lon) # find rows that match mydat
		zs <- zs[!matches,] # remove the matching rows
		zs$wtcpue.proj <- NA
		mydat <- rbind(mydat,zs)
			# then fill wtcpue.proj with 0 or NA
			# then rbind onto mydat
		
		

		if(!all(is.na(mydat$wtcpue.proj))){
			rng <- c(0, 1.01*max(mydat$wtcpue.proj, na.rm=TRUE)) # slightly expanded to capture all values

			thisplot <- levelplot(wtcpue.proj ~ lon*lat|period, data=mydat, at=seq(rng[1], rng[2], length.out=20), colorkey=list(axis.text=list(cex=0.5)), col.regions=cols(100), main=list(label=maintitle, cex=1), ylab=list(label='lat', cex=0.5), xlab=list(label='lon', cex=0.5), scales=list(cex=0.5), layout = c(length(periods), 1)) # projected averaged biomass

			if(plotobs){
				obsinds <- dat$sppocean == spps[i] & dat$wtcpue > 0 # select where present
				latrng <- range(mydat$lat, na.rm=TRUE)
				lonrng <- range(mydat$lon, na.rm=TRUE)
				obsplot <- xyplot(lat ~ lon, data=dat[obsinds,], xlim=lonrng, ylim=latrng, main='Observations', pch=16, cex=0.2, scales=list(cex=0.5), ylab=list(label='lat', cex=0.5), xlab=list(label='lon', cex=0.5), par.settings=list(layout.widths=list(right.padding=-3))) # plot of presences
				grid.arrange(obsplot, thisplot, nrow=1, widths=c(1.2,length(periods))) # plot on one page with observations
		
			} else {
				grid.arrange(thisplot) # plot on one page without observations
			}
		}	
	}

	dev.off()
}