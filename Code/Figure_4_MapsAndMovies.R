library(gpclib)
library(foreign)
library(maptools)
library(RColorBrewer)
library(ggplot2)
library(mgcv)
library(splancs)
library(rgeos)
library(grid)
library(sp)
library(rgdal)	
library(geoR)
library(maps)
library(raster)
library(plyr)

## Source code for analysis
source("./Code/FunctionsForAnalysis.R")
source("./Code/FunctionsForMaps.R")

## Read in the master list of country codes
codes <- as.data.frame(read.csv("./Data/Master_List.csv"))

################################################################################

## GAM mean/SE map at binned_survey_age

test_ages <- c(12, 18, 24, 60)
for(age in test_ages) {

	pdf(paste0("./Figures/Map_", age, "m_Mean.pdf"), height=8, width=6)	
	getSmoothedMap_noSubSIA(binned_survey_age=age, MeanOrSE="Mean", movie=FALSE)
	dev.off()

	pdf(paste0("./Figures/Map_", age, "m_SE.pdf"), height=8, width=6)
	getSmoothedMap_noSubSIA(binned_survey_age=age, MeanOrSE="SE", deltaSEfrom12=FALSE, movie=FALSE)
	dev.off()
	
}

################################################################################


## Coldspots map


## Coldspots significance map

test_ages <- c(12, 18, 24, 60)
coverage <- 80
for(age in test_ages) {

	pdf(paste0("./Figures/Map_", age, "m_", coverage, "p_Coldspots.pdf"), height=8, width=6)
	getSmoothedMap_noSubSIA_ColdSpots(binned_survey_age=age, cutoff_percent=coverage/100, movie=FALSE)
	dev.off()

	pdf(paste0("./Figures/Map_", age, "m_", coverage, "p_Coldspots_Significance.pdf"), height=8, width=6)
	getSmoothedMap_noSubSIA_Coldspots_Significance(binned_survey_age=age, cutoff_percent=coverage/100)
	dev.off()

}

################################################################################


## Map of the prevalence of grid cells as coldspots, between lower_age and upper_age

lower_age <- 12
upper_age <- 60
coverage <- 80

pdf(paste0("./Figures/Map_", lower_age, "-", upper_age, "m_Coldspots_Prevalence.pdf"), height=8, width=6)
getSmoothedMap_noSubSIA_Coldspots_Prevalence(lower_age=lower_age, upper_age=upper_age, cutoff_percent=coverage/100)
dev.off()

## Same as above, filtering population size of under 5 year olds (cutoff_popsize)

lower_age <- 12
upper_age <- 60
coverage <- 80
cutoff_popsize <- c(100, 500, 1000)
for(popsize in cutoff_popsize) {

	pdf(paste0("./Figures/Map_", lower_age, "-", upper_age, "m_", coverage, "p_Coldspots_Prevalence_", popsize, "pop.pdf"), height=8, width=6)
	getSmoothedMap_noSubSIA_Coldspots_Prevalence_Pop(lower_age=lower_age, upper_age=upper_age, cutoff_percent=coverage/100, cutoff_popsize=popsize)
	dev.off()
	
}

################################################################################


## Map of the number of people between [0, 60] months of age

options(scipen=999)

pdf("./Figures/Map_WorldPop.pdf", height=8, width=6)
getSmoothedMap_WorldPop()
dev.off()


## Map of the number of unvaccinated people between [lower_age, upper_age] months of age

lower_age <- 6
test_upper_ages <- c(12, 18, 24, 60)
for(age in test_upper_ages) {

	pdf(paste0("./Figures/Map_", lower_age, "-", age, "m_Unvaccinateds.pdf"), height=8, width=6)
	getSmoothedMap_Unvaccinateds(lower_age=lower_age, upper_age=age)
	dev.off()

}



################################################################################

## Movies of means and coldspots by age

for(m in 6:60){

	png(file=paste0("./Movies/Means_", sprintf("%02d", m), ".png"), res=600, width=480*10, height=480*10)
	getSmoothedMap_noSubSIA(binned_survey_age=m, MeanOrSE="Mean", deltaSEfrom12=FALSE, movie=TRUE)
	dev.off()

}

coverage <- 80

for(m in 6:60){

	png(file=paste0("./Movies/Coldspots_", sprintf("%02d", m), ".png"), res=600, width=480*10, height=480*10)
	getSmoothedMap_noSubSIA_ColdSpots(binned_survey_age=m, cutoff_percent=coverage/100, movie=TRUE)
	dev.off()

}

################################################################################