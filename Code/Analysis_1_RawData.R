library(foreign)
library(maptools)
library(RColorBrewer)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)

# Source code for analysis
source("./Code/FunctionsForAnalysis.R")

# Read in the master list of country codes
codes <- as.data.frame(read.csv("./Data/Master_List.csv"))
	
################################################################################


# Get the pre-processed data for all countries
for(i in 1:length(codes$Country)) getDataByInd_Cluster(codes=codes, input_country=codes$Country[i])

# Get the individual-level SIA data in addition to this, for GAMs
for(i in 1:length(codes$Country)) {

	# Load pre-processed data
	load(paste0("./Data/Manipulated_data/", codes$DHS_code[which(codes$Country==codes$Country[i])], ".RData"))



	# If survey_age_ub_mos < 1, add 0.0001 (this is so that the ages range from 1-60)
  dataInd_country <- dataInd
	dataInd_country$survey_age_ub_mos[which(dataInd_country$survey_age_ub_mos<1)] <- dataInd_country$survey_age_ub_mos[which(dataInd_country$survey_age_ub_mos<1)] + 0.0001
		
	# Round up survey age
	dataInd_country$binned_survey_age <- ceiling(dataInd_country$survey_age_ub_mos)

	# Link up long/lat from dataClust to dataInd_country
	dataInd_country$long <- dataClust$long[match(dataInd_country$cluster_id,dataClust$cluster_id)]
	dataInd_country$lat <- dataClust$lat[match(dataInd_country$cluster_id,dataClust$cluster_id)]

	# Remove places where long/lat are NA
	dataInd_country <- dataInd_country[which(!is.na(dataInd_country$lat)),]


	
	# Remove children less than 6 months of age at survey
	dataInd_country <- dataInd_country[which(dataInd_country$binned_survey_age>=6),]
	


	dataInd_country_subnational <- dataInd_country
	
	# Save the data
	save(dataInd_country_subnational, dataInd_country, file=paste0("./Data/Manipulated_data_for_GAM/", codes$DHS_code[which(codes$Country==codes$Country[i])], ".RData"))

}

################################################################################