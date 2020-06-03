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
library(tidyr)

# Source code for analysis
source("./Code/FunctionsForAnalysis.R")

# Read in the master list of country codes
codes <- as.data.frame(read.csv("./Data/Master_List.csv"))

################################################################################

# Get a country's grid cells from WorldPop, with numbers of lt5y in 2010 as well
# Resolution of WorldPop is 0.00833333 decimal degrees (approx 1km at the equator) 
# We aggregate this up by a factor of 10, meaning 0.0833333 decimal degrees (approx 10 km)

# Weights generaged using UNICEF yearly country population proportions estimates. Using 2020 estimates for each age group
apply.weights <- read.csv("./Data/WorldPop/popEstimate.csv")

for(i in 1:nrow(codes)) {

	# Which country?
	ADM_code <- as.character((codes$ADM_code[i]))
	country.name <- as.character(codes$Country[i])
	
	country.weight <- apply.weights%>%
	  dplyr::filter(Nation == country.name)
	
	
	#If it's NGA
	if(ADM_code =="NGA"){
	  
	  adm0 <- readShapeSpatial(paste0("./Data/ADM/", ADM_code, "_adm/", ADM_code, "_adm0_.shp"))
	  
	  adm0 <- adm0[adm0$ADM0_NAME=="NIGERIA",]
	  
	  #Male, 2020, 0-12 months
	  
	  country_tiff_0_12m <- paste0("./Data/WorldPop/", tolower(ADM_code), "_m_0_2020.tif")
	  country_m_0_12m <- raster(country_tiff_0_12m)
	  country_0_12m <- mask(country_m_0_12m, adm0)
	  
	  country_tiff_12_59m <- paste0("./Data/WorldPop/", tolower(ADM_code), "_m_1_2020.tif")
	  country_m_12_59m <- raster(country_tiff_12_59m)
	  country_m_12_59m <- mask(country_m_12_59m, adm0)
	  
	  #Female, 2020, 0-12 months
	  country_tiff_0_12f <- paste0("./Data/WorldPop/", tolower(ADM_code), "_f_0_2020.tif")
	  country_m_0_12f <- raster(country_tiff_0_12f)
	  country_0_12f <- mask(country_m_0_12f, adm0)
	  
	  country_tiff_12_59f <- paste0("./Data/WorldPop/", tolower(ADM_code), "_f_1_2020.tif")
	  country_m_12_59f <- raster(country_tiff_12_59f)
	  country_m_12_59f <- mask(country_m_12_59f, adm0)
	  
	  #Combine Male and female 
	  
	  country_0_12mf <- overlay(country_0_12m, country_0_12f, fun = sum)
	  country_12_59mf <- overlay(country_m_12_59m, country_m_12_59f, fun = sum)
	  
	  country_12_59_mf_adjusted <- country.weight$Estimate*country_12_59mf
	  
	  #Combine all ages
	  
	  country_0_59mf <- overlay(country_0_12mf, country_12_59_mf_adjusted, fun = sum)
	  
	  #Aggregate up
	  country_0_59mf_agg <- raster::aggregate(country_0_59mf, fun=sum, fact=100)
	  country_0_59mf_agg_df <- as.data.frame(country_0_59mf_agg, xy=TRUE)
	  
	  
	  # Get rid of NA cells
	  country_0_59mf_agg_df_complete <- country_0_59mf_agg_df[complete.cases(country_0_59mf_agg_df),]
	  
	  
	  grid_tmp_lt5y <- data.frame(long=country_0_59mf_agg_df_complete$x, 
	                              lat=country_0_59mf_agg_df_complete$y,
	                              lt5y=country_0_59mf_agg_df_complete$layer)
	  
	  # # Males, 2010, 0-59 months
	  # worldpop_tiff_m <- "./Data/WorldPop/ap10v4_A0005_M_adj.tif"
	  # continent_m <- raster(worldpop_tiff_m)
	  # continent_m_country <- crop(continent_m, extent(adm0))
	  # continent_m_country <- mask(continent_m_country, adm0)
	  # 
	  # # Females, 2010, 0-59 months
	  # worldpop_tiff_f <- "./Data/WorldPop/ap10v4_A0005_F_adj.tif"
	  # continent_f <- raster(worldpop_tiff_f)
	  # continent_f_country <- crop(continent_f, extent(adm0))
	  # continent_f_country <- mask(continent_f_country, adm0)
	  # 
	  # # Combine M + F
	  # continent_country_mf_lt5y <- overlay(continent_m_country, continent_f_country, fun=sum)
	  # continent_country_mf_lt5y_df <- as.data.frame(continent_country_mf_lt5y, xy=TRUE)
	  # 
	  # # Aggregate up
	  # continent_country_mf_lt5y_agg <- raster::aggregate(continent_country_mf_lt5y, fun=sum, fact=10)
	  # continent_country_mf_lt5y_agg_df <- as.data.frame(continent_country_mf_lt5y_agg, xy=TRUE)
	  # 
	  # # Get rid of NA cells
	  # continent_country_mf_lt5y_agg_df_complete <- continent_country_mf_lt5y_agg_df[complete.cases(continent_country_mf_lt5y_agg_df),]
	  # 
	  # grid_tmp_lt5y <- data.frame(long=continent_country_mf_lt5y_agg_df_complete$x, 
	  # 							lat=continent_country_mf_lt5y_agg_df_complete$y,
	  # 							lt5y=continent_country_mf_lt5y_agg_df_complete$layer)
	  
	  # Save data
	  save(grid_tmp_lt5y, file=paste0("./Results/Imputed_WorldPop/", codes$DHS_code[i], ".RData"))
	  
	}
	
	else{
	  
	  adm0 <- readShapeSpatial(paste0("./Data/ADM/", ADM_code, "_adm/", ADM_code, "_adm0_.shp"))
	  
	  #Male, 2020, 0-12 months
	  
	  country_tiff_0_12m <- paste0("./Data/WorldPop/", tolower(ADM_code), "_m_0_2020.tif")
	  country_m_0_12m <- raster(country_tiff_0_12m)
	  country_0_12m <- mask(country_m_0_12m, adm0)
	  
	  country_tiff_12_59m <- paste0("./Data/WorldPop/", tolower(ADM_code), "_m_1_2020.tif")
	  country_m_12_59m <- raster(country_tiff_12_59m)
	  country_m_12_59m <- mask(country_m_12_59m, adm0)
	  
	  #Female, 2020, 0-12 months
	  country_tiff_0_12f <- paste0("./Data/WorldPop/", tolower(ADM_code), "_f_0_2020.tif")
	  country_m_0_12f <- raster(country_tiff_0_12f)
	  country_0_12f <- mask(country_m_0_12f, adm0)
	  
	  country_tiff_12_59f <- paste0("./Data/WorldPop/", tolower(ADM_code), "_f_1_2020.tif")
	  country_m_12_59f <- raster(country_tiff_12_59f)
	  country_m_12_59f <- mask(country_m_12_59f, adm0)
	  
	  #Combine Male and female 
	  
	  country_0_12mf <- overlay(country_0_12m, country_0_12f, fun = sum)
	  country_12_59mf <- overlay(country_m_12_59m, country_m_12_59f, fun = sum)
	  
	  country_12_59_mf_adjusted <- country.weight$Estimate*country_12_59mf
	  
	  #Combine all ages
	  
	  country_0_59mf <- overlay(country_0_12mf, country_12_59_mf_adjusted, fun = sum)
	  
	  #Aggregate up
	  country_0_59mf_agg <- raster::aggregate(country_0_59mf, fun=sum, fact=100)
	  country_0_59mf_agg_df <- as.data.frame(country_0_59mf_agg, xy=TRUE)
	  
	  
	  # Get rid of NA cells
	  country_0_59mf_agg_df_complete <- country_0_59mf_agg_df[complete.cases(country_0_59mf_agg_df),]
	  
	  
	  grid_tmp_lt5y <- data.frame(long=country_0_59mf_agg_df_complete$x, 
	                              lat=country_0_59mf_agg_df_complete$y,
	                              lt5y=country_0_59mf_agg_df_complete$layer)
	  
	  # # Males, 2010, 0-59 months
	  # worldpop_tiff_m <- "./Data/WorldPop/ap10v4_A0005_M_adj.tif"
	  # continent_m <- raster(worldpop_tiff_m)
	  # continent_m_country <- crop(continent_m, extent(adm0))
	  # continent_m_country <- mask(continent_m_country, adm0)
	  # 
	  # # Females, 2010, 0-59 months
	  # worldpop_tiff_f <- "./Data/WorldPop/ap10v4_A0005_F_adj.tif"
	  # continent_f <- raster(worldpop_tiff_f)
	  # continent_f_country <- crop(continent_f, extent(adm0))
	  # continent_f_country <- mask(continent_f_country, adm0)
	  # 
	  # # Combine M + F
	  # continent_country_mf_lt5y <- overlay(continent_m_country, continent_f_country, fun=sum)
	  # continent_country_mf_lt5y_df <- as.data.frame(continent_country_mf_lt5y, xy=TRUE)
	  # 
	  # # Aggregate up
	  # continent_country_mf_lt5y_agg <- raster::aggregate(continent_country_mf_lt5y, fun=sum, fact=10)
	  # continent_country_mf_lt5y_agg_df <- as.data.frame(continent_country_mf_lt5y_agg, xy=TRUE)
	  # 
	  # # Get rid of NA cells
	  # continent_country_mf_lt5y_agg_df_complete <- continent_country_mf_lt5y_agg_df[complete.cases(continent_country_mf_lt5y_agg_df),]
	  # 
	  # grid_tmp_lt5y <- data.frame(long=continent_country_mf_lt5y_agg_df_complete$x, 
	  # 							lat=continent_country_mf_lt5y_agg_df_complete$y,
	  # 							lt5y=continent_country_mf_lt5y_agg_df_complete$layer)
	  
	  # Save data
	  save(grid_tmp_lt5y, file=paste0("./Results/Imputed_WorldPop/", codes$DHS_code[i], ".RData"))
	  
	}
	
	

}

################################################################################