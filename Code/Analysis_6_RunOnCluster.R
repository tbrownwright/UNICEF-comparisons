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

## Read in the master list of country codes
codes <- as.data.frame(read.csv("./Data/Master_List.csv"))

	
################################################################################

## Note: the code below takes too much computation time to run on a single computer
## Currently organized to run on a cluster ("Della" at Princeton)

## ./Results/GAMtoMeanAndSE_Della
## Mean and SE of GAM output between [lower_age, upper_age] months of age at each grid cell
## Output: one file per country

getGAM_Map_noSubSIA <- function(lower_age, upper_age, i) {

	# Load the GAM ouput
	load(paste0("./Results/GAM_fit_phenom/", codes$DHS_code[i], ".RData"))

	# Get a data.frame of grid cells over the country at a certain grid size
	load(paste0("./Results/Imputed_WorldPop/", codes$DHS_code[i], ".RData"))

	# Get border lines to add
	adm0 <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[i], "_adm/", codes$ADM_code[i], "_adm0.shp"))
	
	# For each age
	for(j in lower_age:upper_age) {

		# Predict for certain values of covariates
		newd <- data.frame(long=grid_tmp_lt5y$long, lat=grid_tmp_lt5y$lat, 
						   binned_survey_age=j,
						   SIA_1=0, SIA_2=0, SIA_3=0, SIA_4=0, SIA_5=0, SIA_6=0,
						   SIA_7=0, SIA_8=0, SIA_9=0, SIA_10=0, SIA_11=0, SIA_12=0,
						   SIA_13=0, SIA_14=0, SIA_15=0, SIA_16=0, SIA_17=0,
						   SIA_18=0, SIA_19=0, SIA_20=0, SIA_21=0, SIA_22=0)	

		# Predict at grid cells
		pred <- predict.gam(fit_phenom, newd, se.fit=TRUE)
		
		# Get the Mean and SE of prediction
		grid_tmp_lt5y[,paste0("Mean_", j, "m")] <- pred$fit
		grid_tmp_lt5y[,paste0("SE_", j, "m")] <- pred$se.fit
		
	}
	
	# Convert the data points to RasterLayer --> SPDF for manipulation
	g <- rasterFromXYZ(grid_tmp_lt5y)
	g <- as(g, "SpatialPolygonsDataFrame")

	# Get the intersection of the SPDF and adm0 border, which produces a SpatialPolygons
	g <- rgeos::gIntersection(g, adm0, byid=TRUE)		

	# Convert back to SPDF
	g <- as(g, "SpatialPolygonsDataFrame")
	g@data$id <- rownames(g@data)
	
	# Add all the data back 	
	data_to_add <- cbind(grid_tmp_lt5y[,paste0("Mean_", lower_age:upper_age, "m")], grid_tmp_lt5y[,paste0("SE_", lower_age:upper_age, "m")])
	rownames(data_to_add) <- g@data$id
	g <- spCbind(g, data_to_add)
	
	# Save the SPDF as a data.frame to plot
	g@data$id <- rownames(g@data)
	g_points <- ggplot2::fortify(g, region="id")	
	g_df <- plyr::join(g_points, g@data, by="id")

	# Save the .RData
	save(grid_tmp_lt5y, g, g_df, file=paste0("./Results/GAMtoMeanAndSE_Della/GAMtoMeanAndSE_", codes$DHS_code[i], "_", lower_age, "-", upper_age, "m", ".RData"))

}

for(i in 1:3){

  getGAM_Map_noSubSIA(lower_age=6, upper_age=60, i)  
  
}

################################################################################

## ./Results/GAMtoPeople_Della
## Number of unvaccinated people between [lower_age, upper_age] months of age at each grid cell
## Output: one file per country

getPeopleMap_noSubSIA <- function(lower_age, upper_age, i) {
  
  # Load the GAM ouput
  load(paste0("./Results/GAM_fit_phenom/", codes$DHS_code[i], ".RData"))
  
  # Get a data.frame of grid cells over the country at a certain grid size
  load(paste0("./Results/Imputed_WorldPop/", codes$DHS_code[i], ".RData"))
  grid_tmp_lt5y$z <- 0
  
  # Get border lines to add
  adm0 <- readShapeSpatial(paste0("./Data/ADM/", codes$ADM_code[i], "_adm/", codes$ADM_code[i], "_adm0.shp"))
  
  # For each age
  for(j in lower_age:upper_age) {
    
    # Predict for certain values of covariates
    newd <- data.frame(long=grid_tmp_lt5y$long, lat=grid_tmp_lt5y$lat, 
                       binned_survey_age=j,
                       SIA_1=0, SIA_2=0, SIA_3=0, SIA_4=0, SIA_5=0, SIA_6=0,
                       SIA_7=0, SIA_8=0, SIA_9=0, SIA_10=0, SIA_11=0, SIA_12=0,
                       SIA_13=0, SIA_14=0, SIA_15=0, SIA_16=0, SIA_17=0,
                       SIA_18=0, SIA_19=0, SIA_20=0, SIA_21=0, SIA_22=0)	
    
    # Predict at grid cells
    pred <- predict.gam(fit_phenom, newd)
    
    # Get the cumulative number of unvaccinateds
    grid_tmp_lt5y$z <- grid_tmp_lt5y$z + ((1-plogis(pred)) * grid_tmp_lt5y$lt5y/60)
    
    # Get the proportion unvaccinated in that monthly age group
    grid_tmp_lt5y[,paste0("p_unvacc_", j, "m")] <- 1-plogis(pred)
    
  }
  
  # Convert the data points to RasterLayer --> SPDF for manipulation
  g <- rasterFromXYZ(grid_tmp_lt5y[, c("long", "lat", "z")])
  g <- as(g, "SpatialPolygonsDataFrame")	
  
  # Get the intersection of the SPDF and adm0 border, which produces a SpatialPolygons
  g <- rgeos::gIntersection(g, adm0, byid=TRUE)		
  
  # Get the area of each grid cell in the SP, so that we can reduce the number of unvaccinated people between [lower_age, upper_age] months of age, by the % of that cell that is actually in that country
  area <- NULL
  for(k in 1:length(g)) area[k] <- g@polygons[[k]]@Polygons[[1]]@area
  area <- area/max(area)
  
  # Convert back to SPDF, and add a new column for the ADJUSTED number of unvaccinated people between [lower_age, upper_age] months of age, which we call 'dummy' 	
  g <- as(g, "SpatialPolygonsDataFrame")	
  g$dummy <- grid_tmp_lt5y$z * area
  
  # Save the SPDF as a data.frame to plot
  g@data$id <- rownames(g@data)
  g_points <- ggplot2::fortify(g, region="id")	
  g_df <- plyr::join(g_points, g@data, by="id")
  
  # Save the .RData
  save(grid_tmp_lt5y, g, g_df, area, file=paste0("./Results/GAMtoPeople_Della/GAMtoPeople_", codes$DHS_code[i], "_", lower_age, "-", upper_age, "m", ".RData"))
  
}

for(i in 1:length(codes$Country)){
  
  getPeopleMap_noSubSIA(lower_age=6, upper_age=60, i)  
  
}

