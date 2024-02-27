# prepare data for the analysis. This script finds where the mineral layer
# begins, calculates the weighted means of the soil variables over the layer and 
# projects them in crs3995.
# This script takes in the 1 km resolution tif rasters and writes out
# the weighted means. In the analysis, this section was run on a laptop under
# RStudio as the virtual memory was not available on the cluster and the
# memory allocation required would have been impractical.
# In practice, it may be necessary to run the different variables separately.

library(terra)
library(sf)
library(tidyverse)
library(openxlsx)

# set working directory the path that this script is located in
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# (not set, as this won't work outside rstudio)

# for manual operations:
# combLayers <- readRDS("combLayers.rds")
# SOC_layers <- readRDS("SOC_layers.rds")

terraOptions(memmax = 50) # max memory used in gigabytes. 

# create functions

f_layers <- function(r_data,threshold, direction){
  
  rLayers <- subset(r_data,1)
  
  if (direction < 0) {
    
    # check when turn to mineral layer
    rLayers[subset(r_data,1) < threshold] <- -1
    rLayers[rLayers > 0 & subset(r_data,2) < threshold] <- -2
    rLayers[rLayers > 0 & subset(r_data,3) < threshold] <- -3
    rLayers[rLayers > 0 & subset(r_data,4) < threshold] <- -4
    rLayers[rLayers > 0] <- -5
  } else{
    rLayers[subset(r_data,1) < threshold] <- -1
    rLayers[rLayers > 0 & subset(r_data,2) > threshold] <- -2
    rLayers[rLayers > 0 & subset(r_data,3) > threshold] <- -3
    rLayers[rLayers > 0 & subset(r_data,4) > threshold] <- -4
    rLayers[rLayers > 0] <- -5
    
  }
  return(rLayers)
}


f_meanLayers <- function(r_data,r_layers){
  rMean <- subset(r_data,4)
  rMean <- rMean*NA
  rMean <- rast(rMean, nlyrs=5)
  
  for (d in 1:5) {
    print(d)
    temp_mean <- sum(c(matLayerWeight[d,1] * subset(r_data,1),
                       matLayerWeight[d,2] * subset(r_data,2),
                       matLayerWeight[d,3] * subset(r_data,3),
                       matLayerWeight[d,4] * subset(r_data,4))) /
      sum(matLayerWeight[d,1],matLayerWeight[d,2],matLayerWeight[d,3],matLayerWeight[d,4])
    
    rMean[[d]] <- temp_mean
  }
  
  return(rMean)
}


## need to be done only once

if (file.exists("data_out/soil_data_TN_CN_crs3995.tif")) {
  soilData <- rast("data_out/soil_data_TN_CN_crs3995.tif")# if true, do nothing
} else {
  # create it
  
  # read raster data - crs54052
  
  # SOC dg/kg (1 dg = 10 cg)
  SOC <- rast(c('soc_0-5cm_mean_1km_na.rm.tif',
                'soc_5-15cm_mean_1km_na.rm.tif',
                'soc_15-30cm_mean_1km_na.rm.tif',
                'soc_30-60cm_mean_1km_na.rm.tif')) * 0.01 # to cg/g


  # Nitrogen cg/kg
  TN <- rast(c('nitrogen_0-5cm_mean_1km_na.rm.cropped.tif',
               'nitrogen_5-15cm_mean_1km_na.rm.cropped.tif',
               'nitrogen_15-30cm_mean_1km_na.rm.cropped.tif',
               'nitrogen_30-60cm_mean_1km_na.rm.cropped.tif')) * 0.001 # to cg/g


  # bulk density cg/cm3
  BD <- rast(c('bdod_0-5cm_mean_1km_na.rm.tif',
               'bdod_5-15cm_mean_1km_na.rm.tif',
               'bdod_15-30cm_mean_1km_na.rm.tif',
               'bdod_30-60cm_mean_1km_na.rm.tif')) * 0.01


  # layers weighting (due to different depth of the layers, we need to weight those when taking mean)
  
  matLayerWeight <- rbind(c(1,2,3,6),
                          c(0,2,3,6),
                          c(0,0,3,6),
                          c(0,0,0,6),
                          c(0,0,0,6))
  
  
  
  # identify mineral layers (only with mean layer)
  
  SOC_layers <- f_layers(SOC,10,-1)
 
  TN_layers <- f_layers(TN,1,-1)
 
  BD_layers <- f_layers(BD,1,1)
 
  # lets use same layering for all variables, and thus lets get the max (as negative, min) layer depth for each grid cell
                   
  combLayers <- min(c(SOC_layers,TN_layers,BD_layers))
  plot(combLayers)
  
  # Load Q05 and Q95 rasters
  
  SOC_Q05 <- rast(c('soc_0-5cm_Q0.05_1km_na.rm.tif',
                    'soc_5-15cm_Q0.05_1km_na.rm.tif',
                    'soc_15-30cm_Q0.05_1km_na.rm.tif',
                    'soc_30-60cm_Q0.05_1km_na.rm.tif')) * 0.01
  
  SOC_Q95 <- rast(c('soc_0-5cm_Q0.95_1km_na.rm.tif',
                    'soc_5-15cm_Q0.95_1km_na.rm.tif',
                    'soc_15-30cm_Q0.95_1km_na.rm.tif',
                    'soc_30-60cm_Q0.95_1km_na.rm.tif')) * 0.01
  
  TN_Q05 <- rast(c('nitrogen_0-5cm_Q0.05_1km_na.rm.cropped.tif',
                   'nitrogen_5-15cm_Q0.05_1km_na.rm.cropped.tif',
                   'nitrogen_15-30cm_Q0.05_1km_na.rm.cropped.tif',
                   'nitrogen_30-60cm_Q0.05_1km_na.rm.cropped.tif')) * 0.001
  
  TN_Q95 <- rast(c('nitrogen_0-5cm_Q0.95_1km_na.rm.cropped.tif',
                   'nitrogen_5-15cm_Q0.95_1km_na.rm.cropped.tif',
                   'nitrogen_15-30cm_Q0.95_1km_na.rm.cropped.tif',
                   'nitrogen_30-60cm_Q0.95_1km_na.rm.cropped.tif')) * 0.001
  
  BD_Q05 <- rast(c('bdod_0-5cm_Q0.05_1km_na.rm.tif',
                   'bdod_5-15cm_Q0.05_1km_na.rm.tif',
                   'bdod_15-30cm_Q0.05_1km_na.rm.tif',
                   'bdod_30-60cm_Q0.05_1km_na.rm.tif')) * 0.01
  
  BD_Q95 <- rast(c('bdod_0-5cm_Q0.95_1km_na.rm.tif',
                   'bdod_5-15cm_Q0.95_1km_na.rm.tif',
                   'bdod_15-30cm_Q0.95_1km_na.rm.tif',
                   'bdod_30-60cm_Q0.95_1km_na.rm.tif')) * 0.01
  
  
  # mean over layers
  # this produces now 5-layer rasters, separate for each matrix row
  # they need to be combined with combLayers using selectRange
  SOCmean <- f_meanLayers(SOC,combLayers)
  SOCmean_Q05 <- f_meanLayers(SOC_Q05,combLayers)
  SOCmean_Q95 <- f_meanLayers(SOC_Q95,combLayers)
  
  TNmean <- f_meanLayers(TN,combLayers)
  TNmean_Q05 <- f_meanLayers(TN_Q05,combLayers)
  TNmean_Q95 <- f_meanLayers(TN_Q95,combLayers)
  
  # comblayers was saved manually
  
  combLayers <- readRDS("combLayers.rds") # read previously saved original combLayers
  combLayers_round <- round(combLayers)*(-1) # round to get "integers", convert to positive numbers
  SOCmean <- rast("SOCmean_5lyrs.tif") # read the SOCmean 5-layer result from f_meanLayers
  SOCmean <- selectRange(SOCmean, combLayers_round) # use selectRange to pick correct cells
  writeRaster(SOCmean, "SOCmean.tif") # write the raster in file
 
  # repeat for other variables
  
  SOCmean_Q05_5 <- rast("SOCmean_Q05_5lyrs.tif")
  SOCmean_Q05 <- selectRange(SOCmean_Q05_5, combLayers_round)
  writeRaster(SOCmean_Q05, "SOCmean_Q05.tif")
  
  SOCmean_Q95_5 <- rast("SOCmean_Q95_5lyrs.tif")
  SOCmean_Q95 <- selectRange(SOCmean_Q95_5, combLayers_round)
  writeRaster(SOCmean_Q95, "SOCmean_Q95.tif")
  
  TNmean5 <- rast("TNmean_5lyrs.tif")
  TNmean <- selectRange(TNmean5, combLayers_round)
  writeRaster(TNmean, "TNmean.tif")
  
  TNmean_Q05_5 <- rast("TNmean_Q05_5lyrs.tif")
  TNmean_Q05 <- selectRange(TNmean_Q05_5, combLayers_round)
  writeRaster(TNmean_Q05, "TNmean_Q05.tif")
  
  TNmean_Q95_5 <- rast("TNmean_Q95_5lyrs.tif")
  TNmean_Q95 <- selectRange(TNmean_Q95_5, combLayers_round)
  writeRaster(TNmean_Q95, "TNmean_Q95.tif")
  
  # calculate CN
  
  CNmean <- SOCmean / TNmean
  CNmean_Q05 <- SOCmean_Q05 / TNmean_Q05
  CNmean_Q95 <-  SOCmean_Q95 / TNmean_Q95
  
  writeRaster(CNmean, "CNmean.tif")
  writeRaster(CNmean_Q05, "CNmean_Q05.tif")
  writeRaster(CNmean_Q95, "CNmean_Q95.tif")
  
  
  #plot(CNmean)
  
  # project rasters
  
  # land mask
  land_mask_crs3995 <- rast("land_mask_crs3995.tif")
  
  
  TNmean_crs3995 <- project(TNmean,land_mask_crs3995)
  writeRaster(TNmean_crs3995, "TNmean_crs3995.tif")
  TNmean_Q05_crs3995 <- project(TNmean_Q05,land_mask_crs3995)
  writeRaster(TNmean_Q05_crs3995, "TNmean_Q05_crs3995.tif")
  TNmean_Q95_crs3995 <- project(TNmean_Q95,land_mask_crs3995)
  writeRaster(TNmean_Q95_crs3995, "TNmean_Q95_crs3995.tif")
  
  CNmean_crs3995 <- project(CNmean,land_mask_crs3995)
  writeRaster(CNmean_crs3995, "CNmean_crs3995.tif")
  CNmean_Q05_crs3995 <- project(CNmean_Q05,land_mask_crs3995)
  writeRaster(CNmean_Q05_crs3995, "CNmean_Q05_crs3995.tif")
  CNmean_Q95_crs3995 <- project(CNmean_Q95,land_mask_crs3995)
  writeRaster(CNmean_Q95_crs3995, "CNmean_Q95_crs3995.tif")
  
  
  
  writeRaster(x = c(TNmean_crs3995,TNmean_Q05_crs3995,TNmean_Q95_crs3995, CNmean_crs3995, CNmean_Q05_crs3995,CNmean_Q95_crs3995 ),
              filename = "soil_data_TN_CN_crs3995.tif", gdal="COMPRESS=LZW",overwrite=T)
  
}











