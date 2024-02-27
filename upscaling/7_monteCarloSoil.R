# Monte Carlo simulation of the soil parameters. 

library(terra)
library(sf)
library(tidyverse)
library(openxlsx)
library(data.table)
library(truncnorm)
library(tictoc)
library(lavaan)
library(mvtnorm)

##### read data ----

# land mask
land_mask_crs3995 <- rast("land_mask_crs3995.tif")
land_mask_crs3995[land_mask_crs3995==0] <- NA

# study area mask
 
mask_TundraAlp <- rast("mask_TundraAlp_crs3995.tif")

# read soil data

soilData <- rast("soil_data_TN_CN_crs3995_2.tif")

TNmean_crs3995 <- subset(soilData,1)
TNq05_crs3995 <- subset(soilData,2)
TNq95_crs3995 <- subset(soilData,3)

# create mask to map area for which all variables have values
r_mask <- !is.na(TNmean_crs3995) &  !is.na(CNmean_crs3995) & !is.na(mask_TundraAlp) & !is.na(land_mask_crs3995) & 
   !is.na(TNq05_crs3995) & !is.na(TNq95_crs3995) & !is.na(CNq05_crs3995) & !is.na(CNq95_crs3995)
r_mask[r_mask == 0] <- NA
 
writeRaster(r_mask, filename = "r_mask.tif")

# load CN ratio between calculated CN (used here) and observed bootstrapped values (variable is boots)
load(file="CNboots.rdata")

# ####. data to vector ----

myFun_toVector <- function(rastIn){
  vectOut <- rastIn[r_mask] %>% as.data.frame()
  return(vectOut)
}
 
# TN
 
df_TNmean_crs3995 <- readRDS("df_TNmean_crs3995.rds")
df_TNmean_Q05_crs3995 <- readRDS("df_TNmean_Q05_crs3995.rds")
df_TNmean_Q95_crs3995 <- readRDS("df_TNmean_Q95_crs3995.rds")

# calculate stand dev
df_sdTN=(df_TNmean_Q95_crs3995-df_TNmean_Q05_crs3995)/(2*qnorm(.95))

v_TN <- base::data.frame(TNmean = df_TNmean_crs3995, TNsd = df_sdTN)
names(v_TN) <- c("mean", "sd")

# create 100 datasets

seed = 21
n <- 100
v_TN_n100 <- Map(function(x, y) (rtruncnorm(n, a = 0.005, b = 10, mean = x, sd = y)), (v_TN$mean), (v_TN$sd))
dt_v_TN_n100 <- data.table(matrix(unlist(v_TN_n100), nrow=length(v_TN_n100), byrow=TRUE),stringsAsFactors=FALSE)
saveRDS(dt_v_TN_n100, "dt_v_TN_n100.rds")
 
# SOC
 
df_SOCmean_crs3995 <- readRDS("df_SOCmean_crs3995.rds")
df_SOCmean_Q05_crs3995 <- readRDS("df_SOCmean_Q05_crs3995.rds")
df_SOCmean_Q95_crs3995 <- readRDS("df_SOCmean_Q95_crs3995.rds")
 
df_sdSOC=(df_SOCmean_Q95_crs3995-df_SOCmean_Q05_crs3995)/(2*qnorm(.95))
v_SOC <- base::data.frame(SOCmean = df_SOCmean_crs3995, SOCsd = df_sdSOC)
names(v_SOC) <- c("mean", "sd")

# correlation between TN and SOC means in all data points

cor_result_pearson <- as.numeric(cor(v_TN["mean"], v_SOC["mean"], method="pearson"))

# replace with correlation from measurement data if wanted, 
# cor_measured_data <- 0.9205431

corr_sim <- function(socmean, socsd, tnmean, tnsd, n){
  cors <- matrix(c(1, cor_result_pearson, cor_result_pearson, 1), nrow=2)
  v <- lavaan::cor2cov(cors, sds=c(socsd, tnsd))
  c_n <- mvtnorm::rmvnorm(n, mean=c(socmean, tnmean), sigma = v)
  c_n <- cbind(c_n, c_n[,1]/c_n[,2])
  return(c_n)
}

# now create the datasets for CN

seed = 21

n <- 100

df_in <- data.frame(v_SOC$mean, v_SOC$sd, v_TN$mean, v_TN$sd, n)
names(df_in)<-c("socmean", "socsd", "tnmean", "tnsd", "n")
v_CN_n100 <- pmap(df_in, corr_sim)

# to data.table

dt_v_CN_n100 <- data.table(matrix(unlist(v_CN_n100), nrow=length(v_CN_n100), byrow=TRUE),stringsAsFactors=FALSE)

# repeat boots values for each row of dt_v_CN_n100
boots_rep <- as.data.table (dplyr::slice( as.data.table (matrix(boots, ncol=100)), rep(1:n(), each = nrow(v_CN))), ncol = 100, )

# multiply each column with the boots-data
dt_v_CN_n100_boots <- boots_rep * dt_v_CN_n100

# save data.table

saveRDS(dt_v_CN_n100_boots, "dt_v_CN_n100_boots.rds")








