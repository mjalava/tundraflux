# AFTER SIMULATING SOIL VARIABLES AND SLICING THEM, THIS SCRIPT SHOULD
# BE RUN ON A CLUSTER TO PERFORM MODEL PREDICTION FOR EACH OF THE 
# RASTER CELLS. 
# The related slurm batch script is startmod.sh

library(metafor)
library(data.table)
library(dplyr)

args = commandArgs(trailingOnly=TRUE)
slice <- args[1]

CNname <- paste0("dt_v_CN_n100_",slice,".rds")
TNname <- paste0("dt_v_TN_n100_",slice,".rds")

dt_v_TN_n100 <- readRDS(TNname)
dt_v_CN_n100 <- readRDS(CNname)

load(file="CNboots.rdata")
boots_rep <- as.data.table (dplyr::slice( as.data.table (matrix(boots, ncol=100)), rep(1:n(), each = nrow(dt_v_CN_n100))), ncol = 100, )
dt_v_CN_n100_boots <- dt_v_CN_n100 * boots_rep


dt_result <- cbind(dt_v_CN_n100_boots, dt_v_TN_n100)*0

load('MR_model_multi_apr2023.RData')


for(i in 1:100){
  print(i)
  v_crs3995 <- data.frame(cbind(dt_v_TN_n100[,..i], dt_v_CN_n100_boots[,..i]))
  names(v_crs3995) <- c("SON_Min_CTL", "CN_Min_CTL")
  
  X_mod <- model.matrix(~ SON_Min_CTL+ CN_Min_CTL, data=v_crs3995)
  
  X_mod <- X_mod[,-1] # leave out interception
  
  # predict
  
  pred_v_crs3995 <- predict(multimodel, newmods=X_mod)
  
  meancol <- i*2-1
  sdcol <- i*2
  
  dt_result[,meancol] <- pred_v_crs3995$pred
  dt_result[,sdcol] <- pred_v_crs3995$se
  
}

saveRDS(dt_result, file=paste0("result_", slice, ".rds"))
