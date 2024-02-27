# ONLY FOR SLICING THE SOIL SIMULATION RESULTS, MANUALLY SELECT THE VARIABLES IN THE LOOP

library(data.table)

dt_v_CN_n100_boots <- readRDS("dt_v_CN_n100_boots.rds")
dt_v_TN_n100 <- readRDS("dt_v_TN_n100.rds")

ncores <- 40

datalen <- nrow(dt_v_CN_n100_boots)
slicelen <- ceiling(datalen/ncores)

for(i in 1:ncores){
  if(i < ncores){ # the bulk of the slices before the last one
    lbound <- (i - 1) * slicelen + 1
    ubound <- i * slicelen
  }
  else{ # the last slice
    lbound <- (i - 1) * slicelen + 1
    ubound <- datalen
  }
  slice_CN <- dt_v_CN_n100_boots[lbound:ubound,]
  slice_TN <- dt_v_TN_n100[lbound:ubound,]
  CNname <- paste0("slice_CN_cndelta_",i,".rds")
  TNname <- paste0("slice_TN_cndelta_",i,".rds")
  saveRDS(slice_CN, file = CNname)
  saveRDS(slice_TN, file = TNname)
}
