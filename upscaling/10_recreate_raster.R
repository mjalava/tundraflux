library(terra)

results <- readRDS("mxt_res_1.rds")
for(i in 2:40){
  print(paste0("processing file ", i))
  filename <- paste0("mxt_res_", i, ".rds")
  toappend <- readRDS(filename)
  colnames(toappend) <- colnames(results)
  results <- rbind(results, toappend)
}

saveRDS(results, file="results_mean_sim.rds")

# recreate the raster

r_mask <- rast("r_mask.tif")
r_mask_b <- r_mask
r_mask_b[is.na(r_mask_b)] <- 0

r_mean <- r_mask
r_mean[r_mask_b] <- results$mean
writeRaster(r_mean, "mean_sim.tif")

r_sd <- r_mask
r_sd[r_mask_b] <- results$mixed_sd
writeRaster(r_sd, "sd_mean_sim.tif")


# for the uncertainty runs
r_sd_of_means <- r_mask
r_sd_of_means[r_mask_b] <- results$sd_of_means
writeRaster(r_sd_of_means, "sd_of_means.tif")
