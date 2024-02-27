library(terra)

# These scripts (three almost identical for soc, bdod and nitrogen) are
# separated so that it's easy to run them in parallel. Nitrogen has a different
# extent, so it is cropped first using a bdod tif file to specify the extent.

names <- c("nitrogen_0-5cm_mean_250m.tif",
           "nitrogen_15-30cm_mean_250m.tif",
           "nitrogen_30-60cm_mean_250m.tif",
           "nitrogen_5-15cm_mean_250m.tif",
           "nitrogen_0-5cm_Q0.05_250m.tif",
           "nitrogen_0-5cm_Q0.95_250m.tif",
           "nitrogen_15-30cm_Q0.05_250m.tif",
           "nitrogen_15-30cm_Q0.95_250m.tif",
           "nitrogen_30-60cm_Q0.05_250m.tif",
           "nitrogen_30-60cm_Q0.95_250m.tif",
           "nitrogen_5-15cm_Q0.05_250m.tif",
           "nitrogen_5-15cm_Q0.95_250m.tif")


# read a bdod file and determine extent
b05_250 <- rast("bdod_0-5cm_mean_250m.tif")
extent <- ext(b05_250)
rm(b05_250)

for(name in names){
  r <- rast(name)
  r <- crop(r, extent)
  r_agg <- aggregate(r, fact=4, cores=2, fun=mean, na.rm=TRUE)
  writeRaster(r_agg, gsub("250m", "1km_na.rm.cropped", name), filetype="GTiff", overwrite = FALSE)
}

