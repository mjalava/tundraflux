# creates masks for the study area
# contact: matti.kummu@aalto.fi

library(terra)
library(tidyverse)



# set working directory the path that this script is located in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# soil

SOC_crs3995 <- rast("data_in/SOCC30_circumpolar_crs3995.tif")
crs3995 <- crs(SOC_crs3995)



# permafrost area
# https://apgc.awi.de/dataset/pex/resource/18fe2e48-fe60-4b0b-9469-7cb361542f61

pf_prob_crs3995 <-  project(rast("data_in/UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH/UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH.tif"), 
                            crs3995)
# extend soil extent

SOC_crs3995_ext <- extend(SOC_crs3995,pf_prob_crs3995)
writeRaster(x = SOC_crs3995_ext, filename = "data_out/SOC_crs3995_ext.tif", gdal="COMPRESS=LZW",overwrite=T)

pf_prob_crs3995_soil <-  project(rast("data_in/UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH/UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH.tif"), 
                            SOC_crs3995_ext)

pf_prob_crs3995_soil[pf_prob_crs3995_soil>0] = 1
writeRaster(x = pf_prob_crs3995_soil, filename = "data_out/pf_prob_crs3995.tif", gdal="COMPRESS=LZW",overwrite=T)
pf_prob_crs3995 <- rast("data_out/pf_prob_crs3995.tif")

# land mask

land_mask <- !is.na(rast("data_in/UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH/UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH.tif"))
land_mask_crs3995 <-  project(land_mask, SOC_crs3995_ext)

# tundra mask

# http://137.229.50.172/geodata/arcticAtlasMaster.gdb

maskTundra <- vect("data_in/cp_veg_la_shp/cp_veg_la.shp") %>% 
  project(land_mask_crs3995) %>% 
  terra::rasterize(land_mask_crs3995,"PHYSIOG", na.rm =T) 

# alpine mask
# source: https://onlinelibrary.wiley.com/doi/full/10.1111/ecog.05012
# https://figshare.com/articles/dataset/Global_distribution_and_bioclimatic_characterization_of_alpine_biomes/11710002

zonesAlpine <- vect('data_in/alpine_clusters/alpine_clusters.shp')
r_zonesAlpine <- rasterize(zonesAlpine,SOC_wgs84,field='cluster')
r_zonesAlpine[r_zonesAlpine > 0] <- 10
r_zonesAlpine_crs3995 <- project(x=r_zonesAlpine,SOC_crs3995_ext)

writeRaster(x = r_zonesAlpine_crs3995, filename = "data_out/alpine_zones.tif", gdal="COMPRESS=LZW",overwrite=T)

maskTundraAlpine <- maskAlpine
maskTundraAlpine[maskTundra > 0] = 1
maskTundraAlpine[maskTundraAlpine > 0] = 1

plot(maskTundraAlpine)

writeRaster(x = maskTundraAlpine,
            filename = "data_out/maskTundraAlpine_crsEPSG3995.tif", gdal="COMPRESS=LZW",overwrite=T)

# project to wgs84

maskTundraAlpine_crsWGS84 <- project(maskTundraAlpine, "EPSG:4326")

writeRaster(x = maskTundraAlpine_crsWGS84,
            filename = "data_out/maskTundraAlpine_crsEPSG4326.tif", gdal="COMPRESS=LZW",overwrite=T)


