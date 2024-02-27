
# produces the baseline and modelled respiration
# tabulated results

# code for Fig 6, SI Fig 6, SI Fig 7

# contact: matti.kummu@aalto.fi


library(sf)
library(terra)
library(Rfast)
library(data.table)
library(zoo)

library(openxlsx) #
library(readxl)
library(tidyverse)
library(dplyr) 


# set working directory the path that this script is located in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### read the predROM and errROM (done with super computer)

r_predROM_1km <- rast('results/combined_mean_mixture2.tif')
r_errROM_1km <- rast('results/combined_sd_mixture2.tif')

###!! to get only data uncertainty, use this 
# r_errROM_1km <- rast('results/sd_of_means_mxt_res4.tif')


# soil organic carbon, for reference raster
SOC_crs3995 <- rast("data_out/SOC_crs3995_ext.tif")

# mask to right projection

if (file.exists("data_out/maskTundraAlpine_crs3995.tif")) {
  # if true, read it
  r_tundraAlpMask_crs3995 <- rast("data_out/maskTundraAlpine_crs3995.tif")
} else { # create it
  r_tundraAlpMask_wgs84 <- rast('data_out/maskTundraAlpine_crsEPSG4326.tif')
  r_tundraAlpMask_crs3995 <- project(r_tundraAlpMask_wgs84, r_errROM_1km)
  writeRaster(x = r_tundraAlpMask_crs3995,
              filename = "data_out/maskTundraAlpine_crs3995.tif", gdal="COMPRESS=LZW",overwrite=T)
}


# percentage change

# backtransformed to get the actual response ratio by doing exp(ROM). And then the percentage change is through 100 * (RR - 1).

r_predPERCchange_1km <-  exp(r_predROM_1km) - 1

writeRaster(r_predPERCchange_1km,"results/test.tif",  gdal="COMPRESS=LZW",overwrite=TRUE)
writeRaster(r_predPERCchange_1km+1,"results/test_1.tif",  gdal="COMPRESS=LZW",overwrite=TRUE)

r_errPERCchange_1km <-  exp(r_errROM_1km) - 1

# then calculate respiration change
# resp in g m-2 yr-1, equals to t km-2 yr-1


# respiration


# soil resipiration
# data from https://daac.ornl.gov/CMS/guides/CMS_Global_Soil_Respiration.html

if (file.exists("data_out/resp_soil_CRS3995_1km.tif")) {
  # if true, read it
  resp_soil_CRS3995_1km <- rast("data_out/resp_soil_CRS3995_1km.tif")
} else {
  
  # in gC m-2 yr-1
  resp <- rast('data_in/CMS_Global_Soil_Respiration_1736/data/soil_resp_mean_quantile_regress_forest.tif')
  resp_soil_CRS3995_1km <- project(resp,SOC_crs3995)
  
  writeRaster(x = resp_soil_CRS3995_1km,
              filename = "data_out/resp_soil_CRS3995_1km.tif", gdal="COMPRESS=LZW",overwrite=T)
}




if (file.exists("data_out/resp_plant_CRS3995_1km.tif")) {
  # if true, read it
  resp_plant_CRS3995_1km <- rast("data_out/resp_plant_CRS3995_1km.tif")
  
} else {
  library(foreach)
  library(parallel)
  library(doParallel)
  #plants
  # https://www.nature.com/articles/s41467-017-01774-z
  # https://catalogue.ceh.ac.uk/datastore/eidchub/24489399-5c99-4050-93ee-58ac4b09341a/GlobalRespirationJULESRd25plusbcplusacclim/
  files_resp_plant <- list.files(path = 'data_in/plant_respiration')
  
  nc_data <- ncdf4::nc_open(paste0('data_in/plant_respiration/', files_resp_plant[1]))
  
  cl<-makeCluster(6)
  registerDoParallel(cl)
  
  # read all 34 GCM forced plant respiration maps
  resp_plant <- foreach::foreach(y = 1:length(files_resp_plant), 
                                 .combine='c',
                                 .packages=c('dplyr','terra','sf', 'raster', 'ncdf4')) %dopar%
    {
      
      nc_data <- ncdf4::nc_open(paste0('data_in/plant_respiration/', files_resp_plant[y]))
      
      resp_p_gb <- ncdf4::ncvar_get(nc_data, "resp_p_gb")
      #dim(resp_p_gb) 
      
      lon <- ncdf4::ncvar_get(nc_data, "longitude")
      lat <- ncdf4::ncvar_get(nc_data, "latitude", verbose = F)
      
      r <- raster::raster(t(resp_p_gb[,,1]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), 
                          crs=sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
      r <- flip(r, direction='y')
      
    }
  
  # median over all 34 GCMs
  
  r_resp_plant <- median(rast(raster::stack(resp_plant)), na.rm = T)
  
  # function to fill NA areas with closest value
  fillmode <- function(v, na.rm) {
    if (is.na(v[[5]])) {
      uniqv <- unique(v)
      uniqv <- uniqv[!is.na(uniqv)]
      fillval <- uniqv[which.max(tabulate(match(v, uniqv)))]
      return (fillval)
    } else {
      return (v[[5]])
    }
    
  }
  # run focal with the fillmode function
  r_resp_plant_focal <- terra::focal(r_resp_plant, w = 3, fun = fillmode, na.rm = FALSE)
  
  ref_raster_1arcmin <- rast(ncol=360*60, nrow=180*60)
  r_resp_plant_focal_30arcsec <- resample(r_resp_plant_focal, ref_raster_1arcmin, method = "bilinear")
  
  resp_plant_CRS3995_1km <- project(r_resp_plant_focal_30arcsec, r_predROM_1km)
  
  resp_plant_CRS3995_1km[is.na(resp_plant_CRS3995_1km)] <- NA
  

  # from kg m-2 s-1 to gC m-2 yr-1
  resp_plant_CRS3995_1km <- resp_plant_CRS3995_1km * (365*24*3600 * 1000)
 
  writeRaster(x = resp_plant_CRS3995_1km,
              filename = "data_out/resp_plant_CRS3995_1km.tif", gdal="COMPRESS=LZW",overwrite=T)
}


# combine soil and plant respiration
baseResp <-  resp_soil_CRS3995_1km + resp_plant_CRS3995_1km


# calculate modelled respiration (change * baseline)

modResp <- (r_predPERCchange_1km+1) * baseResp
changeResp <- r_predPERCchange_1km * baseResp

baseResp_masked <- baseResp
baseResp_masked[is.na(r_predPERCchange_1km)] = NA


# multiply with 1e-9 to get to Pg yr-1 
(total_resp <- global(c(baseResp_masked,modResp,changeResp)*1e-9, fun="sum", na.rm=T))


# standard error

r_errPERCchange_1km_crop <- crop(r_errPERCchange_1km,ext(baseResp_masked))
modErrResp_crop <- (r_errPERCchange_1km+1) * baseResp_masked
changeErrResp_crop <- r_errPERCchange_1km * baseResp_masked



# For Tundra only

maskTundra_1km <- vect("data_in/cp_veg_la_shp/cp_veg_la.shp") %>% 
  project(SOC_crs3995) %>% 
  terra::rasterize(SOC_crs3995,"PHYSIOG", na.rm =T) 

maskTundra_1km[maskTundra_1km>0] = 1

# tabulated results for tundra
(total_resp_Tundra <- global(c(maskTundra_1km*baseResp_masked,
                               maskTundra_1km*modResp,
                               maskTundra_1km*changeResp)*1e-9, fun="sum", na.rm=T))

(total_errResp_Tundra <- global(c(maskTundra_1km*baseResp_masked,
                                  maskTundra_1km*modErrResp_crop,
                                  maskTundra_1km*changeErrResp_crop)*1e-9, fun="sum", na.rm=T))

# create results text
text_Tundra_std_dev <- paste0("Tundra: baseline respiration is ",round(total_resp_Tundra[1,1],2), 
                              " PgC yr-1 and the respiration under warming ",
                              round(total_resp_Tundra[2,1],2), " (increase of ", 
                              round( total_resp_Tundra[3,1],2) ," with std.error of ",round(total_errResp_Tundra[3,1],2), 
                              ") PgC yr-1. The increase is thus ", 
                              100*round(total_resp_Tundra[3,1] / total_resp_Tundra[1,1],2),"%"
)


# tabulated results for tundra and alpine


# tabulated results
(total_resp_TundraAlpine <- global(c(baseResp_masked,
                                     modResp,
                                     changeResp)*1e-9, fun="sum", na.rm=T))

(total_errResp_TundraAlpine <- global(c(baseResp_masked,
                                        modErrResp_crop,
                                        changeErrResp_crop)*1e-9, fun="sum", na.rm=T))


text_TundraAlpine_std_dev <- paste0("Tundra-alpine: baseline respiration is ",round(total_resp_TundraAlpine[1,1],2), 
                                    " PgC yr-1 and the respiration under warming ",
                                    round(total_resp_TundraAlpine[2,1],2), " (increase of ", 
                                    round( total_resp_TundraAlpine[3,1],2) ," with std.error of ",round(total_errResp_TundraAlpine[3,1],2), 
                                    ") PgC yr-1. The increase is thus ", 
                                    100*round(total_resp_TundraAlpine[3,1] / total_resp_TundraAlpine[1,1],2),"%"
)

# combine texts
text_comb <- bind_rows(as.data.frame(text_TundraAlpine_std_dev),as.data.frame(text_Tundra_std_dev))

# write out text
write_csv(text_comb, "results/results_text_28jun2023.csv")

# write out rasters

# respiration change 
writeRaster(changeResp,"results/changeResp_tundraAlpine.tif",  gdal="COMPRESS=LZW",overwrite=TRUE)

# percentage change
writeRaster(r_predPERCchange_1km,"results/percentageChangeResp_tundraAlpine.tif",  gdal="COMPRESS=LZW",overwrite=TRUE)

#  base resp
writeRaster(baseResp_masked,"results/baseResp_tundraAlpine.tif",  gdal="COMPRESS=LZW",overwrite=TRUE)

#  resp under warming
writeRaster(modResp,"results/modResp_tundraAlpine.tif",  gdal="COMPRESS=LZW",overwrite=TRUE)

#  standard error
writeRaster(changeErrResp_crop,"results/standError_tundraAlpine.tif",  gdal="COMPRESS=LZW",overwrite=TRUE)


### plotting ----

library(tmap)
library(scico)
library(sf)

SOC_crs3995 <- rast("data_out/SOC_crs3995_ext.tif")

# country borders from natural earth data
shp_cntry <- st_read("data_in/ne_50m_adm0_all_ids/adm0_NatEarth_all_ids.shp")
# simplify the shapefile
shp_cntrySml <- rmapshaper::ms_simplify(shp_cntry, keep = 0.2, keep_shapes = T) %>%
  st_as_sf() 

ext_SSA_vect <- ext(-7e6,8e6,-7e6,7e6)

shp_cntrySml_crs3995 <- vect(shp_cntrySml) %>% 
  project(.,SOC_crs3995) %>% 
  crop(.,ext_SSA_vect)%>% 
  st_as_sf()


##### function to plot -------

# Draw a map of a chose variable
#
# @param r_index: a raster to be plotted
# @param index_label: label that will be placed as the legend title
# @param colorpal: color palette to be used, ordered from low to high value
# @param breakvals: break values to be drawn in legend
# @param color_midpoint: TRUE if the color scale has a midpoint, NULL by default
#                        (no midpoint in color scale)
# @param tocrs: proj4 string of CRS to which the raster is projected if given
#               (by default, no projection is done)
#
# @return tmap object of the index given
f_mappingTMAP <- function(r_index, index_label, colorpal, breakvals,
                          color_midpoint = NULL,
                          polygonFile, bboxSet){
  
  
  
  # create tmap object
  index_map <- tm_shape(r_index, bbox = bboxSet) +
    tm_raster(style = "fixed", # draw gradient instead of classified
              palette = colorpal,
              breaks = breakvals,
              title = index_label,
              midpoint = color_midpoint,
              #legend.reverse = TRUE,
              legend.is.portrait = FALSE) +
    tm_shape(polygonFile, bbox = bboxSet) +
    tm_borders(col=NA,lwd = 0.5) +
    tm_layout(main.title.position = "left",
              legend.bg.color = TRUE,
              legend.outside = TRUE,
              legend.outside.position = "bottom",
              frame = FALSE)
  
  return (index_map)
  
}


#### plots ------

# read in files

# percentage change
r_predPERCchange_1km <-  rast("results/percentageChangeResp_tundraAlpine.tif")


# respiration change 
changeResp <-  rast("results/changeResp_tundraAlpine.tif")
#  base resp
baseResp <-  rast("results/baseResp_tundraAlpine.tif")
#  resp under warming
modResp <-  rast("results/modResp_tundraAlpine.tif")

# standard error
changeErrResp_crop <-  rast("results/standError_tundraAlpine.tif")


# define colour paletters

ROM_pal <- scico(n = 3, palette = "lajolla", begin = 0, end = 0.9, direction = 1)
CHANGE_pal <- scico(n = 3, palette = "roma", begin = 0, end = 1, direction = 1)
resp_pal <- scico(n = 3, palette = "lajolla", begin = 0, end = 1, direction = 1)
bboxTundraAlpine <- c(-5.2e6,-5.2e6, 7.5e6,5e6)



plt_baseResp <- f_mappingTMAP(r_index = aggregate(baseResp, fact = 2, fun = mean, na.rm = T),
                              index_label = "base Resp [gC m-2 yr-1]",
                              colorpal = resp_pal,
                              breakvals = seq(0, 500, 50),
                              #color_midpoint = 0,
                              polygonFile = shp_cntrySml_crs3995,
                              bboxSet = bboxTundraAlpine)


plt_modResp <- f_mappingTMAP(r_index = aggregate(modResp, fact = 2, fun = mean, na.rm = T),
                             index_label = "after warming +1.5°C Resp [gC m-2 yr-1]",
                             colorpal = resp_pal,
                             breakvals = seq(0, 500, 50),
                             #color_midpoint = 0,
                             polygonFile = shp_cntrySml_crs3995,
                             bboxSet = bboxTundraAlpine)

plt_changeResp <- f_mappingTMAP(r_index = aggregate(changeResp, fact = 2, fun = mean, na.rm = T),
                                index_label = "change in Resp [gC m-2 yr-1]",
                                colorpal = CHANGE_pal,
                                breakvals = seq(0, 200, 25),
                                color_midpoint = 0,
                                polygonFile = shp_cntrySml_crs3995,
                                bboxSet = bboxTundraAlpine)

plt_percChangeResp <- f_mappingTMAP(r_index = aggregate(r_predPERCchange_1km, fact = 2, fun = mean, na.rm = T),
                                    index_label = "change in Resp [%]",
                                    colorpal = CHANGE_pal,
                                    breakvals = seq(0, .75, .05),
                                    color_midpoint = 0,
                                    polygonFile = shp_cntrySml_crs3995,
                                    bboxSet = bboxTundraAlpine)

## plot standard error

stErr_pal <- scico(n = 3, palette = "batlow", begin = .5, end = 1, direction = -1)


plt_errRespChange <- f_mappingTMAP(r_index = aggregate(changeErrResp_crop, fact = 2, fun = mean, na.rm = T),
                                   index_label = "standard error [gC m-2 yr-1]",
                                   colorpal = CHANGE_pal,
                                   breakvals = seq(0, 200, 25),
                                   color_midpoint = 0,
                                   polygonFile = shp_cntrySml_crs3995,
                                   bboxSet = bboxTundraAlpine)

errRespChange_rel <- aggregate(abs(changeErrResp_crop/changeResp), fact = 2, fun = mean, na.rm = T)

plt_errRespChange_rel <- f_mappingTMAP(r_index = errRespChange_rel,
                                       index_label = "relative standard error [%]",
                                       colorpal = stErr_pal,
                                       breakvals = seq(0.75,1.75,.1),
                                       #color_midpoint = 0,
                                       polygonFile = shp_cntrySml_crs3995,
                                       bboxSet = bboxTundraAlpine)

# combine plots

pltCombined <- tmap_arrange(plt_baseResp,plt_modResp,
                            plt_changeResp,plt_percChangeResp,
                            plt_errRespChange,plt_errRespChange_rel,
                            ncol=2)

tmap_save(pltCombined,'figures/fig_comb_withError_2023_06_29.pdf',units = 'mm', width = 150, height = 270  )





### plot input data

bboxTundraAlpine <- c(-5.2e6,-5.2e6, 7.5e6,5e6)
zonesPalette <- scico(n = 3, palette = "batlow", begin = 0, end = 1, direction = 1)

zonesTundraAlpine <- maskTundraAlpine
zonesTundraAlpine[maskTundra>0] = 2

plt_zone <- tm_shape(zonesTundraAlpine, bbox = bboxTundraAlpine) +
  tm_raster(style = "cat",
            palette = zonesPalette)+
  tm_shape(shp_cntrySml_crs3995, bbox = bboxTundraAlpine) +
  tm_borders(col=NA,lwd = 0.5) +
  tm_shape(read_sf("data_out/sitesExtractedValues_v5.gpkg"), bbox = bboxTundraAlpine) +
  tm_dots(col='red',size = 0.1) +
  tm_layout(main.title.position = "left",
            legend.bg.color = TRUE,
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            frame = FALSE)

tmap_save(plt_zone,'figures/fig_zonesTundraAlpine.pdf',units = 'mm', width = 120, height = 120  )



SOC_crs3995_1km_ext <- subset(soilData,1)

plt_SOC_crs3995_1km <- f_mappingTMAP(r_index = SOC_crs3995_1km_ext,
                                     index_label = "Soil organic carbon (SOC)",
                                     colorpal = scico(n = 3, palette = "grayC", begin = .1, end = 1, direction = 1),
                                     breakvals = seq(0,10,.5),
                                     #color_midpoint = 0,
                                     polygonFile = shp_cntrySml_crs3995,
                                     bboxSet = bboxTundraAlpine)



plt_TNmean_crs3995_1km <- f_mappingTMAP(r_index = TNmean_crs3995_1km,
                                        index_label = "Total Nitrogen (TN)",
                                        colorpal = scico(n = 3, palette = "bamako", begin = .1, end = 1, direction = -1),
                                        breakvals = seq(0,.5,.025),
                                        #color_midpoint = 0,
                                        polygonFile = shp_cntrySml_crs3995,
                                        bboxSet = bboxTundraAlpine)


plt_BDmean_crs3995_1km <- f_mappingTMAP(r_index = BDmean_crs3995_1km,
                                        index_label = "Buld density (BD)",
                                        colorpal = resp_pal,
                                        breakvals = seq(1,1.5,.025),
                                        #color_midpoint = 0,
                                        polygonFile = shp_cntrySml_crs3995,
                                        bboxSet = bboxTundraAlpine)


plt_CNmean_crs3995_1km <- f_mappingTMAP(r_index = CNmean_crs3995_1km,
                                        index_label = "C:N ratio [-]",
                                        colorpal = scico(n = 3, palette = "batlow", begin = 0, end = 1, direction = -1),
                                        breakvals = seq(0,30,1.5),
                                        #color_midpoint = 0,
                                        polygonFile = shp_cntrySml_crs3995,
                                        bboxSet = bboxTundraAlpine)

pltInput <- tmap_arrange(plt_SOC_crs3995_1km,
                         plt_TNmean_crs3995_1km,
                         plt_BDmean_crs3995_1km,
                         plt_CNmean_crs3995_1km,
                         ncol=2)

tmap_save(pltInput,'figures/fig_input.pdf',units = 'mm', width = 150, height = 180  )


