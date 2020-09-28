library(curl)
library(raster)

# download Hansen et al. 2013 forest loss tiles

lossyear <- scan("F:/Commodity loss/lossyear.txt",what="character",sep="")

for (i in 1:length(lossyear)) {

  file_name <- substr(lossyear[i],73,114)

  curl_download(lossyear[i],
                paste0("F:/Commodity loss/Hansen_data/",file_name))

}

#' download commodity deforestation data from
#' Curtis et al. 2018. Classifying drivers of global forest loss.
#' https://science.sciencemag.org/content/361/6407/1108
#' assign igh projection to aau3445-Data-S3.tif

classification <- raster("F:/Commodity loss/aau3445-Data-S3.tif")

crs(classification) <- "+proj=igh +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +"

# set values of classification != 1 (commodity forest loss) to NA and write output

classification[classification != 1] <- NA

writeRaster(classification,"F:/Commodity loss/commodities_mask.tif")

# apply commodities mask to hansen mosaic and write output

hansen_merge <- raster("F:/Commodity loss/hansen_merge.tif")

commodities_mask <- raster("F:/Commodity loss/commodities_mask_resample.tif")

hansen_mask <- mask(hansen_merge,commodities_mask)

writeRaster(hansen_mask,"F:/Commodity loss/hansen_mask.tif")