library(raster)
library(rgdal)

# note: all spatial files possible to download
# and process with code in Data_collection_and_preprocessing
# they were not uploaded to GitHub because of their
# large file size 
# data were resampled to 10 km resolution for display purposes

hansen_merge <- raster("C:/Users/Sensonomic Admin/Desktop/Commodity_deforestation_analysis_data/hansen_merge_10km.tif")

commodities_mask <- raster("C:/Users/Sensonomic Admin/Desktop/Commodity_deforestation_analysis_data/commodities_mask.tif")

hansen_mask <- raster("C:/Users/Sensonomic Admin/Desktop/Commodity_deforestation_analysis_data/hansen_mask_10km.tif")



hansen_merge[hansen_merge==0] <- NA

hansen_mask[hansen_mask==0] <- NA

hansen_merge_year <- hansen_merge + 2000

hansen_mask_year <- hansen_mask + 2000



countries <- readOGR("C:/Users/Sensonomic Admin/Desktop/Commodity_deforestation_analysis_data/ne_50m_admin_0_countries/ne_50m_admin_0_countries.shp")

countries_no_gl <- countries[-182,]

countries_transform <- spTransform(countries_no_gl,crs(hansen_merge))

countries_crop <- crop(countries_transform, extent(hansen_merge))



sea_countries <- countries_transform[countries_transform$SUBREGION=="South-Eastern Asia",]

sa_countries <- countries_transform[countries_transform$SUBREGION=="South America",]

#' Remove South America islands

sa_xmin <- extent(sa_countries[sa_countries$ADMIN=="Peru",])[1]

sa_xmax <- extent(sa_countries[sa_countries$ADMIN=="Brazil",])[2]

sa_ymin <- extent(sa_countries[sa_countries$ADMIN=="Chile",])[3]

sa_ymax <- extent(sa_countries[sa_countries$ADMIN=="Colombia",])[4]

sa_extent <- extent(sa_xmin,sa_xmax,sa_ymin,sa_ymax)



hansen_merge_sea_crop <- crop(hansen_merge_year,extent(sea_countries))

commodities_mask_sea_crop <- crop(commodities_mask,extent(sea_countries))

hansen_mask_sea_crop <- crop(hansen_mask_year,extent(sea_countries))



hansen_merge_sa_crop <- crop(hansen_merge_year,extent(sa_extent))

commodities_mask_sa_crop <- crop(commodities_mask,extent(sa_extent))

hansen_mask_sa_crop <- crop(hansen_mask_year,extent(sa_extent))



# World maps

dev.new(width=8,height=10,noRStudioGD = TRUE)

par(mar = c(0.5,0,0.5,1))
par(mfrow=c(3,1))

plot(hansen_merge_year,interpolate=TRUE,axes=FALSE,
     col=colorRampPalette(c("purple", "darkorange"))( 17 ),
     legend.args = list(text = 'Year', side = 4, 
                        font = 2, line = 3.5, cex = 0.8))
plot(countries_crop,add=T)
mtext(" A", side = 3, line = -2, adj=0, font=2, cex=1.2)

plot(commodities_mask,col="red",legend=FALSE, interpolate=TRUE,axes=FALSE)
plot(countries_crop,add=T)
mtext(" B", side = 3, line = -2, adj=0, font=2, cex=1.2)

plot(hansen_mask_year,interpolate=TRUE,axes=FALSE,
     col=colorRampPalette(c("purple", "darkorange"))( 17 ),
     legend.args = list(text = 'Year', side = 4, 
                        font = 2, line = 3.5, cex = 0.8))
plot(countries_crop,add=T)
mtext(" C", side = 3, line = -2, adj=0, font=2, cex=1.2)

# Southeast Asia maps

dev.new(width=6,height=10,noRStudioGD = TRUE)

par(mar = c(0.5,0,0.5,1))
par(mfrow=c(3,1))

plot(hansen_merge_sea_crop,interpolate=TRUE,axes=FALSE,
     col=colorRampPalette(c("purple", "darkorange"))( 17 ),
     legend.args = list(text = 'Year', side = 4, 
                        font = 2, line = 3.5, cex = 0.8))
plot(countries_crop,add=T)
mtext(" A", side = 3, line = -2, adj=0, font=2, cex=1.2)

plot(commodities_mask_sea_crop,col="red",legend=FALSE, interpolate=TRUE,axes=FALSE)
plot(countries_crop,add=T)
mtext(" B", side = 3, line = -2, adj=0, font=2, cex=1.2)

plot(hansen_mask_sea_crop,interpolate=TRUE,axes=FALSE,
     col=colorRampPalette(c("purple", "darkorange"))( 17 ),
     legend.args = list(text = 'Year', side = 4, 
                        font = 2, line = 3.5, cex = 0.8))
plot(countries_crop,add=T)
mtext(" C", side = 3, line = -2, adj=0, font=2, cex=1.2)

# South America maps

dev.new(width=5,height=10,noRStudioGD = TRUE)

par(mar = c(0.5,0,0.5,1))
par(mfrow=c(3,1))

plot(hansen_merge_sa_crop,interpolate=TRUE,axes=FALSE,
     col=colorRampPalette(c("purple", "darkorange"))( 17 ),
     legend.args = list(text = 'Year', side = 4, 
                        font = 2, line = 3.5, cex = 0.8))
plot(countries_crop,add=T)
mtext(" A", side = 3, line = -2, adj=0, font=2, cex=1.2)

plot(commodities_mask_sa_crop,col="red",legend=FALSE, interpolate=TRUE,axes=FALSE)
plot(countries_crop,add=T)
mtext(" B", side = 3, line = -2, adj=0, font=2, cex=1.2)

plot(hansen_mask_sa_crop,interpolate=TRUE,axes=FALSE,
     col=colorRampPalette(c("purple", "darkorange"))( 17 ),
     legend.args = list(text = 'Year', side = 4, 
                        font = 2, line = 3.5, cex = 0.8))
plot(countries_crop,add=T)
mtext(" C", side = 3, line = -2, adj=0, font=2, cex=1.2)
