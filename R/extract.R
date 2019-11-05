#'Extract elevation, slope and aspect from rasater data.
#'
#'Extract slope, elevations and aspect from raster data with that attribute base on the landslide points location.
#'
#'@title extractAtr
#'
#'@param d points dataset
#'@param z DEM elevation raster
#'@param slo slope
#'@param asp aspect
#'@param ... extra arguments
#'
#'@importFrom raster extract
#'
#'@return Returns an object of class \code{sp}, which has altitude, slope and aspect data.
#'
#'@export
#'
#'@examples
#'
#'library(rgdal)
#'library(raster)
#'
#'
#'landslPoints<-readOGR(system.file("extdata/desliFixed.shp", package = "landsl"))
#'
#'area<-readOGR(system.file("extdata/Region.shp", package = "landsl"))
#'
#'demc<- raster(system.file("extdata/Mosaic_Clip.tif", package = "landsl"))
#'
#'dem<-mask(demc, area)
#'
#'
#'slope <-slope(dem)
#'
#'aspect<-aspect(dem)
#'
#'ds <-createDataset(landslPoints, area)
#'
#'extractAtr(ds, dem, aspect, slope)
#'
#'
extractAtr = function(d, z, asp, slo, ...){
i<-d
i$ele <-extract(z, i, method='simple')
i$slo <-extract(slo, i, method='simple')
i$asp <-extract(asp, i, method='simple')
return(i)
}
