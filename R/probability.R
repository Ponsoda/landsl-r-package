#'Calculate the probability of landslides on the dataset points base on the informations they contain (slope, elevation and aspect).
#'
#'This function calculates the probability
#'
#'@title probability
#'
#'@param d dataset
#'@param DEM elevation raster
#'@param asp aspect rasater
#'@param slo slope raster
#'@param ... extra arguments
#'
#'@import INLA
#'@import raster
#'@import pROC
#'
#'@return Returns an summary \code{data.frame} of INLA calculation; print dic and auc value and plot roc object.
#'
#'@export
#'
#'@examples
#'library(rgdal)
#'library(raster)
#'
#'landslPoints<-readOGR(system.file("extdata/desliFixed.shp", package = "landsl"))
#'
#'area<-readOGR(system.file("extdata/Region.shp", package = "landsl"))
#'
#'demc<- raster(system.file("extdata/Mosaic_Clip.tif", package = "landsl"))
#'
#'dem<-mask(demc, area)
#'slope <-slope(dem)
#'aspect<-aspect(dem)
#'ds <-createDataset(landslPoints, area)
#'dsEx<-extractAtr(ds, dem, aspect, slope)
#'probability(dsEx, dem, aspect, slope)
#'
probability = function(d, DEM, asp, slo, ...){
  lan <-d$lan
  prob <-inla(lan~d$ele+d$slo+d$asp, family="binomial",data=as.data.frame(d), control.compute=list(dic=TRUE))
  sum <-prob$summary.fixed
  d$pro <-sum$mean[1]+(sum$mean[2]*d$ele)+(sum$mean[3]*d$slo)+(sum$mean[4]*d$asp)
  cat("Dic value =", prob$dic$dic)
  roc_obj <- roc(lan, d$pro, levels = c(0, 1), direction = "<")
  cat("Area under ROC curve (AUC) =", auc(roc_obj))

  return(sum)
}

#'Create a map to show probability of landslides on the dataset points base on the informations they contain (slope, elevation and aspect).
#'
#'This function plots a landslide probability map
#'
#'@title probMap
#'
#'@param sum summary from INLA
#'@param DEM elevation raster
#'@param asp aspect raster
#'@param slo slope raster
#'@param ... extra arguments
#'
#'@import raster
#'
#'@return Returns an object of class \code{raster}, the probability of landslide in the study area.
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
#'slope <-slope(dem)
#'
#'aspect<-aspect(dem)
#'
#'ds <-createDataset(landslPoints, area)
#'
#'dsEx<-extractAtr(ds, dem, aspect, slope)
#'
#'prob<-probability(dsEx, dem, aspect, slope)
#'
#'probMap(prob, dem, aspect, slope)
#'
probMap= function(sum, DEM, asp, slo, ...){
  f <-function(x,y,z)(sum$mean[1]+(sum$mean[2]*x)+(sum$mean[3]*y)+(sum$mean[4]*z))
  map <- overlay(DEM, slo, asp, fun=f, recycle=TRUE)
  map.min = cellStats(map, "min")
  map.max = cellStats(map, "max")
  map<- ((map - map.min) / (map.max - map.min))
  plot(map)
}

