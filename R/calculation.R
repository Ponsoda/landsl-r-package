#'Calculating slope from an elevation raster.
#'
#'Calculating slope from an elevation raster \code{raster}.
#'
#'@title slope
#'
#'@param z dem elevation raster; must be of class \code{raster}
#'@param ... extra arguments
#'
#'@importFrom raster terrain
#'@import rgdal
#'
#'@return Returns an object of class \code{raster}, which has the same extension of the elevation raster and the values of the slope.
#'
#'@export
#'
#'
#'@examples
#'library(rgdal)
#'library(raster)
#'
#'
#'demc<- raster(system.file("extdata/Mosaic_Clip.tif", package = "landsl"))
#'
#'slope(demc)
#'
#'
#'
#'
slope = function(z,...) {
  slo <-terrain(z, opt='slope')
  return(slo)
}

#'Calculating slope from an elevation raster.
#'
#'Calculating slope from an elevation raster \code{raster}.
#'
#'@title aspect
#'
#'@param z dem elevation raster; must be of class \code{raster}
#'@param ... extra arguments
#'
#'@return Returns an object of class \code{raster}, which has the same extension of the elevation raster and the values of the aspect.
#'
#'@export
#'
#'@examples
#'
#'library(rgdal)
#'library(raster)
#'
#'
#'demc<- raster(system.file("extdata/Mosaic_Clip.tif", package = "landsl"))
#'
#'aspect(demc)
#'

aspect = function(z,...) {
  asp <-terrain(z, opt='aspect')
  return(asp)
}
