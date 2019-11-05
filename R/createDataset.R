#'Creating dataset from the landslides points.
#'
#'Returns an object of class \code{sp}, which has landslide points and the same number of random points.
#'
#'@title createDataset
#'
#'@param x landslide points
#'@param m mask
#'@param ... extra arguments
#'
#'@import rgdal
#'@import sp
#'@importFrom methods as
#'
#'@return Returns an object of class \code{sp}, which has landslide points and the same number of random points.
#'
#'@export
#'
#'@examples
#'library(rgdal)
#'
#'landslPoints<-readOGR(system.file("extdata/desliFixed.shp", package = "landsl"))
#'
#'area<-readOGR(system.file("extdata/Region.shp", package = "landsl"))
#'
#'createDataset(landslPoints, area)
#'
createDataset = function(x, m,...) {
  p<-x
  np<-summary(p)$npoints
  p <- p[,0]
  p@data<-data.frame(ID=1:np)
  p$lan <-1

  r <- spsample(m, np, type="random")
  r<-as(r, "SpatialPointsDataFrame")
  r@data<-data.frame(ID=1:np)
  r$lan <-0
  i<-rbind(p,r)

  return(i)
}
