\name{DEM}
\alias{DEM}
\docType{data}
\title{
ss
%%   ~~ data name/kind ... ~~
}
\description{
ss
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("DEM")}
\format{
  The format is:
Formal class 'RasterLayer' [package "raster"] with 12 slots
  ..@ file    :Formal class '.RasterFile' [package "raster"] with 13 slots
  .. .. ..@ name        : chr "C:\\Users\\na-ch\\Desktop\\Geospatial Technologies\\Castell?n\\Spatial Analysis\\Rasters\\Mosaic_Clip.tif"
  .. .. ..@ datanotation: chr "FLT4S"
  .. .. ..@ byteorder   : chr "little"
  .. .. ..@ nodatavalue : num -Inf
  .. .. ..@ NAchanged   : logi FALSE
  .. .. ..@ nbands      : int 1
  .. .. ..@ bandorder   : chr "BIL"
  .. .. ..@ offset      : int 0
  .. .. ..@ toptobottom : logi TRUE
  .. .. ..@ blockrows   : int 1
  .. .. ..@ blockcols   : int 5041
  .. .. ..@ driver      : chr "gdal"
  .. .. ..@ open        : logi FALSE
  ..@ data    :Formal class '.SingleLayerData' [package "raster"] with 13 slots
  .. .. ..@ values    : logi(0)
  .. .. ..@ offset    : num 0
  .. .. ..@ gain      : num 1
  .. .. ..@ inmemory  : logi FALSE
  .. .. ..@ fromdisk  : logi TRUE
  .. .. ..@ isfactor  : logi FALSE
  .. .. ..@ attributes: list()
  .. .. ..@ haveminmax: logi TRUE
  .. .. ..@ min       : num 0
  .. .. ..@ max       : num 2313
  .. .. ..@ band      : int 1
  .. .. ..@ unit      : chr ""
  .. .. ..@ names     : chr "Mosaic_Clip"
  ..@ legend  :Formal class '.RasterLegend' [package "raster"] with 5 slots
  .. .. ..@ type      : chr(0)
  .. .. ..@ values    : logi(0)
  .. .. ..@ color     : logi(0)
  .. .. ..@ names     : logi(0)
  .. .. ..@ colortable: logi(0)
  ..@ title   : chr(0)
  ..@ extent  :Formal class 'Extent' [package "raster"] with 4 slots
  .. .. ..@ xmin: num 654425
  .. .. ..@ xmax: num 775804
  .. .. ..@ ymin: num 5115636
  .. .. ..@ ymax: num 5379390
  ..@ rotated : logi FALSE
  ..@ rotation:Formal class '.Rotation' [package "raster"] with 2 slots
  .. .. ..@ geotrans: num(0)
  .. .. ..@ transfun:function ()
  ..@ ncols   : int 5041
  ..@ nrows   : int 10954
  ..@ crs     :Formal class 'CRS' [package "sp"] with 1 slot
  .. .. ..@ projargs: chr "+proj=utm +zone=18 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
  ..@ history : list()
  ..@ z       : list()
}
\details{
ss
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
ss
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
ss
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(DEM)
## maybe str(DEM) ; plot(DEM) ...
}
\keyword{datasets}
