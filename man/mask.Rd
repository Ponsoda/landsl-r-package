\name{mask}
\alias{mask}
\docType{data}
\title{
mask dataset
%%   ~~ data name/kind ... ~~
}

\usage{data("mask")}
\format{
  The format is:
Formal class 'SpatialPolygonsDataFrame' [package "sp"] with 5 slots
  ..@ data       :'data.frame':	1 obs. of  2 variables:
  .. ..$ ID_2002: Factor w/ 1 level "104": 1
  .. ..$ NOMBRE : Factor w/ 1 level "Palena": 1
  ..@ polygons   :List of 1
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 28
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 713197 5231978
  .. .. .. .. .. .. ..@ area   : num 1.48e+10
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:2430, 1:2] 732293 732440 732455 732538 732442 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 704055 5334187
  .. .. .. .. .. .. ..@ area   : num 7e+07
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:106, 1:2] 705708 705415 705125 704692 704090 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 666084 5265492
  .. .. .. .. .. .. ..@ area   : num 44243503
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:127, 1:2] 667649 667270 667284 667503 667446 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 708411 5343715
  .. .. .. .. .. .. ..@ area   : num 20870436
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:56, 1:2] 708153 708062 707783 707721 707338 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 661605 5279638
  .. .. .. .. .. .. ..@ area   : num 16626824
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:50, 1:2] 661340 661017 660579 660309 660056 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 658505 5267218
  .. .. .. .. .. .. ..@ area   : num 5233173
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:25, 1:2] 658678 658347 658332 658040 657981 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 676900 5272224
  .. .. .. .. .. .. ..@ area   : num 5e+06
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:27, 1:2] 677508 677300 677011 676682 676395 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 656880 5266351
  .. .. .. .. .. .. ..@ area   : num 3390391
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:17, 1:2] 657549 657150 656892 656370 656282 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 657395 5274450
  .. .. .. .. .. .. ..@ area   : num 3238161
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:20, 1:2] 657651 657376 657146 656727 656707 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 695695 5339657
  .. .. .. .. .. .. ..@ area   : num 2752849
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:21, 1:2] 695991 695808 695683 695519 695227 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 659353 5166024
  .. .. .. .. .. .. ..@ area   : num 1829989
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:23, 1:2] 659757 659668 659644 659533 659197 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 662123 5169361
  .. .. .. .. .. .. ..@ area   : num 1241444
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:20, 1:2] 662946 662536 662182 661913 661589 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 656061 5270958
  .. .. .. .. .. .. ..@ area   : num 1192234
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:16, 1:2] 655807 655627 655400 655310 655680 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 660977 5166372
  .. .. .. .. .. .. ..@ area   : num 1079693
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:19, 1:2] 661415 661126 661099 660919 660528 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 696110 5341398
  .. .. .. .. .. .. ..@ area   : num 864409
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:20, 1:2] 696339 696128 695936 695727 695565 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 698702 5329783
  .. .. .. .. .. .. ..@ area   : num 788637
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:14, 1:2] 699195 698930 698736 698538 698389 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 699005 5341477
  .. .. .. .. .. .. ..@ area   : num 605529
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:16, 1:2] 699151 698959 698726 698369 698514 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 704256 5340470
  .. .. .. .. .. .. ..@ area   : num 485073
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:12, 1:2] 704322 704044 703842 703695 703761 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 678146 5235207
  .. .. .. .. .. .. ..@ area   : num 374919
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] 678718 678353 678046 677780 677710 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 697699 5339784
  .. .. .. .. .. .. ..@ area   : num 254034
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] 697616 697458 697424 697546 697792 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 692583 5377873
  .. .. .. .. .. .. ..@ area   : num 245580
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:13, 1:2] 692480 692212 692172 692395 692444 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 675658 5358174
  .. .. .. .. .. .. ..@ area   : num 238963
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] 675774 675461 675312 675434 675651 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 677031 5234768
  .. .. .. .. .. .. ..@ area   : num 224005
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] 677126 676842 676627 676617 676886 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 664594 5169505
  .. .. .. .. .. .. ..@ area   : num 210457
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] 664685 664503 664346 664358 664542 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 658973 5163461
  .. .. .. .. .. .. ..@ area   : num 204021
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] 658958 658823 658888 658737 658870 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 661406 5164149
  .. .. .. .. .. .. ..@ area   : num 134788
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:7, 1:2] 661534 661309 661253 661272 661397 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 692340 5379216
  .. .. .. .. .. .. ..@ area   : num 80092
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:7, 1:2] 692357 692207 692185 692279 692447 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] 692971 5377585
  .. .. .. .. .. .. ..@ area   : num 54581
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:7, 1:2] 693045 692859 692799 692925 693110 ...
  .. .. .. ..@ plotOrder: int [1:28] 1 2 3 4 5 6 7 8 9 10 ...
  .. .. .. ..@ labpt    : num [1:2] 713197 5231978
  .. .. .. ..@ ID       : chr "0"
  .. .. .. ..@ area     : num 1.5e+10
  ..@ plotOrder  : int 1
  ..@ bbox       : num [1:2, 1:2] 654432 5115643 775794 5379384
  .. ..- attr(*, "dimnames")=List of 2
  .. .. ..$ : chr [1:2] "x" "y"
  .. .. ..$ : chr [1:2] "min" "max"
  ..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slot
  .. .. ..@ projargs: chr "+proj=utm +zone=18 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
}

\source{
\url{https://www.openstreetmap.org}
}

\description{
A dataset containing the road network (roads, bikelanes, footpaths, et cetera) of
Roxel, a neighborhood in the city of Münster, Germany. The data is taken from
OpenStreetMap, querying by key = 'highway'. The topology is cleaned with the
v.clean tool in GRASS GIS.
}
\examples{
data(mask)
## maybe str(mask) ; plot(mask) ...
}
\keyword{datasets}
