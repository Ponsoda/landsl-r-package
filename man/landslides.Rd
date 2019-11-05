\name{landslides}
\alias{landslides}
\docType{data}
\title{
landslides dataset
%%   ~~ data name/kind ... ~~
}
\usage{data("landslides")}
\format{
  A data frame with 0 observations on the following 2 variables.
  \describe{
    \item{\code{x}}{a numeric vector}
    \item{\code{y}}{a numeric vector}
  }
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
\keyword{datasets}
