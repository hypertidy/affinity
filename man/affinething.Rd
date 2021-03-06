% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/affinething.R
\name{affinething}
\alias{affinething}
\title{Use affine logic interactively georegister a raster}
\usage{
affinething(x, rgb = FALSE)
}
\arguments{
\item{x}{a raster}

\item{rgb}{use RGB plot for a raster with 3 layers}
}
\value{
matrix of x,y coordinates in the space of the current raster extent
}
\description{
User clicks are collected in a controlled way for use by \code{\link[=domath]{domath()}}.
}
\details{
Obtain control points for the simple affine transform (offset and scale) on an ungeoreferenced image.
}
\examples{
\donttest{
\dontrun{
library(raster)
r <- raster("my_unreferenced_raster.png")
xy <- affinething(r)  ## click on two points that you know a location of
my_x <- c(1000, 2000)
my_y <- c(-1000, -500)
prj <- "+proj=laea +lon=147 +lat_0=-42" ## use your own map projection, that correspond to my_x/my_y
pt <- cbind(my_x, my_y)
## now convert those control points to an extent for your raster
ex <- domath(pt, xy, r, prj)

## now we can fix up the data
r <- raster::setExtent(r, ex)
raster::projection(r) <- prj
## hooray!
}
}
}
