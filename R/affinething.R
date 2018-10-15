
drawPoints <- function(n = 2) {
  print("click on two known points")
  do.call(cbind, locator(n))
}



enterPoints <- function(x1, y1, x2, y2) {
  rbind(c(x1, y1), c(x2, y2))
}

#' affinething
#'
#' Obtain control points for the simple affine transform (offset and scale) on an ungeoreferenced image.
#' @param x a raster
#' @param rgb use RGB plot for a raster with 3 layers
#' @return matrix of x,y coordinates in the space of the current raster extent
#' @export
#'
#' @examples
#' \donttest{
#' \dontrun{
#' library(raster)
#' r <- raster("my_unreferenced_raster.png")
#' xy <- affinething(r)  ## click on two points that you know a location of
#' my_x <- c(1000, 2000)
#' my_y <- c(-1000, -500)
#' prj <- "+proj=laea +lon=147 +lat_0=-42" ## use your own map projection, that correspond to my_x/my_y
#' pt <- cbind(my_x, my_y)
#' ## now convert those control points to an extent for your raster
#' ex <- domath(pt, xy, r, prj)
#'
#' ## now we can fix up the data
#' r <- setExtent(r, ex)
#' projection(r) <- prj
#' ## hooray!
#' }
#' }
affinething <- function(x, rgb = FALSE) {
  if (!interactive()) stop("affinething is only for interactive use")
  if (rgb) {
    raster::plotRGB(x)
  } else {
    if (nlayers(x) == 3) message("raster has 3 layers, maybe use 'rgb = TRUE'?")
    plot(x[[1]])
  }
  drawPoints()
}


#' domath
#'
#' Convert known geographic points with raw graphic control points and a reference raster
#' to an extent for the raster in geography.
#' @param pts known points of 'xy'
#' @param xy 'xy' obtain from `affinething`
#' @param r raster in use
#' @param proj optional projection, if the pts are longlat and the raster is not
#'
#' @return raster extent
#' @export
#' @seealso affinething
domath <- function(pts, xy, r = NULL, proj = NULL) {
  if (is.null(r)) stop("need r input, a raster")
  if (!is.null(proj)) pts <-  rgdal::project(pts, proj)
  cols <- colFromX(r, xy[,1])  ## extent in graphics columns
  rows <- rowFromY(r, xy[,2])  ##  and graphics rows
  scalex <- abs(diff(pts[,1]) / diff(cols))
  scaley <- abs(diff(pts[,2]) / diff(rows))
  offsetx <- pts[1,1] - cols[1] * scalex
  offsety <- pts[1,2] - (nrow(r) - rows[1] + 1) * scaley
  ## this worked by accident on 0,ncol, 0,nrow rasters
#  scalex <- diff(pts[, 1]) / (diff(xy[, 1])/res(r)[1])
#  scaley <- diff(pts[, 2]) / (diff(xy[, 2])/res(r)[2])
#  offsetx <- pts[1,1] - xy[1,1] * scalex
#  offsety <- pts[1,2] - xy[1,2] * scaley


  raster::extent(offsetx, offsetx + scalex * (ncol(r) + 1), offsety, offsety + scaley * (nrow(r) + 1))
  ## override raw index-transform applied to input image
}


#' assignproj
#'
#' @param x
#' @param proj
#'
#' @return
#' @export
#'
#' @examples
assignproj <- function(x, proj = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") {
  projection(x) <- proj
  x
}


#' regions (this should be a service/repo to get stored auxiliaries)
#'
#' @param name
#'
#' @return
#'
#' @examples
regions <- function(name) {
  # switch(name,
  #        #rawxy <- affinething(r)
  #        #pts <- enterPoints(80, -60, 160, 0)
  #        swellAustralia = list(proj = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0",
  #                              extent = extent(64.97631, 201.3147, -69.72891, 26.75375 )),
  #        #rawxy <- affinething(r)
  #        #pts <- enterPoints(-60, -20, 140, -40)
  #        windAntarctica = list(proj = "+proj=stere +lat_0=-90 +lat_ts=-70 +lon_0=90 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs",
  #                              extent = extent(-8830383,12492413,-9648532,10730455))
  # )
NULL
}

#' gdalvrt
#'
#' @param x
#' @param a_ullr
#' @param a_srs
#'
#' @return
#' @export
#'
#' @examples
gdalvrt <- function(x, a_ullr = NULL, a_srs = NULL) {
  inputname <- filename(x)
  outputname <- sprintf("%svrt", substr(inputname, 1L, nchar(inputname)-3))
  cal <- sprintf("gdal_translate %s %s -of VRT", inputname, outputname)
  if (!is.null(a_ullr)) cal <- paste(cal, sprintf("-a_ullr %f %f %f %f", a_ullr[1], a_ullr[2], a_ullr[3], a_ullr[4]))
  if (!is.null(a_srs)) cal <- paste(cal, sprintf("-a_srs %s", a_srs))
  system(cal)

}

