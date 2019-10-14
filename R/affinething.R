
drawPoints <- function(n = 2) {
  print("click on two known points")
  do.call(cbind, graphics::locator(n))
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
#' r <- raster::setExtent(r, ex)
#' raster::projection(r) <- prj
#' ## hooray!
#' }
#' }
affinething <- function(x, rgb = FALSE) {
  if (!interactive()) stop("affinething is only for interactive use")
  if (rgb) {
    raster::plotRGB(x)
  } else {
    if (raster::nlayers(x) == 3) message("raster has 3 layers, maybe use 'rgb = TRUE'?")
    raster::plot(x[[1]])
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
  if (!is.null(proj)) pts <-  reproj::reproj(pts, target = proj, source = 4326)
  cols <- raster::colFromX(r, xy[,1])  ## extent in graphics columns
  rows <- raster::rowFromY(r, xy[,2])  ##  and graphics rows
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
#' @param x spatial object for use with [raster::projection()]
#' @param proj PROJ.4 string
#'
#' @return a spatial object with the projection set
#' @export
assignproj <- function(x, proj = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") {
  raster::projection(x) <- proj
  x
}


