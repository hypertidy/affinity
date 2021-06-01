#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL


#' Monterey Bay elevation
#'
#' Extent is in the examples, stolen from rayshader.
#'
#' A matrix 270x270 of topography. Used in [affinething()] examples.
#' @docType data
#' @examples
#' ex <- c(-122.366765, -121.366765, 36.179392, 37.179392)
#' raster::setExtent(raster::t(raster::raster(monterey)), ex)
"monterey"
