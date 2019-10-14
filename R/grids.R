## DONE vapour rasterio creator
## DONE vapour rasterio to sf-rasterio
## sf-rasterio to vapour rasterio
## DONE geotransform creator
## DONE world file creator
## DONE world file to geotransform
## DONE geotransform to world file
## raster to geotransform
## raster to world file
## raster to rasterio
## rasterio to raster
## sf-rasterio to raster
## stars converters
## devices to rasterio, etc. ...


#' GDAL RasterIO parameter creator
#'
#' Basic function to create the window paramers as used by GDAL RasterIO.
#'
#' Resampling algorithm is one of 'NearestNeighbour' (default), 'Average', 'Bilinear', 'Cubic', 'CubicSpline', 'Gauss', 'Lanczos', 'Mode', but
#' more may be available given the version of GDAL in use.
#' @param src_offset index offset (0-based, top left)
#' @param src_dim source dimension (XY)
#' @param out_dim output dimension (XY, optional src_dim will be used if not set)
#' @param resample resampling algorith for GDAL see details
#' @noRd
#' @examples
#' raster_io0(c(0L, 0L), src_dim = c(24L, 10L))
raster_io0 <- function(src_offset, src_dim, out_dim = src_dim, resample = "NearestNeighbour") {
  ## GDAL, and vapour names:
  ## 'NearestNeighbour' (default), 'Average', 'Bilinear', 'Cubic', 'CubicSpline', 'Gauss', 'Lanczos', 'Mode'
  c(src_offset, src_dim, out_dim)
}

#' The sf/stars RasterIO list
#'
#' We create the list as used by the stars/sf GDAL IO function 'gdal_read()'.
#'
#' Note that the input is a 4 or 6 element vector, with offset 0-based and
#' outputdimensions optional (will use the source window). The resample argument uses the syntax
#' identical to that used in GDAL itself.
#'
#' @param x rasterio params as from [raster_io0()]
#' @param resample resampling algorith as per [raster_io0()]
#' @noRd
#' @examples
#' rio <- raster_io0(c(0L, 0L), src_dim = c(24L, 10L))
#' rasterio_to_sfio(rio)
rasterio_to_sfio <- function(x, resample = "NearestNeighbour") {
  ## sf names:
  # "nearest_neighbour", "bilinear", "cubic", "cubic_spline", "lanczos", "average", "mode", "Gauss".
  algo <- c(NearestNeighbour = "nearest_neighbour",
            Average = "average",
            Bilinear="bilinear",
            Cubic = "cubic",
            CubicSpline = "cubic_spline",
            Gauss = "gauss",
            Lanczos = "lanczos",
            Mode = "mode")[resample]
  if (is.na(algo)) {
    warning(sprintf("resampling algorithm %s unrecognized, using 'NearestNeighbour'", algo))
    algo <- "nearest_neighbour"
  }
  list(nXOff = x[1L] + 1L,
       nYOff = x[2L] + 1L,
       nXSize = x[3L],
       nYSize = x[4L],
       nBufXSize = x[5L],
       nBufYSize = x[6L],
       resample = resample)
}
#' Geo transform parameter creator
#'
#' Basic function to create a geotransform as used by GDAL.
#' @seealso [world_geo()] which uses the same parameters in a different order
#' @param px pixel resolution (XY, Y-negative)
#' @param ul grid offset, top-left corner
#' @param sh affine shear (XY)
#'
#' @return vector of parameters xmin, xres, yskew, ymax, xskew, yres
#' @noRd
#'
#' @examples
#' geo_transform0(px = c(1, -1), ul = c(0, 0))
geo_transform0 <- function(px, ul, sh = c(0, 0)) {
  c(xmin = ul[[1L]],
    xres = px[[1L]],
    yskew = sh[[2L]],
    ymax = ul[[2L]],
    xskew = sh[[1L]],
    yres = px[[2L]])
}

#' @name geot_ransform0
#' @param x worldfile parameters, as per [geo_world0()]
#' @examples
#' (wf <- geo_world0(px = c(1, -1), ul = c(0, 0)))
#' gt <- world_to_geotransform(wf)
#' geotransform_to_world(gt)
world_to_geotransform <- function(x) {
  x <- unname(x)
  px <- x[c(1L, 4L)]
  ul <- x[c(5L, 6L)] + c(-1, -1)* px/2
  sh <- x[c(2L, 3L)]
  geo_transform0(px, ul, sh)
}

#' World file parameter creator
#'
#' Basic function to create a ['world file'](https://en.wikipedia.org/wiki/World_file)
#' as used by various non-geo image formats
#'
#' Note that xmin/xmax are _centre_of_cell_ (of top-left cell) unlike the geotransform which is
#' top-left _corner_of_cell_. The parameters are otherwise the same, but in a different order.
#' @inheritParams geo_transform0
#' @noRd
#' @seealso geo_transform0
#' @return vector of parameters xres, yskew, xskew, yres, xmin, ymax
#' @examples
#' geo_world0(px = c(1, -1), ul = c(0, 0))
geo_world0 <- function(px, ul, sh = c(0, 0)) {
  c(xres = px[[1L]],
    yskew = sh[[2L]],
    xskew = sh[[1L]],
    yres = px[[2L]],
    xmin = ul[[1L]] + px[[1L]]/2,
    ymax = ul[[2L]] + px[[2L]]/2
  )
}
#' @name geo_world0
#' @param x geotransform parameters, as per [geo_transform0()]
#' @examples
#' (gt <- geo_transform0(px = c(1, -1), ul = c(0, 0)))
#' wf <- geotransform_to_world(gt)
#' world_to_geotransform(wf)
geotransform_to_world <- function(x) {
  x <- unname(x)
  px <- x[c(2L, 6L)]
  ul <- x[c(1L, 4L)]
  sh <- x[c(3L, 5L)]
  geo_world0(px, ul, sh)
}
