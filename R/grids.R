## DONE vapour rasterio creator
## DONE vapour rasterio to sf-rasterio
## DONE sf-rasterio to vapour rasterio
## DONE geotransform creator
## DONE world file creator
## DONE world file to geotransform
## DONE geotransform to world file
## DONE (vapour) get gdal geotransform
## DONE gen gdal geotransform
## DONE gen extent from gt+dim
## DONE gen gt from extent+dim
## raster to geotransform
## raster to world file
## raster to rasterio
## rasterio to raster
## sf-rasterio to raster
## stars converters
## devices to rasterio, etc. ...



## creators
##  raster_io0
##  geo_transform0
##  geo_world0

## converters
##  rasterio_to_sfio
##  sfio_to_rasterio TODO
##  gt_dim_to_extent

#' @name geo_transform0
#' @param x geotransform parameters, as per [geo_transform0()]
#' @param dim dimensions x,y of grid (ncol,nrow)
#' @return 4-element extent c(xmin,xmax,ymin,ymax)
#' @export
#' @examples
#' gt_dim_to_extent(geo_transform0(c(1, -1), c(0, 10)), c(5, 10))
gt_dim_to_extent <- function(x, dim) {
  xx <- c(x[1], x[1] + dim[1] * x[2])
  yy <- c(x[4] + dim[2] * x[6], x[4])
  c(xx, yy)
}

#' @name geo_transform0
#' @param x extent parameters, c(xmin,xmax,ymin,ymax)
#' @param dim dimensions x,y of grid (ncol,nrow)
#' @return 6-element [geo_transform0()]
#' @export
#' @examples
#' extent_dim_to_gt(c(0, 5, 0, 10), c(5, 10))
extent_dim_to_gt <- function(x, dim) {
  px <- c(diff(x[1:2])/dim[1L], -diff(x[3:4])/dim[2L])
  geo_transform0(px, c(x[1L], x[4L]))
}
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
#' @return numeric vector of values specifying offset, source dimension, output dimension
#' @name raster_io
#' @export
#' @examples
#' raster_io0(c(0L, 0L), src_dim = c(24L, 10L))
raster_io0 <- function(src_offset, src_dim, out_dim = src_dim, resample = "NearestNeighbour") {
  ## GDAL, and vapour names:
  ## 'NearestNeighbour' (default), 'Average', 'Bilinear', 'Cubic', 'CubicSpline', 'Gauss', 'Lanczos', 'Mode'
  out <- stats::setNames(c(src_offset, src_dim, out_dim), c("offset_x", "offset_y",
                                                     "source_nx", "source_ny",
                                                     "out_nx", "out_ny"))
  attr(out, "resample") <- resample
  out
}

#' @param x a RasterIO parameter list
#'
#' @return a sf-RasterIO parameter list
#' @export
#' @name raster_io
#' @examples
sfio_to_rasterio <- function(x) {
  raster_io0(unlist(x[c("nXOff", "nYOff")]),
             unlist(x[c("nXSize", "nYSize")]),
             unlist(x[c("nBufXSize", "nBufYSize")],
                    resample = x[["resample"]]))
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
#' @name raster_io
#' @export
#' @examples
#' rio <- raster_io0(c(0L, 0L), src_dim = c(24L, 10L))
#' rasterio_to_sfio(rio)
rasterio_to_sfio <- function(x) {
  resample <- attr(x, "resample")
  if (is.null(resample)) {
    resample <- "NearestNeighbour"
  }
  ## sf names:
  # "nearest_neighbour", "bilinear", "cubic", "cubic_spline", "lanczos", "average", "mode", "Gauss".
  algo <- unname(c(NearestNeighbour = "nearest_neighbour",
            Average = "average",
            Bilinear="bilinear",
            Cubic = "cubic",
            CubicSpline = "cubic_spline",
            Gauss = "gauss",
            Lanczos = "lanczos",
            Mode = "mode")[resample])
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
       resample = algo)
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
#' @export
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

#' @name geo_transform0
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
#' @export
#' @seealso geo_transform0
#' @return vector of parameters xres, yskew, xskew, yres, xmin, ymax
#' @export
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
