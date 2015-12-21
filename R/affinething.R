
drawPoints <- function(n = 2) {
  print("click on two known points")
  do.call(cbind, locator(n))
}
enterPoints <- function(x1, y1, x2, y2) {
  rbind(c(x1, y1), c(x2, y2))
}

affinething <- function(x) {
  if (!interactive()) stop("affinething is only for interactive use")
  plot(x)
  drawPoints()
}


domath <- function(pts, xy, proj = NULL) {
  if (!is.null(proj)) pts <-  project(pts, proj)
  scalex <- diff(pts[, 1]) / diff(rawxy[, 1])
  scaley <- diff(pts[, 2]) / diff(rawxy[, 2])
  offsetx <- pts[1,1] - rawxy[1,1] * scalex
  offsety <- pts[1,2] - rawxy[1,2] * scaley

  ## x0, (x0 + ncol * pixelX), y0, (y0 + nrow  * pixelY)

  extent(offsetx, offsetx + scalex * (ncol(r) + 1), offsety, offsety + scaley * (nrow(r) + 1))
  ## override raw index-transform applied to input image
}
assignproj <- function(x, proj = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") {
  projection(x) <- proj
  x
}


regions <- function(name) {
  switch(name,
         #rawxy <- affinething(r)
         #pts <- enterPoints(80, -60, 160, 0)
         swellAustralia = list(proj = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0",
                               extent = extent(64.97631, 201.3147, -69.72891, 26.75375 )),
         #rawxy <- affinething(r)
         #pts <- enterPoints(-60, -20, 140, -40)
         windAntarctica = list(proj = "+proj=stere +lat_0=-90 +lat_ts=-70 +lon_0=90 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs",
                               extent = extent(-8830383,12492413,-9648532,10730455))
  )

}

gdalvrt <- function(x, a_ullr = NULL, a_srs = NULL) {
  inputname <- filename(x)
  outputname <- sprintf("%svrt", substr(inputname, 1L, nchar(inputname)-3))
  cal <- sprintf("gdal_translate %s %s -of VRT", inputname, outputname)
  if (!is.null(a_ullr)) cal <- paste(cal, sprintf("-a_ullr %f %f %f %f", a_ullr[1], a_ullr[2], a_ullr[3], a_ullr[4]))
  if (!is.null(a_srs)) cal <- paste(cal, sprintf("-a_srs %s", a_srs))
  system(cal)

}

