# regions (intended to be a lookup for known un-referenced images, i.e. from BOM)
#regions <- function(name) {
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
#  NULL
#}

#gdalvrt <- function(x, a_ullr = NULL, a_srs = NULL) {
  # inputname <- filename(x)
  # outputname <- sprintf("%svrt", substr(inputname, 1L, nchar(inputname)-3))
  # cal <- sprintf("gdal_translate %s %s -of VRT", inputname, outputname)
  # if (!is.null(a_ullr)) cal <- paste(cal, sprintf("-a_ullr %f %f %f %f", a_ullr[1], a_ullr[2], a_ullr[3], a_ullr[4]))
  # if (!is.null(a_srs)) cal <- paste(cal, sprintf("-a_srs %s", a_srs))
  # system(cal)

#}

