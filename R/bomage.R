#' Surface winds
#' Greater Australian Region
#'
#'



# system(sprintf("gdalinfo %s", tfiles[1]))
# Driver: PNG/Portable Network Graphics
# Files: dev/www.bom.gov.au/charts_data/IDY20300/current/windarrow/10m/IDY20300.windarrow-10m.000.png
# Size is 751, 527
# Coordinate System is `'
# Corner Coordinates:
# Upper Left  (    0.0,    0.0)
# Lower Left  (    0.0,  527.0)
# Upper Right (  751.0,    0.0)
# Lower Right (  751.0,  527.0)
# Center      (  375.5,  263.5)
# Band 1 Block=751x1 Type=Byte, ColorInterp=Palette
# Color Table (RGB with 256 entries)


## 0600 UTC
##"http://www.bom.gov.au/charts_data/IDY20300/current/windarrow/10m/IDY20300.windarrow-10m.000.png?1449640800
##"http://www.bom.gov.au/charts_data/IDY30100/current/primSwell/IDY30100.primSwell.000.png?1450634400"
#' @param x remote file name
#' @details Example remote name
#' http://www.bom.gov.au/charts_data/IDY20300/current/windarrow/10m/IDY20300.windarrow-10m.000.png?1449640800
localname <- function(x) {
  root <- file.path(getOption("default.datadir"), "dev")
  root <- file.path("dev")
  ## insert a daily subfolder
  subfolder <- format(ISOdatetime(1970, 1, 1, 0, 0, 0, tz = "UTC") + as.numeric(tail(unlist(strsplit(x, "\\?")), 1)), "%Y-%m-%d")
  ## strip the http:://
  path <- sapply(strsplit(gsub("^http://", "", x), "\\?"), "[", 1)
  file.path(root,dirname(path), subfolder, basename(path))
}

windremotename <- function(date, region = c("IDY20300", "IDY20001"),  product = c("current"), type = c("windarrow")) {
  date <- as.POSIXct(date, tz = "UTC")
  region <- match.arg(region)
  product <- match.arg(product)
  type <- match.arg(type)
  day <- format(unclass(as.POSIXct(paste(format(as.Date(date)), "06:00:00"), tz = "UTC")))
  #day <- format(date)
  h0 <- sprintf("%03d", seq(0, 69, by = 3))
  # http://www.bom.gov.au/charts_data/IDY20300/current/windarrow/10m/IDY20300.windarrow-10m.000.png?1449640800
  sprintf("http://www.bom.gov.au/charts_data/%s/%s/%s/10m/%s.%s-10m.%s.png?%s",
          region, product, type, region, type, h0, day)

}


swellremotename <- function(date, region = c("IDY30100"),  product = c("current"), type = c("primSwell")) {
  date <- as.POSIXct(date, tz = "UTC")
  region <- match.arg(region)
  product <- match.arg(product)
  type <- match.arg(type)
  day <- unclass(as.POSIXct(paste(format(as.Date(date)), "06:00:00"), tz = "UTC"))
  #day <- format(date)
  h0 <- sprintf("%03d", seq(0, 69, by = 3))
  ##"http://www.bom.gov.au/charts_data/IDY30100/current/primSwell/IDY30100.primSwell.000.png?1450634400"
  sprintf("http://www.bom.gov.au/charts_data/%s/%s/%s/%s.%s.%s.png?%s",
          region, product, type, region, type, h0, day)

}

download_png <- function(remote, local) {
  if (!file.exists(local)) {
    if (!file.exists(dirname(local))) dir.create(dirname(local), recursive = TRUE)
    res <- try(download.file(remote, local, mode = "wb"))
    if (inherits(res, "try-error")) local <- sprintf("could not download %s to %s", remote, local)
  }

  local
}
doswell_download <- function(date) {
  rnames <- swellremotename(date)
  lnames <- localname(rnames)
  report <- character(length(rnames))
  for (i in seq_along(rnames)) report[i] <- download_png(rnames[i], lnames[i])
  report
}
dowindAnt_download <- function(date) {
  rnames <- windremotename(date, region = "IDY20001")
  lnames <- localname(rnames)
  report <- character(length(rnames))
  for (i in seq_along(rnames)) report[i] <- download_png(rnames[i], lnames[i])

  report
}
dowindAus_download <- function(date) {
  rnames <- windremotename(date, region = "IDY20300")
  lnames <- localname(rnames)
  report <- character(length(rnames))
  for (i in seq_along(rnames)) report[i] <- download_png(rnames[i], lnames[i])

  report
}


