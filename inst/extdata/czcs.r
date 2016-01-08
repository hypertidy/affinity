##

f <- "http://oceancolor.gsfc.nasa.gov/cgi/l3/C19843361984366.L3m_MO_CHL_chlor_a_9km.nc.png?sub=img"
fname <- file.path("inst", "extdata", gsub("\\?sub=img", "", basename(f)))
if (!file.exists(fname)) {
  download.file(f, fname,  mode = "wb")
}

library(raster)
r <- brick(fname)

llp <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
gdalvrt(r, a_ullr = c(-180, 90, 180, -90), a_srs = llp)

r <- brick(rgdal::readGDAL(gsub("png$", "vrt", fname)))
