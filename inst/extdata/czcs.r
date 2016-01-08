##

f <- "http://oceancolor.gsfc.nasa.gov/cgi/l3/C19843361984366.L3m_MO_CHL_chlor_a_9km.nc.png?sub=img"

download.file(f, file.path("inst", "extdata", gsub("\\?sub=img", "", basename(f))), mode = "wb")

