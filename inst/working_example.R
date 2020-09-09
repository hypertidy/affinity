x <- raadtools::readice("2020-09-07")  ## as at https://twitter.com/mdsumner/status/1303565190055641088

par(mfrow = c(3, 3))
image(x, axes = FALSE, legend = FALSE)
rio <- rasterio_to_sfio(raster_io0(c(100, 190), c(110, 120), c(620, 520)))

st <- stars::read_stars(tfile, RasterIO = rio)
image(st, add = T, reset = FALSE, col = cols, breaks = brks)
allvals <- st[[1]]
xx <- stars:::st_as_raster(st)
library(raster)
alg <- c(NearestNeighbour = "nearest_neighbour",
         Average = "average",
         Bilinear="bilinear",
         Cubic = "cubic",
         CubicSpline = "cubic_spline",
         Gauss = "Gauss",
         Lanczos = "lanczos",
         Mode = "mode")

for (i in seq_along(alg)) {
  rio$resample <- alg[i]
  st <- stars::read_stars(tfile, RasterIO = rio)
  allvals <- c(allvals, st[[1]])
  image(xx, col = "white", asp = "", axes = FALSE, main = names(alg)[i])
  image(st, add = TRUE,  reset = FALSE, breaks = brks, col = cols)
}

## cheat, do this post-hoc and run again
brks <- quantile(allvals, seq(0, 1, length.out = 11), na.rm = TRUE)
cols <- grey.colors(length(brks) - 1)
