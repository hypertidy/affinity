data("montereybay", package = "rayshader")
image(montereybay)
## why store the class Extent ... but not actually use the raster functions
ex <- attr(montereybay, "extent")
ex <- c(ex@xmin, ex@xmax, ex@ymin, ex@ymax)

## drop the attributes and decimate 2x
monterey <- matrix(montereybay[seq(1, nrow(montereybay), by = 2), seq(1, ncol(montereybay), by  = 2)], nrow(montereybay)/2)
usethis::use_data(monterey, compress = "xz")
