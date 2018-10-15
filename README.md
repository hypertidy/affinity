
<!-- README.md is generated from README.Rmd. Please edit that file -->

# affinething

The goal of affinething is to provide simple control point
georeferencing for un-mapped rasters.

## Installation

You can install the dev version of affinething from
[GitHub](https://CRAN.R-project.org) with:

``` r
devtools::install_github("hypertidy/affinething")
```

## Example

This examples takes an an un-mapped raster and georerences it by
defining some control points.

Generally, we want **diagonal points**, so I tend to think “southwest”
and “northeast”, it doesn’t really matter where they are as long as
there’s some pixels between them in both directions. Monterey Bay is
very recognizable so I read off some long-lat control points using
[mapview](https://r-spatial.github.io/mapview/).

``` r
data("montereybay", package = "rayshader")


library(raster)
#> Loading required package: sp
## we know that rayshader works transpose
r <- t(raster(montereybay))

prj <- "+proj=longlat +datum=WGS84"
## the north tip of Pacific Grove
sw <- c(-121.93348, 36.63674)
## the inlet at Moss Landing
ne <- c(-121.78825, 36.80592)
#mapview::mapview(c(sw[1], ne[1]), c(sw[2], ne[2]), crs = prj)
```

We can obtain raw (graphics) coordinates of those locations from our
image, by plotting it and clicking twice with `affinething()`.

Note the order, the first point is “sw” and the second is “ne” - the
order is not important but it must match.

``` r
## mask the raster so we can see easily where we need to click
xy <- affinething(r > 0)
```

In this example the points
are

``` r
xy <- structure(c(0.65805655219227, 0.858931100128933, 0.367586425626388, 
0.589597209007295), .Dim = c(2L, 2L), .Dimnames = list(NULL, 
    c("x", "y")))
```

<img src="man/figures/README-affine-thing2-1.png" width="100%" />

Now we have everything we need to re-map our raster\! We don’t need to
project our points as the known locations are in the same coordinate
system as the source data. (In other situations we might georeference
using a graticule on a projected
map.)

``` r
mapped <- affinething:::assignproj(setExtent(r, affinething:::domath(rbind(sw, ne), xy, r, proj = NULL)), prj)

m <- rnaturalearth::ne_countries(country = "United States of America", scale = 10)
plot(mapped, col = viridis::viridis(30))
plot(m, add = TRUE)
contour(mapped, levels = -10, lty = 2, add = TRUE)
```

<img src="man/figures/README-affine-remap-1.png" width="100%" />

``` r

#mv <- mapview::mapview(mapped)
```

## Old rough example

``` r
r <- raster("dev/www.bom.gov.au/charts_data/IDY20001/current/windarrow/10m/2015-12-21/IDY20001.windarrow-10m.066.png")
a <- regions("windAntarctica")
projection(r) <- a$proj
extent(r) <- a$extent
plot(r)
library(maptools)
data(wrld_simpl)
library(rgdal)
w <- spTransform(wrld_simpl, projection(r))
plot(w, add = TRUE)


ex <- domath(pts, rawxy)
gdalvrt(r, a_ullr = c(xmin(ex), ymax(ex), xmax(ex), ymin(ex)),
        a_srs = "\"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0\"")

```
