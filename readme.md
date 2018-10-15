---
title: "Simple georerenceing"
author: "Michael Sumner"
date: "`r Sys.Date()`"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Old rough example

```R
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
