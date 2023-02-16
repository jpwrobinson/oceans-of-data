pacman::p_load(tmap, sf, tidyverse)
tmap_options(check.and.fix = TRUE)
# eez<-st_read('eez/World_EEZ_v11_20191118/eez_boundaries_v11.shp') ## multiline version
eez<-st_read('eez/World_EEZ_v11_20191118/eez_v11.shp') # polygons
tm_shape(eez) + tm_polygons()
# crs(eez)

## Projection

## Get size of EEZ by country
area<-st_area(eez)

