pacman::p_load(tmap, sf, tidyverse)
tmap_options(check.and.fix = TRUE)
# eez<-st_read('eez/World_EEZ_v11_20191118/eez_boundaries_v11.shp') ## multiline version
eez<-st_read('eez/World_EEZ_v11_20191118/eez_v11.shp') # polygons
tm_shape(eez) + tm_polygons()
# crs(eez)

## Projection

## Get size of EEZ by country
area<-st_area(eez)



# GPT
# load rgdal package
# library(rgdal)
# 
# # read the shapefile using readOGR function
# my_shapefile <- readOGR('eez/World_EEZ_v11_20191118/eez_v11.shp')
# 
# # get the area of polygons
# area <- data.frame(area = rgeos::gArea(my_shapefile, byid = TRUE))
# 
# # merge with sf object
# eez$area<-are

## eez by country
eez_c<-eez %>% as.data.frame() %>% group_by(SOVEREIGN1) %>% summarise(area = sum(AREA_KM2))

ggplot(eez_c, aes(fct_reorder(SOVEREIGN1, area), area)) + geom_bar(stat='identity') + coord_flip()