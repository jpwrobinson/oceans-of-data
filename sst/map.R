library(tmap)
library(tidyverse)
library(raster)
library(sf)
library(RColorBrewer)

## format
coldpal<-c('#d1e5f0')
warmpal<-brewer.pal('OrRd', n = 9)
pal<-c(coldpal, warmpal)
pal2<-brewer.pal("Greens", n=9)

longlat = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
eck4 = "+proj=eck4 +datum=WGS84"
moll<-"+proj=moll +x_0=0 +y_0=0 +lat_0=0 +lon_0=133"
# moll<-"+proj=moll +lon_0=150 +ellps=WGS84"


# data load and project Mollweide pacific centered
load(file = 'sst/output/dhw_summary_2015-17.Rdata')
crs(maxer) <- longlat
# maxer<-projectRaster(maxer, crs= eck4)
maxer<-projectRaster(maxer, crs= moll)

crs(maxer_m) <- longlat
maxer_m<-projectRaster(maxer_m, crs= moll)

# reefs - convert to lat long
reef<-readxl::read_excel('sst/aan8048_hughes_sm.xlsx') %>% janitor::clean_names() %>%
    mutate(bleach = ifelse(!is.na(x2016) | !is.na(x2015), 'bleach', NA)) %>% 
    filter(!is.na(bleach)) %>% 
    dplyr::select(location, lat, long, bleach) %>% 
    mutate(latitude = str_replace_all(lat, 'º', ''),
           latitude = str_replace_all(latitude, 'S', ''),
           latitude = str_replace_all(latitude, 'N', ''),
           latitude = ifelse(str_detect(lat, 'S'), -as.numeric(latitude), as.numeric(latitude))) %>% 
    mutate(longitude = str_replace_all(long, 'º', ''),
           longitude = str_replace_all(longitude, 'E', ''),
           longitude = str_replace_all(longitude, 'W', ''),
           longitude = ifelse(str_detect(long, 'W'), -as.numeric(longitude), as.numeric(longitude))) %>% 
    st_as_sf(coords = c("longitude","latitude"), crs = 4326) %>% 
    st_transform(reef, crs=moll)



lay<-tm_layout(inner.margins=c(0,0,0,0), #legend.position = c("left", "top"), 
               legend.frame = FALSE,  
               # legend.bg.color="grey90",
               frame = FALSE,
               outer.bg.color = 'white', 
               legend.outside.position = "bottom",
               # legend.height = 50,
               legend.outside.size = .6,
               legend.outside = TRUE, 
               earth.boundary = FALSE, bg.color="white")


# plot
ds=TRUE
pdf(file = 'sst/map_fig_dhw.pdf', height = 5, width = 20)
tm_shape(maxer, raster.downsample=ds) + 
    tm_raster(palette=pal, style='cont', breaks = c(0, 4, 8, 12, 16, 18, 22), stretch.palette = TRUE,
              colorNA = 'black', showNA = FALSE,  legend.is.portrait = FALSE) +
    tm_shape(reef) + tm_symbols(size=0.2, alpha=0.5) +
    lay
dev.off()

pdf(file = 'sst/map_fig_date.pdf', height = 5, width = 20)
days<-c(0, 6)
tm_shape(maxer_m, raster.downsample=ds) + 
    tm_raster(palette=pal2, style='cont', breaks = c(days, days+12, days+12+12, 36),
              colorNA = 'black', showNA = FALSE,  legend.is.portrait = FALSE) +
    tm_shape(reef) + tm_symbols(size=0.2, alpha=0.5) +
    lay
dev.off()

pdf(file = 'sst/legends.pdf', height = 5, width=6)
tm_shape(maxer_m, raster.downsample=ds) + 
    tm_raster(palette=pal2, style='cont', breaks = c(days, days+12, days+12+12, 36),
              colorNA = 'black', showNA = FALSE,  legend.is.portrait = FALSE) +
    tm_shape(reef) + tm_symbols(size=0.2, alpha=0.5) +
    lay + tm_layout(legend.only = TRUE)

tm_shape(maxer, raster.downsample=ds) + 
    tm_raster(palette=pal, style='cont', breaks = c(0, 4, 8, 12, 16, 18, 22), stretch.palette = TRUE,
              colorNA = 'black', showNA = FALSE,  legend.is.portrait = FALSE) +
    tm_shape(reef) + tm_symbols(size=0.2, alpha=0.5) +
    lay+ tm_layout(legend.only = TRUE)
dev.off()


## for reef DHW ID
# tmap_mode("view")
# qtm(maxer)

## extract DHW by reef
reef$DHW<-raster::extract(maxer, reef %>% dplyr::select(geometry))
reef$DHW_month<-raster::extract(maxer_m, reef %>% dplyr::select(geometry))


## dumped overlays
# tm_shape(World) + tm_borders('grey20') +
# tm_grid(projection = longlat, col='white', lwd = 0.8, alpha=0.8) +
# tm_shape(maxer, raster.downsample=TRUE) + 
# tm_raster(palette=pal, style='cont', breaks = seq(0, 30, 5), colorNA = 'transparent', showNA = FALSE) +