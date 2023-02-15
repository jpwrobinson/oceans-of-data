library(raster)
library(stars)
library(terra) ## terra faster than raster for spatial algebra
library(tidyverse)

## Reading in 2016 data - daily SST anomaly files, per grid. 
# Load raster and output csv. Combine all days for each grid, filter max temperature, bind all cells, save csv.

files<-list.files('sst/2016')
files<-files[!str_detect(files, '.md5')]

# reading rasters into stack, one per day 
rastlist<-paste0('sst/2016/', files)
rasters<-terra::rast(rastlist, '//sea_surface_temperature_anomaly')
summary(rasters[[1]])


dates <- str_split_fixed(rastlist,  '_', 4)[,4]
dates<-str_replace_all(dates, '.nc', '')
dates<-as.Date(as.character(dates), format = "%Y%m%d")
month<-as.integer(format(dates, '%m'))


rast_mm<-tapp(rasters, month, 'mean')
names(rast_mm)<-unique(month)


sst_max <- terra::app(rast_mm, max) # 9.5 seconds
sst_month_max <- terra::app(rast_mm, which.max) # 9.5 seconds
plot(sst_month_max)
