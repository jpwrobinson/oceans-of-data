library(raster)
# library(stars)
# library(terra) ## terra faster than raster for spatial algebra
library(tidyverse)

## Reading in 2016 data - daily DHW files, per grid. 
# Note this script was originally run for SST anomaly files and the output summary objects exist in directory
# Load raster and output csv. Combine all days for each grid, filter max temperature, bind all cells, save csv.


years<-c(2015, 2016, 2017)
    for(i in 2:length(years)){
    
        ptm <- proc.time()
        files<-list.files(paste0('sst/dhw/', years[i]))
        files<-files[!str_detect(files, '.md5')]
        
        # reading rasters into stack, one per day 
        rastlist<-paste0('sst/dhw/',years[i],'/', files)
        # rasters <- stack(rastlist, varname = 'sea_surface_temperature_anomaly', quick=TRUE)
        rasters <- stack(rastlist, varname = 'degree_heating_week', quick=TRUE)
        # rasters <- crop(rasters, cropper)
        
        # month indice
        dates <- str_split_fixed(rastlist,  '_', 4)[,4]
        dates<-str_replace_all(dates, '.nc', '')
        dates<-as.Date(as.character(dates), format = "%Y%m%d")
        month<-as.integer(format(dates, '%m'))
        
        ## get monthly max monthly anomaly by pixel
        smax <- stackApply(rasters, month, fun=max)
        # 
        # # summary max annual by pixel
        # maxer<-max(smax) ## raster version - slow, but terra failing with multiple nc files
        # # summary max annual month by pixel
        # maxer_m<-which.max(smax)
        # 
        print(paste('Years:', years[i], 'Time:'))
        print(proc.time() - ptm)
        
        # save(maxer, maxer_m, file = paste0('sst/output/', 'sst_max_summary_', years[i], '.Rdata'))
        save(smax, file = paste0('sst/output/', 'dhw_summary_', years[i], '.Rdata'))
        beepr::beep()
    }


## Load years and re-estimate max, crop extent to tropics
cropper <- extent(-180, 180,-40, 40)
for(i in 1:length(years)){
    
    rm(smax)
    load(paste0('sst/output/', 'dhw_summary_', years[i], '.Rdata'))
    smax <- crop(smax, cropper)
    names(smax)<-paste(names(smax), years[i], sep='_')
    assign(paste('smax', years[i], sep='_'), smax)
}

# combine years and estimate max anomaly value + yr.month
smax<-stack(smax_2015, smax_2016, smax_2017)
maxer<-max(smax) 
maxer_m<-which.max(smax)

plot(maxer_m)
save(maxer, maxer_m, file = 'sst/output/dhw_summary_2015-17.Rdata')
