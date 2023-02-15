library(tidyverse)
library(janitor)
library(RCurl)

ll<-read.csv('sst/csv/dhw_latlon_list.csv') %>% clean_names() 
ll<-ll[-1,] %>% 
    mutate(crw_sstanomaly=as.numeric(crw_sstanomaly)) %>% 
    filter(!is.nan(crw_sstanomaly))

lats<-ll$latitude
lons<-ll$longitude

## example url for full extract on 1 day
url<-'https://pae-paha.pacioos.hawaii.edu/erddap/griddap/dhw_5km.csv?CRW_SSTANOMALY%5B(2016-01-01T12:00:00Z):1:(2016-01-01T12:00:00Z)%5D%5B(30.025):1:(-29.975)%5D%5B(-179.975):1:(179.975)%5D'

maxer<-numeric()

dim(ll)[1] / 10000 ## 642 requests needed

## testing 100 requests
runs<-10000*c(1:100)

for(i in 1:length(runs)){
        
    # data request for
    print(paste('Latitude:', lats[runs[i-1]+1],'to', lats[runs[i]]))
    print(paste('Longitude:', lons[runs[i-1]+1], lons[runs[i]]))
    
    # create url for specific lat lon pixel, start of 2016 to start of 2018
    url_shifter<-paste0(
        'https://pae-paha.pacioos.hawaii.edu/erddap/griddap/dhw_5km.csv?CRW_SSTANOMALY%5B(2016-01-01T12:00:00Z):1:(2018-01-01T12:00:00Z)%5D%5B(',
        lats[runs[i-1]+1],
        '):1:(',
        lats[runs[i]],
        ')%5D%5B(',
        lons[runs[i-1]+1],
        '):1:(',
        lons[runs[i]],
        ')%5D')
    
    if(i == 1){
        url_shifter<-paste0(
            'https://pae-paha.pacioos.hawaii.edu/erddap/griddap/dhw_5km.csv?CRW_SSTANOMALY%5B(2016-01-01T12:00:00Z):1:(2018-01-01T12:00:00Z)%5D%5B(',
            lats[1],
            '):1:(',
            lats[runs[i]-1],
            ')%5D%5B(',
            lons[1],
            '):1:(',
            lons[runs[i]-1],
            ')%5D')
    }
    
    # request csv from url
    scrap<-getURL(url_shifter)
    
    
    # import, tidy and format
    df<-read.csv(text=scrap) %>% clean_names() %>% filter(!row_number()==1)
    df$time<-as.Date(df$time)
    df$latitude<-as.numeric(df$latitude)
    df$longitude<-as.numeric(df$longitude)
    df$crw_sstanomaly<-as.numeric(df$crw_sstanomaly)
    
    # select max anomaly date + temp
    max<-df %>% group_by(latitude, longitude) %>% 
        slice_max(crw_sstanomaly) %>% 
        as.data.frame()
    
    # cleanup
    rm(df)
    rm(scrap)
    
    # save result
    write.csv(max, file = paste0('sst/csv/scrap_dhw_', i, '.csv'))
    maxer<-rbind(maxer, max)
}
