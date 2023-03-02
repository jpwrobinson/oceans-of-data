pacman::p_load(tmap, sf, tidyverse, packcircles, countrycode, cowplot)
tmap_options(check.and.fix = TRUE)
# eez<-st_read('eez/World_EEZ_v11_20191118/eez_boundaries_v11.shp') ## multiline version
eez<-st_read('eez/World_EEZ_v11_20191118/eez_v11.shp') # polygons
# tm_shape(eez) + tm_polygons()
# crs(eez)

## Projection

## Get size of EEZ by country
# area<-st_area(eez)



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
eez_c<-eez %>% as.data.frame() %>% 
		mutate(SOVEREIGN1 = recode(SOVEREIGN1, 'Republic of Mauritius' = 'Mauritius')) %>% 
		group_by(SOVEREIGN1) %>% summarise(area = sum(AREA_KM2))

# ggplot(eez_c, aes(fct_reorder(SOVEREIGN1, area), area)) + geom_bar(stat='identity') + coord_flip()


# https://www.un.org/ohrlls/content/list-sids
# not including territories and colonies
sids<-c('Antigua and Barbuda','Guyana','Singapore', 'Bahamas','Haiti','St. Kitts and Nevis','Bahrain',
	'Jamaica','St. Lucia','Barbados','Kiribati','St. Vincent and the Grenadines','Belize','Maldives',
	'Seychelles','Cabo Verde','Marshall Islands','Solomon Islands','Comoros','Federated States of Micronesia',
	'Suriname','Cuba','Mauritius','Timor-Leste','Dominica','Nauru','Tonga','Dominican Republic','Palau',
	'Trinidad and Tobago','Fiji','Papua New Guinea','Tuvalu','Grenada','Samoa','Vanuatu','Guinea-Bissau','Sao Tome and Principe')


sids_c<-data.frame(country=sids,
	iso3c = countrycode(sids, origin = 'country.name', destination = 'iso3c'))

eez_c<-eez_c %>% mutate(SOVEREIGN1 = recode(SOVEREIGN1, Comores = 'Comoros', Micronesia = 'Federated States of Micronesia'),
					iso3c = countrycode(SOVEREIGN1, origin = 'country.name', destination = 'iso3c'),
					sids = ifelse(iso3c %in% sids_c$iso3c, 'SID', 'not SID'),
					SOVEREIGN1 = recode(SOVEREIGN1, 'Federated States of Micronesia' = 'Micronesia',
													'Papua New Guinea' = 'Papua\nNew Guinea',
													'Solomon Islands' = 'Solomon\nIslands',
													'Cape Verde' = 'Cape\nVerde',
													'Marshall Islands' = "Mashall\nIslands")) %>% 
	arrange(sids)

# Generate the layout 

# by SID (scaling issue)
packing_sid <- circleProgressiveLayout(eez_c$area[eez_c$sids == 'SID']) 
dat.sid <- circleLayoutVertices(packing_sid, npoints=50)

packing_NOsid <- circleProgressiveLayout(eez_c$area[eez_c$sids != 'SID']) 
dat.NOsid <- circleLayoutVertices(packing_NOsid, npoints=50)

packing_sid$label<-with(eez_c %>% filter(sids == 'SID'), paste0(SOVEREIGN1, '\n', scales::label_comma()(area)))
packing_NOsid$label<-with(eez_c %>% filter(sids != 'SID'), paste0(SOVEREIGN1, '\n', scales::label_comma()(area)))


# for facets (keep scaling)
packing <- circleProgressiveLayout(eez_c$area) 
dat <- circleLayoutVertices(packing, npoints=50)

packing$label_eez<-with(eez_c, SOVEREIGN1)
packing$label_area<-with(eez_c, scales::label_comma()(area))
packing$sid<-eez_c$sids
packing$id<-1:nrow(eez_c)
dat$sid<-ifelse(dat$id %in% packing$id[packing$sid == 'SID'], 'SID', 'not SID')


g1<-ggplot(data = dat.NOsid) + 
  geom_polygon(aes(x, y, group = id), colour = 'transparent', 
               fill = "#1e3f5a", alpha = 0.7, show.legend = FALSE) +
  geom_text(data = packing_NOsid %>% filter(radius > 750), aes(x, y, label = label), size = 2.5, col='white') +
  coord_equal() +
  theme_void()

g2<-ggplot(data = dat.sid) + 
  geom_polygon(aes(x, y, group = id), colour = 'transparent', 
               fill = "#1e3f5a", alpha = 0.7, show.legend = FALSE) +
  geom_text(data = packing_sid %>% filter(radius > 750), aes(x, y, label = label), size = 2.5, col='white') +
  coord_equal() +
  theme_void()


g3<-ggplot(data = dat) + 
  geom_polygon(aes(x, y, group = id), colour = 'transparent', 
               fill = "#1e3f5a", alpha = 0.7, show.legend = FALSE) +
  geom_text(data = packing %>% filter(radius > 750 & sid != 'SID'), aes(x, y, label = label_eez), size = 2.6, col='white', lineheight = .8) +
  geom_text(data = packing %>% filter(sid == 'SID' & radius > 350), aes(x, y, label = label_eez), size = 2, col='white', lineheight = .8) +
  geom_text(data = packing %>% filter(radius > 750 & sid != 'SID'), aes(x, y-380, label = label_area), size = 2, col='white') +
  coord_equal() +
  facet_wrap(~sid) +
  theme_void() +
  theme(strip.text = element_blank())


pdf(file = 'eez/eez_sid.pdf', height=7, width =12)
# plot_grid(g1, g2, nrow =1)
g3
dev.off()
