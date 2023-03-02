pacman::p_load(tmap, sf, tidyverse, packcircles, countrycode, cowplot, scales)
tmap_options(check.and.fix = TRUE)
source('theme_black.R')
# eez<-st_read('eez/World_EEZ_v11_20191118/eez_boundaries_v11.shp') ## multiline version
eez<-st_read('eez/World_EEZ_v11_20191118/eez_v11.shp') # polygons

## eez by country
eez_c<-eez %>% as.data.frame() %>% 
		mutate(SOVEREIGN1 = recode(SOVEREIGN1, 'Republic of Mauritius' = 'Mauritius')) %>% 
		group_by(SOVEREIGN1) %>% 
		summarise(area = sum(AREA_KM2)) %>% 
		ungroup() %>% 
		mutate(area_s = rescale(area, to = c(0,1)))

eez_t<-eez %>% as.data.frame() %>% 
		mutate(SOVEREIGN1 = recode(SOVEREIGN1, 'Republic of Mauritius' = 'Mauritius')) %>% 
		group_by(SOVEREIGN1, TERRITORY1) %>% 
		summarise(area = sum(AREA_KM2)) %>% 
		ungroup() %>% 
		mutate(area_s = rescale(area, to = c(0,1))) %>% 
		mutate(terri = ifelse(SOVEREIGN1 == TERRITORY1, 'Empire', 'Colony'))

# ggplot(eez_c, aes(fct_reorder(SOVEREIGN1, area), area)) + geom_bar(stat='identity') + coord_flip()

# Generate the layout 
eez_fac<-rbind(eez_c %>% mutate(TERRITORY1 = NA, terri = NA, type = 'aorg'), eez_t %>% mutate(type = 'col')) %>% 
		arrange(desc(terri))

# for facets (keep scaling)
packing <- circleProgressiveLayout(eez_fac$area) 
dat <- circleLayoutVertices(packing, npoints=50)

packing$label_eez<-with(eez_fac, ifelse(type=='aorg', SOVEREIGN1, TERRITORY1))
packing$label_area<-with(eez_fac, label_comma()(area))
packing$id<-1:nrow(eez_fac)
packing$type<-eez_fac$type
packing$terri<-eez_fac$terri
dat$type<-packing$type[match(dat$id, packing$id)]
dat$terri<-packing$terri[match(dat$id, packing$id)]


g3<-ggplot(data = dat) + 
  geom_polygon(aes(x, y, group = id), colour = 'transparent', 
               fill = "#1e3f5a", alpha = 0.7, show.legend = FALSE) +
  geom_polygon(data = dat %>% filter(terri == 'Colony'), aes(x, y, group = id), col = 'transparent', 
               fill = "darkred", alpha = 1, show.legend = FALSE) +
  geom_text(data = packing %>% filter(radius > 900), aes(x, y, label = label_eez), size = 2.6, col='white', lineheight = .8) +
  geom_text(data = packing %>% filter(radius > 900), aes(x, y-400, label = label_area), size = 2, col='white') +
  coord_equal() +
  facet_wrap(~type) +
  theme_void() 



# Generate the layout 
# separately but scaled

# country level
packing1 <- circleProgressiveLayout(eez_c$area) 
dat1 <- circleLayoutVertices(packing1, npoints=50)

packing1$label_eez<-with(eez_c, SOVEREIGN1)
packing1$label_area<-with(eez_c, label_comma()(area))
packing1$id<-1:nrow(eez_c)

## now territory level, using max country level area as baseline size
packing2 <- circleProgressiveLayout(c(max(eez_c$area), eez_t$area))[-1,]
dat2 <- circleLayoutVertices(packing2, npoints=50)

packing2$label_eez<-with(eez_t, TERRITORY1)
packing2$label_area<-with(eez_t, label_comma()(area))
packing2$id<-1:nrow(eez_t)
packing2$terri<-eez_t$terri
dat2$terri<-packing2$terri[match(dat2$id, packing2$id)]

g4<-ggplot(data = dat1) + 
  geom_polygon(aes(x, y, group = id), colour = 'transparent', 
               fill = "#1e3f5a", alpha = 1, show.legend = FALSE) +
  geom_text(data = packing1 %>% filter(radius > 900), aes(x, y+100, label = label_eez), size = 2.6, col='white', lineheight = .8) +
  geom_text(data = packing1 %>% filter(radius > 900), aes(x, y-300, label = label_area), size = 2, col='white') +
  coord_equal() +
  theme_void() + theme(plot.background=element_rect(fill='black', color='black'), panel.background = element_rect(fill = "black", color  =  NA)) 

g5<-ggplot(data = dat2) + 
  geom_polygon(aes(x, y, group = id), colour = 'transparent', 
               fill = "#1e3f5a", alpha = 1, show.legend = FALSE) +
  geom_polygon(data = dat2 %>% filter(terri == 'Colony'), aes(x, y, group = id), col = 'transparent', 
               fill = "darkred", alpha = 1, show.legend = FALSE) +
  geom_text(data = packing2 %>% filter(radius > 900), aes(x, y+100, label = label_eez), size = 2.6, col='white', lineheight = .8) +
  geom_text(data = packing2 %>% filter(radius > 650 & radius < 900), aes(x, y+100, label = label_eez), size = 2.2, col='white', lineheight = .8) +
  geom_text(data = packing2 %>% filter(radius > 900), aes(x, y-300, label = label_area), size = 2, col='white') +
  coord_equal() +
  theme_void() + theme(plot.background=element_rect(fill='black', color='black'), panel.background = element_rect(fill = "black", color  =  NA)) 



pdf(file = 'eez/eez_sid_territory.pdf', height=7, width =12)
plot_grid(g4, g5)
# g3
dev.off()


