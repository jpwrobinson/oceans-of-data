pacman::p_load(tmap, sf, tidyverse, packcircles, countrycode, cowplot, scales)
tmap_options(check.and.fix = TRUE)
source('theme_black.R')
# eez<-st_read('eez/World_EEZ_v11_20191118/eez_boundaries_v11.shp') ## multiline version
eez<-st_read('eez/World_EEZ_v11_20191118/eez_v11.shp') # polygons

## eez by country
eez_c<-eez %>% as.data.frame() %>% 
		group_by(SOVEREIGN1) %>% 
        mutate(n_territory = n_distinct(TERRITORY1),
               n_territory = ifelse(SOVEREIGN1=='Kiribati', 1, n_territory)) %>% 
        group_by(SOVEREIGN1, n_territory) %>% 
		summarise(area = sum(AREA_KM2)) %>% 
		ungroup() %>% 
		mutate(area_s = rescale(area, to = c(0,1)),
		       SOVEREIGN1 = recode(SOVEREIGN1, 'Republic of Mauritius' = 'Mauritius',
		                                        'Federal Republic of Somalia' = 'Somalia'))

eez_t<-eez %>% as.data.frame() %>% 
		group_by(SOVEREIGN1, TERRITORY1) %>% 
		summarise(area = sum(AREA_KM2)) %>% 
		ungroup() %>% 
		mutate(area_s = rescale(area, to = c(0,1))) %>% 
        group_by(SOVEREIGN1) %>% 
        mutate(n_territory = n_distinct(TERRITORY1)) %>% 
		mutate(terri = ifelse(SOVEREIGN1 == TERRITORY1, 'Empire', 'Colony'),
		       n_territory = ifelse(SOVEREIGN1=='Kiribati', 1, n_territory), 
		       terri = ifelse(SOVEREIGN1=='Kiribati', 'Country', terri),
		       terri = ifelse(n_territory == 1, 'Country', terri),
		       TERRITORY1 = recode(TERRITORY1, 'Federated States of Micronesia' = 'Micronesia',
                        'Papua New Guinea' = 'Papua\nNew Guinea',
                        'Solomon Islands' = 'Solomon\nIslands',
                        'French Polynesia' = 'French\nPolynesia',
                        'Marshall Islands' = 'Marshall\nIslands',
                        'Cape Verde' = 'Cape\nVerde',
                        'Marshall Islands' = "Mashall\nIslands"),
		       SOVEREIGN1 = recode(SOVEREIGN1, 'Republic of Mauritius' = 'Mauritius')) %>% 
    arrange(terri)

eez_terri_a<-eez_t %>% group_by(terri, SOVEREIGN1) %>% 
        summarise(area_terri = sum(area)) %>% ungroup()

## join total colony areas to sovereign
eez_c<-eez_c %>% 
        left_join(eez_terri_a %>% filter(terri == 'Colony') %>% select(SOVEREIGN1, area_terri)) %>% 
        mutate(area_prop = area_terri / area  * 100)

# ggplot(eez_c, aes(fct_reorder(SOVEREIGN1, area), area)) + geom_bar(stat='identity') + coord_flip()

# Generate the layout 
eez_fac<-rbind(eez_c %>% select(-area_terri, -area_prop) %>% mutate(TERRITORY1 = NA, terri = NA, type = 'aorg'), eez_t %>% mutate(type = 'col')) %>% 
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

packing1$label_eez<-str_replace_all(with(eez_c, SOVEREIGN1), '\\ ', '\n')
packing1$label_area<-with(eez_c, label_comma()(area))
packing1$area_prop<-eez_c$area_prop
packing1$label_area2<-with(eez_c, paste0(round(area_prop,0), '%'))
packing1$id<-1:nrow(eez_c)
packing1$n_territory<-eez_c$n_territory

## now territory level, using max country level area as baseline size
packing2 <- circleProgressiveLayout(c( eez_t$area, max(eez_c$area)))[-252,]
dat2 <- circleLayoutVertices(packing2, npoints=50)

packing2$label_eez<-eez_t$TERRITORY1
packing2$label_eez2<-str_replace_all(packing2$label_eez, '\\ ', '\n')
packing2$label_eez2<-str_replace_all(packing2$label_eez2, 'Islands', 'Is.')
packing2$label_eez2<-str_replace_all(packing2$label_eez2, 'Island', 'Is.')
packing2$label_eez2<-ifelse(str_length(packing2$label_eez)>20, '', packing2$label_eez2)
packing2$label_area<-with(eez_t, label_comma()(area))
packing2$id<-1:nrow(eez_t)
packing2$terri<-eez_t$terri
dat2$terri<-packing2$terri[match(dat2$id, packing2$id)]

g4<-ggplot(data = dat1) + 
  geom_polygon(aes(x, y, group = id), colour = 'transparent', 
               fill = "#1e3f5a", alpha = 1, show.legend = FALSE) +
  geom_text(data = packing1 %>% filter(area_prop > 10 & radius > 400), aes(x, y+150, label = label_eez), size = 2.6, col='white', lineheight = .8) +
    geom_text(data = packing1 %>% filter(area_prop < 10 | is.na(area_prop) & radius > 400), aes(x, y, label = label_eez), size = 1.8, col='grey80', lineheight = .8) +
  # geom_text(data = packing1 %>% filter(n_territory > 1 & radius > 650), aes(x, y-300, label = label_area), size = 2, col='white') +
    geom_text(data = packing1 %>% filter(area_prop > 10 & radius > 400), aes(x, y-400, label = label_area2), size = 2, col='white') +
  coord_equal() +
  theme_void() + theme(plot.background=element_rect(fill='black', color='black'), panel.background = element_rect(fill = "black", color  =  NA)) 

g5<-ggplot(data = dat2) + 
  geom_polygon(aes(x, y, group = id), colour = 'transparent', 
               fill = "#1e3f5a", alpha = 1, show.legend = FALSE) +
  geom_polygon(data = dat2 %>% filter(terri == 'Colony'), aes(x, y, group = id), col = 'transparent', 
               fill = "darkred", alpha = 1, show.legend = FALSE) +
  # geom_text(data = packing2 %>% filter(terri == 'Empire'), aes(x, y, label = label_eez), size = 2.2, col='white', lineheight = .8) +
    geom_text(data = packing2 %>% filter(terri == 'Colony' & radius>400), aes(x, y, label = label_eez2), size = 2.2, col='white', lineheight = .8) +
    geom_text(data = packing2 %>% filter(label_eez == 'Antarctica'), aes(x, y, label = label_eez), size = 2.2, col='grey90', lineheight = .8) +
  # geom_text(data = packing2 %>% filter(radius > 650 & radius < 900), aes(x, y+100, label = label_eez), size = 2.2, col='white', lineheight = .8) +
  # geom_text(data = packing2 %>% filter(radius > 900), aes(x, y-300, label = label_area), size = 2, col='white') +
  coord_equal() +
  theme_void() + theme(plot.background=element_rect(fill='black', color='black'), panel.background = element_rect(fill = "black", color  =  NA)) 



pdf(file = 'eez/eez_sid_territory.pdf', height=7, width =12)
plot_grid(g4, g5)
# g3
dev.off()


