# Figures 
library(scico)
#https://www.data-imaginist.com/2018/scico-and-the-colour-conundrum/

library(broom)
library(sp)

#julian day
#http://uop.whoi.edu/UOPinstruments/frodo/aer/julian-day-table.html

#color schemes
#https://www.data-imaginist.com/2018/scico-and-the-colour-conundrum/

#prep ------

#projection:

Albers <-
  crs(
    '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96
       +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'
  )

# aea.proj <- 
#   "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96+x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
# 
# aea.proj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96+x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

#get shapefiles of states and update projection
us<-getData("GADM", country='USA', level=1,download=TRUE)
states_all_sites <- us[us$NAME_1 %in% c(
  'Wyoming',#'Colorado'
  'Montana','Kansas',#'New Mexico',
  'North Dakota','South Dakota','Nebraska',
  'Oklahoma'),]

#states_all_sites <- sp::spTransform(states_all_sites, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
states_all_sites <- sp::spTransform(states_all_sites, 
                                    CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96
       +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')) 

#prep for ggplotting
#https://stackoverflow.com/questions/62435609/plot-shapefile-with-ggplot2
states_all_sites_df <- fortify(states_all_sites)
states_all_sites_tidy <- tidy(states_all_sites)

# Recategorizes data as required for plotting
states_all_sites$id <- row.names(states_all_sites)
states_all_sites_tidy <- left_join(states_all_sites_tidy, states_all_sites@data)



#-------------------------------------------------------------------------------
# day of 90% growth in normal and dry years-----

#import

#sgs
day_90_sgs <- raster('./../../Data/CDD/day_of_90/day_90_shortgrass_steppe.tif')
plot(day_90_sgs)
day_90_sgs_df <- data.frame(rasterToPoints(day_90_sgs))
day_90_sgs_df$Ecoregion <- 'Shortgrass steppe'
colnames(day_90_sgs_df) <- c('x','y','doy','Ecoregion')
median(day_90_sgs_df$doy) #263 = September 20

#nmp
day_90_nmp <- raster('./../../Data/CDD/day_of_90/day_90_northern_mixed_prairies.tif')
plot(day_90_nmp)
day_90_nmp_df <- data.frame(rasterToPoints(day_90_nmp))
day_90_nmp_df$Ecoregion <- 'Northern mixed prairies'
colnames(day_90_nmp_df) <- c('x','y','doy','Ecoregion')
median(day_90_nmp_df$doy) #248 = September 5

day_90_df <- rbind(day_90_nmp_df,day_90_sgs_df)

#filter out extreme high and low values
day_90_df <- day_90_df %>%
  dplyr::filter(doy < 297) %>%
  dplyr::filter(doy > 65)

day_90_pdf <- ggplot(day_90_df, aes(x = doy, fill = Ecoregion)) +
  #scale_y_continuous(expand = c(0,0),limits = c(0,1.02)) +
  #scale_x_continuous(expand = c(0,0),limits = c(-1.5,0.6)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.02)) +
  scale_x_continuous(expand = c(0, 0), limits = c(230, 281)) +
  geom_density(color = 'black', alpha = 0.5, aes(y = ..scaled..)) +
  scale_fill_manual(values = c(
    'Northern mixed prairies' = 'black',
    'Shortgrass steppe' = 'white'
  )) +
  xlab('Day of 90% growth') +
  ylab('Probability density') +
  theme(
    axis.text.x = element_text(color = 'black', size = 13),
    #angle=25,hjust=1),
    axis.text.y = element_text(color = 'black', size = 13),
    axis.title = element_text(color = 'black', size = 16),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = c(0.70, 0.25),
    #legend.position = 'none',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 15),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black")
  )

#save to file
png(height = 1700,width=2000,res=300,'./../../Figures/day90_distributions.png')

print(day_90_pdf)

dev.off()


# nmp_look <- stack(day_90_nmp,max_sens_doy_nmp)
# plot(nmp_look)

#combine
day_90_sgs_nmp <- raster::merge(day_90_sgs,day_90_nmp,tolerance=0.2)
crs(day_90_sgs_nmp) <- Albers
plot(day_90_sgs_nmp)
day_90_sgs_nmp <- data.frame(rasterToPoints(day_90_sgs_nmp))

#filter out extreme high and low values
day_90_sgs_nmp <- day_90_sgs_nmp %>%
  dplyr::filter(layer < 297) %>%
  dplyr::filter(layer > 65)

str(day_90_sgs_nmp)
head(day_90_sgs_nmp)

#play around with creating day ranges
# day_90_sgs_nmp_2 <- day_90_sgs_nmp %>%
#   group_by(x,y) %>%
#   summarise(
#   layer2 = paste((layer-16), min(layer), sep = "-"))
# 
# head(day_90_sgs_nmp_2)
# day_90_sgs_nmp_2$layer2 <- as.numeric(day_90_sgs_nmp_2$layer2)

#turn to raster and fix projection, and then back to dataframe for plotting
day_90_sgs_nmp <- rasterFromXYZ(day_90_sgs_nmp)
proj4string(day_90_sgs_nmp) <- CRS("+proj=longlat")
day_90_sgs_nmp <-projectRaster(day_90_sgs_nmp, crs='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96
+        +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
day_90_sgs_nmp_df <- data.frame(rasterToPoints(day_90_sgs_nmp))


day_90_sgs_nmp_map <- ggplot() +
  geom_polygon(data=states_all_sites_tidy, mapping=aes(x = long, y = lat,group=group),
               color = "black", size = 0.1,fill=NA) +
  geom_raster(data=day_90_sgs_nmp_df, mapping=aes(x = x, y = y, fill = layer)) + 
  coord_equal() +
  scale_fill_scico('Day of 90% growth',palette = 'batlow',direction=1) +
  xlab('') +
  ylab('') +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme(
    axis.text.x = element_blank(), #angle=25,hjust=1),
    axis.text.y = element_blank(),
    axis.title.x = element_text(color='black',size=10),
    axis.title.y = element_text(color='black',size=10),
    axis.ticks = element_blank(),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    #legend.text = element_text(size=2),
    #legend.position = c(0.7,0.1),
    #legend.margin =margin(r=5,l=5,t=5,b=5),
    #legend.position = c(0.0,0.1),
    legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_blank(),
    axis.line.y = element_blank())


#save to file
png(height = 1500,width=920,res=300,'./../../Figures/day_90_growth_map.png')

print(day_90_sgs_nmp_map)

dev.off()


#impact of drought on the 90% day of growth

#import

#sgs
day_90_drought_sgs <-
  raster('./../../Data/CDD/day_of_90/day_90_droughtshortgrass_steppe.tif')
plot(day_90_drought_sgs)
day_90_drought_sgs <- stack(day_90_drought_sgs, day_90_sgs)
plot(day_90_drought_sgs)
day_90_drought_sgs_2 <-
  day_90_drought_sgs$day_90_droughtshortgrass_steppe -
  day_90_drought_sgs$day_90_shortgrass_steppe
plot(day_90_drought_sgs_2)
summary(day_90_drought_sgs_2)
#median = 4

plot(layer~y,data=rasterToPoints(day_90_drought_sgs_2))

#nmp
day_90_drought_nmp <-
  raster('./../../Data/CDD/day_of_90/day_90_droughtnorthern_mixed_prairies.tif')
plot(day_90_drought_nmp)
day_90_drought_nmp <- stack(day_90_drought_nmp, day_90_nmp)
plot(day_90_drought_nmp)
day_90_drought_nmp_2 <-
  day_90_drought_nmp$day_90_droughtnorthern_mixed_prairies -
  day_90_drought_nmp$day_90_northern_mixed_prairies
plot(day_90_drought_nmp_2)
summary(day_90_drought_nmp_2)
#median = 0
hist(day_90_drought_nmp_2$layer)

#combine
day_90_drought <-
  raster::merge(day_90_drought_nmp_2, day_90_drought_sgs_2, tolerance = 0.20)
crs(day_90_drought) <- Albers
plot(day_90_drought)
summary(day_90_drought)
day_90_drought_df <- data.frame(rasterToPoints(day_90_drought))
str(day_90_drought_df)

day_90_sgs_nmp_drought_map <-
  ggplot(day_90_drought_df, aes(x = x, y = y, fill = layer)) +
  geom_raster() +
  coord_equal() +
  #geom_sf()
  scale_fill_scico(
    'Drought impact to day \n of 90% growth (days)',
    palette = 'vik',
    direction = -1,
    midpoint = 0) +
  xlab('') +
  ylab('') +
  theme(
    axis.text.x = element_blank(),
    #angle=25,hjust=1),
    axis.text.y = element_blank(),
    axis.title.x = element_text(color = 'black', size = 10),
    axis.title.y = element_text(color = 'black', size = 10),
    axis.ticks = element_blank(),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    #legend.text = element_text(size=2),
    #legend.position = c(0.7,0.1),
    #legend.margin =margin(r=5,l=5,t=5,b=5),
    legend.position = c(0.30, 0.3),
    #legend.position = 'top',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 10),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    #make the borders clear in prep for just have two axes
    axis.line.x = element_blank(),
    axis.line.y = element_blank()
  )

#save to file
png(
  height = 1500,
  width = 2000,
  res = 300,
  './../../Figures/day_90_drought_growth_map.png'
)

print(day_90_sgs_nmp_drought_map)

dev.off()

#PDF of drought impact to 90% growth

#nmp
day_90_drought_nmp_2_df <- data.frame(rasterToPoints(day_90_drought_nmp_2))
day_90_drought_nmp_2_df$region <- 'Northern mixed prairies'

#sgs
day_90_drought_sgs_2_df <- data.frame(rasterToPoints(day_90_drought_sgs_2))
day_90_drought_sgs_2_df$region <- 'Shortgrass steppe'

#join
day_90_drought_nmp_sgs_2_df <- rbind(day_90_drought_nmp_2_df,day_90_drought_sgs_2_df)
head(day_90_drought_nmp_sgs_2_df)

#plot it
drought_day90_pdf <- ggplot(day_90_drought_nmp_sgs_2_df, aes(x = layer, fill = region)) +
  scale_y_continuous(expand = c(0,0)) +
  #scale_x_continuous(expand = c(0,0),limits = c(-1.5,0.6)) +
  #scale_y_continuous(expand = c(0, 0), limits = c(0, 1.02)) +
  #scale_x_continuous(expand = c(0, 0), limits = c(230, 281)) +
  geom_histogram(color='black',binwidth = 1) +
  #geom_density(color = 'black', alpha = 0.5, aes(y = ..scaled..)) +
  scale_fill_manual(values = c(
    'Northern mixed prairies' = 'grey70',
    'Shortgrass steppe' = 'white'
  )) +
  xlab('Drought impact to day of 90% growth') +
  ylab('Count') +
  theme(
    axis.text.x = element_text(color = 'black', size = 13),
    #angle=25,hjust=1),
    axis.text.y = element_text(color = 'black', size = 13),
    axis.title = element_text(color = 'black', size = 16),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = c(0.77, 0.55),
    #legend.position = 'none',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 15),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black")
  )

#save to file
png(height = 1700,width=2000,res=300,'./../../Figures/day90_drought_distributions.png')

print(drought_day90_pdf)

dev.off()



#-------------------------------------------------------------------------------
# day of 50% growth in normal and dry years-----

#import

#sgs
day_50_sgs <- raster('./../../Data/CDD/day_of_50/day_50_shortgrass_steppe.tif')
plot(day_50_sgs)
day_50_sgs_df <- data.frame(rasterToPoints(day_50_sgs))
day_50_sgs_df$Ecoregion <- 'Shortgrass steppe'
colnames(day_50_sgs_df) <- c('x','y','doy','Ecoregion')
median(day_50_sgs_df$doy) #179

#nmp
day_50_nmp <- raster('./../../Data/CDD/day_of_50/day_50_northern_mixed_prairies.tif')
plot(day_50_nmp)
day_50_nmp_df <- data.frame(rasterToPoints(day_50_nmp))
day_50_nmp_df$Ecoregion <- 'Northern mixed prairies'
colnames(day_50_nmp_df) <- c('x','y','doy','Ecoregion')
median(day_50_nmp_df$doy) #174

day_50_df <- rbind(day_50_nmp_df,day_50_sgs_df)

#filter out extreme high and low values
day_50_df <- day_50_df %>%
  dplyr::filter(doy < 297) %>%
  dplyr::filter(doy > 65)

day_50_pdf <- ggplot(day_50_df, aes(x = doy, fill = Ecoregion)) +
  #scale_y_continuous(expand = c(0,0),limits = c(0,1.02)) +
  #scale_x_continuous(expand = c(0,0),limits = c(-1.5,0.6)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.02)) +
  scale_x_continuous(expand = c(0, 0), limits = c(130, 281)) +
  geom_density(color = 'black', alpha = 0.5, aes(y = ..scaled..)) +
  scale_fill_manual(values = c(
    'Northern mixed prairies' = 'black',
    'Shortgrass steppe' = 'white'
  )) +
  xlab('Day of 50% growth') +
  ylab('Probability density') +
  theme(
    axis.text.x = element_text(color = 'black', size = 13),
    #angle=25,hjust=1),
    axis.text.y = element_text(color = 'black', size = 13),
    axis.title = element_text(color = 'black', size = 16),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = c(0.70, 0.25),
    #legend.position = 'none',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 15),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black")
  )

#save to file
png(height = 1700,width=2000,res=300,'./../../Figures/day50_distributions.png')

print(day_50_pdf)

dev.off()


# nmp_look <- stack(day_50_nmp,max_sens_doy_nmp)
# plot(nmp_look)

#combine
day_50_sgs_nmp <- raster::merge(day_50_sgs,day_50_nmp,tolerance=0.2)
crs(day_50_sgs_nmp) <- Albers
plot(day_50_sgs_nmp)
day_50_sgs_nmp <- data.frame(rasterToPoints(day_50_sgs_nmp))

#filter out extreme high and low values
day_50_sgs_nmp <- day_50_sgs_nmp %>%
  dplyr::filter(layer < 297) %>%
  dplyr::filter(layer > 65)

str(day_50_sgs_nmp)
head(day_50_sgs_nmp)

#play around with creating day ranges
# day_50_sgs_nmp_2 <- day_50_sgs_nmp %>%
#   group_by(x,y) %>%
#   summarise(
#   layer2 = paste((layer-16), min(layer), sep = "-"))
# 
# head(day_50_sgs_nmp_2)
# day_50_sgs_nmp_2$layer2 <- as.numeric(day_50_sgs_nmp_2$layer2)

#turn to raster and fix projection, and then back to dataframe for plotting
day_50_sgs_nmp <- rasterFromXYZ(day_50_sgs_nmp)
proj4string(day_50_sgs_nmp) <- CRS("+proj=longlat")
day_50_sgs_nmp <-projectRaster(day_50_sgs_nmp, crs='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96
+        +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
day_50_sgs_nmp_df <- data.frame(rasterToPoints(day_50_sgs_nmp))


# day_50_sgs_nmp_map <- ggplot() +
#   geom_polygon(data=states_all_sites_tidy, mapping=aes(x = long, y = lat,group=group),
#                color = "black", size = 0.1,fill=NA) +
#   geom_raster(data=day_50_sgs_nmp_df, mapping=aes(x = x, y = y, fill = layer)) + 
#   coord_equal() +
#   scale_fill_scico('Day of 50% growth',palette = 'batlow',direction=1) +
#   xlab('') +
#   ylab('') +
#   scale_x_continuous(expand=c(0,0)) +
#   scale_y_continuous(expand=c(0,0)) +
#   theme(
#     axis.text.x = element_blank(), #angle=25,hjust=1),
#     axis.text.y = element_blank(),
#     axis.title.x = element_text(color='black',size=10),
#     axis.title.y = element_text(color='black',size=10),
#     axis.ticks = element_blank(),
#     legend.key = element_blank(),
#     #legend.title = element_blank(),
#     #legend.text = element_text(size=2),
#     #legend.position = c(0.7,0.1),
#     #legend.margin =margin(r=5,l=5,t=5,b=5),
#     #legend.position = c(0.0,0.1),
#     legend.position = 'top',
#     strip.background =element_rect(fill="white"),
#     strip.text = element_text(size=10),
#     panel.background = element_rect(fill=NA),
#     panel.border = element_blank(), #make the borders clear in prep for just have two axes
#     axis.line.x = element_blank(),
#     axis.line.y = element_blank())
# 
# 
# #save to file
# png(height = 1500,width=920,res=300,'./../../Figures/day_50_growth_map.png')
# 
# print(day_50_sgs_nmp_map)
# 
# dev.off()


#impact of drought on the 50% day of growth

#import

#sgs
day_50_drought_sgs <-
  raster('./../../Data/CDD/day_of_50/day_50_droughtshortgrass_steppe.tif')
plot(day_50_drought_sgs)
day_50_drought_sgs <- stack(day_50_drought_sgs, day_50_sgs)
plot(day_50_drought_sgs)
day_50_drought_sgs_2 <-
  day_50_drought_sgs$day_50_droughtshortgrass_steppe -
  day_50_drought_sgs$day_50_shortgrass_steppe
plot(day_50_drought_sgs_2)
summary(day_50_drought_sgs_2)
#median = -22
#179-22

#plot(layer~y,data=rasterToPoints(day_50_drought_sgs_2))

#nmp
day_50_drought_nmp <-
  raster('./../../Data/CDD/day_of_50/day_50_droughtnorthern_mixed_prairies.tif')
plot(day_50_drought_nmp)
day_50_drought_nmp <- stack(day_50_drought_nmp, day_50_nmp)
plot(day_50_drought_nmp)
day_50_drought_nmp_2 <-
  day_50_drought_nmp$day_50_droughtnorthern_mixed_prairies -
  day_50_drought_nmp$day_50_northern_mixed_prairies
plot(day_50_drought_nmp_2)
summary(day_50_drought_nmp_2)
#median = -11
hist(day_50_drought_nmp_2$layer)

#combine
day_50_drought <-
  raster::merge(day_50_drought_nmp_2, day_50_drought_sgs_2, tolerance = 0.20)
proj4string(day_50_drought) <- CRS("+proj=longlat")
day_50_drought <-projectRaster(day_50_drought, crs='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96
+        +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
day_50_drought_df <- data.frame(rasterToPoints(day_50_drought))

# % of pixels with negative values (advanced day50)
(day_50_drought_df %>%
  filter(layer < 0) %>%
  summarise(length(layer)))/(length(day_50_drought_df$layer))
#0.90 of pixels see day of 50% growth occurr earlier during drought
  

day_50_sgs_nmp_drought_map <-
   ggplot() +
  geom_polygon(data=states_all_sites_tidy, mapping=aes(x = long, y = lat,group=group),
               color = "black", size = 0.1,fill=NA) +
  geom_raster(data=day_50_drought_df, mapping=aes(x = x, y = y, fill = layer)) + 
  coord_equal() +
  scale_fill_scico('Drought impact to day \n of 50% total growth (days)',palette = 'roma',direction=-1) +
  xlab('') +
  ylab('') +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme(
    axis.text.x = element_blank(), #angle=25,hjust=1),
    axis.text.y = element_blank(),
    axis.title.x = element_text(color='black',size=10),
    axis.title.y = element_text(color='black',size=10),
    axis.ticks = element_blank(),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    #legend.text = element_text(size=2),
    #legend.position = c(0.7,0.1),
    #legend.margin =margin(r=5,l=5,t=5,b=5),
    #legend.position = c(0.0,0.1),
    legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_blank(),
    axis.line.y = element_blank())

# #save to file
# png(
#   height = 1500,
#   width = 2000,
#   res = 300,
#   './../../Figures/day_50_drought_growth_map.png'
# )
# 
# print(day_50_sgs_nmp_drought_map)
# 
# dev.off()

#PDF of drought impact to 50% growth

#nmp
day_50_drought_nmp_2_df <- data.frame(rasterToPoints(day_50_drought_nmp_2))
day_50_drought_nmp_2_df$region <- 'Northern mixed prairies'

#sgs
day_50_drought_sgs_2_df <- data.frame(rasterToPoints(day_50_drought_sgs_2))
day_50_drought_sgs_2_df$region <- 'Shortgrass steppe'

#join
day_50_drought_nmp_sgs_2_df <- rbind(day_50_drought_nmp_2_df,day_50_drought_sgs_2_df)
head(day_50_drought_nmp_sgs_2_df)

#plot it
drought_day50_pdf <- ggplot(day_50_drought_nmp_sgs_2_df, aes(x = layer, fill = region)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.02)) +
  geom_density(color = 'black', alpha = 0.5, aes(y = ..scaled..)) +
  scale_fill_manual(values = c(
    'Northern mixed prairies' = 'grey70',
    'Shortgrass steppe' = 'white'
  )) +
  geom_vline(xintercept = 0,color='red') +
  xlab('Drought impact to day of 50% of total growth (days)') +
  ylab('Probability density') +
  theme(
    axis.text.x = element_text(color = 'black', size = 7),
    #angle=25,hjust=1),
    axis.text.y = element_text(color = 'black', size = 7),
    axis.title = element_text(color = 'black', size = 10),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 5),
    legend.position = c(0.82, 0.7),
    #legend.position = 'none',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 15),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black")
  )

#save to file
# png(height = 1700,width=2000,res=300,'./../../Figures/day50_drought_distributions.png')
# 
# print(drought_day50_pdf)
# 
# dev.off()

#try to make inset
library(grid)
vp <- viewport(width = 0.44, height = 0.39, x = 0.23,y=0.27)
# y = unit(0.7, "lines"), just = c("right",
#                                  "bottom")

#executing the inset, you create a function the utlizes all the previous code
library(grid)

full <- function() {
  print(day_50_sgs_nmp_drought_map)
  print(drought_day50_pdf , vp = vp)
}


png(height = 1700,width=2000,res=300,'Figures/day50_drought_inset_plot.png')

full()

dev.off()



#-------------------------------------------------------------------------------
# day of 25% growth in normal and dry years -----

#import

#sgs
day_25_sgs <- raster('./../../Data/CDD/day_of_25/day_25_shortgrass_steppe.tif')
plot(day_25_sgs)
day_25_sgs_df <- data.frame(rasterToPoints(day_25_sgs))
day_25_sgs_df$Ecoregion <- 'Shortgrass steppe'
colnames(day_25_sgs_df) <- c('x','y','doy','Ecoregion')
median(day_25_sgs_df$doy) #136

#nmp
day_25_nmp <- raster('./../../Data/CDD/day_of_25/day_25_northern_mixed_prairies.tif')
plot(day_25_nmp)
day_25_nmp_df <- data.frame(rasterToPoints(day_25_nmp))
day_25_nmp_df$Ecoregion <- 'Northern mixed prairies'
colnames(day_25_nmp_df) <- c('x','y','doy','Ecoregion')
median(day_25_nmp_df$doy) #146

day_25_df <- rbind(day_25_nmp_df,day_25_sgs_df)

#filter out extreme high and low values
day_25_df <- day_25_df %>%
  dplyr::filter(doy < 297) %>%
  dplyr::filter(doy > 65)

day_25_pdf <- ggplot(day_25_df, aes(x = doy, fill = Ecoregion)) +
  #scale_y_continuous(expand = c(0,0),limits = c(0,1.02)) +
  #scale_x_continuous(expand = c(0,0),limits = c(-1.5,0.6)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.02)) +
  scale_x_continuous(expand = c(0, 0), limits = c(100, 181)) +
  geom_density(color = 'black', alpha = 0.5, aes(y = ..scaled..)) +
  scale_fill_manual(values = c(
    'Northern mixed prairies' = 'black',
    'Shortgrass steppe' = 'white'
  )) +
  xlab('Day of 25% growth') +
  ylab('Probability density') +
  theme(
    axis.text.x = element_text(color = 'black', size = 13),
    #angle=25,hjust=1),
    axis.text.y = element_text(color = 'black', size = 13),
    axis.title = element_text(color = 'black', size = 16),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = c(0.70, 0.25),
    #legend.position = 'none',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 15),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black")
  )

#save to file
png(height = 1700,width=2000,res=300,'./../../Figures/day25_distributions.png')

print(day_25_pdf)

dev.off()


# nmp_look <- stack(day_25_nmp,max_sens_doy_nmp)
# plot(nmp_look)

#combine
day_25_sgs_nmp <- raster::merge(day_25_sgs,day_25_nmp,tolerance=0.2)
crs(day_25_sgs_nmp) <- Albers
plot(day_25_sgs_nmp)
day_25_sgs_nmp <- data.frame(rasterToPoints(day_25_sgs_nmp))

#filter out extreme high and low values
day_25_sgs_nmp <- day_25_sgs_nmp %>%
  dplyr::filter(layer < 297) %>%
  dplyr::filter(layer > 65)

str(day_25_sgs_nmp)
head(day_25_sgs_nmp)

#play around with creating day ranges
# day_25_sgs_nmp_2 <- day_25_sgs_nmp %>%
#   group_by(x,y) %>%
#   summarise(
#   layer2 = paste((layer-16), min(layer), sep = "-"))
# 
# head(day_25_sgs_nmp_2)
# day_25_sgs_nmp_2$layer2 <- as.numeric(day_25_sgs_nmp_2$layer2)

#turn to raster and fix projection, and then back to dataframe for plotting
day_25_sgs_nmp <- rasterFromXYZ(day_25_sgs_nmp)
proj4string(day_25_sgs_nmp) <- CRS("+proj=longlat")
day_25_sgs_nmp <-projectRaster(day_25_sgs_nmp, crs='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96
+        +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
day_25_sgs_nmp_df <- data.frame(rasterToPoints(day_25_sgs_nmp))


day_25_sgs_nmp_map <- ggplot() +
  geom_polygon(data=states_all_sites_tidy, mapping=aes(x = long, y = lat,group=group),
               color = "black", size = 0.1,fill=NA) +
  geom_raster(data=day_25_sgs_nmp_df, mapping=aes(x = x, y = y, fill = layer)) + 
  coord_equal() +
  scale_fill_scico('Day of 25% growth',palette = 'batlow',direction=1) +
  xlab('') +
  ylab('') +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme(
    axis.text.x = element_blank(), #angle=25,hjust=1),
    axis.text.y = element_blank(),
    axis.title.x = element_text(color='black',size=10),
    axis.title.y = element_text(color='black',size=10),
    axis.ticks = element_blank(),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    #legend.text = element_text(size=2),
    #legend.position = c(0.7,0.1),
    #legend.margin =margin(r=5,l=5,t=5,b=5),
    #legend.position = c(0.0,0.1),
    legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_blank(),
    axis.line.y = element_blank())


#save to file
png(height = 1250,width=920,res=300,'./../../Figures/day_25_growth_map.png')

print(day_25_sgs_nmp_map)

dev.off()


#impact of drought on the 25% day of growth

#import

#sgs
day_25_drought_sgs <-
  raster('./../../Data/CDD/day_of_25/day_25_droughtshortgrass_steppe.tif')
plot(day_25_drought_sgs)
day_25_drought_sgs <- stack(day_25_drought_sgs, day_25_sgs)
plot(day_25_drought_sgs)
day_25_drought_sgs_2 <-
  day_25_drought_sgs$day_25_droughtshortgrass_steppe -
  day_25_drought_sgs$day_25_shortgrass_steppe
plot(day_25_drought_sgs_2)
summary(day_25_drought_sgs_2)
#median = -21

plot(layer~y,data=rasterToPoints(day_25_drought_sgs_2))

#nmp
day_25_drought_nmp <-
  raster('./../../Data/CDD/day_of_25/day_25_droughtnorthern_mixed_prairies.tif')
plot(day_25_drought_nmp)
day_25_drought_nmp <- stack(day_25_drought_nmp, day_25_nmp)
plot(day_25_drought_nmp)
day_25_drought_nmp_2 <-
  day_25_drought_nmp$day_25_droughtnorthern_mixed_prairies -
  day_25_drought_nmp$day_25_northern_mixed_prairies
plot(day_25_drought_nmp_2)
summary(day_25_drought_nmp_2)
#median = -14
hist(day_25_drought_nmp_2$layer)

#combine
#combine
day_25_drought <-
  raster::merge(day_50_drought_nmp_2, day_50_drought_sgs_2, tolerance = 0.20)
proj4string(day_25_drought) <- CRS("+proj=longlat")
day_25_drought <-projectRaster(day_25_drought, crs='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96
+        +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
day_25_drought_df <- data.frame(rasterToPoints(day_25_drought))

day_25_sgs_nmp_drought_map <-
  ggplot() +
  geom_polygon(data=states_all_sites_tidy, mapping=aes(x = long, y = lat,group=group),
               color = "black", size = 0.1,fill=NA) +
  geom_raster(data=day_25_drought_df, mapping=aes(x = x, y = y, fill = layer)) + 
  coord_equal() +
  scale_fill_scico('Drought impact to day \n of 25% growth (days)',palette = 'batlow',direction=-1) +
  xlab('') +
  ylab('') +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme(
    axis.text.x = element_blank(), #angle=25,hjust=1),
    axis.text.y = element_blank(),
    axis.title.x = element_text(color='black',size=10),
    axis.title.y = element_text(color='black',size=10),
    axis.ticks = element_blank(),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    #legend.text = element_text(size=2),
    #legend.position = c(0.7,0.1),
    #legend.margin =margin(r=5,l=5,t=5,b=5),
    #legend.position = c(0.0,0.1),
    legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_blank(),
    axis.line.y = element_blank())

#save to file
png(
  height = 1500,
  width = 2000,
  res = 300,
  './../../Figures/day_25_drought_growth_map.png'
)

print(day_25_sgs_nmp_drought_map)

dev.off()

#PDF of drought impact to 25% growth

#nmp
day_25_drought_nmp_2_df <- data.frame(rasterToPoints(day_25_drought_nmp_2))
day_25_drought_nmp_2_df$region <- 'Northern mixed prairies'

#sgs
day_25_drought_sgs_2_df <- data.frame(rasterToPoints(day_25_drought_sgs_2))
day_25_drought_sgs_2_df$region <- 'Shortgrass steppe'

#join
day_25_drought_nmp_sgs_2_df <- rbind(day_25_drought_nmp_2_df,day_25_drought_sgs_2_df)
head(day_25_drought_nmp_sgs_2_df)

#plot it
drought_day25_pdf <- ggplot(day_25_drought_nmp_sgs_2_df, aes(x = layer, fill = region)) +
  scale_y_continuous(expand = c(0,0)) +
  #scale_x_continuous(expand = c(0,0),limits = c(-1.5,0.6)) +
  #scale_y_continuous(expand = c(0, 0), limits = c(0, 1.02)) +
  #scale_x_continuous(expand = c(0, 0), limits = c(230, 281)) +
  geom_histogram(color='black',binwidth = 1) +
  #geom_density(color = 'black', alpha = 0.5, aes(y = ..scaled..)) +
  scale_fill_manual(values = c(
    'Northern mixed prairies' = 'grey70',
    'Shortgrass steppe' = 'white'
  )) +
  xlab('Drought impact to day of 25% growth') +
  ylab('Count') +
  theme(
    axis.text.x = element_text(color = 'black', size = 13),
    #angle=25,hjust=1),
    axis.text.y = element_text(color = 'black', size = 13),
    axis.title = element_text(color = 'black', size = 16),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = c(0.77, 0.55),
    #legend.position = 'none',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 15),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black")
  )

#save to file
png(height = 1700,width=2000,res=300,'./../../Figures/day25_drought_distributions.png')

print(drought_day25_pdf)

dev.off()
#-------------------------------------------------------------------------------
# day of maximum GPP (unclear if will do) ------




#-------------------------------------------------------------------------------
# growth curves in average and dry years ------


#import SGS
#mean
growth_curve_absolute_mean_sgs <- 
  read_csv('./../../Data/CDD/growth_curves/growth_curve_absolute_shortgrass_steppe.csv')
head(growth_curve_absolute_mean_sgs,1)

#drought
growth_curve_drought_absolute_mean_sgs <- 
  read_csv('./../../Data/CDD/growth_curves/drought_growth_curve_absolute_shortgrass_steppe.csv')
head(growth_curve_drought_absolute_mean_sgs,1)

#growth dynamics
growth_drynamics_sgs <- 
  read_csv('./../../Data/growth_dynamics/drought_gpp_reduction_shortgrass_steppe.csv')
head(growth_drynamics_sgs,1)


#import NMP
#mean
growth_curve_absolute_mean_nmp <- 
  read_csv('./../../Data/CDD/growth_curves/growth_curve_absolute_northern_mixed_prairies.csv')
head(growth_curve_absolute_mean_nmp,1)

#drought
growth_curve_drought_absolute_mean_nmp <- 
  read_csv('./../../Data/CDD/growth_curves/drought_growth_curve_absolute_northern_mixed_prairies.csv')
head(growth_curve_drought_absolute_mean_nmp,1)

#growth dynamics
growth_drynamics_nmp <- 
  read_csv('./../../Data/growth_dynamics/drought_gpp_reduction_northern_mixed_prairies.csv')
head(growth_drynamics_nmp,1)

#SGS growth curve figure
png(height = 1500,width=3000,res=300,'./../../Figures/multi_panel_growth_curves')


par(mfrow=c(1,2),cex = 0.5,lwd = 0.5,oma=c(3.2,2,1,1),mar = c(3,3,3,3))

# plot it out panel A: sgs
plot(gpp ~ day, growth_curve_absolute_mean_sgs,col='grey',pch=19,cex=0.1,
     ylab='',
     xlab='')
lines(gpp ~ day, growth_curve_drought_absolute_mean_sgs,col='red',pch=19,lwd=5)
lines(gpp ~ day, growth_curve_absolute_mean_sgs,col='grey',pch=19,lwd=5)
#abline(v=155)
text(160, 176, "June 28th",cex=1)
points(176, 176,pch=19,cex=3)
text(172, 97, "June 6th",cex=1)
points(157,102,pch=19,cex=3)
legend(175, 50, legend=c("Average year", "Drought year"),         #alpha legend: 0.015, 150
       col=c("grey", "red"), lty=1.1,lwd=4,cex=2,box.lty=0)
legend(175, 75, legend=c("50% of total production"),         #alpha legend: 0.015, 150
       col=c("black"), pch=19,box.lty=0,cex=2)
mtext('Julian day of year',side=1,line=3.0,cex=1.5)
mtext('Cumulative GPP',side=2,line=2.5,cex=1.5)
mtext('Shortgrass steppe',side=3,line=0.5,cex=1.5)

# plot it out panel B: nmp
plot(gpp ~ day, growth_curve_absolute_mean_nmp,col='grey',pch=19,
     ylab='',
     xlab='')
points(gpp ~ day, growth_curve_drought_absolute_mean_nmp,col='red',pch=19)
lines(gpp ~ day, growth_curve_drought_absolute_mean_nmp,col='red',pch=19,lwd=5)
lines(gpp ~ day, growth_curve_absolute_mean_nmp,col='grey',pch=19,lwd=5)
#abline(v=155)
text(158, 210, "June 23rd",cex=1)
points(174, 210,pch=19,cex=3)
text(180, 170, "June 12th",cex=1)
points(163,170,pch=19,cex=3)
mtext('Julian day of year',side=1,line=3.0,cex=1.5)
#mtext('GPP',side=2,line=2.5,cex=1.5)
mtext('Northern mixed prairies',side=3,line=0.5,cex=1.5)

#dev.off()

#inset SGS
par(fig = c(0.05,0.30,0.60,0.95), new = TRUE)
plot(perc_change~doy,data=growth_drynamics_sgs,cex=0.1,
     xlab='Julian day',ylab='Drought impact (% change in GPP)')
lines(perc_change~doy,data=growth_drynamics_sgs)
lines(upper~as.numeric(as.integer(doy)),growth_drynamics_sgs)
lines(lower~doy,growth_drynamics_sgs)
abline(h=0)
mtext('Julian day of year',side=1,line=2.35,cex=0.75)
mtext('GPP impact (%)',side=2,line=2.0,cex=0.75)

#inset NMP
par(fig = c(0.55,0.80,0.60,0.95), new = TRUE)
plot(perc_change~doy,data=growth_drynamics_nmp,cex=0.1,
     xlab='Julian day',ylab='Drought impact (% change in GPP)')
lines(perc_change~doy,data=growth_drynamics_nmp)
lines(upper~as.numeric(as.integer(doy)),growth_drynamics_nmp)
lines(lower~doy,growth_drynamics_nmp)
abline(h=0)
mtext('Julian day of year',side=1,line=2.25,cex=0.75)
mtext('GPP impact (%)',side=2,line=2.35,cex=0.75)


dev.off()

#stopped here



#174-11

#-------------------------------------------------------------------------------
# VPD change during drought by season ------

#import
Ecoregion <- 'shortgrass_steppe'
seasonal_vpd_sgs <- read.csv(paste0(
     './../../Data/Climate/Ecoregion/',
     Ecoregion,
     '/PRISM/VPD_change.csv'
   )) 

Ecoregion <- 'northern_mixed_prairies'
seasonal_vpd_nmp <- read.csv(paste0(
  './../../Data/Climate/Ecoregion/',
  Ecoregion,
  '/PRISM/VPD_change.csv'
)) 

seasonal_vpd_sgs_nmp <- rbind(seasonal_vpd_nmp,seasonal_vpd_sgs)
head(seasonal_vpd_sgs_nmp,1)

vpd_change <- ggplot(seasonal_vpd_sgs_nmp, aes(x = abs_change, fill = season)) +
  facet_wrap(~ecoregion,scales='free') +
  #scale_y_continuous(expand = c(0,0),limits = c(0,1.02)) +
  #scale_x_continuous(expand = c(0,0),limits = c(-1.5,0.6)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.02)) +
  #scale_x_continuous(expand = c(0, 0), limits = c(230, 281)) +
  geom_density(color = 'black', alpha = 0.5, aes(y = ..scaled..)) +
  #geom_histogram()
  scale_fill_manual(values = c(
    'spring' = 'black',
    'summer' = 'white'
  )) +
  xlab('Increase in maximum VPD') +
  ylab('Probability density') +
  theme(
    axis.text.x = element_text(color = 'black', size = 13),
    #angle=25,hjust=1),
    axis.text.y = element_text(color = 'black', size = 13),
    axis.title = element_text(color = 'black', size = 16),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = c(0.4, 0.75),
    #legend.position = 'none',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 15),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black")
  )

#save to file
png(height = 1700,width=2500,res=300,'./../../Figures/vpd_change.png')

print(vpd_change)

dev.off()

#-------------------------------------------------------------------------------
# Change in proportion of spring precipiation during drought ------

#import
seasonal_change_spring_sgs_df <- 
  read.csv(paste0('./../../Data/Climate/Ecoregion/shortgrass_steppe/Precipitation/seasonal_change_PPT.csv'))
seasonal_change_spring_sgs_df$Ecoregion <- 'Shortgrass steppe'
seasonal_change_spring_sgs <- seasonal_change_spring_sgs_df %>% select(x,y,change_in_perc_spring)
seasonal_change_spring_sgs <- rasterFromXYZ(seasonal_change_spring_sgs)

seasonal_change_spring_nmp_df <- 
  read.csv(paste0('./../../Data/Climate/Ecoregion/northern_mixed_prairies/Precipitation/seasonal_change_PPT.csv'))
seasonal_change_spring_nmp_df$Ecoregion <- 'Northern mixed prairies'
seasonal_change_spring_nmp <- seasonal_change_spring_nmp_df %>% select(x,y,change_in_perc_spring)
seasonal_change_spring_nmp <- rasterFromXYZ(seasonal_change_spring_nmp)
#some pixels missing for NMP, need to check this...

seasonal_change_spring_ppt <- raster::merge(seasonal_change_spring_nmp,seasonal_change_spring_sgs,
                                            tolerance=0.2)
plot(seasonal_change_spring_ppt)

# seasonal_change_spring_ppt <- rbind(seasonal_change_sgs,seasonal_change_nmp)
# head(seasonal_change_spring_ppt,1)
# rm(seasonal_change_sgs,seasonal_change_nmp)
# 
# seasonal_change_spring_ppt <- seasonal_change_spring_ppt %>%
#   select(x,y,change_in_perc_spring)

#turn to raster and fix projection, and then back to dataframe for plotting
seasonal_change_spring_ppt  <- rasterFromXYZ(seasonal_change_spring_ppt)
proj4string(seasonal_change_spring_ppt ) <- CRS("+proj=longlat")
seasonal_change_spring_ppt  <-projectRaster(seasonal_change_spring_ppt , 
crs='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96
+        +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
seasonal_change_spring_ppt_df <- data.frame(rasterToPoints(seasonal_change_spring_ppt))
#plot(seasonal_change_spring_ppt)

spring_ppt_map <- ggplot() +
  geom_polygon(data=states_all_sites_tidy, mapping=aes(x = long, y = lat,group=group),
               color = "black", size = 0.1,fill=NA) +
  geom_raster(data=seasonal_change_spring_ppt_df, mapping=aes(x = x, y = y, fill = layer)) + 
  coord_equal() +
  scale_fill_scico('Change in % of spring precipitation',palette = 'roma',direction=1) +
  xlab('') +
  ylab('') +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme(
    axis.text.x = element_blank(), #angle=25,hjust=1),
    axis.text.y = element_blank(),
    axis.title.x = element_text(color='black',size=10),
    axis.title.y = element_text(color='black',size=10),
    axis.ticks = element_blank(),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    #legend.text = element_text(size=2),
    #legend.position = c(0.7,0.1),
    #legend.margin =margin(r=5,l=5,t=5,b=5),
    #legend.position = c(0.0,0.1),
    legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_blank(),
    axis.line.y = element_blank())

#inset plot
spring_ppt_binded <- rbind(seasonal_change_spring_sgs_df,seasonal_change_spring_nmp_df)

spring_ppt_pdf <- ggplot(spring_ppt_binded, aes(x = change_in_perc_spring, fill = Ecoregion)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.02)) +
  geom_density(color = 'black', alpha = 0.5, aes(y = ..scaled..)) +
  scale_fill_manual(values = c(
    'Northern mixed prairies' = 'grey70',
    'Shortgrass steppe' = 'white'
  )) +
  geom_vline(xintercept = 0,color='red') +
  xlab('Change in spring PPT (%)') +
  ylab('Probability density') +
  theme(
    axis.text.x = element_text(color = 'black', size = 7),
    #angle=25,hjust=1),
    axis.text.y = element_text(color = 'black', size = 7),
    axis.title = element_text(color = 'black', size = 10),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 5),
    legend.position = c(0.82, 0.81),
    #legend.position = 'none',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 15),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black")
  )


#make and save the inset plot
library(grid)
vp <- viewport(width = 0.44, height = 0.39, x = 0.23,y=0.27)
# y = unit(0.7, "lines"), just = c("right",
#                                  "bottom")

#executing the inset, you create a function the utlizes all the previous code
library(grid)

full <- function() {
  print(spring_ppt_map)
  print(spring_ppt_pdf , vp = vp)
}


png(height = 1700,width=2000,res=300,'Figures/change_in_perc_spring_PPT_drought.png')

full()

dev.off()

