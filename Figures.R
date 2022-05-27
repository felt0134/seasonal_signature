# Figures 
library(scico)
#https://www.data-imaginist.com/2018/scico-and-the-colour-conundrum/

library(broom)
library(sp)
library(grid)

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
  'Wyoming','Colorado','Oklahoma','Kansas',
  'Montana','New Mexico','Texas',
  'North Dakota','South Dakota','Nebraska'),]

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


#combine
day_90_sgs_nmp <- raster::merge(day_90_sgs,day_90_nmp,tolerance=0.2)
crs(day_90_sgs_nmp) <- Albers
plot(day_90_sgs_nmp)
day_90_sgs_nmp <- data.frame(rasterToPoints(day_90_sgs_nmp))

#filter out extreme high and low values
day_90_sgs_nmp <- day_90_sgs_nmp %>%
  dplyr::filter(layer < 297) %>%
  dplyr::filter(layer > 65)

#turn to raster and fix projection, and then back to dataframe for plotting
day_90_sgs_nmp <- rasterFromXYZ(day_90_sgs_nmp)
proj4string(day_90_sgs_nmp) <- CRS("+proj=longlat")
day_90_sgs_nmp <-projectRaster(day_90_sgs_nmp, crs='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96
+        +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
day_90_sgs_nmp_df <- data.frame(rasterToPoints(day_90_sgs_nmp))


#impact of drought on the 90% day of growth

#sgs
day_90_drought_sgs <-
  raster('./../../Data/CDD/day_of_90/day_90_droughtshortgrass_steppe.tif')
#plot(day_90_drought_sgs)
day_90_drought_sgs <- stack(day_90_drought_sgs, day_90_sgs)
plot(day_90_drought_sgs)
day_90_drought_sgs_2 <-
  day_90_drought_sgs$day_90_droughtshortgrass_steppe -
  day_90_drought_sgs$day_90_shortgrass_steppe
#plot(day_90_drought_sgs_2)
summary(day_90_drought_sgs_2)
#median = 4

#nmp
day_90_drought_nmp <-
  raster('./../../Data/CDD/day_of_90/day_90_droughtnorthern_mixed_prairies.tif')
#plot(day_90_drought_nmp)
day_90_drought_nmp <- stack(day_90_drought_nmp, day_90_nmp)
#plot(day_90_drought_nmp)
day_90_drought_nmp_2 <-
  day_90_drought_nmp$day_90_droughtnorthern_mixed_prairies -
  day_90_drought_nmp$day_90_northern_mixed_prairies
#plot(day_90_drought_nmp_2)
summary(day_90_drought_nmp_2)
#median = 0

#combine
day_90_drought <-
  raster::merge(day_90_drought_nmp_2, day_90_drought_sgs_2, tolerance = 0.20)
proj4string(day_90_drought) <- CRS("+proj=longlat")
day_90_drought <-projectRaster(day_90_drought, crs='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96
+        +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
day_90_drought_df <- data.frame(rasterToPoints(day_90_drought))

day_90_sgs_nmp_drought_map <-
  ggplot() +
  geom_polygon(data=states_all_sites_tidy, mapping=aes(x = long, y = lat,group=group),
               color = "black", size = 0.1,fill=NA) +
  geom_raster(data=day_90_drought_df, mapping=aes(x = x, y = y, fill = layer)) + 
  coord_equal() +
  scale_fill_scico('Drought impact to day of\n50% total growth (days)',palette = 'roma',direction=1) +
  xlab('') +
  ylab('') +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  coord_fixed(xlim=c(-1500000,0), ylim=c(9e+05,3100000)) + #crop
  theme(
    axis.text.x = element_blank(), #angle=25,hjust=1),
    axis.text.y = element_blank(),
    axis.title.x = element_text(color='black',size=10),
    axis.title.y = element_text(color='black',size=10),
    axis.ticks = element_blank(),
    legend.key = element_blank(),
    legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_blank(),
    axis.line.y = element_blank())

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
str(day_90_drought_nmp_sgs_2_df)
cor.test(day_90_drought_nmp_sgs_2_df$y,day_90_drought_nmp_sgs_2_df$layer)


#plot it
drought_day90_pdf <- ggplot(day_90_drought_nmp_sgs_2_df, aes(x = layer, fill = region)) +
  scale_y_continuous(expand = c(0,0)) +
  #scale_y_continuous(expand = c(0, 0), limits = c(0, 1.02)) +
  #geom_density(color = 'black', alpha = 0.5, aes(y = ..scaled..)) +
  geom_histogram(color='black') +
  scale_fill_manual(values = c(
    'Northern mixed prairies' = 'grey70',
    'Shortgrass steppe' = 'white'
  )) +
  #geom_vline(xintercept = 0,color='red') +
  xlab('Drought impact to day of\n90% total growth (days)') +
  ylab('Count') +
  theme(
    axis.text.x = element_text(color = 'black', size = 7),
    #angle=25,hjust=1),
    axis.text.y = element_text(color = 'black', size = 7),
    axis.title = element_text(color = 'black', size = 10),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 6),
    #legend.position = c(0.82, 0.7),
    legend.position = 'top',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 15),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black")
  )

#try to make inset
vp <- viewport(width = 0.44, height = 0.39, x = 0.23,y=0.27)
# y = unit(0.7, "lines"), just = c("right",
#                                  "bottom")

#executing the inset, you create a function the utlizes all the previous code
full <- function() {
  print(day_90_sgs_nmp_drought_map)
  print(drought_day90_pdf , vp = vp)
}


png(height = 1700,width=2000,res=300,'Figures/day90_drought_inset_plot.png')

full()

dev.off()


#correlation of day 90 impact and latitude 
day_90_drought_sgs_2_lat <- data.frame(rasterToPoints(day_90_drought_sgs_2))
day_90_drought_sgs_2_lat$ecoregion = 'Shortgrass steppe'

day_90_drought_nmp_2_lat <- data.frame(rasterToPoints(day_90_drought_nmp_2))
day_90_drought_nmp_2_lat$ecoregion = 'Northern mixed prairies'

day_90_drought_2_lat <- rbind(day_90_drought_sgs_2_lat,day_90_drought_nmp_2_lat)

cor.test(day_90_drought_2_lat$y,day_90_drought_2_lat$layer,method='spearman')

day_90_lat_plot <- ggplot(day_90_drought_2_lat, aes(x = y, y=layer,color = ecoregion)) +
  geom_point(alpha=0.5) +
  scale_colour_manual(values = c(
    'Shortgrass steppe' = 'green4',
    'Northern mixed prairies' = 'steelblue2'
  )) +
  geom_hline(yintercept = 0,size=2,color='grey') +
  stat_smooth(method='lm',color='black',size=2) +
  #geom_vline(xintercept = 0,color='red') +
  ylab('Effect of drought on day at which\n90% carbon uptake is achieved (days)') +
  xlab('Latitude') +
  annotate("text", x=32, y=17, label= "Delayed") +
  annotate("text", x=32, y=-15, label= "Advanced") +
  theme(
    axis.text.x = element_text(color = 'black', size = 15),
    #angle=25,hjust=1),
    axis.text.y = element_text(color = 'black', size = 15),
    axis.title = element_text(color = 'black', size = 18),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    #legend.position = c(0.82, 0.95),
    legend.position = 'top',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 15),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black")
  )

png(height = 1500,width=2000,res=300,'Figures/day90_drought_latitude_relationship.png')

day_90_lat_plot

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

# day_50_df <- rbind(day_50_nmp_df,day_50_sgs_df)
# 
# #filter out extreme high and low values
# day_50_df <- day_50_df %>%
#   dplyr::filter(doy < 297) %>%
#   dplyr::filter(doy > 65)


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

#turn to raster and fix projection, and then back to dataframe for plotting
day_50_sgs_nmp <- rasterFromXYZ(day_50_sgs_nmp)
proj4string(day_50_sgs_nmp) <- CRS("+proj=longlat")
day_50_sgs_nmp <-projectRaster(day_50_sgs_nmp, crs='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96
+        +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
day_50_sgs_nmp_df <- data.frame(rasterToPoints(day_50_sgs_nmp))


#impact of drought on the 50% day of growth

#import

#sgs
day_50_drought_sgs <-
  raster('./../../Data/CDD/day_of_50/day_50_droughtshortgrass_steppe.tif')
day_50_drought_sgs <- stack(day_50_drought_sgs, day_50_sgs)
day_50_drought_sgs_2 <-
  day_50_drought_sgs$day_50_droughtshortgrass_steppe -
  day_50_drought_sgs$day_50_shortgrass_steppe
# plot(day_50_drought_sgs_2)
# summary(day_50_drought_sgs_2)
#median = -22
#179-22

#plot(layer~y,data=rasterToPoints(day_50_drought_sgs_2))

#nmp
day_50_drought_nmp <-
  raster('./../../Data/CDD/day_of_50/day_50_droughtnorthern_mixed_prairies.tif')
day_50_drought_nmp <- stack(day_50_drought_nmp, day_50_nmp)
day_50_drought_nmp_2 <-
  day_50_drought_nmp$day_50_droughtnorthern_mixed_prairies -
  day_50_drought_nmp$day_50_northern_mixed_prairies
summary(day_50_drought_nmp_2)
#median = -11


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
#0.90 of pixels see day of 50% growth occur earlier during drought
  

day_50_sgs_nmp_drought_map <-
   ggplot() +
  geom_polygon(data=states_all_sites_tidy, mapping=aes(x = long, y = lat,group=group),
               color = "black", size = 0.1,fill=NA) +
  geom_raster(data=day_50_drought_df, mapping=aes(x = x, y = y, fill = layer)) + 
  coord_equal() +
  scale_fill_scico('Effect of drought on day at which\nhalf of total C uptake occurs (days)',palette = 'roma',direction=-1) +
  xlab('') +
  ylab('') +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  coord_fixed(xlim=c(-1500000,0), ylim=c(9e+05,3100000)) + #crop 
  theme(
    axis.text.x = element_blank(), #angle=25,hjust=1),
    axis.text.y = element_blank(),
    axis.title.x = element_text(color='black',size=10),
    axis.title.y = element_text(color='black',size=10),
    axis.ticks = element_blank(),
    legend.key = element_blank(),
    legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_blank(),
    axis.line.y = element_blank())


#PDF of drought impact to 50% growth

#nmp
day_50_drought_nmp_2_df <- data.frame(rasterToPoints(day_50_drought_nmp_2))
day_50_drought_nmp_2_df$region <- 'Northern mixed prairies'

#sgs
day_50_drought_sgs_2_df <- data.frame(rasterToPoints(day_50_drought_sgs_2))
day_50_drought_sgs_2_df$region <- 'Shortgrass steppe'

#join
day_50_drought_nmp_sgs_2_df <- rbind(day_50_drought_nmp_2_df,day_50_drought_sgs_2_df)
#head(day_50_drought_nmp_sgs_2_df,1)

#plot it
drought_day50_pdf <- ggplot(day_50_drought_nmp_sgs_2_df, aes(x = layer, fill = region)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.02)) +
  geom_density(color = 'black', alpha = 0.5, aes(y = ..scaled..)) +
  scale_fill_manual(values = c(
    'Northern mixed prairies' = 'steelblue2',
    'Shortgrass steppe' = 'green4'
  )) +
  geom_vline(xintercept = 0,color='black') +
  xlab("Effect of drought on day at which\nhalf of total C uptake occurs (days)") +
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



#try to make inset
vp <- viewport(width = 0.44, height = 0.39, x = 0.23,y=0.27)
# y = unit(0.7, "lines"), just = c("right",
#                                  "bottom")

#executing the inset, you create a function the utlizes all the previous code
full <- function() {
  print(day_50_sgs_nmp_drought_map)
  print(drought_day50_pdf , vp = vp)
}


png(height = 1700,width=2000,res=300,'Figures/day50_drought_inset_plot.png')

full()

dev.off()

#correlation of day 50 impact and latitude 
day_50_drought_sgs_2_lat <- data.frame(rasterToPoints(day_50_drought_sgs_2))
day_50_drought_sgs_2_lat$ecoregion = 'Shortgrass steppe'

day_50_drought_nmp_2_lat <- data.frame(rasterToPoints(day_50_drought_nmp_2))
day_50_drought_nmp_2_lat$ecoregion = 'Northern mixed prairies'

day_50_drought_2_lat <- rbind(day_50_drought_sgs_2_lat,day_50_drought_nmp_2_lat)

cor.test(day_50_drought_2_lat$y,day_50_drought_2_lat$layer,method='spearman',exact=FALSE)
#0.35

day_50_lat_plot <- ggplot(day_50_drought_2_lat, aes(x = y, y=layer,color = ecoregion)) +
  geom_point(alpha=0.25) +
  scale_colour_manual(values = c(
    'Shortgrass steppe' = 'green4',
    'Northern mixed prairies' = 'steelblue2'
  )) +
  geom_hline(yintercept = 0,size=2,color='grey') +
  stat_smooth(method='lm',color='black',size=2) +
  #geom_vline(xintercept = 0,color='red') +
  ylab('Effect of drought on day at which\n50% carbon uptake is achieved (days)') +
  xlab('Latitude') +
  annotate("text", x=46, y=40, label= "Delayed") +
  annotate("text", x=46, y=-50, label= "Advanced") +
  theme(
    axis.text.x = element_text(color = 'black', size = 15),
    #angle=25,hjust=1),
    axis.text.y = element_text(color = 'black', size = 15),
    axis.title = element_text(color = 'black', size = 18),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    #legend.position = c(0.82, 0.95),
    legend.position = 'top',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 15),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black")
  )

png(height = 1500,width=2000,res=300,'Figures/day50_drought_latitude_relationship.png')

day_50_lat_plot

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

#turn to raster and fix projection, and then back to dataframe for plotting
day_25_sgs_nmp <- rasterFromXYZ(day_25_sgs_nmp)
proj4string(day_25_sgs_nmp) <- CRS("+proj=longlat")
day_25_sgs_nmp <-projectRaster(day_25_sgs_nmp, crs='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96
+        +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
day_25_sgs_nmp_df <- data.frame(rasterToPoints(day_25_sgs_nmp))


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

#combine
day_25_drought <-
  raster::merge(day_25_drought_nmp_2, day_25_drought_sgs_2, tolerance = 0.20)
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
  scale_fill_scico('Drought impact to day \n of 25% growth (days)',palette = 'roma',direction=1) +
  xlab('') +
  ylab('') +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  coord_fixed(xlim=c(-1500000,0), ylim=c(9e+05,3100000)) + #crop
  theme(
    axis.text.x = element_blank(), #angle=25,hjust=1),
    axis.text.y = element_blank(),
    axis.title.x = element_text(color='black',size=10),
    axis.title.y = element_text(color='black',size=10),
    axis.ticks = element_blank(),
    legend.key = element_blank(),
    legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_blank(),
    axis.line.y = element_blank())


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
  geom_density(color = 'black', alpha = 0.5, aes(y = ..scaled..)) +
  scale_fill_manual(values = c(
    'Northern mixed prairies' = 'grey70',
    'Shortgrass steppe' = 'white'
  )) +
  #geom_vline(xintercept = 0,color='red') +
  xlab('Drought impact to day of\n25% total growth') +
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
    legend.position = c(0.80, 0.7),
    #legend.position = 'none',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 15),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black")
  )


vp <- viewport(width = 0.44, height = 0.39, x = 0.23,y=0.27)
# y = unit(0.7, "lines"), just = c("right",
#                                  "bottom")

#executing the inset, you create a function the utlizes all the previous code
full <- function() {
  print(day_25_sgs_nmp_drought_map)
  print(drought_day25_pdf , vp = vp)
}


png(height = 1700,width=2000,res=300,'Figures/day25_drought_inset_plot.png')

full()

dev.off()


#correlation of day 50 impact and latitude 
day_25_drought_sgs_2_lat <- data.frame(rasterToPoints(day_25_drought_sgs_2))
day_25_drought_sgs_2_lat$ecoregion = 'Shortgrass steppe'

day_25_drought_nmp_2_lat <- data.frame(rasterToPoints(day_25_drought_nmp_2))
day_25_drought_nmp_2_lat$ecoregion = 'Northern mixed prairies'

day_25_drought_2_lat <- rbind(day_25_drought_sgs_2_lat,day_25_drought_nmp_2_lat)

cor.test(day_25_drought_2_lat$y,day_25_drought_2_lat$layer,method='spearman',exact=FALSE)
#0.51

day_25_lat_plot <- ggplot(day_25_drought_2_lat, aes(x = y, y=layer,color = ecoregion)) +
  geom_point(alpha=0.25) +
  scale_colour_manual(values = c(
    'Shortgrass steppe' = 'green4',
    'Northern mixed prairies' = 'steelblue2'
  )) +
  geom_hline(yintercept = 0,size=2,color='grey') +
  stat_smooth(method='lm',color='black',size=2) +
  #geom_vline(xintercept = 0,color='red') +
  ylab('Effect of drought on day at which\n25% carbon uptake is achieved (days)') +
  xlab('Latitude') +
  annotate("text", x=46, y=14, label= "Delayed") +
  annotate("text", x=46, y=-32, label= "Advanced") +
  theme(
    axis.text.x = element_text(color = 'black', size = 15),
    #angle=25,hjust=1),
    axis.text.y = element_text(color = 'black', size = 15),
    axis.title = element_text(color = 'black', size = 18),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    #legend.position = c(0.82, 0.95),
    legend.position = 'top',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 15),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black")
  )

png(height = 1500,width=2000,res=300,'Figures/day25_drought_latitude_relationship.png')

day_25_lat_plot

dev.off()

#-------------------------------------------------------------------------------
# day of maximum GPP (unclear if will do) ------




#-------------------------------------------------------------------------------
# growth curves in average and dry years ------


#import SGS
#mean
growth_curve_absolute_mean_sgs <- 
  read_csv('./../../Data/growth_curves/average_growth_curve_shortgrass_steppe.csv')
head(growth_curve_absolute_mean_sgs,1)

#drought
growth_curve_drought_absolute_mean_sgs <- 
  read_csv('./../../Data/growth_curves/drought_growth_curve_shortgrass_steppe.csv')
head(growth_curve_drought_absolute_mean_sgs,1)

#growth dynamics
growth_drynamics_sgs <- 
  read_csv('./../../Data/growth_dynamics/drought_gpp_reduction_shortgrass_steppe.csv')
head(growth_drynamics_sgs,1)

growth_drynamics_sgs$upper <- growth_drynamics_sgs$perc_change + growth_drynamics_sgs$ci_99
growth_drynamics_sgs$lower <- growth_drynamics_sgs$perc_change - growth_drynamics_sgs$ci_99

#import NMP
#mean
growth_curve_absolute_mean_nmp <- 
  read_csv('./../../Data/growth_curves/average_growth_curve_northern_mixed_prairies.csv')
head(growth_curve_absolute_mean_nmp,1)

#drought
growth_curve_drought_absolute_mean_nmp <- 
  read_csv('./../../Data/growth_curves/drought_growth_curve_northern_mixed_prairies.csv')
head(growth_curve_drought_absolute_mean_nmp,1)

#growth dynamics
growth_drynamics_nmp <- 
  read_csv('./../../Data/growth_dynamics/drought_gpp_reduction_northern_mixed_prairies.csv')
head(growth_drynamics_nmp,1)

#SGS growth curve figure
png(height = 1500,width=3000,res=300,'Figures/multi_panel_growth_curves')

par(mfrow=c(1,2),cex = 0.5,lwd = 0.5,oma=c(3.2,6,1,1),mar = c(3,1.25,3,3))
#?par
# plot it out panel A: sgs
plot(mean ~ doy, growth_curve_absolute_mean_sgs,col='black',type='l',
     ylab='',
     xlab='',las=1,cex.axis=1.5,ylim=c(0,391))
polygon(c(growth_curve_absolute_mean_sgs$doy,rev(growth_curve_absolute_mean_sgs$doy)),
        c(growth_curve_absolute_mean_sgs$lower_temporal,rev(growth_curve_absolute_mean_sgs$upper_temporal)),
        col = "grey", border = F)
polygon(c(growth_curve_absolute_mean_sgs$doy,rev(growth_curve_absolute_mean_sgs$doy)),
        c(growth_curve_absolute_mean_sgs$lower_spatial,rev(growth_curve_absolute_mean_sgs$upper_spatial)),
        col = "black", border = F)
polygon(c(growth_curve_drought_absolute_mean_sgs$doy,rev(growth_curve_drought_absolute_mean_sgs$doy)),
        c(growth_curve_drought_absolute_mean_sgs$lower,rev(growth_curve_drought_absolute_mean_sgs$upper)),
        col = "red", border = F)
#lines(mean ~ doy, growth_curve_drought_absolute_mean_sgs,col='black',pch=19,lwd=1)
#abline(v=155)
text(162, 176, "June 28th",cex=1)
points(178, 176,pch=19,cex=3)
text(170, 97, "June 6th",cex=1)
points(157,102,pch=19,cex=3)
legend(175, 60, legend=c("Average year", "Drought year"),         #alpha legend: 0.015, 150
       col=c("grey", "red"), lty=1.1,lwd=4,cex=2,box.lty=0)
legend(175, 90, legend=c("50% of total production"),         #alpha legend: 0.015, 150
       col=c("black"), pch=19,box.lty=0,cex=2)
mtext('Julian day of year',side=1,line=3.75,cex=1.5)
mtext(expression("Cumulative carbon uptake " (g~C~m^-2)),side=2,line=3.5,cex=1.5)
mtext('Shortgrass steppe',side=3,line=0.5,cex=1.25)
mtext('a',side=3,line=0.5,cex=1.5,adj=-0.05)
#?mtext

# plot it out panel B: nmp
plot(mean ~ doy, growth_curve_absolute_mean_nmp,col='black',type='l',
     ylab='',
     xlab='',las=1,cex.axis=1.5,ylim=c(0,480))
polygon(c(growth_curve_absolute_mean_nmp$doy,rev(growth_curve_absolute_mean_nmp$doy)),
        c(growth_curve_absolute_mean_nmp$lower_temporal,rev(growth_curve_absolute_mean_nmp$upper_temporal)),
        col = "grey", border = F)
polygon(c(growth_curve_absolute_mean_nmp$doy,rev(growth_curve_absolute_mean_nmp$doy)),
        c(growth_curve_absolute_mean_nmp$lower_spatial,rev(growth_curve_absolute_mean_nmp$upper_spatial)),
        col = "black", border = F)
polygon(c(growth_curve_drought_absolute_mean_nmp$doy,rev(growth_curve_drought_absolute_mean_nmp$doy)),
        c(growth_curve_drought_absolute_mean_nmp$lower,rev(growth_curve_drought_absolute_mean_nmp$upper)),
        col = "red", border = F)

text(158, 210, "June 23rd",cex=1)
points(174, 210,pch=19,cex=3)
text(179, 170, "June 12th",cex=1)
points(163,170,pch=19,cex=3)
mtext('Julian day of year',side=1,line=3.75,cex=1.5)
#mtext('GPP',side=2,line=2.5,cex=1.5)
mtext('Northern mixed prairies',side=3,line=0.5,cex=1.25)
mtext('b',side=3,line=0.5,cex=1.5,adj=-0.05)

#inset SGS (add the polygons for uncertainty)
panel.first = rect(c(1,7), -1e6, c(3,10), 1e6, col='green', border=NA)
par(fig = c(0.05,0.3,0.60,0.95), new = TRUE)
plot(perc_change~doy,data=growth_drynamics_sgs,type='l',
     xlab='',ylab='',las=1)
rect(151,-70,243,350,col = 'grey95')
rect(60,-70,151,350,col = 'grey')
polygon(c(growth_drynamics_sgs$doy,rev(growth_drynamics_sgs$doy)),
        c(growth_drynamics_sgs$lower,rev(growth_drynamics_sgs$upper)),
        col = "black", border = F)
text(100, -50, "Spring",cex=1)
text(200, -20, "Summer",cex=1)
text(275, -20, "Fall",cex=1)
abline(h=0,col='black',lty='dashed')
mtext('Julian day of year',side=1,line=2.35,cex=0.75)
mtext('% Change in C uptake',side=2,line=2.5,cex=0.6)

#inset NMP
par(fig = c(0.55,0.78,0.60,0.95), new = TRUE)
plot(perc_change~doy,data=growth_drynamics_nmp,type='l',
     xlab='',ylab='',las=1)
rect(151,-50,243,350,col = 'grey95')
rect(60,-50,151,350,col = 'grey')
polygon(c(growth_drynamics_nmp$doy,rev(growth_drynamics_nmp$doy)),
        c(growth_drynamics_nmp$lower,rev(growth_drynamics_nmp$upper)),
        col = "black", border = F)
# lines(perc_change~doy,data=growth_drynamics_nmp)
# lines(upper~as.numeric(as.integer(doy)),growth_drynamics_nmp)
# lines(lower~doy,growth_drynamics_nmp)
abline(h=0,col='black',lty='dashed')
mtext('Julian day of year',side=1,line=2.35,cex=0.75)
mtext('% Change in C uptake',side=2,line=2.5,cex=0.6)

#?rect
dev.off()


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
seasonal_vpd_sgs$ecoregion <- 'Shortgrass steppe'

Ecoregion <- 'northern_mixed_prairies'
seasonal_vpd_nmp <- read.csv(paste0(
  './../../Data/Climate/Ecoregion/',
  Ecoregion,
  '/PRISM/VPD_change.csv'
)) 
seasonal_vpd_nmp$ecoregion <- 'Northern mixed prairies'

seasonal_vpd_sgs_nmp <- rbind(seasonal_vpd_nmp,seasonal_vpd_sgs)
head(seasonal_vpd_sgs_nmp,1)

vpd_change <- ggplot(seasonal_vpd_sgs_nmp, aes(x = abs_change, fill = season)) +
  facet_wrap(~ecoregion,ncol=1) +
  #scale_y_continuous(expand = c(0,0),limits = c(0,1.02)) +
  #scale_x_continuous(expand = c(0,0),limits = c(-1.5,0.6)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.02)) +
  #scale_x_continuous(expand = c(0, 0), limits = c(230, 281)) +
  geom_density(color = 'black', alpha = 0.5, aes(y = ..scaled..)) +
  geom_vline(xintercept = 0,color='red') +
  scale_fill_manual(values = c(
    'spring' = 'black',
    'summer' = 'white'
  )) +
  xlab('Increase in average daily maximum VPD (hPa)') +
  ylab('Probability density') +
  theme(
    axis.text.x = element_text(color = 'black', size = 13),
    #angle=25,hjust=1),
    axis.text.y = element_text(color = 'black', size = 13),
    axis.title = element_text(color = 'black', size = 25),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 15),
    legend.position = c(0.6, 0.8),
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
png(height = 3000,width=2500,res=300,'Figures/vpd_change.png')

print(vpd_change)

dev.off()

#-------------------------------------------------------------------------------
# change in proportion of spring precipitation during drought ------

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
proj4string(seasonal_change_spring_ppt) <- CRS("+proj=longlat")
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
  scale_fill_scico('Change in % of\nspring precipitation',palette = 'roma',direction=1) +
  xlab('') +
  ylab('') +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  coord_fixed(xlim=c(-1500000,0), ylim=c(9e+05,3100000)) + #crop 
  theme(
    axis.text.x = element_blank(), #angle=25,hjust=1),
    axis.text.y = element_blank(),
    axis.title.x = element_text(color='black',size=10),
    axis.title.y = element_text(color='black',size=10),
    axis.ticks = element_blank(),
    legend.key = element_blank(),
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
    'Northern mixed prairies' = 'steelblue2',
    'Shortgrass steppe' = 'green4'
  )) +
  geom_vline(xintercept = 0,color='black') +
  xlab('Change in % of\nspring precipitation') +
  ylab('Probability density') +
  theme(
    axis.text.x = element_text(color = 'black', size = 7),
    #angle=25,hjust=1),
    axis.text.y = element_text(color = 'black', size = 7),
    axis.title = element_text(color = 'black', size = 10),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 6),
    #legend.position = c(0.83, 0.81),
    legend.position = 'top',
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


#-------------------------------------------------------------------------------
# points used for randomized VPD subset ------

#load file (will need to update workign directory)
rangeland_npp_covariates<-readRDS(file.path(test_wd, "Dryland_NPP.rds")) #loads file and name it annualSWA_OctDec I guess

nm_sgs<-rangeland_npp_covariates %>%
  dplyr::filter(region==c('northern_mixed_prairies','shortgrass_steppe')) 

mean_production_raster<-rasterFromXYZ(mean_npp)
#plot(mean_production_raster)

#import shapefules

#SGS
SGS.shape<-readOGR(dsn="/Volumes/GoogleDrive/My Drive/range-resilience/scope/study-area-shapefiles/SGS",layer="SGS")
#plot(SGS.shape)
SGS.shape@bbox <- as.matrix(extent(mean_production_raster))
#plot(SGS.shape)
SGS.shape.2 <- sp::spTransform(SGS.shape, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#NMP
NorthernMixedSubset.shape<-readOGR(dsn="/Volumes/GoogleDrive/My Drive/range-resilience/scope/study-area-shapefiles/NorthernMixedSubset",layer="NorthernMixedSubset")
#plot(NorthernMixedSubset.shape)
NorthernMixedSubset.shape@bbox <- as.matrix(extent(mean_production_raster))
plot(NorthernMixedSubset.shape)
#step 2:
NorthernMixedSubset.shape.2 <- sp::spTransform(NorthernMixedSubset.shape, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
#plot(NorthernMixedSubset.shape.2)
crop(NorthernMixedSubset.shape.2,mean_production_raster)

#combine
sgs_nmp_combined<-NorthernMixedSubset.shape.2 +SGS.shape.2

#import VPD dataset
Ecoregion <- 'shortgrass_steppe'
seasonal_vpd_sgs <- read.csv(paste0(
  './../../Data/Climate/Ecoregion/',
  Ecoregion,
  '/PRISM/VPD_change.csv'
)) 
seasonal_vpd_sgs$ecoregion <- 'Shortgrass steppe'

Ecoregion <- 'northern_mixed_prairies'
seasonal_vpd_nmp <- read.csv(paste0(
  './../../Data/Climate/Ecoregion/',
  Ecoregion,
  '/PRISM/VPD_change.csv'
)) 
seasonal_vpd_nmp$ecoregion <- 'Northern mixed prairies'

seasonal_vpd_sgs_nmp <- rbind(seasonal_vpd_nmp,seasonal_vpd_sgs)
head(seasonal_vpd_sgs_nmp,1)

png(height = 2000,width=2000,res=300,'Figures/VPD_sites.png')
plot(test,lwd=0.1)
plot(NorthernMixedSubset.shape.2,col='steelblue2', lwd = .1,add=TRUE)
plot(SGS.shape.2, lwd = 0.1,col='green4', lwd = .1,add=TRUE)
points(y~x,data=seasonal_vpd_sgs_nmp,col='black',cex=.25)
legend(-120, 38, legend=c("Shortgrass steppe", "Northern mixed prairies"),         #alpha legend: 0.015, 150
       col=c("green4", "steelblue2"), lty=1.1,lwd=4,cex=1,box.lty=0)
dev.off()




#-------------------------------------------------------------------------------
# ecoregion distributions (to do) ------

library(rgdal)

#shapefile referecne for state outlines. This will results in a sp file being downloaded...
us<-getData("GADM", country='USA', level=1,download=TRUE)
states_all_sites <- us[us$NAME_1 %in% c(
  'Colorado','Wyoming',
  'Montana','Texas','Kansas','New Mexico',
  'North Dakota','South Dakota','Nebraska',
  'Oklahoma'),]

states_all_sites <- sp::spTransform(states_all_sites, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
states_all_sites <- spTransform(states_all_sites, crs(Albers))
plot(states_all_sites)

#set directory
test_wd<-"/Volumes/GoogleDrive/My Drive/range-resilience/Sensitivity/Processing NPP Data/NPP Data processing"

#load file (will need to update working directory)
rangeland_npp_covariates<-readRDS(file.path(test_wd, "Dryland_NPP.rds")) #loads file and name it annualSWA_OctDec I guess

nm_sgs<-rangeland_npp_covariates %>%
  dplyr::filter(region==c('northern_mixed_prairies','shortgrass_steppe')) 

mean_npp<-aggregate(npp~x+y,mean,data=nm_sgs)

mean_production_raster<-rasterFromXYZ(mean_npp)
#plot(mean_production_raster)

#import shapefules

#SGS
SGS.shape<-readOGR(dsn="/Volumes/GoogleDrive/My Drive/range-resilience/scope/study-area-shapefiles/SGS",layer="SGS")
#plot(SGS.shape)
SGS.shape@bbox <- as.matrix(extent(mean_production_raster))
#plot(SGS.shape)
SGS.shape.2 <- sp::spTransform(SGS.shape, crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
SGS.shape.3 <- sp::spTransform(SGS.shape.2, crs(Albers))
plot(SGS.shape.3)

#NMP
NorthernMixedSubset.shape<-readOGR(dsn="/Volumes/GoogleDrive/My Drive/range-resilience/scope/study-area-shapefiles/NorthernMixedSubset",layer="NorthernMixedSubset")
#plot(NorthernMixedSubset.shape)
NorthernMixedSubset.shape@bbox <- as.matrix(extent(mean_production_raster))
plot(NorthernMixedSubset.shape)
#step 2:
NorthernMixedSubset.shape.2 <- sp::spTransform(NorthernMixedSubset.shape, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
#plot(NorthernMixedSubset.shape.2)
NorthernMixedSubset.shape.3 <- sp::spTransform(NorthernMixedSubset.shape.2, crs(Albers))
plot(NorthernMixedSubset.shape.3)
crop(NorthernMixedSubset.shape.2,mean_production_raster)

#combine
test<-NorthernMixedSubset.shape.3 + SGS.shape.3

png(height = 2000,width=2100,res=300,'Figures/ecoregions.png')
plot(test,lwd=0.1)
plot(NorthernMixedSubset.shape.3,col='steelblue2', lwd = .1,add=TRUE)
plot(SGS.shape.3, lwd = 0.1,col='green4', lwd = .1,add=TRUE)
legend(-2090000, 1916791.1, legend=c("Shortgrass steppe", "Northern mixed prairies"),         #alpha legend: 0.015, 150
       col=c("green4", "steelblue2"), pch=19,cex=1,box.lty=0)
 plot(states_all_sites,add=TRUE,lwd=0.5)

dev.off()

#-------------------------------------------------------------------------------
# 1 km subset of gpp dynamics during drought --------


#import SGS
#mean
growth_curve_absolute_mean_sgs_1km <- 
  read_csv('./../../Data/growth_curves/one_km_subset/average_growth_curve_shortgrass_steppe.csv')
head(growth_curve_absolute_mean_sgs_1km,1)

#drought
growth_curve_drought_absolute_mean_sgs_1km <- 
  read_csv('./../../Data/growth_curves/one_km_subset/drought_growth_curve_shortgrass_steppe.csv')
head(growth_curve_drought_absolute_mean_sgs_1km,1)

#growth dynamics
growth_drynamics_sgs_1km <- 
  read_csv('./../../Data/growth_dynamics/one_km_subset/drought_gpp_reduction_1km_shortgrass_steppe.csv')
head(growth_drynamics_sgs,1)

growth_drynamics_sgs_1km$upper <- growth_drynamics_sgs_1km$perc_change + growth_drynamics_sgs_1km$ci_99
growth_drynamics_sgs_1km$lower <- growth_drynamics_sgs_1km$perc_change - growth_drynamics_sgs_1km$ci_99

#import NMP
#mean
growth_curve_absolute_mean_nmp_1km <- 
  read_csv('./../../Data/growth_curves/one_km_subset/average_growth_curve_northern_mixed_prairies.csv')
head(growth_curve_absolute_mean_nmp_1km,1)

#drought
growth_curve_drought_absolute_mean_nmp_1km <- 
  read_csv('./../../Data/growth_curves/one_km_subset/drought_growth_curve_northern_mixed_prairies.csv')
head(growth_curve_drought_absolute_mean_nmp_1km,1)

#growth dynamics
growth_drynamics_nmp_1km <- 
  read_csv('./../../Data/growth_dynamics/one_km_subset/drought_gpp_reduction_1km_northern_mixed_prairies.csv')
head(growth_drynamics_nmp_1km,1)

growth_drynamics_nmp_1km$upper <- growth_drynamics_nmp_1km$perc_change + growth_drynamics_nmp_1km$ci_99
growth_drynamics_nmp_1km$lower <- growth_drynamics_nmp_1km$perc_change - growth_drynamics_nmp_1km$ci_99


#SGS growth curve figure
png(height = 1500,width=3000,res=300,'Figures/multi_panel_growth_curves_1km')


par(mfrow=c(1,2),cex = 0.5,lwd = 0.5,oma=c(3.2,6,1,1),mar = c(3,1.25,3,3))
#?par
# plot it out panel A: sgs
plot(mean ~ doy, growth_curve_absolute_mean_sgs_1km,col='black',type='l',
     ylab='',
     xlab='',ylim=c(0,400),las=1,cex.axis=1.5)
polygon(c(growth_curve_absolute_mean_sgs_1km$doy,rev(growth_curve_absolute_mean_sgs_1km$doy)),
        c(growth_curve_absolute_mean_sgs_1km$lower_temporal,rev(growth_curve_absolute_mean_sgs_1km$upper_temporal)),
        col = "grey", border = F)
polygon(c(growth_curve_absolute_mean_sgs_1km$doy,rev(growth_curve_absolute_mean_sgs_1km$doy)),
        c(growth_curve_absolute_mean_sgs_1km$lower_spatial,rev(growth_curve_absolute_mean_sgs_1km$upper_spatial)),
        col = "black", border = F)
polygon(c(growth_curve_drought_absolute_mean_sgs_1km$doy,rev(growth_curve_drought_absolute_mean_sgs_1km$doy)),
        c(growth_curve_drought_absolute_mean_sgs_1km$lower,rev(growth_curve_drought_absolute_mean_sgs_1km$upper)),
        col = "red", border = F)
legend(175, 50, legend=c("Average year", "Drought year"),         #alpha legend: 0.015, 150
       col=c("grey", "red"), lty=1.1,lwd=4,cex=2,box.lty=0)
# legend(175, 75, legend=c("50% of total production"),         #alpha legend: 0.015, 150
#        col=c("black"), pch=19,box.lty=0,cex=2)
mtext('Julian day of year',side=1,line=3.75,cex=1.5)
mtext(expression("Cumulative carbon uptake " (g~C~m^-2)),side=2,line=3.5,cex=1.5)
mtext('Shortgrass steppe',side=3,line=0.5,cex=1.25)
mtext('a',side=3,line=0.5,cex=1.5,adj=-0.05)
#?mtext

# plot it out panel B: nmp
plot(mean ~ doy, growth_curve_absolute_mean_nmp_1km,col='black',type='l',
     ylab='',
     xlab='',las=1,cex.axis=1.5,ylim=c(0,510))
polygon(c(growth_curve_absolute_mean_nmp_1km$doy,rev(growth_curve_absolute_mean_nmp_1km$doy)),
        c(growth_curve_absolute_mean_nmp_1km$lower_temporal,rev(growth_curve_absolute_mean_nmp_1km$upper_temporal)),
        col = "grey", border = F)
polygon(c(growth_curve_absolute_mean_nmp_1km$doy,rev(growth_curve_absolute_mean_nmp_1km$doy)),
        c(growth_curve_absolute_mean_nmp_1km$lower_spatial,rev(growth_curve_absolute_mean_nmp_1km$upper_spatial)),
        col = "black", border = F)
polygon(c(growth_curve_drought_absolute_mean_nmp_1km$doy,rev(growth_curve_drought_absolute_mean_nmp_1km$doy)),
        c(growth_curve_drought_absolute_mean_nmp_1km$lower,rev(growth_curve_drought_absolute_mean_nmp_1km$upper)),
        col=adjustcolor("red",alpha.f=0.70), border = F)

mtext('Julian day of year',side=1,line=3.75,cex=1.5)
#mtext('GPP',side=2,line=2.5,cex=1.5)
mtext('Northern mixed prairies',side=3,line=0.5,cex=1.25)
mtext('b',side=3,line=0.5,cex=1.5,adj=-0.05)

#inset SGS (add the polygons for uncertainty)
panel.first = rect(c(1,7), -1e6, c(3,10), 1e6, col='green', border=NA)
par(fig = c(0.05,0.30,0.60,0.95), new = TRUE)
plot(perc_change~doy,data=growth_drynamics_sgs_1km,type='l',
     xlab='',ylab='',las=1,ylim=c(-70,10))
rect(151,-75,243,350,col = 'grey95')
rect(60,-75,151,350,col = 'grey')
polygon(c(growth_drynamics_sgs_1km$doy,rev(growth_drynamics_sgs_1km$doy)),
        c(growth_drynamics_sgs_1km$lower,rev(growth_drynamics_sgs_1km$upper)),
        col = "black", border = F)
text(100, -50, "Spring",cex=1)
text(200, -20, "Summer",cex=1)
text(275, -20, "Fall",cex=1)
abline(h=0,col='black',lty='dashed')
mtext('Julian day of year',side=1,line=2.35,cex=0.75)
mtext('% Change in C uptake',side=2,line=2.5,cex=0.6)

#inset NMP
par(fig = c(0.55,0.80,0.60,0.95), new = TRUE)
plot(perc_change~doy,data=growth_drynamics_nmp_1km,type='l',
     xlab='',ylab='',las=1,ylim=c(-50,75))
rect(151,-60,243,350,col = 'grey95')
rect(60,-60,151,350,col = 'grey')
polygon(c(growth_drynamics_nmp_1km$doy,rev(growth_drynamics_nmp_1km$doy)),
        c(growth_drynamics_nmp_1km$lower,rev(growth_drynamics_nmp_1km$upper)),
        col = "black", border = F)
abline(h=0,col='black',lty='dashed')
mtext('Julian day of year',side=1,line=2.35,cex=0.75)
mtext('% Change in C uptake',side=2,line=2.5,cex=0.6)

#?rect
dev.off()




#-------------------------------------------------------------------------------
# drought years map and barchart ------



Ecoregion <- 'shortgrass_steppe'
driest_year_sgs <- 
  read.csv(paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/growing_season/drought_precip_year_',Ecoregion,'.csv'))
driest_year_sgs$ecoregion <- 'Shortgrass steppe'

#select five most common years
driest_year_sgs_count <- aggregate(ppt~year + ecoregion,length,data=driest_year_sgs)
driest_year_sgs_count <- driest_year_sgs_count %>% arrange(desc(ppt)) %>%
  dplyr::filter(ppt >34)
  

Ecoregion <- 'northern_mixed_prairies'
driest_year_nmp <- 
  read.csv(paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/growing_season/drought_precip_year_',Ecoregion,'.csv'))
driest_year_nmp$ecoregion <- 'Northern mixed prairies'

#select five most common years
driest_year_nmp_count <- aggregate(ppt~year + ecoregion,length,data=driest_year_nmp)
driest_year_nmp_count <- driest_year_nmp_count %>% arrange(desc(ppt)) %>%
  filter(ppt > 872)

five_driest_years <- rbind(driest_year_sgs_count,driest_year_nmp_count)

head(five_driest_years,1)

five_driest_years$year <- as.factor(five_driest_years$year)

  driest_years_barchat <- ggplot(five_driest_years,aes(reorder(year,-ppt),ppt,fill=ecoregion)) +
    stat_summary(fun='mean',geom='bar',color='black') +
    ylab('Number of pixels') +
    xlab('Driest year (2003-2020)') +
    scale_fill_manual(values = c(
      'Shortgrass steppe' = 'green4',
      'Northern mixed prairies' = 'steelblue2'
    )) +
    #scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    theme(
      axis.text.x = element_text(color = 'black', size = 10),
      #angle=25,hjust=1),
      axis.text.y = element_text(color = 'black', size = 10),
      axis.title = element_text(color = 'black', size = 15),
      axis.ticks = element_line(color = 'black'),
      legend.key = element_blank(),
      legend.title = element_blank(),
      legend.text = element_text(size = 6),
      legend.position = 'none',
      #legend.position = c(0.65, 0.8),
      #legend.position = 'none',
      strip.background = element_rect(fill = "white"),
      strip.text = element_text(size = 15),
      panel.background = element_rect(fill = NA),
      panel.border = element_blank(),
      #make the borders clear in prep for just have two axes
      axis.line.x = element_line(colour = "black"),
      axis.line.y = element_line(colour = "black")
    )


#raster is acting weird
  str(driest_year_sgs)
#driest_year_sgs$year <- as.factor(driest_year_sgs$year)
driest_year_sgs_raster <- rasterFromXYZ(driest_year_sgs[c(1,2,4)])
driest_year_nmp_raster <- rasterFromXYZ(driest_year_nmp[c(1,2,4)])

driest_year_raster <- 
  raster::merge(driest_year_sgs_raster,driest_year_nmp_raster,
                tolerance=0.75)
plot(driest_year_raster)

driest_year_map <- rasterFromXYZ(driest_year_raster)
proj4string(driest_year_raster) <- CRS("+proj=longlat")
driest_year_raster <-projectRaster(driest_year_raster, crs='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96
+        +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')

driest_year_map_df <- data.frame(rasterToPoints(driest_year_raster))
driest_year_map_df$layer <- round(driest_year_map_df$layer)

  driest_year_map_plot <-
    ggplot() +
    geom_polygon(data=states_all_sites_tidy, mapping=aes(x = long, y = lat,group=group),
                 color = "black", size = 0.1,fill=NA) +
    geom_raster(data=driest_year_map_df, mapping=aes(x = x, y = y, fill =layer)) + 
    coord_equal() +
    scale_fill_scico('Driest year\n(2003-2020)',palette = 'batlow',direction=1) +
    xlab('') +
    ylab('') +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    coord_fixed(xlim=c(-1500000,0), ylim=c(9e+05,3100000)) + #crop 
    theme(
      axis.text.x = element_blank(), #angle=25,hjust=1),
      axis.text.y = element_blank(),
      axis.title.x = element_text(color='black',size=10),
      axis.title.y = element_text(color='black',size=10),
      axis.ticks = element_blank(),
      legend.key = element_blank(),
      legend.text = element_text(size=7),
      #legend.title = element_blank(),
      legend.position = 'top',
      plot.margin = margin(0.0,1,0.0,0,"cm"),
      strip.background =element_rect(fill="white"),
      strip.text = element_text(size=10),
      panel.background = element_rect(fill=NA),
      panel.border = element_blank(), #make the borders clear in prep for just have two axes
      axis.line.x = element_blank(),
      axis.line.y = element_blank())
  
  
  #make the inset
  
  vp <- viewport(width = 0.44, height = 0.39, x = 0.23,y=0.27)
  # y = unit(0.7, "lines"), just = c("right",
  #                                  "bottom")
  
  #executing the inset, you create a function the utlizes all the previous code
  full <- function() {
    print(driest_year_map_plot)
    print(driest_years_barchat , vp = vp)
  }
  
  
  png(height = 1700,width=2000,res=300,'Figures/driest_years.png')
  
  full()
  
  dev.off()
  
  


#-------------------------------------------------------------------------------
# growth curves (impacts through time) NDVI subset -----
  
  #growth dynamics NDVI
  
  #SGS
  growth_drynamics_ndvi_sgs <- 
    read_csv('./../../Data/growth_dynamics/drought_ndvi_reduction_shortgrass_steppe.csv')
  head(growth_drynamics_ndvi_sgs,1)
  
  growth_drynamics_ndvi_sgs$upper <- growth_drynamics_ndvi_sgs$perc_change + growth_drynamics_ndvi_sgs$ci_99
  growth_drynamics_ndvi_sgs$lower <- growth_drynamics_ndvi_sgs$perc_change - growth_drynamics_ndvi_sgs$ci_99
  
  #northern mixed prairies
  growth_drynamics_ndvi_nmp <- 
    read_csv('./../../Data/growth_dynamics/drought_ndvi_reduction_northern_mixed_prairies.csv')
  head(growth_drynamics_ndvi_sgs,1)
  
  growth_drynamics_ndvi_sgs$upper <- growth_drynamics_ndvi_sgs$perc_change + growth_drynamics_ndvi_sgs$ci_99
  growth_drynamics_ndvi_sgs$lower <- growth_drynamics_ndvi_sgs$perc_change - growth_drynamics_ndvi_sgs$ci_99
  
  
  #NMP
  growth_drynamics_ndvi_nmp <- 
    read_csv('./../../Data/growth_dynamics/drought_ndvi_reduction_northern_mixed_prairies.csv')
  head(growth_drynamics_ndvi_nmp,1)
  
  growth_drynamics_ndvi_nmp$upper <- growth_drynamics_ndvi_nmp$perc_change + growth_drynamics_ndvi_nmp$ci_99
  growth_drynamics_ndvi_nmp$lower <- growth_drynamics_ndvi_nmp$perc_change - growth_drynamics_ndvi_nmp$ci_99
  
  
  png(height = 1500,width=3000,res=300,'Figures/multi_panel_growth_curves_NDVI')
  
  par(mfrow=c(1,2),cex = 0.5,lwd = 0.5,oma=c(3.2,6,1,1),mar = c(3,1.25,3,3))
  
  plot(perc_change~doy,data=growth_drynamics_ndvi_sgs,type='l',
       xlab='',ylab='',las=1,ylim=c(-35,1))
  rect(151,-70,243,350,col = 'grey95')
  rect(60,-70,151,350,col = 'grey')
  polygon(c(growth_drynamics_ndvi_sgs$doy,rev(growth_drynamics_ndvi_sgs$doy)),
          c(growth_drynamics_ndvi_sgs$lower,rev(growth_drynamics_ndvi_sgs$upper)),
          col = "black", border = F)
  mtext('Shortgrass steppe',side=3,line=0.5,cex=1.25)
  text(100, -20, "Spring",cex=1.5)
  text(200, -20, "Summer",cex=1.5)
  text(275, -20, "Fall",cex=1.5)
  abline(h=0,col='black',lty='dashed')
  mtext('a',side=3,line=0.5,cex=1.5,adj=-0.05)
  mtext('% Change in NDVI',side=2,line=3,cex=1.75)
  
  
  plot(perc_change~doy,data=growth_drynamics_ndvi_nmp,type='l',
       xlab='',ylab='',las=1,ylim=c(-25,40))
  rect(151,-70,243,350,col = 'grey95')
  rect(60,-70,151,350,col = 'grey')
  polygon(c(growth_drynamics_ndvi_nmp$doy,rev(growth_drynamics_ndvi_nmp$doy)),
          c(growth_drynamics_ndvi_nmp$lower,rev(growth_drynamics_ndvi_nmp$upper)),
          col = "black", border = F)
  mtext('Northern Mixed prairies',side=3,line=0.5,cex=1.25)
  mtext('b',side=3,line=0.5,cex=1.5,adj=-0.05)
  abline(h=0,col='black',lty='dashed')
  mtext('Julian day of year',side=1,line=4,adj=-1,cex=1.75)

  
  dev.off()
  
#-------------------------------------------------------------------------------
# growth dynamics absolute ------
  
  
  #growth dynamics
  growth_drynamics_absolute_sgs <- 
    read_csv('./../../Data/growth_dynamics/drought_gpp_reduction_absolute_shortgrass_steppe.csv')
  head(growth_drynamics_absolute_sgs,1)
  
  growth_drynamics_absolute_sgs$upper <- growth_drynamics_absolute_sgs$abs_change + growth_drynamics_absolute_sgs$ci_99
  growth_drynamics_absolute_sgs$lower <- growth_drynamics_absolute_sgs$abs_change - growth_drynamics_absolute_sgs$ci_99
  
  #import NMP

  #growth dynamics
  growth_drynamics_absolute_nmp <- 
    read_csv('./../../Data/growth_dynamics/drought_gpp_reduction_absolute_northern_mixed_prairies.csv')
  head(growth_drynamics_absolute_nmp,1)
  
  growth_drynamics_absolute_nmp$upper <- growth_drynamics_absolute_nmp$abs_change + growth_drynamics_absolute_nmp$ci_99
  growth_drynamics_absolute_nmp$lower <- growth_drynamics_absolute_nmp$abs_change - growth_drynamics_absolute_nmp$ci_99
  
  png(height = 3000,width=4000,res=300,'Figures/multi_panel_gpp_impacts_absolute.png')
  
  #setup
  par(mfrow=c(2,1),cex = 0.5,lwd = 0.5,oma=c(3.2,9,1,1),mar = c(3,2.25,3,3))
  #?par
  
 #SGS 
  plot(abs_change~doy,data=growth_drynamics_absolute_sgs,type='l',
       xlab='',ylab='',las=1,cex.axis=2)
  rect(151,-70,243,350,col = 'grey95')
  rect(60,-70,151,350,col = 'grey')
  polygon(c(growth_drynamics_absolute_sgs$doy,rev(growth_drynamics_absolute_sgs$doy)),
          c(growth_drynamics_absolute_sgs$lower,rev(growth_drynamics_absolute_sgs$upper)),
          col = "black", border = F)
  text(100, -15, "Spring",cex=2.5)
  text(200, -5, "Summer",cex=2.5)
  text(275, -15, "Fall",cex=2.5)
  abline(h=0,col='black',lty='dashed')
  mtext('Shortgrass steppe',side=3,line=0.5,cex=2)
  mtext('a',side=3,line=0.5,cex=1.5,adj=0.0)

  
  #NMP
  plot(abs_change~doy,data=growth_drynamics_absolute_nmp,type='l',
       xlab='',ylab='',las=1,cex.axis=2)
  rect(151,-70,243,350,col = 'grey95')
  rect(60,-70,151,350,col = 'grey')
  polygon(c(growth_drynamics_absolute_nmp$doy,rev(growth_drynamics_absolute_nmp$doy)),
          c(growth_drynamics_absolute_nmp$lower,rev(growth_drynamics_absolute_nmp$upper)),
          col = "black", border = F)
  abline(h=0,col='black',lty='dashed')
  mtext('Northern mixed prairies',side=3,line=0.5,cex=2)
  mtext('Julian day of year',side=1,line=4,cex=2)
  mtext(expression("GPP impact "(gCm^-2~'16 days')),side=2,line=4,adj = -15, cex=2)
  mtext('b',side=3,line=0.5,cex=1.5,adj=0.0)
  
  dev.off()

#-------------------------------------------------------------------------------
# growth cycles -----
  
  #growth dynamics
  
  #SGS
  
  #median
  growth_cycle_sgs <- 
    read_csv('./../../Data/Growth_Cycle/average_cycle_shortgrass_steppe.csv')
  head(growth_cycle_sgs,1)
  
  growth_cycle_sgs$upper <- growth_cycle_sgs$gpp_average + growth_cycle_sgs$ci_99
  growth_cycle_sgs$lower <- growth_cycle_sgs$gpp_average - growth_cycle_sgs$ci_99
  
  #drought
  growth_cycle_drought_sgs <- 
    read_csv('./../../Data/Growth_Cycle/drought_cycle_shortgrass_steppe.csv')
  head(growth_cycle_drought_sgs,1)
  
  growth_cycle_drought_sgs$upper <- growth_cycle_drought_sgs$gpp_drought + growth_cycle_drought_sgs$ci_99
  growth_cycle_drought_sgs$lower <- growth_cycle_drought_sgs$gpp_drought - growth_cycle_drought_sgs$ci_99
  
  #import NMP
  
  #median
  growth_cycle_nmp <- 
    read_csv('./../../Data/Growth_Cycle/average_cycle_northern_mixed_prairies.csv')
  head(growth_cycle_nmp,1)
  
  growth_cycle_nmp$upper <- growth_cycle_nmp$gpp_average + growth_cycle_nmp$ci_99
  growth_cycle_nmp$lower <- growth_cycle_nmp$gpp_average - growth_cycle_nmp$ci_99
  
  #drought
  growth_cycle_drought_nmp <- 
    read_csv('./../../Data/Growth_Cycle/drought_cycle_northern_mixed_prairies.csv')
  head(growth_cycle_drought_nmp,1)
  
  growth_cycle_drought_nmp$upper <- growth_cycle_drought_nmp$gpp_drought + growth_cycle_drought_nmp$ci_99
  growth_cycle_drought_nmp$lower <- growth_cycle_drought_nmp$gpp_drought - growth_cycle_drought_nmp$ci_99
  
  png(height = 3000,width=4000,res=300,'Figures/multi_panel_gpp_growth_cycle.png')
  
  #setup
  par(mfrow=c(2,1),cex = 0.5,lwd = 0.5,oma=c(3.2,9,1,1),mar = c(3,2.25,3,3))
  #?par
  
  #SGS 
  plot(gpp_average~doy,data=growth_cycle_sgs,type='l',
       xlab='',ylab='',las=1,cex.axis=2,ylim=c(0,40))
  rect(151,-70,243,350,col = 'grey95')
  rect(60,-70,151,350,col = 'grey')
  polygon(c(growth_cycle_sgs$doy,rev(growth_cycle_sgs$doy)),
          c(growth_cycle_sgs$lower,rev(growth_cycle_sgs$upper)),
          col = "black", border = F)
  polygon(c(growth_cycle_drought_sgs$doy,rev(growth_cycle_drought_sgs$doy)),
          c(growth_cycle_drought_sgs$lower,rev(growth_cycle_drought_sgs$upper)),
          col = "red", border = F)
  text(100, 35, "Spring",cex=3)
  text(200, 35, "Summer",cex=3)
  text(275, 35, "Fall",cex=3)
  mtext('Shortgrass steppe',side=3,line=0.5,cex=2)
  mtext('a',side=3,line=0.5,cex=1.5,adj=0.0)
  
  
  #NMP
  plot(gpp_average~doy,data=growth_cycle_nmp,type='l',
       xlab='',ylab='',las=1,cex.axis=2)
  rect(151,-70,243,350,col = 'grey95')
  rect(60,-70,151,350,col = 'grey')
  polygon(c(growth_cycle_nmp$doy,rev(growth_cycle_nmp$doy)),
          c(growth_cycle_nmp$lower,rev(growth_cycle_nmp$upper)),
          col = "black", border = F)
  polygon(c(growth_cycle_drought_nmp$doy,rev(growth_cycle_drought_nmp$doy)),
          c(growth_cycle_drought_nmp$lower,rev(growth_cycle_drought_nmp$upper)),
          col = "red", border = F)
  legend(250, 40, legend=c("Average year", "Drought year"),         #alpha legend: 0.015, 150
         col=c("black", "red"), lty=1.1,lwd=4,cex=2,box.lty=0)
  mtext('Northern mixed prairies',side=3,line=0.5,cex=2)
  mtext('Julian day of year',side=1,line=4.5,cex=2.5)
  mtext(expression("Carbon uptake " (g~C~m^-2~'16 days')),side=2,line=4,adj = -1, cex=2.5)
  mtext('b',side=3,line=0.5,cex=1.5,adj=0.0)
  
  dev.off()
  
  #test
  
#-------------------------------------------------------------------------------
# correlation between annual and growing season GSP ------

Ecoregion <- 'shortgrass_steppe'
  sgs_a_gs_precip <- 
    read.csv(paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/annual_gs_subset.csv'))
head(sgs_a_gs_precip)
cor(sgs_a_gs_precip$annual_precip,sgs_a_gs_precip$gs_precip,method = 'spearman')
#0.97


Ecoregion <- 'northern_mixed_prairies'
nmp_a_gs_precip <- 
  read.csv(paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/annual_gs_subset.csv'))
head(nmp_a_gs_precip)
cor(nmp_a_gs_precip$annual_precip,nmp_a_gs_precip$gs_precip,method = 'spearman')
#0.97


png(height = 1500,width=3000,res=300,'Figures/annual_growing_season_comparison.png')

#two-panel plot
par(mfrow=c(1,2),cex = 0.5,lwd = 0.5,oma=c(3.2,6,1,1),mar = c(3,1.25,3,3))
#?par

# plot it out panel A: sgs
plot(gs_precip ~ annual_precip, sgs_a_gs_precip,col='black',
     ylab='',
     xlab='',las=1,cex.axis=1.5)
mtext('Growing season precipitation (mm)',side=2,line=4.5,cex=1.5)
mtext('a',side=3,line=0.5,cex=1.5,adj=-0.05)
mtext('Shortgrass steppe',side=3,line=0.5,cex=1.75)


# plot it out panel B: nmp
plot(gs_precip ~ annual_precip, nmp_a_gs_precip,col='black',
     ylab='',
     xlab='',las=1,cex.axis=1.5)
mtext('b',side=3,line=0.5,cex=1.5,adj=-0.05)
mtext('Northern mixed prairies',side=3,line=0.5,cex=1.75)
mtext('Annual precipitation (mm)',side=1,line=4,adj=-1,cex=1.5)

dev.off()
  
#-------------------------------------------------------------------------------
# % of annual GPP from months not analyzed -------

#shortgrass
Ecoregion <- 'shortgrass_steppe'
sgs_annual_gpp <-
  read.csv(paste0('./../../Data/GPP/Ecoregion/',Ecoregion,'/full_year_subset.csv'))
head(sgs_annual_gpp)

#full year
sgs_annual_gpp_full_year <- aggregate(gpp_mean ~ x+y+year,sum,data=sgs_annual_gpp)

#growing season subset
sgs_annual_gpp_subset <- sgs_annual_gpp %>%
  dplyr::filter(doy < 57 | doy > 297) %>%
  group_by(x,y,year) %>%
  summarise(shoulder_season_sum = sum(gpp_mean))

sgs_gs_annual_gpp_sums <- merge(sgs_annual_gpp_subset,sgs_annual_gpp_full_year,
                                by=c('x','y','year'))

sgs_gs_annual_gpp_sums$perc <- 
  (sgs_gs_annual_gpp_sums$shoulder_season_sum/sgs_gs_annual_gpp_sums$gpp_mean)*100
summary(sgs_gs_annual_gpp_sums)
#8%
hist(sgs_gs_annual_gpp_sums$perc)

#northern mixed prairies
Ecoregion <- 'northern_mixed_prairies'
nmp_annual_gpp <-
  read.csv(paste0('./../../Data/GPP/Ecoregion/',Ecoregion,'/full_year_subset.csv'))
head(sgs_annual_gpp)

#full year
nmp_annual_gpp_full_year <- aggregate(gpp_mean ~ x+y+year,sum,data=nmp_annual_gpp)

#growing season subset
nmp_annual_gpp_subset <- nmp_annual_gpp %>%
  dplyr::filter(doy < 57 | doy > 297) %>%
  group_by(x,y,year) %>%
  summarise(shoulder_season_sum = sum(gpp_mean))

nmp_gs_annual_gpp_sums <- merge(nmp_annual_gpp_subset,nmp_annual_gpp_full_year,
                                by=c('x','y','year'))

nmp_gs_annual_gpp_sums$perc <- 
  (nmp_gs_annual_gpp_sums$shoulder_season_sum/nmp_gs_annual_gpp_sums$gpp_mean)*100
summary(nmp_gs_annual_gpp_sums)
#2%
hist(nmp_gs_annual_gpp_sums$perc)


png(height = 1500,width=3000,res=300,'Figures/C_uptake_shoulder_season.png')

#two-panel plot
par(mfrow=c(1,2),cex = 0.5,lwd = 0.5,oma=c(3.2,6,1,1),mar = c(3,1.25,3,3))
#?par

# plot it out panel A: sgs
hist(sgs_gs_annual_gpp_sums$perc,
     ylab='',main='', ylim=c(0,400),
     xlab='',las=1,cex.axis=1.5)
mtext('Number of pixels',side=2,line=4.5,cex=1.5)
mtext('a',side=3,line=0.5,cex=1.5,adj=-0.05)
mtext('Shortgrass steppe',side=3,line=0.5,cex=1.75)

# plot it out panel B: nmp
hist(nmp_gs_annual_gpp_sums$perc,
     ylab='', main='',
     xlab='',las=1,cex.axis=1.5)
mtext('b',side=3,line=0.5,cex=1.5,adj=-0.05)
mtext('Northern mixed prairies',side=3,line=0.5,cex=1.75)

mtext('% Carbon uptake',side=1,line=4,adj=-0.75,cex=1.5)

dev.off()


#-------------------------------------------------------------------------------
# variograms of drought impact to day of 50% C uptake ------


library(gstat)
library(sp)

#drought impact to day of 50 growth

#advance of day 50 sgs
day_50_sgs <- raster('./../../Data/CDD/day_of_50/day_50_shortgrass_steppe.tif')
day_50_drought_sgs <-
  raster('./../../Data/CDD/day_of_50/day_50_droughtshortgrass_steppe.tif')
day_50_drought_sgs <- stack(day_50_drought_sgs, day_50_sgs)
day_50_drought_sgs_2 <-
  day_50_drought_sgs$day_50_droughtshortgrass_steppe -
  day_50_drought_sgs$day_50_shortgrass_steppe

#advance of day 50 nmp
day_50_nmp <- raster('./../../Data/CDD/day_of_50/day_50_northern_mixed_prairies.tif')
day_50_drought_nmp <-
  raster('./../../Data/CDD/day_of_50/day_50_droughtnorthern_mixed_prairies.tif')
day_50_drought_nmp <- stack(day_50_drought_nmp, day_50_nmp)
day_50_drought_nmp_2 <-
  day_50_drought_nmp$day_50_droughtnorthern_mixed_prairies -
  day_50_drought_nmp$day_50_northern_mixed_prairies

#combine
# day_50_drought <-
#   raster::merge(day_50_drought_nmp_2, day_50_drought_sgs_2, tolerance = 0.20)
# proj4string(day_50_drought) <- CRS("+proj=longlat")
# plot(day_50_drought)

#SGS
point_data_50 <- as(day_50_drought_sgs_2, 'SpatialPointsDataFrame')

TheVariogram_50_sgs = variogram(layer ~1,data = point_data_50,width = 10)
summary(TheVariogram_50_sgs)
plot(TheVariogram_50_sgs)

TheVariogramModel_50_sgs <- vgm(psill=200, model="Exp", nugget=50, range=100)
plot(TheVariogram_50_sgs, model=TheVariogramModel_50_sgs) 
FittedModel_50_sgs <- fit.variogram(TheVariogram_50_sgs, model=TheVariogramModel_50_sgs)
FittedModel_50_sgs
#Range = 49.43 km

plot(TheVariogram_50_sgs, model=FittedModel_50_sgs,xlab='Distance (km)',
     ylab = 'Semivariance',cex=1,lwd=2,col='black')

#NMP
point_data_50 <- as(day_50_drought_nmp_2, 'SpatialPointsDataFrame')

TheVariogram_50_nmp = variogram(layer ~1,data = point_data_50,width = 10)
summary(TheVariogram_50_nmp)
plot(TheVariogram_50_nmp)

TheVariogramModel_50_nmp <- vgm(psill=50, model="Exp", nugget=10, range=200)
plot(TheVariogram_50_nmp, model=TheVariogramModel_50_nmp) 
FittedModel_50_nmp <- fit.variogram(TheVariogram_50_nmp, model=TheVariogramModel_50_nmp)
FittedModel_50_nmp
#Range = 201.1 km

plot(TheVariogram_50_nmp, model=FittedModel_50_nmp,xlab='Distance (km)',
     ylab = 'Semivariance',cex=1,lwd=2,col='black')
abline(h=c(0.025,0.075),col=4,lty=2)


#two variograms
png(height = 1500,width=3000,res=300,'Figures/day_50_variograms')


par(mfrow=c(1,2),cex = 0.5,lwd = 0.5,oma=c(3.2,6,1,1),mar = c(3,1.25,3,3))

#SGS
plot(gamma~dist,data=TheVariogram_50_sgs,xlab='',ylab='',cex=2)
abline(v=49.4,lwd=3,col='red',add=T)
mtext('Semivariance',side=2,line=3.5,cex=1.5)
mtext('Shortgrass steppe',side=3,line=0.5,cex=1.25)
mtext('a',side=3,line=0.5,cex=1.5,adj=-0.05)

#NMP
plot(gamma~dist,data=TheVariogram_50_nmp,xlab='',ylab='',cex=2)
abline(v=201.1,lwd=3,col='red',add=T)
mtext('Northern mixed prairies',side=3,line=0.5,cex=1.25)
mtext('b',side=3,line=0.5,cex=1.5,adj=-0.05)
mtext('Distance (km)',side=1,line=3.75,adj=-.5,cex=1.5)

dev.off()


#-------------------------------------------------------------------------------

