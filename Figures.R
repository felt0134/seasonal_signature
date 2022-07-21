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
# day of 75% C uptake in normal and dry years (updated 6/17/2022) -----

#sgs
day_75_drought_sgs <-
  raster('./../../Data/CDD/day_of_75/day_75_drought_impact_shortgrass_steppe.tif')
# plot(day_75_drought_sgs)
# summary(day_75_drought_sgs)
#median = 3

#nmp
day_75_drought_nmp <-
  raster('./../../Data/CDD/day_of_75/day_75_drought_impact_northern_mixed_prairies.tif')
#summary(day_75_drought_nmp)
#median = -4

#combine
day_75_drought <-
  raster::merge(day_75_drought_nmp, day_75_drought_sgs,tolerance = 0.20)
#proj4string(day_75_drought) <- CRS("+proj=longlat")
day_75_drought <-projectRaster(day_75_drought, crs='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96
+        +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
day_75_drought_df <- data.frame(rasterToPoints(day_75_drought))

# % of pixels with negative values (advanced day75)
(day_75_drought_df %>%
    filter(layer < 0) %>%
    summarise(length(layer)))/(length(day_75_drought_df$layer))
#0.61 of pixels see day of 75% growth occur earlier during extreme drought

day_75_sgs_nmp_drought_map <-
  ggplot() +
  geom_polygon(data=states_all_sites_tidy, mapping=aes(x = long, y = lat,group=group),
               color = "black", size = 0.1,fill=NA) +
  geom_raster(data=day_75_drought_df, mapping=aes(x = x, y = y, fill = layer)) + 
  coord_equal() +
  scale_fill_scico('Effect of drought by day at which\n75% of total C uptake occurs (days)',
                   palette = 'roma',direction = -1,midpoint = 0) +
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


#PDF of drought impact to 75% growth

#nmp
day_75_drought_nmp_2_df <- data.frame(rasterToPoints(day_75_drought_nmp))
day_75_drought_nmp_2_df$region <- 'Northern mixed prairies'
colnames(day_75_drought_nmp_2_df) <- c('x','y','day_75','region')

#sgs
day_75_drought_sgs_2_df <- data.frame(rasterToPoints(day_75_drought_sgs))
day_75_drought_sgs_2_df$region <- 'Shortgrass steppe'
colnames(day_75_drought_sgs_2_df) <- c('x','y','day_75','region')

#join
day_75_drought_nmp_sgs_2_df <- rbind(day_75_drought_nmp_2_df,day_75_drought_sgs_2_df)
#head(day_75_drought_nmp_sgs_2_df,1)

#plot it
drought_day75_pdf <- ggplot(day_75_drought_nmp_sgs_2_df, aes(x = day_75, fill = region)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.02)) +
  geom_density(color = 'black', alpha = 0.5, aes(y = ..scaled..)) +
  scale_fill_manual(values = c(
    'Northern mixed prairies' = 'steelblue2',
    'Shortgrass steppe' = 'green4'
  )) +
  geom_vline(xintercept = 0,color='black') +
  xlab("Effect of drought on day by which\n75% of total C uptake occurs (days)") +
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
    #legend.position = c(0.82, 0.7),
    legend.position = 'top',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 15),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#try to make inset
vp <- viewport(width = 0.44, height = 0.39, x = 0.23,y=0.27)
# y = unit(0.7, "lines"), just = c("right",
#                                  "bottom")

#executing the inset, you create a function the utlizes all the previous code
full <- function() {
  print(day_75_sgs_nmp_drought_map)
  print(drought_day75_pdf , vp = vp)
}


png(height = 1700,width=2000,res=300,'Figures/day75_drought_inset_plot.png')

full()

dev.off()

#cleanup
rm(day_75_drought,day_75_drought_df,day_75_drought_nmp,day_75_drought_nmp_2_df,
   day_75_drought_sgs_2_df,day_75_drought_sgs, day_75_sgs_nmp_drought_map,
   drought_day75_pdf)


#follow up: correlation of day 75 impact and latitude 

cor.test(day_75_drought_nmp_sgs_2_df$y,day_75_drought_nmp_sgs_2_df$day_75 ,method='spearman',exact=FALSE)
#-0.24

day_75_lat_plot <- ggplot(day_75_drought_nmp_sgs_2_df, aes(x = y, y = day_75,color = region)) +
  geom_point(alpha=0.3) +
  scale_colour_manual(values = c(
    'Shortgrass steppe' = 'green4',
    'Northern mixed prairies' = 'steelblue2'
  )) +
  geom_hline(yintercept = 0,size=2,color='grey') +
  stat_smooth(method='lm',color='black',size=2) +
  #geom_vline(xintercept = 0,color='red') +
  ylab("Effect of drought on day by which\n75% of total C uptake occurs (days)") +
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
    axis.line.y = element_line(colour = "black"))

png(height = 1500,width=2000,res=300,'Figures/day75_drought_latitude_relationship.png')

day_75_lat_plot

dev.off()


#cleanup
rm(day_75_lat_plot,day_75_drought_nmp_sgs_2_df)


#-------------------------------------------------------------------------------
# day of 50% C uptake in normal and dry years (updated 6/16/2022) -----

#sgs
day_50_drought_sgs <-
  raster('./../../Data/CDD/day_of_50/day_50_drought_impact_shortgrass_steppe.tif')
# plot(day_50_drought_sgs)
# summary(day_50_drought_sgs)
#median = -22

#nmp
day_50_drought_nmp <-
  raster('./../../Data/CDD/day_of_50/day_50_drought_impact_northern_mixed_prairies.tif')
# summary(day_50_drought_nmp)
#median = -12

#combine
day_50_drought <-
  raster::merge(day_50_drought_nmp, day_50_drought_sgs,tolerance = 0.20)
#proj4string(day_50_drought) <- CRS("+proj=longlat")
day_50_drought <-projectRaster(day_50_drought, crs='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96
+        +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
day_50_drought_df <- data.frame(rasterToPoints(day_50_drought))

# % of pixels with negative values (advanced day50)
(day_50_drought_df %>%
    filter(layer < 0) %>%
    summarise(length(layer)))/(length(day_50_drought_df$layer))
#0.90 of pixels see day of 50% growth occur earlier during extreme drought

day_50_sgs_nmp_drought_map <-
  ggplot() +
  geom_polygon(data=states_all_sites_tidy, mapping=aes(x = long, y = lat,group=group),
               color = "black", size = 0.1,fill=NA) +
  geom_raster(data=day_50_drought_df, mapping=aes(x = x, y = y, fill = layer)) + 
  coord_equal() +
  scale_fill_scico('Effect of drought on day by which\nhalf of total C uptake occurs (days)',
                   palette = 'roma',direction = -1,midpoint = 0) +
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
day_50_drought_nmp_2_df <- data.frame(rasterToPoints(day_50_drought_nmp))
day_50_drought_nmp_2_df$region <- 'Northern mixed prairies'
colnames(day_50_drought_nmp_2_df) <- c('x','y','day_50','region')

#sgs
day_50_drought_sgs_2_df <- data.frame(rasterToPoints(day_50_drought_sgs))
day_50_drought_sgs_2_df$region <- 'Shortgrass steppe'
colnames(day_50_drought_sgs_2_df) <- c('x','y','day_50','region')

#join
day_50_drought_nmp_sgs_2_df <- rbind(day_50_drought_nmp_2_df,day_50_drought_sgs_2_df)
#head(day_50_drought_nmp_sgs_2_df,1)
#summary(day_50_drought_nmp_sgs_2_df)

#plot it
drought_day50_pdf <- ggplot(day_50_drought_nmp_sgs_2_df, aes(x = day_50, fill = region)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.02)) +
  geom_density(color = 'black', alpha = 0.5, aes(y = ..scaled..)) +
  scale_fill_manual(values = c(
    'Northern mixed prairies' = 'steelblue2',
    'Shortgrass steppe' = 'green4'
  )) +
  geom_vline(xintercept = 0,color='black') +
  xlab("Effect of drought on day by which\nhalf of total C uptake occurs (days)") +
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
    #legend.position = c(0.82, 0.7),
    legend.position = 'top',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 15),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

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

#cleanup
rm(day_50_drought,day_50_drought_df,day_50_drought_nmp,day_50_drought_nmp_2_df,
   day_50_drought_sgs_2_df,day_50_drought_sgs, day_50_sgs_nmp_drought_map,
   drought_day50_pdf)


#follow up: correlation of day 50 impact and latitude 

cor.test(day_50_drought_nmp_sgs_2_df$y,day_50_drought_nmp_sgs_2_df$day_50 ,method='spearman',exact=FALSE)
#0.34

day_50_lat_plot <- ggplot(day_50_drought_nmp_sgs_2_df, aes(x = y, y = day_50,color = region)) +
  geom_point(alpha=0.3) +
  scale_colour_manual(values = c(
    'Shortgrass steppe' = 'green4',
    'Northern mixed prairies' = 'steelblue2'
  )) +
  geom_hline(yintercept = 0,size=2,color='grey') +
  stat_smooth(method='lm',color='black',size=2) +
  #geom_vline(xintercept = 0,color='red') +
  ylab('Effect of drought by day by which half of\ntotal carbon uptake is achieved (days)') +
  xlab('Latitude') +
  annotate("text", x=46, y=40, label= "Delayed") +
  annotate("text", x=46, y=-50, label= "Advanced") +
  theme(
    axis.text.x = element_text(color = 'black', size = 15),
    #angle=25,hjust=1),
    axis.text.y = element_text(color = 'black', size = 15),
    axis.title = element_text(color = 'black', size = 15),
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

#cleanup
rm(day_50_lat_plot,day_50_drought_nmp_sgs_2_df)

#-------------------------------------------------------------------------------
# day of 25% C uptake in normal and dry years (updated 6/17/2022) -----

#sgs
day_25_drought_sgs <-
  raster('./../../Data/CDD/day_of_25/day_25_drought_impact_shortgrass_steppe.tif')

#nmp
day_25_drought_nmp <-
  raster('./../../Data/CDD/day_of_25/day_25_drought_impact_northern_mixed_prairies.tif')


#combine
day_25_drought <-
  raster::merge(day_25_drought_nmp, day_25_drought_sgs,tolerance = 0.20)
#proj4string(day_25_drought) <- CRS("+proj=longlat")
day_25_drought <-projectRaster(day_25_drought, crs='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96
+        +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
day_25_drought_df <- data.frame(rasterToPoints(day_25_drought))

# % of pixels with negative values (advanced day25)
(day_25_drought_df %>%
    filter(layer < 0) %>%
    summarise(length(layer)))/(length(day_25_drought_df$layer))
#0.90 of pixels see day of 25% growth occur earlier during extreme drought

day_25_sgs_nmp_drought_map <-
  ggplot() +
  geom_polygon(data=states_all_sites_tidy, mapping=aes(x = long, y = lat,group=group),
               color = "black", size = 0.1,fill=NA) +
  geom_raster(data=day_25_drought_df, mapping=aes(x = x, y = y, fill = layer)) + 
  coord_equal() +
  scale_fill_scico('Effect of drought on day by which\n25% of total C uptake occurs (days)',
                   palette = 'roma',direction=-1,midpoint = 0) +
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
day_25_drought_nmp_2_df <- data.frame(rasterToPoints(day_25_drought_nmp))
day_25_drought_nmp_2_df$region <- 'Northern mixed prairies'
colnames(day_25_drought_nmp_2_df) <- c('x','y','day_25','region')

#sgs
day_25_drought_sgs_2_df <- data.frame(rasterToPoints(day_25_drought_sgs))
day_25_drought_sgs_2_df$region <- 'Shortgrass steppe'
colnames(day_25_drought_sgs_2_df) <- c('x','y','day_25','region')

#join
day_25_drought_nmp_sgs_2_df <- rbind(day_25_drought_nmp_2_df,day_25_drought_sgs_2_df)
#head(day_25_drought_nmp_sgs_2_df,1)

#plot it
drought_day25_pdf <- ggplot(day_25_drought_nmp_sgs_2_df, aes(x = day_25, fill = region)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.02)) +
  geom_density(color = 'black', alpha = 0.5, aes(y = ..scaled..)) +
  scale_fill_manual(values = c(
    'Northern mixed prairies' = 'steelblue2',
    'Shortgrass steppe' = 'green4'
  )) +
  geom_vline(xintercept = 0,color='black') +
  xlab("Effect of drought on day by which\n25% of total C uptake occurs (days)") +
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
    #legend.position = c(0.82, 0.7),
    legend.position = 'top',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 15),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#try to make inset
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

#cleanup
rm(day_25_drought,day_25_drought_df,day_25_drought_nmp,day_25_drought_nmp_2_df,
   day_25_drought_sgs_2_df,day_25_drought_sgs, day_25_sgs_nmp_drought_map,
   drought_day25_pdf)


#follow up: correlation of day 25 impact and latitude 

cor.test(day_25_drought_nmp_sgs_2_df$y,day_25_drought_nmp_sgs_2_df$day_25 ,method='spearman',exact=FALSE)
#0.49

day_25_lat_plot <- ggplot(day_25_drought_nmp_sgs_2_df, aes(x = y, y = day_25,color = region)) +
  geom_point(alpha=0.5) +
  scale_colour_manual(values = c(
    'Shortgrass steppe' = 'green4',
    'Northern mixed prairies' = 'steelblue2'
  )) +
  geom_hline(yintercept = 0,size=2,color='grey') +
  stat_smooth(method='lm',color='black',size=2) +
  #geom_vline(xintercept = 0,color='red') +
  ylab("Effect of drought on day by which\n25% of total C uptake occurs (days)") +
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

png(height = 1500,width=2000,res=300,'Figures/day25_drought_latitude_relationship.png')

day_25_lat_plot

dev.off()


#cleanup
rm(day_25_lat_plot,day_25_drought_nmp_sgs_2_df)

#-------------------------------------------------------------------------------
# day of maximum GPP (unclear if will do) ------




#-------------------------------------------------------------------------------
# growth curves in average and dry years (updated 6/15/2022) ------


#import SGS
#mean
growth_curve_absolute_mean_sgs <- 
  read_csv('./../../Data/growth_curves/average_growth_curve_shortgrass_steppe.csv')
#head(growth_curve_absolute_mean_sgs,1)

#drought
growth_curve_drought_absolute_mean_sgs <- 
  read_csv('./../../Data/growth_curves/drought_growth_curve_shortgrass_steppe.csv')
#head(growth_curve_drought_absolute_mean_sgs,1)

#import NMP
#mean
growth_curve_absolute_mean_nmp <- 
  read_csv('./../../Data/growth_curves/average_growth_curve_northern_mixed_prairies.csv')
head(growth_curve_absolute_mean_nmp,1)

#drought
growth_curve_drought_absolute_mean_nmp <- 
  read_csv('./../../Data/growth_curves/drought_growth_curve_northern_mixed_prairies.csv')
head(growth_curve_drought_absolute_mean_nmp,1)

#SGS growth curve figure
png(height = 1500,width=1500,res=300,'Figures/multi_panel_growth_curves_2')

#par(mfrow=c(1,2),cex = 0.5,lwd = 0.5,oma=c(3.2,6,1,1),mar = c(3,1.25,3,3))
par(mfrow=c(2,1),cex = 0.5,lwd = 0.5,oma=c(3.2,9,1,1),mar = c(3,2.25,3,3))
# plot it out panel A: sgs
plot(mean ~ doy, growth_curve_absolute_mean_sgs,col='black',type='l',
     ylab='',
     xlab='',las=1,cex.axis=1.5,ylim=c(0,391))
# polygon(c(growth_curve_absolute_mean_sgs$doy,rev(growth_curve_absolute_mean_sgs$doy)),
#         c(growth_curve_absolute_mean_sgs$temporal_ci_25,rev(growth_curve_absolute_mean_sgs$temporal_ci_75)),
#         col=adjustcolor("grey",alpha.f=0.70), border = F)
polygon(c(growth_curve_absolute_mean_sgs$doy,rev(growth_curve_absolute_mean_sgs$doy)),
        c(growth_curve_absolute_mean_sgs$spatial_ci_25,rev(growth_curve_absolute_mean_sgs$spatial_ci_75)),
        col=adjustcolor("grey",alpha.f=0.50), border = F)
polygon(c(growth_curve_drought_absolute_mean_sgs$doy,rev(growth_curve_drought_absolute_mean_sgs$doy)),
        c(growth_curve_drought_absolute_mean_sgs$spatial_25,rev(growth_curve_drought_absolute_mean_sgs$spatial_75)),
        col=adjustcolor("red",alpha.f=0.40), border = F)
lines(mean ~ doy, growth_curve_drought_absolute_mean_sgs,col='red',pch=19,lwd=1.25)
lines(mean ~ doy, growth_curve_absolute_mean_sgs,col='black',pch=19,lwd=1.25)
#abline(v=155)
# text(162, 176, "June 28th",cex=1)
# points(178, 176,pch=19,cex=3)
# text(170, 97, "June 6th",cex=1)
# points(157,102,pch=19,cex=3)
legend(75, 350, legend=c("Average year", "Drought year"),         #alpha legend: 0.015, 150
       col=c("black", "red"), lty=1.1,lwd=4,cex=1.25,box.lty=0)
# legend(175, 90, legend=c("50% of total production"),         #alpha legend: 0.015, 150
#        col=c("black"), pch=19,box.lty=0,cex=2)
#mtext('Julian day of year',side=1,line=3.75,cex=1.5)
mtext('Shortgrass steppe',side=3,line=0.5,cex=1)
mtext('a',side=3,line=0.5,cex=1,adj=-0.05)
#?mtext

# plot it out panel B: nmp
plot(mean ~ doy, growth_curve_absolute_mean_nmp,col='black',type='l',
     ylab='',
     xlab='',las=1,cex.axis=1.5,ylim=c(0,580))
# polygon(c(growth_curve_absolute_mean_nmp$doy,rev(growth_curve_absolute_mean_nmp$doy)),
#         c(growth_curve_absolute_mean_nmp$temporal_ci_25,rev(growth_curve_absolute_mean_nmp$temporal_ci_75)),
#         col = "grey", border = F)
polygon(c(growth_curve_absolute_mean_nmp$doy,rev(growth_curve_absolute_mean_nmp$doy)),
        c(growth_curve_absolute_mean_nmp$spatial_ci_25,rev(growth_curve_absolute_mean_nmp$spatial_ci_75)),
        col=adjustcolor("grey",alpha.f=0.50), border = F)
polygon(c(growth_curve_drought_absolute_mean_nmp$doy,rev(growth_curve_drought_absolute_mean_nmp$doy)),
        c(growth_curve_drought_absolute_mean_nmp$spatial_25,rev(growth_curve_drought_absolute_mean_nmp$spatial_75)),
        col=adjustcolor("red",alpha.f=0.40), border = F)
lines(mean ~ doy, growth_curve_drought_absolute_mean_nmp,col='red',pch=19,lwd=1.25)
lines(mean ~ doy, growth_curve_absolute_mean_nmp,col='black',pch=19,lwd=1.25)

# text(158, 210, "June 23rd",cex=1)
# points(174, 210,pch=19,cex=3)
# text(179, 170, "June 12th",cex=1)
# points(163,170,pch=19,cex=3)
mtext('Julian day of year',side=1,line=3.75,cex=1.25)
#mtext('GPP',side=2,line=2.5,cex=1.5)
mtext('Northern mixed prairies',side=3,line=0.5,cex=1)
mtext('b',side=3,line=0.5,cex=1,adj=-0.05)
mtext(expression("Cumulative carbon uptake " (g~C~m^-2)),side=2,line=4.5,adj=-.15,cex=1.25)

dev.off()


# #inset SGS (add the polygons for uncertainty)
# panel.first = rect(c(1,7), -1e6, c(3,10), 1e6, col='green', border=NA)
# par(fig = c(0.05,0.3,0.60,0.95), new = TRUE)
# plot(perc_change~doy,data=growth_drynamics_sgs,type='l',
#      xlab='',ylab='',las=1)
# rect(151,-70,243,350,col = 'grey95')
# rect(60,-70,151,350,col = 'grey')
# polygon(c(growth_drynamics_sgs$doy,rev(growth_drynamics_sgs$doy)),
#         c(growth_drynamics_sgs$lower,rev(growth_drynamics_sgs$upper)),
#         col = "black", border = F)
# text(100, -50, "Spring",cex=1)
# text(200, -20, "Summer",cex=1)
# text(275, -20, "Fall",cex=1)
# abline(h=0,col='black',lty='dashed')
# mtext('Julian day of year',side=1,line=2.35,cex=0.75)
# mtext('% Change in C uptake',side=2,line=2.5,cex=0.6)
# 
# #inset NMP
# par(fig = c(0.55,0.78,0.60,0.95), new = TRUE)
# plot(perc_change~doy,data=growth_drynamics_nmp,type='l',
#      xlab='',ylab='',las=1)
# rect(151,-50,243,350,col = 'grey95')
# rect(60,-50,151,350,col = 'grey')
# polygon(c(growth_drynamics_nmp$doy,rev(growth_drynamics_nmp$doy)),
#         c(growth_drynamics_nmp$lower,rev(growth_drynamics_nmp$upper)),
#         col = "black", border = F)
# # lines(perc_change~doy,data=growth_drynamics_nmp)
# # lines(upper~as.numeric(as.integer(doy)),growth_drynamics_nmp)
# # lines(lower~doy,growth_drynamics_nmp)
# abline(h=0,col='black',lty='dashed')
# mtext('Julian day of year',side=1,line=2.35,cex=0.75)
# mtext('% Change in C uptake',side=2,line=2.5,cex=0.6)
# 
# #?rect
# dev.off()


#-------------------------------------------------------------------------------
# VPD change during drought by season (updated 6/16/2022) ------

#import
Ecoregion <- 'shortgrass_steppe'
seasonal_vpd_sgs <- read.csv(paste0(
     './../../Data/Climate/Ecoregion/',
     Ecoregion,
     '/PRISM/VPD_change.csv'
   )) 

seasonal_vpd_sgs$ecoregion <- 'Shortgrass steppe'
seasonal_vpd_sgs$ecoregion_2 <- 'a'

Ecoregion <- 'northern_mixed_prairies'
seasonal_vpd_nmp <- read.csv(paste0(
  './../../Data/Climate/Ecoregion/',
  Ecoregion,
  '/PRISM/VPD_change.csv'
)) 

seasonal_vpd_nmp$ecoregion <- 'Northern mixed prairies'
seasonal_vpd_nmp$ecoregion_2 <- 'b'

seasonal_vpd_sgs_nmp <- rbind(seasonal_vpd_nmp,seasonal_vpd_sgs)
head(seasonal_vpd_sgs_nmp,1)

eco_names <- as_labeller(
  c( "a" = "Shortgrass steppe", "b" = 'Northern mixed prairies'))

vpd_change <- ggplot(seasonal_vpd_sgs_nmp, aes(x = abs_change, fill = season)) +
  facet_wrap(~ecoregion_2,ncol=1,labeller = eco_names) +
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
  xlab('Increase in average daily maximum VPD (kPa)') +
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
    legend.position = c(0.6, 0.65),
    #legend.position = 'none',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 15),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#save to file
png(height = 3000,width=2750,res=300,'Figures/vpd_change.png')

print(vpd_change)

dev.off()

#-------------------------------------------------------------------------------
# change in proportion of spring precipitation during drought ------

#import
seasonal_change_spring_sgs_df <- 
  read.csv(paste0('./../../Data/Climate/Ecoregion/shortgrass_steppe/Precipitation/seasonal_change_PPT.csv'))
seasonal_change_spring_sgs_df$Ecoregion <- 'Shortgrass steppe'
seasonal_change_spring_sgs <- seasonal_change_spring_sgs_df %>% 
  dplyr::select(x,y,change_spring_prop)
seasonal_change_spring_sgs <- rasterFromXYZ(seasonal_change_spring_sgs)

seasonal_change_spring_nmp_df <- 
  read.csv(paste0('./../../Data/Climate/Ecoregion/northern_mixed_prairies/Precipitation/seasonal_change_PPT.csv'))
seasonal_change_spring_nmp_df$Ecoregion <- 'Northern mixed prairies'
seasonal_change_spring_nmp <- seasonal_change_spring_nmp_df %>% 
  dplyr::select(x,y,change_spring_prop)
seasonal_change_spring_nmp <- rasterFromXYZ(seasonal_change_spring_nmp)
#some pixels missing for NMP, need to check this...

seasonal_change_spring_ppt <- raster::merge(seasonal_change_spring_nmp,seasonal_change_spring_sgs,
                                            tolerance=0.2)
#plot(seasonal_change_spring_ppt)

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
  scale_fill_scico('Change in % of\nspring precipitation',palette = 'roma',
                   direction = 1,midpoint = 0) +
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

spring_ppt_pdf <- ggplot(spring_ppt_binded, aes(x = change_spring_prop, fill = Ecoregion)) +
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
    axis.line.y = element_line(colour = "black"))


#make and save the inset plot
library(grid)
vp <- viewport(width = 0.44, height = 0.39, x = 0.23,y=0.27)
# y = unit(0.7, "lines"), just = c("right",
#                                  "bottom")

#executing the inset, you create a function the utlizes all the previous code
full <- function() {
  print(spring_ppt_map)
  print(spring_ppt_pdf , vp = vp)
}


png(height = 1700,width=2000,res=300,'Figures/change_in_perc_spring_PPT_drought.png')

full()

dev.off()

#cleanup
rm(spring_ppt_map,spring_ppt_pdf,spring_ppt_binded,seasonal_change_spring_sgs_df,
   seasonal_change_spring_nmp_df,seasonal_change_spring_nmp,seasonal_change_spring_sgs)


#-------------------------------------------------------------------------------
# points used for randomized VPD subset ------

#load file (will need to update working directory)
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
# growth dynamics 1 km subset (update 6/23/2022) --------

#absolute change in C uptake/GPP through time

#import sgs
growth_drynamics_absolute_sgs_1km <- 
  read_csv('./../../Data/growth_dynamics/one_km_subset/drought_gpp_reduction_absolute_shortgrass_steppe.csv')
#head(growth_drynamics_absolute_sgs_1km,1)

#import NMP
growth_drynamics_absolute_nmp <- 
  read_csv('./../../Data/growth_dynamics/one_km_subset/drought_gpp_reduction_absolute_northern_mixed_prairies.csv')
#head(growth_drynamics_absolute_nmp_1km,1)

#filepath
png(height = 3000,width=3000,res=300,'Figures/multi_panel_gpp_impacts_absolute_1km.png')

#setup
par(mfrow=c(2,1),cex = 0.5,lwd = 0.5,oma=c(3.2,9,1,1),mar = c(3,2.25,3,3))

#sgs
plot(abs_change~doy,data=growth_drynamics_absolute_sgs_1km,type='l',
     xlab='',ylab='',las=1,cex.axis=2,ylim=c(-26,5))
rect(151,-70,243,350,col = 'grey95')
rect(60,-70,151,350,col = 'grey')
polygon(c(growth_drynamics_absolute_sgs_1km$doy,rev(growth_drynamics_absolute_sgs_1km$doy)),
        c(growth_drynamics_absolute_sgs_1km$ci_75,rev(growth_drynamics_absolute_sgs_1km$ci_25)),
        col = "black", border = F)
text(100, -20, "Spring",cex=3)
text(200, -5, "Summer",cex=3)
text(275, -20, "Fall",cex=3)
text(200, 1, "Median carbon uptake",cex=2)
abline(h=0,col='black',lty='dashed')
mtext('Shortgrass steppe',side=3,line=0.5,cex=1.5)
mtext('a',side=3,line=0.5,cex=2,adj=0.0)
lines(abs_change~doy,data=growth_drynamics_absolute_sgs_1km,type='l',col='white',lwd=2)

#nmp
plot(abs_change~doy,data=growth_drynamics_absolute_nmp,type='l',
     xlab='',ylab='',las=1,cex.axis=2,ylim=c(-35,10))
rect(151,-70,243,350,col = 'grey95')
rect(60,-70,151,350,col = 'grey')
polygon(c(growth_drynamics_absolute_nmp$doy,rev(growth_drynamics_absolute_nmp$doy)),
        c(growth_drynamics_absolute_nmp$ci_25,rev(growth_drynamics_absolute_nmp$ci_75)),
        col = "black", border = F)
abline(h=0,col='black',lty='dashed')
mtext('Northern mixed prairies',side=3,line=0.5,cex=1.5)
mtext('Day of year',side=1,line=4.5,cex=2.5)
mtext(expression("Change in carbon uptake "(~g~C~m^-2~'16 days')),side=2,line=4,adj = -0.1, cex=2.5)
mtext('b',side=3,line=0.5,cex=2,adj=0.0)
lines(abs_change~doy,data=growth_drynamics_absolute_nmp,type='l',col='white',lwd=4)

dev.off()

#-------------------------------------------------------------------------------
# drought years map and barchart (updated 6/17/2022) ------

library(rgdal)

#map of distributions

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
test_wd <- "/Volumes/GoogleDrive/My Drive/range-resilience/Sensitivity/Processing NPP Data/NPP Data processing"

#load file (will need to update working directory)
rangeland_npp_covariates <- readRDS(file.path(test_wd, "Dryland_NPP.rds")) #loads file and name it annualSWA_OctDec I guess

nm_sgs <- rangeland_npp_covariates %>%
  dplyr::filter(region == c('northern_mixed_prairies','shortgrass_steppe')) 

mean_npp <- aggregate(npp~x+y,mean,data=nm_sgs)

mean_production_raster<-rasterFromXYZ(mean_npp)
#plot(mean_production_raster)

#import shapefiles

#SGS
SGS.shape<-readOGR(dsn="/Volumes/GoogleDrive/My Drive/range-resilience/scope/study-area-shapefiles/SGS",layer="SGS")
#plot(SGS.shape)
SGS.shape@bbox <- as.matrix(extent(mean_production_raster))
#plot(SGS.shape)
SGS.shape.2 <- sp::spTransform(SGS.shape, crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
SGS.shape.3 <- sp::spTransform(SGS.shape.2, crs(Albers))
sgs_shape_df_tidy <- tidy(SGS.shape.3)
sgs_shape_df_tidy <- left_join(sgs_shape_df_tidy, SGS.shape.3@data)
#plot(SGS.shape.3)

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
#crop(NorthernMixedSubset.shape.2,mean_production_raster)

nmp_shape_df_tidy <- tidy(NorthernMixedSubset.shape.3)
nmp_shape_df_tidy$ecoregion <- 'Northern mixed prairies'
sgs_shape_df_tidy$ecoregion <- 'Shortgrass steppe'

# Recategorizes data as required for plotting
#nmp_shape_df_tidy <- left_join(nmp_shape_df_tidy, NorthernMixedSubset.shape.3@data)

distributions <- ggplot() +
  geom_polygon(data=states_all_sites_tidy, mapping=aes(x = long, y = lat,group=group),
               color = "black", size = 0.1,fill=NA) +
  geom_polygon(data=nmp_shape_df_tidy, mapping=aes(x = long, y = lat,group=group,fill=ecoregion),
               color = "black", size = 0.1) + #fill='steelblue2'
  geom_polygon(data=sgs_shape_df_tidy, mapping=aes(x = long, y = lat,group=group,fill=ecoregion),
               color = "black", size = 0.1) + #fill='green4'
  scale_fill_manual(values = c(
    'Shortgrass steppe' = 'green4',
    'Northern mixed prairies' = 'steelblue2'
  )) +
  coord_equal() +
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
    plot.margin = margin(0.0,1,0.0,0.0,"cm"),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_blank(),
    axis.line.y = element_blank())

#
#

#barchart of driest years
Ecoregion <- 'shortgrass_steppe'
driest_year_sgs <- 
  read.csv(paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/growing_season/drought_precip_year_',Ecoregion,'.csv'))
driest_year_sgs$ecoregion <- 'Shortgrass steppe'

#select five most common years
driest_year_sgs_count <- aggregate(ppt_min~year + ecoregion,length,data=driest_year_sgs)
driest_year_sgs_count <- driest_year_sgs_count %>% arrange(desc(ppt_min)) %>%
  dplyr::filter(ppt_min > 34)
  
Ecoregion <- 'northern_mixed_prairies'
driest_year_nmp <- 
  read.csv(paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/growing_season/drought_precip_year_',Ecoregion,'.csv'))
driest_year_nmp$ecoregion <- 'Northern mixed prairies'

#select five most common years
driest_year_nmp_count <- aggregate(ppt_min~year + ecoregion,length,data=driest_year_nmp)
driest_year_nmp_count <- driest_year_nmp_count %>% arrange(desc(ppt_min)) %>%
  filter(ppt_min > 878)

five_driest_years <- rbind(driest_year_sgs_count,driest_year_nmp_count)

head(five_driest_years,1)

five_driest_years$year <- as.factor(five_driest_years$year)

  driest_years_barchat <- ggplot(five_driest_years,aes(reorder(year,-ppt_min),ppt_min,fill=ecoregion)) +
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
      axis.line.y = element_line(colour = "black"))

#
#

#map make of driest years
  
#raster is acting weird
#str(driest_year_sgs)
#driest_year_sgs$year <- as.factor(driest_year_sgs$year)
driest_year_sgs_raster <- rasterFromXYZ(driest_year_sgs[c(2,3,4)])
driest_year_nmp_raster <- rasterFromXYZ(driest_year_nmp[c(2,3,4)])

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
  
#make multipanel figure
  
  library(patchwork)
  
  png(height = 1500,width=3000,res=300,'Figures/driest_years_V2.png')
  
  
  #p123 <- (distributions/ driest_years_barchat)|driest_year_map_plot
  p123 <- distributions + driest_years_barchat + driest_year_map_plot 
  p123 + plot_annotation(tag_levels = "a")
  
  dev.off()
  
#
#
  
#cleanup
rm(distributionsm,driest_year_map,driest_year_map_df,driest_year_map_plot,
   driest_year_nmp,driest_year_nmp_count,driest_year_nmp_raster,
   driest_year_raster,driest_year_sgs,driest_year_sgs_count,five_driest_years,
   mean_npp,mean_precip_sgs,mean_production_raster,nm_sgs,nmp_shape_df_tidy,
   NorthernMixedSubset.shape,NorthernMixedSubset.shape.2,NorthernMixedSubset.shape.3,
   p123,rangeland_npp_covariates,sgs_shape_df_tidy,SGS.shape,SGS.shape.2,SGS.shape.3,
   driest_year_sgs_raster,driest_years_barchat,distributions)


#-------------------------------------------------------------------------------
# growth dynamics NDVI subset (Updated 6/23/2022 2022) -----
  
#growth dynamics NDVI

#SGS
growth_drynamics_ndvi_sgs <- 
  read_csv('./../../Data/growth_dynamics/drought_ndvi_reduction_absolute_shortgrass_steppe.csv')
head(growth_drynamics_ndvi_sgs,1)

#northern mixed prairies
growth_drynamics_ndvi_nmp <- 
  read_csv('./../../Data/growth_dynamics/drought_ndvi_reduction_absolute_northern_mixed_prairies.csv')
head(growth_drynamics_ndvi_nmp,1)

#filepath
png(height = 3000,width=3000,res=300,'Figures/multi_panel_growth_curves_NDVI')

#setup
par(mfrow=c(2,1),cex = 0.5,lwd = 0.5,oma=c(3.2,9,1,1),mar = c(3,2.25,3,3))

#sgs
plot(abs_change~doy,data=growth_drynamics_ndvi_sgs,type='l',
     xlab='',ylab='',las=1,cex.axis=2,ylim=c(-.15,.01))
rect(151,-70,243,350,col = 'grey95')
rect(60,-70,151,350,col = 'grey')
polygon(c(growth_drynamics_ndvi_sgs$doy,rev(growth_drynamics_ndvi_sgs$doy)),
        c(growth_drynamics_ndvi_sgs$ci_75,rev(growth_drynamics_ndvi_sgs$ci_25)),
        col = "black", border = F)
text(100, -.1, "Spring",cex=3)
text(200, -.03, "Summer",cex=3)
text(275, -.020, "Fall",cex=3)
text(200, .005, "Median NDVI",cex=2)
abline(h=0,col='black',lty='dashed')
mtext('Shortgrass steppe',side=3,line=0.5,cex=1.5)
mtext('a',side=3,line=0.5,cex=2,adj=0.0)
lines(abs_change~doy,data=growth_drynamics_ndvi_sgs,type='l',col='white',lwd=2)

#nmp
plot(abs_change~doy,data=growth_drynamics_ndvi_nmp,type='l',
     xlab='',ylab='',las=1,cex.axis=2,ylim=c(-.15,.06))
rect(151,-70,243,350,col = 'grey95')
rect(60,-70,151,350,col = 'grey')
polygon(c(growth_drynamics_ndvi_nmp$doy,rev(growth_drynamics_ndvi_nmp$doy)),
        c(growth_drynamics_ndvi_nmp$ci_25,rev(growth_drynamics_ndvi_nmp$ci_75)),
        col = "black", border = F)
abline(h=0,col='black',lty='dashed')
mtext('Northern mixed prairies',side=3,line=0.5,cex=1.5)
mtext('Day of year',side=1,line=4.5,cex=2.25)
mtext(expression("Change in NDVI"),side=2,line=5,adj = 3, cex=2.5)
mtext('b',side=3,line=0.5,cex=2,adj=0.0)
lines(abs_change~doy,data=growth_drynamics_ndvi_nmp,type='l',col='white',lwd=4)

dev.off()
  
#-------------------------------------------------------------------------------
# growth dynamics absolute (updated 6/15/2022) ------
  
#absolute change in C uptake/GPP through time
  
#import sgs
  growth_drynamics_absolute_sgs <- 
    read_csv('./../../Data/growth_dynamics/drought_gpp_reduction_absolute_shortgrass_steppe.csv')
  head(growth_drynamics_absolute_sgs,1)
  
  #import NMP
  growth_drynamics_absolute_nmp <- 
    read_csv('./../../Data/growth_dynamics/drought_gpp_reduction_absolute_northern_mixed_prairies.csv')
  head(growth_drynamics_absolute_nmp,1)
  
  #filepath
  png(height = 3000,width=3000,res=300,'Figures/multi_panel_gpp_impacts_absolute.png')
  
  #setup
  par(mfrow=c(2,1),cex = 0.5,lwd = 0.5,oma=c(3.2,9,1,1),mar = c(3,2.25,3,3))
  
  #sgs
  plot(abs_change~doy,data=growth_drynamics_absolute_sgs,type='l',
       xlab='',ylab='',las=1,cex.axis=2,ylim=c(-26,5))
  rect(151,-70,243,350,col = 'grey95')
  rect(60,-70,151,350,col = 'grey')
  #rect(168,-70,226,350,col=adjustcolor("red",alpha.f=0.50))
  polygon(c(growth_drynamics_absolute_sgs$doy,rev(growth_drynamics_absolute_sgs$doy)),
          c(growth_drynamics_absolute_sgs$ci_75,rev(growth_drynamics_absolute_sgs$ci_25)),
          col = "black", border = F)
  text(100, -20, "Spring",cex=3)
  text(201, -5, "Summer",cex=3)
  text(275, -20, "Fall",cex=3)
  text(201, 1, "Median carbon uptake",cex=2.5)
  #text(173, 3, "Range of peak reductions",cex=1.5)
  abline(h=0,col='black',lty='dashed')
  mtext('Shortgrass steppe',side=3,line=0.5,cex=1.5)
  mtext('a',side=3,line=0.5,cex=2,adj=0.0)
  lines(abs_change~doy,data=growth_drynamics_absolute_sgs,type='l',col='white',lwd=2)
  
  #nmp
  plot(abs_change~doy,data=growth_drynamics_absolute_nmp,type='l',
       xlab='',ylab='',las=1,cex.axis=2,ylim=c(-35,10))
  rect(151,-70,243,350,col = 'grey95')
  rect(60,-70,151,350,col = 'grey')
  #rect(171,-70,187,350,col=adjustcolor("red",alpha.f=0.50))
  polygon(c(growth_drynamics_absolute_nmp$doy,rev(growth_drynamics_absolute_nmp$doy)),
          c(growth_drynamics_absolute_nmp$ci_25,rev(growth_drynamics_absolute_nmp$ci_75)),
          col = "black", border = F)
  abline(h=0,col='black',lty='dashed')
  mtext('Northern mixed prairies',side=3,line=0.5,cex=1.5)
  mtext('Day of year',side=1,line=4.5,cex=2.5)
  mtext(expression("Drought impact to carbon uptake "(g~C~m^-2~'16 days')),side=2,line=4,adj = -0.1, cex=2.25)
  mtext('b',side=3,line=0.5,cex=2,adj=0.0)
  lines(abs_change~doy,data=growth_drynamics_absolute_nmp,type='l',col='white',lwd=4)
  
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
# growth dynamics relative (updated 6/15/2022) ------


png(height = 3000,width=3000,res=300,'Figures/multi_panel_gpp_impacts_relative.png')

#setup
par(mfrow=c(2,1),cex = 0.5,lwd = 0.5,oma=c(3.2,9,1,1),mar = c(3,2.25,3,3))


#sgs
plot(perc_change~doy,data=growth_drynamics_sgs,type='l',
     xlab='',ylab='',las=1,cex.axis=2,ylim=c(-80,55))
rect(151,-90,243,350,col = 'grey95')
rect(60,-90,151,350,col = 'grey')
polygon(c(growth_drynamics_sgs$doy,rev(growth_drynamics_sgs$doy)),
        c(growth_drynamics_sgs$ci_75,rev(growth_drynamics_sgs$ci_25)),
        col = "black", border = F)
text(100, -50, "Spring",cex=3)
text(200, -20, "Summer",cex=3)
text(275, 20, "Fall",cex=3)
text(200, 5, "Median carbon uptake",cex=2)
abline(h=0,col='black',lty='dashed')
mtext('Shortgrass steppe',side=3,line=0.5,cex=1.5)
mtext('a',side=3,line=0.5,cex=2,adj=0.0)
lines(perc_change~doy,data=growth_drynamics_sgs,type='l',col='white',lwd=2)

#nmp
plot(perc_change~doy,data=growth_drynamics_nmp,type='l',
     xlab='',ylab='',las=1,cex.axis=2,ylim=c(-60,120))
rect(151,-70,243,350,col = 'grey95')
rect(60,-70,151,350,col = 'grey')
polygon(c(growth_drynamics_nmp$doy,rev(growth_drynamics_nmp$doy)),
        c(growth_drynamics_nmp$ci_25,rev(growth_drynamics_nmp$ci_75)),
        col = "black", border = F)
abline(h=0,col='black',lty='dashed')
mtext('Northern mixed prairies',side=3,line=0.5,cex=1.5)
mtext('Day of year',side=1,line=4.5,cex=2.5)
mtext('% Change in carbon uptake',side=2,line=4.5,adj = -1.75, cex=2.5)
mtext('b',side=3,line=0.5,cex=2,adj=0.0)
lines(perc_change~doy,data=growth_drynamics_nmp,type='l',col='white',lwd=2)

dev.off()

#-------------------------------------------------------------------------------
# cumulative carbon uptake 1 km subset (updated 6/23/2022) ------


#import SGS
#mean
growth_curve_absolute_mean_sgs_1km <- 
  read_csv('./../../Data/growth_curves/one_km_subset/average_growth_curve_shortgrass_steppe.csv')
#head(growth_curve_absolute_mean_sgs_1km,1)

#drought
growth_curve_drought_absolute_mean_sgs_1km <- 
  read_csv('./../../Data/growth_curves/one_km_subset/drought_growth_curve_shortgrass_steppe.csv')
#head(growth_curve_drought_absolute_mean_sgs_1km,1)

#import NMP
#mean
growth_curve_absolute_mean_nmp_1km <- 
  read_csv('./../../Data/growth_curves/one_km_subset/average_growth_curve_northern_mixed_prairies.csv')
#head(growth_curve_absolute_mean_nmp_1km,1)

#drought
growth_curve_drought_absolute_mean_nmp_1km <- 
  read_csv('./../../Data/growth_curves/one_km_subset/drought_growth_curve_northern_mixed_prairies.csv')
#head(growth_curve_drought_absolute_mean_nmp_1km,1)

#SGS growth curve figure
png(height = 1500,width=1500,res=300,'Figures/multi_panel_growth_curves_2_1km')

par(mfrow=c(2,1),cex = 0.5,lwd = 0.5,oma=c(3.2,9,1,1),mar = c(3,2.25,3,3))

# plot it out panel A: sgs
plot(mean ~ doy, growth_curve_absolute_mean_sgs_1km,col='black',type='l',
     ylab='',
     xlab='',las=1,cex.axis=1.5,ylim=c(0,391))
polygon(c(growth_curve_absolute_mean_sgs_1km$doy,rev(growth_curve_absolute_mean_sgs_1km$doy)),
        c(growth_curve_absolute_mean_sgs_1km$spatial_ci_25,rev(growth_curve_absolute_mean_sgs_1km$spatial_ci_75)),
        col=adjustcolor("grey",alpha.f=0.50), border = F)
polygon(c(growth_curve_drought_absolute_mean_sgs_1km$doy,rev(growth_curve_drought_absolute_mean_sgs_1km$doy)),
        c(growth_curve_drought_absolute_mean_sgs_1km$spatial_25,rev(growth_curve_drought_absolute_mean_sgs_1km$spatial_75)),
        col=adjustcolor("red",alpha.f=0.40), border = F)
lines(mean ~ doy, growth_curve_drought_absolute_mean_sgs_1km,col='red',pch=19,lwd=1.25)
lines(mean ~ doy, growth_curve_absolute_mean_sgs_1km,col='black',pch=19,lwd=1.25)
legend(75, 350, legend=c("Average year", "Drought year"),         #alpha legend: 0.015, 150
       col=c("black", "red"), lty=1.1,lwd=4,cex=1.25,box.lty=0)
mtext('Shortgrass steppe',side=3,line=0.5,cex=1)
mtext('a',side=3,line=0.5,cex=1,adj=-0.05)

# plot it out panel B: nmp
plot(mean ~ doy, growth_curve_absolute_mean_nmp_1km,col='black',type='l',
     ylab='',
     xlab='',las=1,cex.axis=1.5,ylim=c(0,580))
polygon(c(growth_curve_absolute_mean_nmp_1km$doy,rev(growth_curve_absolute_mean_nmp_1km$doy)),
        c(growth_curve_absolute_mean_nmp_1km$spatial_ci_25,rev(growth_curve_absolute_mean_nmp_1km$spatial_ci_75)),
        col=adjustcolor("grey",alpha.f=0.50), border = F)
polygon(c(growth_curve_drought_absolute_mean_nmp_1km$doy,rev(growth_curve_drought_absolute_mean_nmp_1km$doy)),
        c(growth_curve_drought_absolute_mean_nmp_1km$spatial_25,rev(growth_curve_drought_absolute_mean_nmp_1km$spatial_75)),
        col=adjustcolor("red",alpha.f=0.40), border = F)
lines(mean ~ doy, growth_curve_drought_absolute_mean_nmp_1km,col='red',pch=19,lwd=1.25)
lines(mean ~ doy, growth_curve_absolute_mean_nmp_1km,col='black',pch=19,lwd=1.25)
mtext('Julian day of year',side=1,line=3.75,cex=1.25)
mtext('Northern mixed prairies',side=3,line=0.5,cex=1)
mtext('b',side=3,line=0.5,cex=1,adj=-0.05)
mtext(expression("Cumulative carbon uptake " (g~C~m^-2)),side=2,line=4.5,adj=-.15,cex=1.25)

dev.off()

#cleanup
rm(growth_curve_absolute_mean_sgs_1km,growth_curve_absolute_mean_nmp_1km,
   growth_curve_drought_absolute_mean_sgs_1km,growth_curve_drought_absolute_mean_nmp_1km)
#-------------------------------------------------------------------------------
# % total reduction in carbon uptake (updated 6/27/2022) -----

#import both datasets
max_total_reduction_sgs_df <- 
  read.csv('./../../Data/growth_dynamics/max_total_reduction_shortgrass_steppe.csv')

max_total_reduction_nmp_df <- 
  read.csv('./../../Data/growth_dynamics/max_total_reduction_northern_mixed_prairies.csv')

#import and get total for sgs
total_reduction_sgs <- max_total_reduction_sgs_df %>%
  dplyr::filter(type=='total') %>%
  select(x,y,perc_reduction)

#summary(total_reduction_sgs)

total_reduction_sgs <- rasterFromXYZ(total_reduction_sgs)
crs(total_reduction_sgs) <- "+proj=longlat +datum=WGS84"

#import and get total for nmp
total_reduction_nmp <- max_total_reduction_nmp_df %>%
  dplyr::filter(type=='total') %>%
  select(x,y,perc_reduction)

total_reduction_nmp <- rasterFromXYZ(total_reduction_nmp)
crs(total_reduction_nmp) <- "+proj=longlat +datum=WGS84"

#combine
total_reduction <- merge(total_reduction_nmp,total_reduction_sgs,tolerance = 0.20)

total_reduction <-projectRaster(total_reduction, crs='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96
+        +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
total_reduction <- data.frame(rasterToPoints(total_reduction))

total_reduction_map <-
  ggplot() +
  geom_polygon(data=states_all_sites_tidy, mapping=aes(x = long, y = lat,group=group),
               color = "black", size = 0.1,fill=NA) +
  geom_raster(data = total_reduction, mapping=aes(x = x, y = y, fill = layer)) + 
  coord_equal() +
  scale_fill_scico('Change in total carbon uptake (%)',
                   palette = 'roma',direction = 1,midpoint = 0) +
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


#now do PDF 

total_reduction_nmp <- data.frame(rasterToPoints(total_reduction_nmp))
total_reduction_nmp$Ecoregion <- 'Northern mixed prairies'

total_reduction_sgs <- data.frame(rasterToPoints(total_reduction_sgs))
total_reduction_sgs$Ecoregion <- 'Shortgrass steppe'

total_reduction_rbind <- rbind(total_reduction_nmp,total_reduction_sgs)

total_reduction_pdf <- ggplot(total_reduction_rbind, aes(x = perc_reduction, fill = Ecoregion)) +
  scale_y_continuous(expand = c(0,0)) +
  #scale_y_continuous(expand = c(0, 0), limits = c(0, 1.02)) +
  geom_density(color = 'black', alpha = 0.5, aes(y = ..scaled..)) +
  scale_fill_manual(values = c(
    'Northern mixed prairies' = 'steelblue2',
    'Shortgrass steppe' = 'green4'
  )) +
  #geom_vline(xintercept = 0,color='black') +
  xlab("Change in total carbon uptake (%)") +
  ylab('Probability density') +
  theme(
    axis.text.x = element_text(color = 'black', size = 10),
    #angle=25,hjust=1),
    axis.text.y = element_text(color = 'black', size = 10),
    axis.title = element_text(color = 'black', size = 10),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    #legend.position = c(0.82, 0.7),
    legend.position = 'top',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 9),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


#try to make inset
vp <- viewport(width = 0.44, height = 0.39, x = 0.23,y=0.27)
# y = unit(0.7, "lines"), just = c("right",
#                                  "bottom")

#executing the inset, you create a function the utlizes all the previous code
full <- function() {
  print(total_reduction_map)
  print(total_reduction_pdf , vp = vp)
}


png(height = 1700,width=2000,res=300,'Figures/total_reduction_map.png')

full()

dev.off()

#cleanup
rm(max_total_reduction_nmp_df,max_total_reduction_sgs_df,
   total_reduction_map,total_reduction_nmp,total_reduction_sgs,
   total_reduction_rbind,total_reduction_pdf,vp,total_reduction)



#-------------------------------------------------------------------------------
# peak reduction in carbon uptake figure (%) (updated 6/27/2022) ------


#import both datasets
max_total_reduction_sgs_df <- 
  read.csv('./../../Data/growth_dynamics/max_total_reduction_shortgrass_steppe.csv')

max_total_reduction_nmp_df <- 
  read.csv('./../../Data/growth_dynamics/max_total_reduction_northern_mixed_prairies.csv')

#import and get total for sgs
max_reduction_sgs <- max_total_reduction_sgs_df %>%
  dplyr::filter(type=='max') %>%
  dplyr::filter(doy > 73) %>%
  dplyr::filter(doy < 297) %>%
  dplyr::select(x,y,perc_reduction)


max_reduction_sgs <- rasterFromXYZ(max_reduction_sgs)
crs(max_reduction_sgs) <- "+proj=longlat +datum=WGS84"

#import and get total for nmp
max_reduction_nmp <- max_total_reduction_nmp_df %>%
  dplyr::filter(type=='max') %>%
  dplyr::filter(doy > 73) %>%
  dplyr::filter(doy < 297) %>%
  dplyr::select(x,y,perc_reduction)

max_reduction_nmp <- rasterFromXYZ(max_reduction_nmp)
crs(max_reduction_nmp) <- "+proj=longlat +datum=WGS84"

#combine
max_reduction <- merge(max_reduction_nmp,max_reduction_sgs,tolerance = 0.20)

max_reduction <-projectRaster(max_reduction, crs='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96
+        +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
max_reduction <- data.frame(rasterToPoints(max_reduction))

max_reduction_map <-
  ggplot() +
  geom_polygon(data=states_all_sites_tidy, mapping=aes(x = long, y = lat,group=group),
               color = "black", size = 0.1,fill=NA) +
  geom_raster(data=max_reduction, mapping=aes(x = x, y = y, fill = layer)) + 
  coord_equal() +
  scale_fill_scico('Maximum reduction\nin carbon uptake (%)',
                   palette = 'batlow',direction = 1) +
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



#day of peak reduction


#import and get total for sgs
max_reduction_doy_sgs <- max_total_reduction_sgs_df %>%
  dplyr::filter(type == 'max') %>%
  dplyr::select(x,y,doy) %>%
  dplyr::filter(doy > 73) %>%
  dplyr::filter(doy < 297)

max_reduction_doy_sgs <- rasterFromXYZ(max_reduction_doy_sgs)
crs(max_reduction_doy_sgs) <- "+proj=longlat +datum=WGS84"

#import and get total for nmp
max_reduction_doy_nmp <- max_total_reduction_nmp_df %>%
  dplyr::filter(type == 'max') %>%
  select(x,y,doy) %>%
  dplyr::filter(doy > 73) %>%
  dplyr::filter(doy < 297)

max_reduction_doy_nmp <- rasterFromXYZ(max_reduction_doy_nmp)
crs(max_reduction_doy_nmp) <- "+proj=longlat +datum=WGS84"

#combine
max_reduction_doy <- merge(max_reduction_doy_nmp,max_reduction_doy_sgs,tolerance = 0.20)

max_reduction_doy <-projectRaster(max_reduction_doy, crs='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96
+        +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
max_reduction_doy <- data.frame(rasterToPoints(max_reduction_doy))

max_reduction_doy_map <-
  ggplot() +
  geom_polygon(data=states_all_sites_tidy, mapping=aes(x = long, y = lat,group=group),
               color = "black", size = 0.1,fill=NA) +
  geom_raster(data=max_reduction_doy, mapping=aes(x = x, y = y, fill = layer)) + 
  coord_equal() +
  scale_fill_scico('Day of maximum reduction\nin carbon uptake',
                   palette = 'batlow',direction = 1) +
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


#now do PDFs 

max_reduction_doy_nmp <- data.frame(rasterToPoints(max_reduction_doy_nmp))
max_reduction_doy_nmp$ecoregion <- 'Northern mixed prairies'

max_reduction_doy_sgs <- data.frame(rasterToPoints(max_reduction_doy_sgs))
max_reduction_doy_sgs$ecoregion <- 'Shortgrass steppe'

max_reduction_doy_rbind <- rbind(max_reduction_doy_nmp,max_reduction_doy_sgs)

#peak reduction day of year PDF
max_reduction_doy_pdf <- ggplot(max_reduction_doy_rbind, aes(x = doy, fill = ecoregion)) +
  scale_y_continuous(expand = c(0,0)) +
  #scale_y_continuous(expand = c(0, 0), limits = c(0, 1.02)) +
  geom_density(color = 'black', alpha = 0.5, aes(y = ..scaled..)) +
  scale_fill_manual(values = c(
    'Northern mixed prairies' = 'steelblue2',
    'Shortgrass steppe' = 'green4'
  )) +
  geom_vline(xintercept = 230,color='black') +
  xlab("Day of maximum reduction in carbon uptake") +
  ylab('Probability density') +
  theme(
    axis.text.x = element_text(color = 'black', size = 10),
    #angle=25,hjust=1),
    axis.text.y = element_text(color = 'black', size = 10),
    axis.title = element_text(color = 'black', size = 10),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    #legend.position = c(0.82, 0.7),
    legend.position = 'none',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 10),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


max_reduction_nmp <- data.frame(rasterToPoints(max_reduction_nmp))
max_reduction_nmp$ecoregion <- 'Northern mixed prairies'

max_reduction_sgs <- data.frame(rasterToPoints(max_reduction_sgs))
max_reduction_sgs$ecoregion <- 'Shortgrass steppe'

max_reduction_rbind <- rbind(max_reduction_nmp,max_reduction_sgs)

#peak reduction PDF
max_reduction_pdf <- ggplot(max_reduction_rbind, aes(x = perc_reduction, fill = ecoregion)) +
  scale_y_continuous(expand = c(0,0)) +
  #scale_y_continuous(expand = c(0, 0), limits = c(0, 1.02)) +
  geom_density(color = 'black', alpha = 0.5, aes(y = ..scaled..)) +
  scale_fill_manual(values = c(
    'Northern mixed prairies' = 'steelblue2',
    'Shortgrass steppe' = 'green4'
  )) +
  #geom_vline(xintercept = 0,color='black') +
  xlab("Maximum reduction in carbon uptake (%)") +
  ylab('Probability density') +
  theme(
    axis.text.x = element_text(color = 'black', size = 10),
    #angle=25,hjust=1),
    axis.text.y = element_text(color = 'black', size = 10),
    axis.title = element_text(color = 'black', size = 10),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 9),
    #legend.position = c(0.82, 0.7),
    legend.position = 'top',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 10),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

library(patchwork)

png(height = 3000,width=2500,res=300,'Figures/day_of_max_reduction.png')

p123 <- max_reduction_map + max_reduction_doy_map + max_reduction_pdf +
  max_reduction_doy_pdf + plot_layout(ncol = 2)
p123 + plot_annotation(tag_levels = "a")

dev.off()

#cleanup
rm(max_reduction,max_reduction_doy,max_reduction_doy_map,
   max_reduction_doy_nmp,max_reduction_doy_pdf,max_reduction_doy_rbind,
   max_reduction_doy_sgs,max_reduction_map,max_reduction_nmp,
   max_reduction_pdf,max_reduction_rbind,max_redution_sgs,
   max_total_reduction_nmp_df,max_total_reduction_sgs_df,p123,max_reduction_sgs)

#-------------------------------------------------------------------------------
# total reduction in carbon uptake absolute (updated 6/27/2022) -------


#import both datasets
max_total_reduction_sgs_df <- 
  read.csv('./../../Data/growth_dynamics/max_total_reduction_shortgrass_steppe.csv')

max_total_reduction_nmp_df <- 
  read.csv('./../../Data/growth_dynamics/max_total_reduction_northern_mixed_prairies.csv')

#import and get total for sgs
total_abs_reduction_sgs <- max_total_reduction_sgs_df %>%
  dplyr::filter(type=='total') %>%
  select(x,y,reduction)

#summary(total_reduction_sgs)

total_abs_reduction_sgs <- rasterFromXYZ(total_abs_reduction_sgs)
crs(total_abs_reduction_sgs) <- "+proj=longlat +datum=WGS84"

#import and get total for nmp
total_abs_reduction_nmp <- max_total_reduction_nmp_df %>%
  dplyr::filter(type=='total') %>%
  select(x,y,reduction)

total_abs_reduction_nmp <- rasterFromXYZ(total_abs_reduction_nmp)
crs(total_abs_reduction_nmp) <- "+proj=longlat +datum=WGS84"

#combine
total_abs_reduction <- merge(total_abs_reduction_nmp,total_abs_reduction_sgs,tolerance = 0.20)

total_abs_reduction <-projectRaster(total_abs_reduction, crs='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96
+        +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
total_abs_reduction <- data.frame(rasterToPoints(total_abs_reduction))

total_abs_reduction_map <-
  ggplot() +
  geom_polygon(data=states_all_sites_tidy, mapping=aes(x = long, y = lat,group=group),
               color = "black", size = 0.1,fill=NA) +
  geom_raster(data = total_abs_reduction, mapping=aes(x = x, y = y, fill = layer)) + 
  coord_equal() +
  scale_fill_scico(bquote('Change in total carbon uptake ('*'g C'~ m^-2*')'),
                   palette = 'roma',direction = 1,midpoint = 0) +
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


#now do PDF 

total_abs_reduction_nmp <- data.frame(rasterToPoints(total_abs_reduction_nmp))
total_abs_reduction_nmp$Ecoregion <- 'Northern mixed prairies'

total_abs_reduction_sgs <- data.frame(rasterToPoints(total_abs_reduction_sgs))
total_abs_reduction_sgs$Ecoregion <- 'Shortgrass steppe'

total_abs_reduction_rbind <- rbind(total_abs_reduction_nmp,total_abs_reduction_sgs)

total_abs_reduction_pdf <- ggplot(total_abs_reduction_rbind, aes(x = reduction, fill = Ecoregion)) +
  scale_y_continuous(expand = c(0,0)) +
  #scale_y_continuous(expand = c(0, 0), limits = c(0, 1.02)) +
  geom_density(color = 'black', alpha = 0.5, aes(y = ..scaled..)) +
  scale_fill_manual(values = c(
    'Northern mixed prairies' = 'steelblue2',
    'Shortgrass steppe' = 'green4'
  )) +
  #geom_vline(xintercept = 0,color='black') +
  #xlab("Change in total carbon uptake") +
  xlab(bquote('Change in total carbon uptake ('*'g C'~ m^-2*')')) +
  ylab('Probability density') +
  theme(
    axis.text.x = element_text(color = 'black', size = 10),
    #angle=25,hjust=1),
    axis.text.y = element_text(color = 'black', size = 10),
    axis.title = element_text(color = 'black', size = 10),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    #legend.position = c(0.82, 0.7),
    legend.position = 'top',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 9),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


#try to make inset
vp <- viewport(width = 0.44, height = 0.39, x = 0.23,y=0.27)
# y = unit(0.7, "lines"), just = c("right",
#                                  "bottom")

#executing the inset, you create a function the utlizes all the previous code
full <- function() {
  print(total_abs_reduction_map)
  print(total_abs_reduction_pdf , vp = vp)
}


png(height = 1700,width=2000,res=300,'Figures/total_abs_reduction_map.png')

full()

dev.off()

#cleanup
rm(max_total_reduction_nmp_df,max_total_reduction_sgs_df,
   total_abs_reduction_map,total_abs_reduction_nmp,total_abs_reduction_sgs,
   total_abs_reduction_rbind,total_abs_reduction_pdf,vp,total_abs_reduction)


#-------------------------------------------------------------------------------
# peak reduction in carbon uptake absolute (updated 6/27/2022) ------


#import both datasets
max_total_reduction_sgs_df <- 
  read.csv('./../../Data/growth_dynamics/max_total_reduction_shortgrass_steppe.csv')

max_total_reduction_nmp_df <- 
  read.csv('./../../Data/growth_dynamics/max_total_reduction_northern_mixed_prairies.csv')

#import and get total for sgs
peak_abs_reduction_sgs <- max_total_reduction_sgs_df %>%
  dplyr::filter(type=='max') %>%
  dplyr::filter(doy < 297) %>%
  dplyr::filter(doy > 73) %>%
  dplyr::select(x,y,reduction)

#summary(peak_reduction_sgs)

peak_abs_reduction_sgs <- rasterFromXYZ(peak_abs_reduction_sgs)
crs(peak_abs_reduction_sgs) <- "+proj=longlat +datum=WGS84"

#import and get peak for nmp
peak_abs_reduction_nmp <- max_total_reduction_nmp_df %>%
  dplyr::filter(type=='max') %>%
  dplyr::filter(doy < 297) %>%
  dplyr::filter(doy > 73) %>%
  dplyr::select(x,y,reduction)

peak_abs_reduction_nmp <- rasterFromXYZ(peak_abs_reduction_nmp)
crs(peak_abs_reduction_nmp) <- "+proj=longlat +datum=WGS84"

#combine
peak_abs_reduction <- merge(peak_abs_reduction_nmp,peak_abs_reduction_sgs,tolerance = 0.20)

peak_abs_reduction <-projectRaster(peak_abs_reduction, crs='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96
+        +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
peak_abs_reduction <- data.frame(rasterToPoints(peak_abs_reduction))

peak_abs_reduction_map <-
  ggplot() +
  geom_polygon(data=states_all_sites_tidy, mapping=aes(x = long, y = lat,group=group),
               color = "black", size = 0.1,fill=NA) +
  geom_raster(data = peak_abs_reduction, mapping=aes(x = x, y = y, fill = layer)) + 
  coord_equal() +
  scale_fill_scico(bquote('Maximum reduction in carbon uptake ('*'g C'~ m^-2~'16 days'*')'),
                   palette = 'batlow',direction = 1) +
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


#now do PDF 

peak_abs_reduction_nmp <- data.frame(rasterToPoints(peak_abs_reduction_nmp))
peak_abs_reduction_nmp$Ecoregion <- 'Northern mixed prairies'

peak_abs_reduction_sgs <- data.frame(rasterToPoints(peak_abs_reduction_sgs))
peak_abs_reduction_sgs$Ecoregion <- 'Shortgrass steppe'

peak_abs_reduction_rbind <- rbind(peak_abs_reduction_nmp,peak_abs_reduction_sgs)

peak_abs_reduction_pdf <- ggplot(peak_abs_reduction_rbind, aes(x = reduction, fill = Ecoregion)) +
  scale_y_continuous(expand = c(0,0)) +
  #scale_y_continuous(expand = c(0, 0), limits = c(0, 1.02)) +
  geom_density(color = 'black', alpha = 0.5, aes(y = ..scaled..)) +
  scale_fill_manual(values = c(
    'Northern mixed prairies' = 'steelblue2',
    'Shortgrass steppe' = 'green4'
  )) +
  #geom_vline(xintercept = 0,color='black') +
  #xlab("Change in peak carbon uptake") +
  xlab(bquote('Maximum reduction ('*'g C'~ m^-2~'16 days'*')')) +
  ylab('Probability density') +
  theme(
    axis.text.x = element_text(color = 'black', size = 10),
    #angle=25,hjust=1),
    axis.text.y = element_text(color = 'black', size = 10),
    axis.title = element_text(color = 'black', size = 10),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    #legend.position = c(0.82, 0.7),
    legend.position = 'top',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 9),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


#try to make inset
vp <- viewport(width = 0.44, height = 0.39, x = 0.23,y=0.27)
# y = unit(0.7, "lines"), just = c("right",
#                                  "bottom")

#executing the inset, you create a function the utlizes all the previous code
full <- function() {
  print(peak_abs_reduction_map)
  print(peak_abs_reduction_pdf , vp = vp)
}


png(height = 1700,width=2000,res=300,'Figures/peak_abs_reduction_map.png')

full()

dev.off()

#cleanup
rm(max_total_reduction_nmp_df,max_total_reduction_sgs_df,
   peak_abs_reduction_map,peak_abs_reduction_nmp,peak_abs_reduction_sgs,
   peak_abs_reduction_rbind,peak_abs_reduction_pdf,vp,peak_abs_reduction)
#-------------------------------------------------------------------------------
# VPD change during drought by season (updated 7/13/2022 - make main figure) ------

#import
Ecoregion <- 'shortgrass_steppe'
seasonal_vpd_sgs <- read.csv(paste0(
  './../../Data/Climate/Ecoregion/',
  Ecoregion,
  '/PRISM/VPD_change.csv'
)) 

seasonal_vpd_sgs$ecoregion <- 'Shortgrass steppe'
seasonal_vpd_sgs$ecoregion_2 <- 'a'

Ecoregion <- 'northern_mixed_prairies'
seasonal_vpd_nmp <- read.csv(paste0(
  './../../Data/Climate/Ecoregion/',
  Ecoregion,
  '/PRISM/VPD_change.csv'
)) 

seasonal_vpd_nmp$ecoregion <- 'Northern mixed prairies'
seasonal_vpd_nmp$ecoregion_2 <- 'b'

seasonal_vpd_sgs_nmp <- rbind(seasonal_vpd_nmp,seasonal_vpd_sgs)
head(seasonal_vpd_sgs_nmp,1)

eco_names <- as_labeller(
  c( "a" = "Shortgrass steppe", "b" = 'Northern mixed prairies'))

vpd_change <- ggplot(seasonal_vpd_sgs_nmp, aes(x = abs_change, fill = season)) +
  facet_wrap(~ecoregion_2,ncol=1,labeller = eco_names) +
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
  xlab('Drought increase in average \ndaily maximum VPD (kPa)') +
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
    legend.position = c(0.6, 0.65),
    #legend.position = 'none',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 15),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#save to file
png(height = 3000,width=2750,res=300,'Figures/vpd_change.png')

print(vpd_change)

dev.off()

#NDVI correlation with this change

ecoregion_list <- c('shortgrass_steppe','northern_mixed_prairies')
vpd_gpp_list <- list()
for(i in ecoregion_list){
  Ecoregion = i
  seasonal_vpd <- 
    read.csv(paste0( './../../Data/Climate/Ecoregion/',
                     Ecoregion,
                     '/PRISM/VPD_change.csv'))
  #head(seasonal_vpd_nmp,1)
  
  seasonal_vpd <- seasonal_vpd %>%
    dplyr::select(x,y,abs_change,season) %>%
    dplyr::filter(season == 'summer') %>%
    dplyr::select(x,y,abs_change)
  
  #seasonal_vpd_sgs_nmp <- rbind(seasonal_vpd_nmp,seasonal_vpd_sgs)
  
  #subset by season
  # seasonal_vpd_sgs_spring <- subset(seasonal_vpd_sgs,season == 'spring')
  # seasonal_vpd_sgs_summer <- subset(seasonal_vpd_sgs,season == 'summer')
  
  
  
  #import C uptake datasets
  max_total_reduction <- 
    read.csv(paste0('./../../Data/growth_dynamics/max_total_reduction_NDVI_',Ecoregion,'.csv'))
  #head(max_total_reduction_sgs_df,1)
  
  
  # max_total_reduction_nmp_df <- 
  #   read.csv('./../../Data/growth_dynamics/max_total_reduction_northern_mixed_prairies.csv')
  # 
  # max_total_reduction_sgs_nmp <- rbind(max_total_reduction_sgs_df,max_total_reduction_nmp_df)
  
  #import and get total for sgs
  peak_abs_reduction <- max_total_reduction %>%
    dplyr::filter(type == 'max') %>%
    dplyr::filter(doy < 297) %>%
    dplyr::filter(doy > 73) %>%
    dplyr::select(x,y,reduction)
  
  #convert to raster and resample
  peak_abs_reduction <- rasterFromXYZ(peak_abs_reduction)
  seasonal_vpd <- rasterFromXYZ(seasonal_vpd)
  #plot(seasonal_vpd_sgs)
  
  peak_abs_reduction <- resample(peak_abs_reduction,seasonal_vpd)
  
  #convert back to dataframe and merge
  peak_abs_reduction <- data.frame(rasterToPoints(peak_abs_reduction))
  seasonal_vpd  <- data.frame(rasterToPoints(seasonal_vpd))
  
  max_total_reduction_vpd <- merge(peak_abs_reduction,
                                   seasonal_vpd,by = c('x','y'))
  
  max_total_reduction_vpd$ecoregion <- Ecoregion
  
  if(Ecoregion=='shortgrass_steppe'){
    
    max_total_reduction_vpd$ecoregion <- gsub('shortgrass_steppe','Shortgrass steppe',max_total_reduction_vpd$ecoregion)
    max_total_reduction_vpd$ecoregion_2 <- gsub('Shortgrass steppe','a',max_total_reduction_vpd$ecoregion)
    
  }else{
    
    max_total_reduction_vpd$ecoregion <- gsub('northern_mixed_prairies','Northern mixed prairies',
                                              max_total_reduction_vpd$ecoregion)
    max_total_reduction_vpd$ecoregion_2 <- gsub('Northern mixed prairies','b',max_total_reduction_vpd$ecoregion)
    
  }
  
  print(Ecoregion)
  print(summary(lm(reduction~abs_change,data=max_total_reduction_vpd)))
  
  vpd_gpp_list[[i]] <- max_total_reduction_vpd
  
}

#summer r-squared
#SGS: 0.40
#NMP: 0.38

#spring r-squared
#SGS: 0.15
#NMP: 0.17

#slopes are comparable

#convert to df
vpd_gpp_df <- list_to_df(vpd_gpp_list)

#labeler
eco_names <- as_labeller(
  c( "a" = "Shortgrass steppe", "b" = 'Northern mixed prairies'))

library(ggpubr)
?stat_cor
ndvi_vpd_plot <- ggplot(vpd_gpp_df,aes(abs_change,reduction)) +
  geom_point(pch=1,size=2)+
  stat_smooth(method='lm',color='black',size=0.5) +
  stat_cor(aes(label=..rr.label..), label.y.npc = 'bottom', label.x.npc = 'left') +
  facet_wrap(~ecoregion_2,ncol=1,labeller = eco_names,scales='free') +
  xlab('Drought increase in summer \nVPD (kPa)') +
  ylab('Maximum reduction in NDVI') +
  # annotate(geom = 'text', x = 0, y = -.20, 
  #          label = bquote("R^2 == 0.40"), parse = TRUE) +
  # annotate(geom = 'text', x = 0, y = -.30, 
  #          label = bquote("R^2 == 0.40"), parse = TRUE) +
  theme(
    axis.text.x = element_text(color = 'black', size = 13),
    #angle=25,hjust=1),
    axis.text.y = element_text(color = 'black', size = 13),
    axis.title = element_text(color = 'black', size = 25),
    axis.ticks = element_line(color = 'black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 15),
    legend.position = c(0.6, 0.65),
    #legend.position = 'none',
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 15),
    panel.background = element_rect(fill = NA),
    panel.border = element_blank(),
    #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


library(patchwork)

png(height = 3000,width=4000,res=300,'Figures/vpd_ndvi_change.png')

p123 <- vpd_change + ndvi_vpd_plot + plot_layout(ncol = 2)
p123 + plot_annotation(tag_levels = "a")

dev.off()


#-------------------------------------------------------------------------------
