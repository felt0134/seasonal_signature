

#workshopping code


# NDVI import -----


library(plotrix)
plan(multisession, workers = 10)
options(future.globals.maxSize = 8000 * 1024^2) #https://github.com/satijalab/seurat/issues/1845
period_list <- seq(1, 15, 1) #set periods
period_list <-
  as.character(period_list) #easier when they are characters
year_list <- seq(2003, 2020, 1) #set years
year_list <-
  as.character(year_list) #easier when they are characters


# first do the GPP import----

#loop through each year and period combination
Ecoregion <- 'shortgrass_steppe'

#list to store outputs in
gpp_list <- list()
i = 11
#run the loop
for (i in period_list) {
  filepath <-
    dir(
      paste0(
        './../../Data/NDVI/Ecoregion/',
        Ecoregion,
        '/MODIS_NDVI/Period/',
        i,
        '/'
      ),
      full.names = T
    )
  test <- lapply(filepath, format_gpp_df)
  test <- data.frame(do.call('rbind', test))
  #test <- lapply(test,rasterToPoints)
  gpp_list[[i]] <- test
  
}




#convert to raster
raster_file <- raster(filepath[10])
plot(raster_file)


#extract year from the name of the raster to later add to dataframe
year_val <- substr(names(raster_file), 6, 9)

#convert to dataframe and add year and period columns
df <- data.frame(rasterToPoints(raster_file))
df$year <- year_val
df$period <-
  gsub(paste0("GPP_", year_val, '_'), '', names(raster_file))
colnames(df) <- c('x', 'y', 'gpp', 'year', 'period')

#return formatted dataframe
return(df)


#convert list of dataframes to a single dataframe
gpp_df <- do.call('rbind', gpp_list)
rm(gpp_list, test) #get rid of excess stuff

#make unique id for each site
gpp_df_mean <- aggregate(gpp ~ x + y, mean, data = gpp_df)
gpp_df_mean$id_value <- seq.int(nrow(gpp_df_mean))
#head(gpp_df_mean)

#import conversion of period to day of year to map period on to DOY
doy_conversion <- read.csv('./../../Data/GPP/period_day_match.csv')

#add on day of year and ID columns
gpp_df <- merge(gpp_df, doy_conversion[c(2, 3)], by = c('period'))
gpp_df <- merge(gpp_df, gpp_df_mean[c(1, 2, 4)], by = c('x', 'y'))

#create a vector of unique sites IDs
id_list <- unique(gpp_df$id_value)
# ecoregion distributions (add to main figure script) ------

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


library(patchwork)

png(height = 1500,width=3000,res=300,'Figures/driest_years_V2.png')


#p123 <- (distributions/ driest_years_barchat)|driest_year_map_plot
p123 <- distributions + driest_years_barchat + driest_year_map_plot
p123 + plot_annotation(tag_levels = "a")

dev.off()




# ndvi raster format ----
i=1
filepath <-
  dir(
    paste0(
      './../../Data/ndvi/Ecoregion/',
      Ecoregion,
      '/MODIS_ndvi/Period/',
      i,
      '/'
    ),
    full.names = T
  )

format_ndvi_df <- function(x) {
  #convert to raster
  raster_file <- raster(i)
  
  #extract year from the name of the raster to later add to dataframe
  year_val <- substr(names(raster_file), 6, 9)
  
  #convert to dataframe and add year and period columns
  df <- data.frame(rasterToPoints(raster_file))
  plot(raster_file)
  df$year <- year_val
  df$period <-
    gsub(paste0("NDVI_", year_val, '_'), '', names(raster_file))
  colnames(df) <- c('x', 'y', 'ndvi', 'year', 'period')
  
  #return formatted dataframe
  return(df)
  
  
}
# growth cycle -----


#get each 95% CI for each day of the prediction
doy_list <- c(65:297)

#loop
gpp_normal_list <- list()
gpp_drought_list <- list()
gpp_predicted_list_average <- list()
gpp_predicted_list_drought <- list()
#test_list <- c(1:10)

for(i in doy_list){
  
  for(j in test_list){
    
    #average
    gpp_predicted_average <- data.frame(predict(growth_spline_list[[j]], i))
    gpp_predicted_average$id_val <- j
    gpp_predicted_list_average[[j]] <- gpp_predicted_average
    
    #drought
    gpp_predicted_drought <- data.frame(predict(growth_drought_spline_list[[j]], i))
    gpp_predicted_drought$id_val <- j
    gpp_predicted_list_drought[[j]] <- gpp_predicted_drought
    
  }
  
  #average growth cycle
  
  #convert to data frame and remove values below zero
  gpp_predicted_list_average_df <- list_to_df(gpp_predicted_list_average)
  colnames(gpp_predicted_list_average_df) <- c('doy','gpp_average','id_val')
  gpp_predicted_list_average_df <- gpp_predicted_list_average_df %>%
    filter(gpp_average > 0)
  
  #sample size
  ss <- nrow(gpp_predicted_list_average_df)
  
  #get median
  gpp_predicted_average <- aggregate(gpp_average~doy,median,data=gpp_predicted_list_average_df)
  
  #get 95% CI
  gpp_predicted_average$ci_99 <- std.error(gpp_predicted_list_average_df$gpp_average)*2.576
  gpp_predicted_average$sample_size <- ss
  gpp_normal_list[[i]] <- gpp_predicted_average

  
  #drought growth cycle
  
  #convert to data frame and remove values below zero
  gpp_predicted_list_drought_df <- list_to_df(gpp_predicted_list_drought)
  colnames(gpp_predicted_list_drought_df) <- c('doy','gpp_drought','id_val')
  gpp_predicted_list_drought_df <- gpp_predicted_list_drought_df %>%
    filter(gpp_drought > 0)
  
  #sample size
  ss <- nrow(gpp_predicted_list_drought_df)
  
  #get median
  gpp_predicted_drought <- aggregate(gpp_drought~doy,median,data=gpp_predicted_list_drought_df)
  
  #get 95% CI
  gpp_predicted_drought$ci_99 <- std.error(gpp_predicted_list_drought_df$gpp_drought)*2.576
  gpp_predicted_drought$sample_size <- ss
  gpp_drought_list[[i]] <- gpp_predicted_drought
  
  
}


gpp_normal_list_df <- list_to_df(gpp_normal_list)
head(gpp_normal_list_df,1)
gpp_drought_list_df <- list_to_df(gpp_drought_list)

plot(gpp_average~doy,data=gpp_normal_list_df)
lines(gpp_drought~doy,data=gpp_drought_list_df,col='red')


# growth curve with temporal 95 CI -----


get_average_growth_curve_absolute_spline_ci  <- function(i) {
  
  #subset to a given pixel
  growth_id <- gpp_df %>%
    dplyr::filter(id_value == i)
  
  x <- unique(growth_id %>% pull(x))
  y <- unique(growth_id %>% pull(y))
  
  #first get temporal variation
  year_list <- list()
  for(j in unique(growth_id$year)){
    
    subset <- subset(growth_id,year==j)
    
    subset <- subset %>% arrange(doy)
    
    subset <-
      data.frame(subset, gpp_ci = cumsum(subset$gpp))
    
    year_list[[j]] <- subset
    
    #str(subset)
    
  }
  
  year_df <- do.call('rbind',year_list)
  #plot(gpp_2~doy,data=year_df)
  rm(year_list)
  
  growth_id_ci <- aggregate(gpp_ci ~ doy, ci_99, data = year_df)
  #plot(gpp_2 ~ doy,growth_id_ci)
  rm(year_df)
  
  gpp.doy.spl_ci <-
    with(growth_id_ci, smooth.spline(doy, gpp_ci))
  #lines(gpp.doy.spl_ci, col = "blue")
  
  #rm(growth_id_cmulative)
  
  #run model through a sequence of days
  # doy <- data.frame(seq(from = 65, to = 297, by = 1))
  # gpp_predicted_ci <- data.frame(predict(gpp.doy.spl_ci, doy))
  # colnames(gpp_predicted_ci) <- c('day', 'temporal_ci')
  # plot(gpp~day,gpp_predicted_ci)
  # 
  
  #now do average growth curve
  
  growth_id <- aggregate(gpp ~ doy, mean, data = growth_id)
  #plot(gpp~doy,growth_id)
  
  #for that pixel, get cumulative GPP throughout the year
  growth_id_cmulative <-
    data.frame(growth_id, gpp_2 = cumsum(growth_id$gpp))
  
  #str(growth_id_cmulative)
  #head(growth_id_cmulative)
  #plot(gpp_2 ~ doy,data= growth_id_cmulative)
  
  #rm(growth_id)
  
  #create spline model of growth curve
  gpp.doy.spl <-
    with(growth_id_cmulative, smooth.spline(doy, gpp_2))
  #lines(gpp.doy.spl, col = "blue")
  
  # rm(growth_id_cmulative)
  # 
  # #run model through a sequence of days
  # doy <- data.frame(seq(from = 65, to = 297, by = 1))
  # gpp_predicted <- data.frame(predict(gpp.doy.spl, doy))
  # colnames(gpp_predicted) <- c('day', 'gpp')
  # #plot(gpp ~ day,gpp_predicted)
  # 
  # #rm(gpp_predicted)
  # 
  # gpp_predicted$x <- x
  # gpp_predicted$y <- y
  # 
  # gpp_predicted <- gpp_predicted %>%
  #   select(x,y,day,gpp)
  # 
  # gpp_predicted <- merge(gpp_predicted,gpp_predicted_ci,by=c('day'))
  # 
  
  return(list(gpp.doy.spl,gpp.doy.spl_ci))
  
  
  
}


look <- get_average_growth_curve_absolute_ci(100)

# variograms of day 50,90,25 (USE THIS FRAMEWORK) --------

library(gstat)
library(sp)

#drought impact to day of 50 C uptake

#shortgrass steppe 
point_data_50_sgs <- as(day_50_drought_sgs, 'SpatialPointsDataFrame')

TheVariogram_50_sgs = variogram(day_50_drought_impact_shortgrass_steppe ~1,
                                data = point_data_50_sgs,width = 10)
summary(TheVariogram_50_sgs)
plot(TheVariogram_50_sgs)

TheVariogramModel_50_sgs <- vgm(psill=300, model="Exp", nugget=50, range=100)
plot(TheVariogram_50_sgs, model=TheVariogramModel_50_sgs) 
FittedModel_50_sgs <- fit.variogram(TheVariogram_50_sgs, model=TheVariogramModel_50_sgs)
FittedModel_50_sgs
#Range = 50.1 km

plot(TheVariogram_50_sgs, model=FittedModel_50_sgs,xlab='Distance (km)',
     ylab = 'Semivariance',cex=1,lwd=2,col='black')
abline(v=100)
?abline

#NMP
point_data_50_nmp <- as(day_50_drought_nmp, 'SpatialPointsDataFrame')

TheVariogram_50_nmp = variogram(day_50_drought_impact_northern_mixed_prairies ~1,
                                data = point_data_50_nmp,width = 10)
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

# drought impact to day of 90% growth

#advance of day 50 sgs
day_90_sgs <- raster('./../../Data/CDD/day_of_90/day_90_shortgrass_steppe.tif')
day_90_drought_sgs <-
  raster('./../../Data/CDD/day_of_90/day_90_droughtshortgrass_steppe.tif')
day_90_drought_sgs <- stack(day_90_drought_sgs, day_90_sgs)
day_90_drought_sgs_2 <-
  day_90_drought_sgs$day_90_droughtshortgrass_steppe -
  day_90_drought_sgs$day_90_shortgrass_steppe

#advance of day 90 nmp
day_90_nmp <- raster('./../../Data/CDD/day_of_90/day_90_northern_mixed_prairies.tif')
day_90_drought_nmp <-
  raster('./../../Data/CDD/day_of_90/day_90_droughtnorthern_mixed_prairies.tif')
day_90_drought_nmp <- stack(day_90_drought_nmp, day_90_nmp)
day_90_drought_nmp_2 <-
  day_90_drought_nmp$day_90_droughtnorthern_mixed_prairies -
  day_90_drought_nmp$day_90_northern_mixed_prairies

# #combine
# day_90_drought <-
#   raster::merge(day_90_drought_nmp_2, day_90_drought_sgs_2, tolerance = 0.20)
# proj4string(day_90_drought) <- CRS("+proj=longlat")
# plot(day_90_drought)

#SGS
point_data_90_sgs <- as(day_90_drought_sgs_2, 'SpatialPointsDataFrame')

TheVariogram_90_sgs = variogram(layer ~1,data = point_data_90_sgs,width = 10)
plot(TheVariogram_90_sgs)

TheVariogramModel_90_sgs <- vgm(psill=15, model="Exp", nugget=3, range=200)
plot(TheVariogram_90_sgs, model=TheVariogramModel_90_sgs) 
FittedModel_90_sgs <- fit.variogram(TheVariogram_90_sgs, model=TheVariogramModel_90_sgs)
FittedModel_90_sgs
#Range = 112.1

plot(TheVariogram_90_sgs, model=FittedModel_90_sgs,xlab='Distance (km)',
     ylab = 'Semivariance',cex=1,lwd=2,col='black')

#NMP
point_data_90_nmp <- as(day_90_drought_nmp_2, 'SpatialPointsDataFrame')

TheVariogram_90_nmp = variogram(layer ~1,data = point_data_90_nmp,width = 10)
plot(TheVariogram_90_nmp)

TheVariogramModel_90_nmp <- vgm(psill=8, model="Exp", nugget=1, range=200)
plot(TheVariogram_90_nmp, model=TheVariogramModel_90_nmp) 
FittedModel_90_nmp <- fit.variogram(TheVariogram_90_nmp, model=TheVariogramModel_90_nmp)
FittedModel_90_nmp
#Range = 160.72

plot(TheVariogram_90_nmp, model=FittedModel_90_nmp,xlab='Distance (km)',
     ylab = 'Semivariance',cex=1,lwd=2,col='black')


#drought impact to day of 25% growth

#advance of day 25 sgs
day_25_sgs <- raster('./../../Data/CDD/day_of_25/day_25_shortgrass_steppe.tif')
day_25_drought_sgs <-
  raster('./../../Data/CDD/day_of_25/day_25_droughtshortgrass_steppe.tif')
day_25_drought_sgs <- stack(day_25_drought_sgs, day_25_sgs)
day_25_drought_sgs_2 <-
  day_25_drought_sgs$day_25_droughtshortgrass_steppe -
  day_25_drought_sgs$day_25_shortgrass_steppe

#advance of day 25 nmp
day_25_nmp <- raster('./../../Data/CDD/day_of_25/day_25_northern_mixed_prairies.tif')
day_25_drought_nmp <-
  raster('./../../Data/CDD/day_of_25/day_25_droughtnorthern_mixed_prairies.tif')
day_25_drought_nmp <- stack(day_25_drought_nmp, day_25_nmp)
day_25_drought_nmp_2 <-
  day_25_drought_nmp$day_25_droughtnorthern_mixed_prairies -
  day_25_drought_nmp$day_25_northern_mixed_prairies

# #combine
# day_25_drought <-
#   raster::merge(day_25_drought_nmp_2, day_25_drought_sgs_2, tolerance = 0.20)
# proj4string(day_25_drought) <- CRS("+proj=longlat")
# plot(day_25_drought)


#SGS
point_data_25_sgs <- as(day_25_drought_sgs_2, 'SpatialPointsDataFrame')

TheVariogram_25_sgs = variogram(layer ~1,data = point_data_25_sgs,width = 10)
plot(TheVariogram_25_sgs)

TheVariogramModel_25_sgs <- vgm(psill=50, model="Exp", nugget=10, range=150)
plot(TheVariogram_25_sgs, model=TheVariogramModel_25_sgs) 
FittedModel_25_sgs <- fit.variogram(TheVariogram_25_sgs, model=TheVariogramModel_25_sgs)
FittedModel_25_sgs
#Range = 56.68

plot(TheVariogram_25_sgs, model=FittedModel_25_sgs,xlab='Distance (km)',
     ylab = 'Semivariance',cex=1,lwd=2,col='black')

#NMP
point_data_25_nmp <- as(day_25_drought_nmp_2, 'SpatialPointsDataFrame')

TheVariogram_25_nmp = variogram(layer ~1,data = point_data_25_nmp,width = 10)
plot(TheVariogram_25_nmp)

TheVariogramModel_25_nmp <- vgm(psill=40, model="Exp", nugget=5, range=300)
plot(TheVariogram_25_nmp, model=TheVariogramModel_25_nmp) 
FittedModel_25_nmp <- fit.variogram(TheVariogram_25_nmp, model=TheVariogramModel_25_nmp)
FittedModel_25_nmp
#Range = 136.1

plot(TheVariogram_25_nmp, model=FittedModel_25_nmp,xlab='Distance (km)',
     ylab = 'Semivariance',cex=1,lwd=2,col='black')



# k. test of random subsample -----

library(plotrix)

# day of 50% C uptake

#advance of day 50 sgs
day_50_sgs <- raster('./../../Data/CDD/day_of_50/day_50_shortgrass_steppe.tif')
day_50_drought_sgs <-
  raster('./../../Data/CDD/day_of_50/day_50_droughtshortgrass_steppe.tif')
day_50_drought_sgs <- stack(day_50_drought_sgs, day_50_sgs)
day_50_drought_sgs_2 <-
  day_50_drought_sgs$day_50_droughtshortgrass_steppe -
  day_50_drought_sgs$day_50_shortgrass_steppe

day_50_drought_sgs <- data.frame(rasterToPoints(day_50_drought_sgs_2))



#advance of day 50 nmp
day_50_nmp <- raster('./../../Data/CDD/day_of_50/day_50_northern_mixed_prairies.tif')
day_50_drought_nmp <-
  raster('./../../Data/CDD/day_of_50/day_50_droughtnorthern_mixed_prairies.tif')
day_50_drought_nmp <- stack(day_50_drought_nmp, day_50_nmp)
day_50_drought_nmp_2 <-
  day_50_drought_nmp$day_50_droughtnorthern_mixed_prairies -
  day_50_drought_nmp$day_50_northern_mixed_prairies

day_50_drought_nmp <- data.frame(rasterToPoints(day_50_drought_nmp_2))

ks_test_bootstrap <- function(data_1,data_2){

d_list <- list()
for(i in 1:1000){

#get random subset of 100
day_50_drought_nmp_rand <- day_50_drought_nmp %>%
  dplyr::sample_n(100)


hist(day_50_drought_nmp_rand$layer)

day_50_drought_sgs_rand <- day_50_drought_sgs %>%
  dplyr::sample_n(100)
hist(day_50_drought_sgs_rand$layer)
plot(y~x,day_50_drought_sgs_rand)

test <- ks.test(day_50_drought_sgs_rand$layer,day_50_drought_nmp_rand$layer,exact=F)
D <- data.frame(test$statistic)

d_list[[i]] <- D

}

d_df <- do.call('rbind',d_list)

return(d_df)

}

hist(d_df$test.statistic)
summary(d_df)
#calculate 99% confidence intervals
ci_99(d_df$test.statistic)


##


# day of 90% C uptake

#advance of day 50 sgs
day_90_sgs <- raster('./../../Data/CDD/day_of_90/day_90_shortgrass_steppe.tif')
day_90_drought_sgs <-
  raster('./../../Data/CDD/day_of_90/day_90_droughtshortgrass_steppe.tif')
day_90_drought_sgs <- stack(day_90_drought_sgs, day_90_sgs)
day_90_drought_sgs_2 <-
  day_90_drought_sgs$day_90_droughtshortgrass_steppe -
  day_90_drought_sgs$day_90_shortgrass_steppe

day_90_drought_sgs <- data.frame(rasterToPoints(day_90_drought_sgs_2))

#advance of day 90 nmp
day_90_nmp <- raster('./../../Data/CDD/day_of_90/day_90_northern_mixed_prairies.tif')
day_90_drought_nmp <-
  raster('./../../Data/CDD/day_of_90/day_90_droughtnorthern_mixed_prairies.tif')
day_90_drought_nmp <- stack(day_90_drought_nmp, day_90_nmp)
day_90_drought_nmp_2 <-
  day_90_drought_nmp$day_90_droughtnorthern_mixed_prairies -
  day_90_drought_nmp$day_90_northern_mixed_prairies

day_90_drought_nmp <- data.frame(rasterToPoints(day_90_drought_nmp_2))

d_list_90 <- list()
for(i in 1:1000){
  
  #get random subset of 100
  day_90_drought_nmp_rand <- day_90_drought_nmp %>%
    dplyr::sample_n(100)
  hist(day_90_drought_nmp_rand$layer)
  
  day_90_drought_sgs_rand <- day_90_drought_sgs %>%
    dplyr::sample_n(100)
  hist(day_90_drought_sgs_rand$layer)
  plot(y~x,day_90_drought_sgs_rand)
  
  test <- ks.test(day_90_drought_sgs_rand$layer,day_90_drought_nmp_rand$layer,exact=F)
  D <- data.frame(test$statistic)
  
  d_list_90[[i]] <- D
  
}

d_df_90 <- do.call('rbind',d_list_90)
hist(d_df_90$test.statistic)
summary(d_df_90)
#calculate 99% confidence intervals


##


# day of 25% C uptake

#advance of day 50 sgs
day_25_sgs <- raster('./../../Data/CDD/day_of_25/day_25_shortgrass_steppe.tif')
day_25_drought_sgs <-
  raster('./../../Data/CDD/day_of_25/day_25_droughtshortgrass_steppe.tif')
day_25_drought_sgs <- stack(day_25_drought_sgs, day_25_sgs)
day_25_drought_sgs_2 <-
  day_25_drought_sgs$day_25_droughtshortgrass_steppe -
  day_25_drought_sgs$day_25_shortgrass_steppe

day_25_drought_sgs <- data.frame(rasterToPoints(day_25_drought_sgs_2))

#advance of day 25 nmp
day_25_nmp <- raster('./../../Data/CDD/day_of_25/day_25_northern_mixed_prairies.tif')
day_25_drought_nmp <-
  raster('./../../Data/CDD/day_of_25/day_25_droughtnorthern_mixed_prairies.tif')
day_25_drought_nmp <- stack(day_25_drought_nmp, day_25_nmp)
day_25_drought_nmp_2 <-
  day_25_drought_nmp$day_25_droughtnorthern_mixed_prairies -
  day_25_drought_nmp$day_25_northern_mixed_prairies

day_25_drought_nmp <- data.frame(rasterToPoints(day_25_drought_nmp_2))

d_list_25 <- list()
for(i in 1:1000){
  
  #get random subset of 100
  day_25_drought_nmp_rand <- day_25_drought_nmp %>%
    dplyr::sample_n(100)
  hist(day_25_drought_nmp_rand$layer)
  
  day_25_drought_sgs_rand <- day_25_drought_sgs %>%
    dplyr::sample_n(100)
  hist(day_25_drought_sgs_rand$layer)
  plot(y~x,day_25_drought_sgs_rand)
  
  test <- ks.test(day_25_drought_sgs_rand$layer,day_25_drought_nmp_rand$layer,exact=F)
  D <- data.frame(test$statistic)
  
  d_list_25[[i]] <- D
  
}

d_df_25 <- do.call('rbind',d_list_25)
hist(d_df_25$test.statistic)
summary(d_df_25)
#calculate 99% confidence intervals

#all comparisons significant 



# variograms of % change in spring ppt -----


Ecoregion <- 'shortgrass_steppe'
source('seasonal_precip_temp_analysis.R')
head(spring_summer_precip_drought)
hist(spring_summer_precip_drought$change_in_perc_spring)

ks.test(spring_summer_precip_drought$change_in_perc_spring)

spring_precip <- spring_summer_precip_drought %>%
  select(x,y,change_in_perc_spring)

spring_precip_raster <- rasterFromXYZ(spring_precip)

point_data_spring_precip <- as(spring_precip_raster, 'SpatialPointsDataFrame')

TheVariogram_50_spring_precip = variogram(change_in_perc_spring ~1,
                                          data = point_data_spring_precip,width=0.1)
plot(TheVariogram_50_spring_precip)

TheVariogramModel_50_spring_precip <- vgm(psill=200, model="Exp", nugget=50, range=1)
plot(TheVariogram_50_spring_precip, model=TheVariogramModel_50_spring_precip) 
FittedModel_50_spring_precip <- fit.variogram(TheVariogram_50_spring_precip, model=TheVariogramModel_50_spring_precip)
FittedModel_50_spring_precip
#Range = 49.43 km

plot(TheVariogram_50_spring_precip, model=FittedModel_50_spring_precip,xlab='Distance (km)',
     ylab = 'Semivariance',cex=1,lwd=2,col='black')


# quadrature ------


get_average_growth_curve_absolute_spline_ci  <- function(i) {
  
  #subset to a given pixel
  growth_id <- gpp_df %>%
    dplyr::filter(id_value == 100)
  
  x <- unique(growth_id %>% pull(x))
  y <- unique(growth_id %>% pull(y))
  
  #first get temporal variation
  year_list <- list()
  for(j in unique(growth_id$year)){
    
    subset <- subset(growth_id,year==j)
    
    subset <- subset %>% arrange(doy)
    
    subset <-
      data.frame(subset, gpp_ci = cumsum(subset$gpp))
    
    year_list[[j]] <- subset
    
    #str(subset)
    
  }
  
  year_df <- do.call('rbind',year_list)
  #plot(gpp_2~doy,data=year_df)
  rm(year_list)
  
  growth_id_ci <- aggregate(gpp_ci ~ doy, sd, data = year_df)
  rm(year_df)
  
  gpp.doy.spl_ci <-
    with(growth_id_ci, smooth.spline(doy, gpp_ci))
  
  
  #now do average growth curve
  growth_id <- aggregate(gpp ~ doy, mean, data = growth_id)
  #plot(gpp~doy,growth_id)
  
  #for that pixel, get cumulative GPP throughout the year
  growth_id_cmulative <-
    data.frame(growth_id, gpp_2 = cumsum(growth_id$gpp))
  
  #create spline model of growth curve
  gpp.doy.spl <-
    with(growth_id_cmulative, smooth.spline(doy, gpp_2))
  
  
  return(list(gpp.doy.spl,gpp.doy.spl_ci))
  
  
}


cum_sum_quadrature <- function(x){
  
  median_val <- aggregate(gpp_ci ~ doy,median,data=year_df)
  colnames(median_val) <- c('doy','gpp_doy_median')
  
  merged <- merge(median_val,year_df,by=c('doy'))
  
  merged$diff <- merged$gpp_doy_median -  merged$gpp_ci
  
  (merged$gpp_doy_median)^2 +  merged$gpp_ci

  sd(c(1,1,1,1))

  

}

?sd
?var

  
#------


#import SGS
#mean
growth_curve_absolute_mean_sgs_1km <- 
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
head(growth_curve_absolute_mean_nmp,2)

#drought
growth_curve_drought_absolute_mean_nmp <- 
  read_csv('./../../Data/growth_curves/drought_growth_curve_northern_mixed_prairies.csv')
head(growth_curve_drought_absolute_mean_nmp,2)


#SGS growth curve figure
png(height = 1500,width=3000,res=300,'Figures/multi_panel_growth_curves')

par(mfrow=c(1,2),cex = 0.5,lwd = 0.5,oma=c(3.2,6,1,1),mar = c(3,1.25,3,3))
#?par
# plot it out panel A: sgs
plot(mean ~ doy, growth_curve_absolute_mean_sgs,col='black',type='l',
     ylab='',
     xlab='',las=1,cex.axis=1.5,ylim=c(0,391))
polygon(c(growth_curve_absolute_mean_sgs$doy,rev(growth_curve_absolute_mean_sgs$doy)),
        c(growth_curve_absolute_mean_sgs$temporal_ci_25,rev(growth_curve_absolute_mean_sgs$temporal_ci_75)),
        col = "grey", border = F)
lines(mean ~ doy, growth_curve_absolute_mean_sgs,col='white',pch=19,lwd=2)
polygon(c(growth_curve_absolute_mean_sgs$doy,rev(growth_curve_absolute_mean_sgs$doy)),
        c(growth_curve_absolute_mean_sgs$lower_spatial,rev(growth_curve_absolute_mean_sgs$upper_spatial)),
        col = "black", border = F)
polygon(c(growth_curve_drought_absolute_mean_sgs$doy,rev(growth_curve_drought_absolute_mean_sgs$doy)),
        c(growth_curve_drought_absolute_mean_sgs$lower,rev(growth_curve_drought_absolute_mean_sgs$upper)),
        col = "red", border = F)
lines(mean ~ doy, growth_curve_drought_absolute_mean_sgs,col='black',pch=19,lwd=1)
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



# seasonality function ------


seasonality_summary <- function(Ecoregion,climate,season){
  
  #import driest year
  driest_year <- 
    read.csv(paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/growing_season/drought_precip_year_',Ecoregion,'.csv'))
  driest_year <- driest_year %>%
    dplyr::select(x,y,year,ppt_min)
  #head(driest_year,1)
  
  
  climate = 'temperature'
  season = 'spring'
  Ecoregion = 'shortgrass_steppe'
  
  
  #import summer precip data
  filepath_summer_precip <-
    dir(
      paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/',climate,'/',season,'/'   
      ),
      full.names = T
    )
  
  if(climate=='precipitation'){
    
    import_function <- seasonal_precip_import_function
    
  }else{
    
    import_function <- seasonal_temp_import_function
    
  }
  
  test_seasonal <- lapply(filepath_summer_precip, import_function)
  test_seasonal <- list_to_df(test_seasonal)
  #head(test_seasonal,1)
  
  
  colnames(test_seasonal) <- c('x','y','climate','year')
  
  #get unique ID for each coordinate
  id_value_df <- aggregate(climate ~ x + y,mean,data = test_seasonal)
  id_value_df$id_value <- seq.int(nrow(id_value_df))
  id_value_df <- id_value_df %>%
    dplyr::select(x,y,id_value)
  test_seasonal <- merge(id_value_df,test_seasonal,by=c('x','y'))
  
  #merge summer precip with driest year
  test_seasonal_drought <- merge(test_seasonal,driest_year,by=c('x','y','year')) #will use this later for merging
  test_seasonal_drought_2 <- test_seasonal_drought %>%
    dplyr::select(x,y,year,id_value) %>%
    rename('dry_year' = 'year')
  
  id_list <- unique(test_seasonal_drought_2$id_value)
  
  #filter out the driest year from the summer ppt data
  seasonal_list <- list()
  for(i in id_list){
    
    dry_year_subset <- subset(test_seasonal_drought_2,id_value == i)
    dry_year = as.numeric(dry_year_subset$dry_year)
    
    seasonal_id <- subset(test_seasonal,id_value == i)
    
    seasonal_id <- seasonal_id %>%
      dplyr::filter(year != dry_year)
    
    seasonal_list[[i]] <- seasonal_id 
    
    #print(as.numeric(i)/as.numeric(length(id_list)))
    
  }
  
  seasonal_df <- do.call('rbind',seasonal_list)
  head(seasonal_df,1)
  
  #get mean and merge with drought year
  seasonal_mean <- aggregate(climate ~ x + y,mean,data = seasonal_df)
  seasonal_mean <- merge(seasonal_mean,test_seasonal_drought[c(1,2,5)],by = c('x','y'))
  
  colnames(seasonal_mean) <- c('x','y','mean','drought')
  # head(seasonal_mean,1)
  
  #percent decrease 
  seasonal_mean$perc_decrease <- 
    round(((seasonal_mean$drought - seasonal_mean$mean)/
             seasonal_mean$mean)*100,2)
  #hist(seasonal_mean$perc_decrease_seasonal)
  
  #absolute decrease 
  seasonal_mean$abs_decrease <- 
    seasonal_mean$drought - seasonal_mean$mean
  
  head(seasonal_mean,1)
  
  #make meaningful column names
  colnames(seasonal_mean) <-
    c('x','y',paste0(season,'_',climate,'_mean'),
      paste0(season,'_',climate,'_drought'),paste0('perc_decrease_',season,'_',climate),
      paste0('abs_decrease_',season,'_',climate))
  
  seasonal_mean$ecoregion <- Ecoregion
  
  return(seasonal_mean)
  
}

# gpp 50 functions -----


get_50_gpp_no_drought <- function(i) {
  
  #subset to a given pixel
  growth_id <- ppt_gpp %>%
    dplyr::filter(id_value == 100)
  
  #ID lat/lon up front
  x <- unique(growth_id %>% pull(x))
  y <- unique(growth_id %>% pull(y))
  
  ppt_sum <- aggregate(ppt ~ year,sum,data=growth_id)
  colnames(ppt_sum) <- c('year','ppt_sum')
  
  growth_id <- merge(growth_id,ppt_sum,by=c('year'))
  
  
  #get year with lowest precip
  min_ppt <- min(growth_id$ppt_sum) 
  
  #subset to years above this value
  growth_id  <- growth_id %>%
    filter(ppt_sum > min_ppt)
  
  growth_id <- aggregate(gpp ~ doy, median, data = growth_id)
  
  #for that pixel, get cumulative GPP throughout the year
  growth_id_cmulative <-
    data.frame(growth_id, gpp_2 = cumsum(growth_id$gpp))
  growth_id_cmulative$gpp_3 <-
    100 * (growth_id_cmulative$gpp_2 / max(growth_id_cmulative$gpp_2))
  
  rm(growth_id)
  
  #create spline model of growth curve
  gpp.doy.spl <-
    with(growth_id_cmulative, smooth.spline(doy, gpp_3))
  #lines(gpp.doy.spl, col = "blue")
  
  rm(growth_id_cmulative)
  
  #run model through a sequence of days
  doy <- data.frame(seq(from = 65, to = 297, by = 1))
  gpp_predicted <- data.frame(predict(gpp.doy.spl, doy))
  colnames(gpp_predicted) <- c('day', 'gpp_perc')
  
  #get day of year where roughly 50% of cumulative growth has occurred and turn into dataframe
  gpp_predicted_50 <- gpp_predicted %>%
    dplyr::filter(gpp_perc < 50.1)
  
  rm(gpp_predicted)
  
  doy_50 <-
    max(gpp_predicted_50$day) #this is a rough approximation
  
  doy_50_df <- data.frame(doy_50)
  colnames(doy_50_df) <- c('doy_50')
  doy_50_df$x <- x
  doy_50_df$y <- y
  
  doy_50_df <- doy_50_df[c(2, 3, 1)]
  
  return(doy_50_df)
  
  
  
}

 
# day 50 plot ------


#sgs
day_50_drought_sgs <-
  raster('./../../Data/CDD/day_of_50/day_50_drought_impact_shortgrass_steppe.tif')
# plot(day_50_drought_sgs)
# summary(day_50_drought_sgs)
#median = -22
#179-22

#nmp
day_50_drought_nmp <-
  raster('./../../Data/CDD/day_of_50/day_50_drought_impact_northern_mixed_prairies.tif')
summary(day_50_drought_nmp)
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



# variograms of day 50 -----
# updated NDVI spline ------


ndvi_spline_drought_2 <- function(i) {
  
  growth_id <- ppt_ndvi %>%
    dplyr::filter(id_value == 100)
  
  #ID lat/lon up front
  x <- unique(growth_id %>% pull(x))
  y <- unique(growth_id %>% pull(y))
  
  #get total precip
  ppt_sum <- aggregate(ppt ~ year,sum,data=growth_id)
  colnames(ppt_sum) <- c('year','ppt_sum')
  
  growth_id <- merge(growth_id,ppt_sum,by=c('year'))
  
  #get year with lowest precip
  min_ppt <- min(growth_id$ppt_sum) + 0.1
  
  #subset to years above this value
  growth_id  <- growth_id %>%
    filter(ppt_sum < min_ppt)
  
  #create spline model of growth curve
  ndvi_doy_drought_spl <- with(growth_id, smooth.spline(doy, ndvi))
  #plot(ndvi ~ doy, data = growth_id)
  #lines(ndvi_doy_drought_spl, col = "red")
  
  
  return(ndvi_doy_drought_spl)
  
  
}


# updated NDVI plot ------



#growth dynamics

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


# 1 km subset plot ------


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


# map o day of max sensitivity -----

#function to get total and maximum C uptake differences
get_max_total_reduction  <- function(i) {
  
  #subset to a given pixel
  growth_id <- ppt_gpp %>%
    dplyr::filter(id_value == i)
  
  #ID lat/lon up front
  x <- unique(growth_id %>% pull(x))
  y <- unique(growth_id %>% pull(y))
  
  ppt_sum <- aggregate(ppt ~ year,sum,data=growth_id)
  colnames(ppt_sum) <- c('year','ppt_sum')
  
  growth_id <- merge(growth_id,ppt_sum,by=c('year'))
  
  
  #get year with lowest precip
  min_ppt <- min(ppt_sum$ppt_sum) 
  
  #subset to years above this value
  growth_id_2  <- growth_id %>%
    dplyr::filter(ppt_sum > min_ppt)
  
  #subset to years above below value
  growth_id_drought  <- growth_id %>%
    dplyr::filter(ppt_sum == min_ppt)
  
  growth_id_drought <- growth_id_drought %>%
    arrange(doy)
  
  #average year cumulative
  
  #now do average growth curve
  growth_id_2 <- aggregate(gpp ~ doy, median, data = growth_id_2)
  #plot(gpp~doy,growth_id)
  
  #growth_id_drought <- aggregate(gpp ~ doy, median, data = growth_id_drought)
  
  #for that pixel, get cumulative GPP throughout the year
  growth_id_cmulative <-
    data.frame(growth_id, gpp_2 = cumsum(growth_id_2$gpp))
  
  #create spline model of average cumulative growth curve
  gpp_doy_spl_cumulative <-
    with(growth_id_cmulative, smooth.spline(doy, gpp_2))
  
  #create a spline model for growth dynamics
  gpp_doy_spl <-
    with(growth_id_2, smooth.spline(doy, gpp))
  
  #now do same for drought year
  growth_id_cmulative_drought <-
    data.frame(growth_id_drought, gpp_2 = cumsum(growth_id_drought$gpp))
  
  gpp_doy_spl_cumulative_drought <-
    with(growth_id_cmulative_drought, smooth.spline(doy, gpp_2))
  
  gpp_doy_spl_drought <-
    with(growth_id_drought, smooth.spline(doy, gpp))
  

  #get difference between drought and normal years for total GPP/C uptake
  doy_df_cumulative_difference <- data.frame(predict(gpp_doy_spl_cumulative,297)$y - 
                                               predict(gpp_doy_spl_cumulative_drought,297)$y)
  colnames(doy_df_cumulative_difference) <- 'reduction'
  doy_df_cumulative_difference$doy <- 297
  doy_df_cumulative_difference$type <- 'total'
  doy_df_cumulative_difference <- doy_df_cumulative_difference %>%
    dplyr::select(doy,reduction,type)
  
  #get maximum reduction
  doy_df_max_diff_average <- data.frame(predict(gpp_doy_spl,seq(73,297,by=1)))
  colnames(doy_df_max_diff_average) <- c('doy','average_gpp')
  doy_df_max_diff_drought <- data.frame(predict(gpp_doy_spl_drought,seq(73,297,by=1)))
  colnames(doy_df_max_diff_drought) <- c('doy','drought_gpp')
  doy_df_max_diff_average_drought <- merge(doy_df_max_diff_average,doy_df_max_diff_drought,
                                           by = 'doy')
  doy_df_max_diff_average_drought$reduction <- 
    doy_df_max_diff_average_drought$drought_gpp - doy_df_max_diff_average_drought$average_gpp
  
  max <- max(doy_df_max_diff_average_drought$reduction)
  min <- min(doy_df_max_diff_average_drought$reduction)
  
  min_reduction <- doy_df_max_diff_average_drought %>%
    select(doy,reduction) %>%
    dplyr::filter(reduction==max) 
  min_reduction$type <- 'min'
  
  max_reduction <- doy_df_max_diff_average_drought %>%
    select(doy,reduction) %>%
    dplyr::filter(reduction==min) 
  max_reduction$type <- 'max'
  
  max_min_reduction <- rbind(max_reduction,min_reduction)
  max_min_total_reduction <- rbind(max_min_reduction, doy_df_cumulative_difference)
  max_min_total_reduction$x <- x
  max_min_total_reduction$y <- y
  
  max_min_total_reduction <- max_min_total_reduction %>%
    dplyr::select(x,y,doy,type,reduction)
  
  #drought year cumulative
  
  
  #create dataframe of cumulative C uptake by day for aerage year
  
  
  return(max_min_total_reduction)
  
  
}


# % total reductions -----

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

#peak reduction figure (%) ------


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
  #geom_vline(xintercept = 0,color='black') +
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

# % total reduction absolute -------


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



# peak reduction absolute ------


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



# correlate VPD with NDVI change (in progress) -------


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

ggplot(vpd_gpp_df,aes(abs_change,reduction)) +
  geom_point(pch=1,size=3)+
  stat_smooth(method='lm',color='black',size=0.5) +
  facet_wrap(~ecoregion_2,ncol=1,labeller = eco_names,scales='free') +
  xlab('Increase in maximum vapor pressure deficit (kPa)') +
  ylab('Maximum reduction in NDVI') +
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

