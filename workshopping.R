

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
library(rgdal)

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


# growth curev with temporal 95 CI -----


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
