
# wrangle seasonal climate data to get % changes in spring and summer
# temperature and precipitation for drought years

#note this script compares the extreme driest year to all other years 
#excluding the extreme drought year


# spring and summer precipitation -----

#spring
spring_ppt <- seasonality_summary(Ecoregion = Ecoregion,
                                     climate = 'precipitation',
                                     season = 'spring')


#summer
summer_ppt <- seasonality_summary(Ecoregion = Ecoregion,
                                                       climate = 'precipitation',
                                                       season = 'summer')

#combine
spring_summer_ppt <- merge(spring_ppt,summer_ppt,by = c('x','y','ecoregion'))

rm(spring_ppt,summer_ppt)

spring_summer_ppt$mean_total <- spring_summer_ppt$spring_precipitation_mean + 
  spring_summer_ppt$summer_precipitation_mean

spring_summer_ppt$drought_total <- spring_summer_ppt$summer_precipitation_drought + 
  spring_summer_ppt$spring_precipitation_drought


spring_summer_ppt$spring_prop_average <- spring_summer_ppt$spring_precipitation_mean/spring_summer_ppt$mean_total
#hist(spring_summer_ppt$spring_prop_average)

spring_summer_ppt$spring_prop_drought <- spring_summer_ppt$spring_precipitation_drought/spring_summer_ppt$drought_total
#hist(spring_summer_ppt$spring_prop_drought,col='red',add=T)

spring_summer_ppt$change_spring_prop <- (spring_summer_ppt$spring_prop_drought -
  spring_summer_ppt$spring_prop_average)*100
#hist(spring_summer_ppt$change_spring_prop)

#export
write.csv(spring_summer_ppt,
          paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/seasonal_change_PPT.csv'))

rm(spring_summer_ppt)

#-------------------------------------------------------------------------------
# spring and summer temperature -----

#spring
spring_temp <- seasonality_summary(Ecoregion = Ecoregion,
                                  climate = 'temperature',
                                  season = 'spring')


#summer
summer_temp <- seasonality_summary(Ecoregion = Ecoregion,
                                  climate = 'temperature',
                                  season = 'summer')

#combine
spring_summer_temp <- merge(spring_temp,summer_temp,by = c('x','y','ecoregion'))

rm(spring_temp,summer_temp)


#export
write.csv(spring_summer_temp,
          paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/temperature/seasonal_change_temperature.csv'))

rm(spring_summer_ppt)

#-------------------------------------------------------------------------------
#VPD -------

#repetitive code among the two seasons. Could be turned into a function. Only
#ru thisn once.

#Run this once

vpd_2003_2017 <-
  read.csv(paste0(
    './../../Data/Climate/Ecoregion/',
    Ecoregion,
    '/PRISM/PRISM_vpdmax_stable_4km_200303_201708.csv'
  ))

vpd_2018_2020 <-
  read.csv(paste0(
    './../../Data/Climate/Ecoregion/',
    Ecoregion,
    '/PRISM/PRISM_vpdmax_stable_4km_201803_202008.csv'
  ))

vpd_2003_2020 <- rbind(vpd_2018_2020,vpd_2003_2017)
head(vpd_2003_2020)
rm(vpd_2003_2017,vpd_2018_2020)

vpd_2003_2020$year <- as.numeric(substr(vpd_2003_2020$Date,1,4))
vpd_2003_2020$month <- as.numeric(substr(vpd_2003_2020$Date,6,7))

str(vpd_2003_2020)

vpd_2003_2020 <- vpd_2003_2020 %>%
  select(Longitude,Latitude,vpdmax..hPa.,year,month,Name) %>%
  filter(month > 2) %>%
  filter(month < 9) %>%
  rename(x = Longitude,
         y = Latitude,
         vpd_max = vpdmax..hPa.)

head(vpd_2003_2020)

#filter by spring
vpd_spring <- vpd_2003_2020 %>%
  filter(month < 6)
vpd_spring$vpd_max <- vpd_spring$vpd_max*0.1

vpd_spring <- aggregate(vpd_max ~ x + y + year,mean,data=vpd_spring)

#unique(vpd_spring$month)

vpd_summer <- vpd_2003_2020 %>%
  filter(month > 5)
vpd_summer$vpd_max <- vpd_summer$vpd_max*0.1

vpd_summer <- aggregate(vpd_max ~ x + y + year,mean,data=vpd_summer)

#unique(vpd_summer$month)

#import driest years
driest_year <-
  read.csv(paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/growing_season/drought_precip_year_',Ecoregion,'.csv'))
driest_year <- driest_year %>%
  dplyr::select(x,y,year,ppt_min)
head(driest_year)

driest_year_raster <- rasterFromXYZ(driest_year[c(1,2,3)])
crs(driest_year_raster) <- "+proj=longlat +datum=WGS84"

years <- as.factor(seq(2003,2020,1))
#plot(driest_year_raster)

#Summer VPD
vpd_summer_list <- list()

for(i in years){

  vpd_summer_2 <- subset(vpd_summer,year == i)

  vpd_summer_2 <- vpd_summer_2 %>%
    dplyr::select(x,y,vpd_max)

  vpd_summer_2 <- rasterFromXYZ(vpd_summer_2,digits=1)
  crs(vpd_summer_2) <- "+proj=longlat +datum=WGS84"
  #vpd_summer_2 <- resample(driest_year_raster,vpd_summer_2)
  driest_year_raster_resampled <- resample(driest_year_raster,vpd_summer_2)
  vpd_summer_2 <- data.frame(rasterToPoints(vpd_summer_2))
  vpd_summer_2$year <- i

  vpd_summer_list[[i]] <- vpd_summer_2

}

#vpd_summer_list[10]

#vpd df
vpd_summer_df <- list_to_df(vpd_summer_list)
rm(vpd_summer_list)
head(vpd_summer_df)

#driest year resampled back to df (the raster remains the same in each iteration)
driest_year_raster_resampled_df <-
  data.frame(rasterToPoints(driest_year_raster_resampled))

#get unique coordinate ID and rename 'year' column
driest_year_raster_resampled_df$id_value <- seq.int(nrow(driest_year_raster_resampled_df))
driest_year_raster_resampled_df <- driest_year_raster_resampled_df %>%
  dplyr::rename('dry_year' = 'year')

#merge with vpd data
vpd_summer_df  <- merge(driest_year_raster_resampled_df,vpd_summer_df,by = c('x','y'))

#filter out driest year for calculating mean
id_list <- unique(vpd_summer_df$id_value)

#filter out the driest year from the summer ppt data
summer_vpd_list <- list()
summer_vpd_list_drought <- list()
for(i in id_list){
  
  dry_year_subset <- subset(vpd_summer_df,id_value == i)
  dry_year = as.numeric(unique(dry_year_subset$dry_year))
  
  #summer_id <- subset(test_seasonal,id_value==i)
  
  #subset out the dry year for calculating average
  dry_year_subset_average <- dry_year_subset %>%
    dplyr::filter(year != dry_year)
  
  summer_vpd_list[[i]] <- dry_year_subset_average
  
  #subset to the dry year for calculating driest year
  dry_year_subset_drought <- dry_year_subset %>%
    dplyr::filter(year == dry_year)
  
  summer_vpd_list_drought[[i]] <- dry_year_subset_drought
  

}

#turn to dataframes 
vpd_summer_df_average <- list_to_df(summer_vpd_list)
rm(summer_vpd_list)
head(vpd_summer_df_average,1)

vpd_summer_df_drought <- list_to_df(summer_vpd_list_drought)
rm(summer_vpd_list_drought)
vpd_summer_df_drought <- vpd_summer_df_drought %>%
  dplyr::select(x,y,vpd_max) %>%
  dplyr::rename('vpd_max_drought' = 'vpd_max')
head(vpd_summer_df_drought,1)

#get average
vpd_summer_df_average <- aggregate(vpd_max ~ x + y,mean,data=vpd_summer_df_average)

#merge the two
vpd_summer_drought <- merge(vpd_summer_df_average,vpd_summer_df_drought,
                            by=c('x','y'))
head(vpd_summer_drought)
rm(vpd_summer_df_average,vpd_summer_df_drought)

#get % change
vpd_summer_drought$perc_change <- round(((vpd_summer_drought$vpd_max_drought -
                                            vpd_summer_drought$vpd_max)/vpd_summer_drought$vpd_max),2)*100

#hist(vpd_summer_drought$perc_change)

vpd_summer_drought$season <- 'summer'

#get abs change
vpd_summer_drought$abs_change <- vpd_summer_drought$vpd_max_drought -
  vpd_summer_drought$vpd_max

hist(vpd_summer_drought$abs_change)

#
#

#Spring VPD
vpd_spring_list <- list()

for(i in years){
  
  vpd_spring_2 <- subset(vpd_spring,year == i)
  
  vpd_spring_2 <- vpd_spring_2 %>%
    dplyr::select(x,y,vpd_max)
  
  vpd_spring_2 <- rasterFromXYZ(vpd_spring_2,digits=1)
  crs(vpd_spring_2) <- "+proj=longlat +datum=WGS84"
  #vpd_spring_2 <- resample(driest_year_raster,vpd_spring_2)
  driest_year_raster_resampled <- resample(driest_year_raster,vpd_spring_2)
  vpd_spring_2 <- data.frame(rasterToPoints(vpd_spring_2))
  vpd_spring_2$year <- i
  
  vpd_spring_list[[i]] <- vpd_spring_2
  
}

#vpd_spring_list[10]

#vpd df
vpd_spring_df <- list_to_df(vpd_spring_list)
rm(vpd_spring_list)
head(vpd_spring_df)

#driest year resampled back to df (the raster remains the same in each iteration)
driest_year_raster_resampled_df <-
  data.frame(rasterToPoints(driest_year_raster_resampled))

#get unique coordinate ID and rename 'year' column
driest_year_raster_resampled_df$id_value <- seq.int(nrow(driest_year_raster_resampled_df))
driest_year_raster_resampled_df <- driest_year_raster_resampled_df %>%
  dplyr::rename('dry_year' = 'year')

#merge with vpd data
vpd_spring_df  <- merge(driest_year_raster_resampled_df,vpd_spring_df,by = c('x','y'))

#filter out driest year for calculating mean
id_list <- unique(vpd_spring_df$id_value)

#filter out the driest year from the spring ppt data
spring_vpd_list <- list()
spring_vpd_list_drought <- list()
for(i in id_list){
  
  dry_year_subset <- subset(vpd_spring_df,id_value == i)
  dry_year = as.numeric(unique(dry_year_subset$dry_year))
  
  #spring_id <- subset(test_seasonal,id_value==i)
  
  #subset out the dry year for calculating average
  dry_year_subset_average <- dry_year_subset %>%
    dplyr::filter(year != dry_year)
  
  spring_vpd_list[[i]] <- dry_year_subset_average
  
  #subset to the dry year for calculating driest year
  dry_year_subset_drought <- dry_year_subset %>%
    dplyr::filter(year == dry_year)
  
  spring_vpd_list_drought[[i]] <- dry_year_subset_drought
  
  
}

#turn to dataframes 
vpd_spring_df_average <- list_to_df(spring_vpd_list)
rm(spring_vpd_list)
head(vpd_spring_df_average,1)

vpd_spring_df_drought <- list_to_df(spring_vpd_list_drought)
rm(spring_vpd_list_drought)
vpd_spring_df_drought <- vpd_spring_df_drought %>%
  dplyr::select(x,y,vpd_max) %>%
  dplyr::rename('vpd_max_drought' = 'vpd_max')
head(vpd_spring_df_drought,1)

#get average
vpd_spring_df_average <- aggregate(vpd_max ~ x + y,mean,data=vpd_spring_df_average)

#merge the two
vpd_spring_drought <- merge(vpd_spring_df_average,vpd_spring_df_drought,
                            by=c('x','y'))
head(vpd_spring_drought)
rm(vpd_spring_df_average,vpd_spring_df_drought)

#get % change
vpd_spring_drought$perc_change <- round(((vpd_spring_drought$vpd_max_drought -
                                            vpd_spring_drought$vpd_max)/vpd_spring_drought$vpd_max),2)*100

vpd_spring_drought$season <- 'spring'

#get abs change
vpd_spring_drought$abs_change <- vpd_spring_drought$vpd_max_drought -
  vpd_spring_drought$vpd_max

hist(vpd_spring_drought$abs_change,col='red',add=T)

#
#


#combine
vpd_spring_summer_drought <- rbind(vpd_spring_drought,vpd_summer_drought)
vpd_spring_summer_drought$Ecoregion <- Ecoregion


#save to file
write.csv(vpd_spring_summer_drought,paste0(
  './../../Data/Climate/Ecoregion/',
  Ecoregion,
  '/PRISM/VPD_change.csv'
))


#cleanup
rm(vpd_spring_drought,vpd_summer_drought,vpd_spring,vpd_spring_2,
   vpd_spring_df,vpd_spring_drought,driest_year,driest_year_raster,
   driest_year_raster_resampled,driest_year_raster_resampled_df,
   dry_year_subset,dry_year_subset_average,dry_year_subset_drought,
   vpd_2003_2020,vpd_summer,vpd_summer_2,
   vpd_summer_df,vpd_summer_mean,vpd_summer_df_mean,vpd_spring_summer_drought)



