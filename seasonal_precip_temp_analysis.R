
# wrangle seasonal climate data to get % changes in spring and summer
# temperature and precipitation for drought years

#temperature and precipitation -----

#import driest year
driest_year <- 
  read.csv(paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/growing_season/drought_precip_year_',Ecoregion,'.csv'))

#head(driest_year)

#import summer precip data
filepath_summer_precip <-
  dir(
    paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/summer/'    
    ),
    full.names = T
  )
test_summer_ppt <- lapply(filepath_summer_precip, summer_precip_function)
test_summer_ppt <- list_to_df(test_summer_ppt)
test_summer_ppt_mean <- aggregate(summer_precip~x+y,mean,data=test_summer_ppt)

#merge summer precip with driest year
test_summer_ppt_drought <- merge(test_summer_ppt,driest_year,by=c('x','y','year'))
colnames(test_summer_ppt_drought) <- c('x','y','year','summer_precip_drought','annual_precip')
head(test_summer_ppt_drought)

#merge with mean summer precip
test_summer_ppt_drought <- merge(test_summer_ppt_drought,test_summer_ppt_mean,
                                 by=c('x','y'))

head(test_summer_ppt_drought)

#percent decrease 
test_summer_ppt_drought$perc_decrease <- 
  round(((test_summer_ppt_drought$summer_precip_drought - test_summer_ppt_drought$summer_precip)/
           test_summer_ppt_drought$summer_precip)*100,2)

#absolute decrease 
test_summer_ppt_drought$abs_decrease <- 
  test_summer_ppt_drought$summer_precip_drought - test_summer_ppt_drought$summer_precip
           
head(test_summer_ppt_drought)

rm(test_summer_ppt,test_summer_ppt_mean)
# hist(test_summer_ppt_drought$perc_decrease)
# summary(test_summer_ppt_drought)


#import spring pecip data
filepath_spring_precip <-
  dir(
    paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/spring/'    
    ),
    full.names = T
  )
test_spring_ppt <- lapply(filepath_spring_precip, spring_precip_function)
test_spring_ppt <- list_to_df(test_spring_ppt)
test_spring_ppt_mean <- aggregate(spring_precip~x+y,mean,data=test_spring_ppt)

#merge spring precip with driest year
test_spring_ppt_drought <- merge(test_spring_ppt,driest_year,by=c('x','y','year'))
colnames(test_spring_ppt_drought) <- c('x','y','year','spring_precip_drought','annual_precip')
#head(test_spring_ppt_drought)

#merge with mean spring precip
test_spring_ppt_drought <- merge(test_spring_ppt_drought,test_spring_ppt_mean,
                                 by=c('x','y'))

#head(test_spring_ppt_drought)

#percent decrease 
test_spring_ppt_drought$perc_decrease <- 
  round(((test_spring_ppt_drought$spring_precip_drought - test_spring_ppt_drought$spring_precip)/
           test_spring_ppt_drought$spring_precip)*100,2)

#absolute decrease 
test_spring_ppt_drought$abs_decrease <- 
  test_spring_ppt_drought$spring_precip_drought - test_spring_ppt_drought$spring_precip


rm(test_spring_ppt,test_sring_ppt_mean)

# head(test_spring_ppt_drought)
# summary(test_spring_ppt_drought)
# hist(test_spring_ppt_drought)
#hist(test_summer_ppt_drought$perc_decrease,add=T,col='blue')


#Temperature


#import summer temp data
filepath_summer_temp <-
  dir(
    paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Temperature/summer/'    
    ),
    full.names = T
  )
test_summer_temp <- lapply(filepath_summer_temp, summer_temp_function)
test_summer_temp <- list_to_df(test_summer_temp)
test_summer_temp_mean <- aggregate(summer_temp~x+y,mean,data=test_summer_temp)

#merge summer temp with driest year
test_summer_temp_drought <- merge(test_summer_temp,driest_year,by=c('x','y','year'))
colnames(test_summer_temp_drought) <- c('x','y','year','summer_temp_drought','annual_precip')
head(test_summer_temp_drought)

#merge with mean summer temp
test_summer_temp_drought <- merge(test_summer_temp_drought,test_summer_temp_mean,
                                 by=c('x','y'))

head(test_summer_temp_drought)

#percent change
test_summer_temp_drought$perc_change <- 
  round(((test_summer_temp_drought$summer_temp_drought - test_summer_temp_drought$summer_temp)/
           test_summer_temp_drought$summer_temp)*100,2)


#absolute change
test_summer_temp_drought$abs_change <- 
  test_summer_temp_drought$summer_temp_drought - test_summer_temp_drought$summer_temp
summary(test_summer_temp_drought)
#hist(test_summer_temp_drought$abs_change,col='red')

rm(test_summer_temp,test_summer_temp_mean)
# hist(test_summer_temp_drought$perc_change)
# summary(test_summer_temp_drought$perc_change)

#import spring pecip data
filepath_spring_temp <-
  dir(
    paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Temperature/spring/'    
    ),
    full.names = T
  )
test_spring_temp <- lapply(filepath_spring_temp, spring_temp_function)
test_spring_temp <- list_to_df(test_spring_temp)
test_spring_temp_mean <- aggregate(spring_temp~x+y,mean,data=test_spring_temp)

#merge spring temp with driest year
test_spring_temp_drought <- merge(test_spring_temp,driest_year,by=c('x','y','year'))
colnames(test_spring_temp_drought) <- c('x','y','year','spring_temp_drought','annual_precip')
#head(test_spring_temp_drought)

#merge with mean spring temp
test_spring_temp_drought <- merge(test_spring_temp_drought,test_spring_temp_mean,
                                 by=c('x','y'))

#head(test_spring_temp_drought)

#percent change
test_spring_temp_drought$perc_change <- 
  round(((test_spring_temp_drought$spring_temp_drought - test_spring_temp_drought$spring_temp)/
           test_spring_temp_drought$spring_temp)*100,2)

#absolute change
test_spring_temp_drought$abs_change <- 
  test_spring_temp_drought$spring_temp_drought - test_spring_temp_drought$spring_temp
summary(test_spring_temp_drought)
hist(test_spring_temp_drought$abs_change)

rm(test_spring_temp,test_spring_temp_mean)

# head(test_spring_temp_drought)
# summary(test_spring_temp_drought)
# hist(test_spring_temp_drought$perc_change)
#hist(test_summer_temp_drought$perc_change,add=T,col='blue')


#VPD -------

Ecoregion <- 'shortgrass_steppe'

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

vpd_spring <- aggregate(vpd_max ~ x + y + year,mean,data=vpd_spring)

#unique(vpd_spring$month)

vpd_summer <- vpd_2003_2020 %>%
  filter(month > 5)

vpd_summer <- aggregate(vpd_max ~ x + y + year,mean,data=vpd_summer)

#unique(vpd_summer$month)

#import driest years
driest_year <- 
  read.csv(paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/growing_season/drought_precip_year_',Ecoregion,'.csv'))
head(driest_year)

driest_year_raster <- rasterFromXYZ(driest_year[c(1,2,4)])
crs(driest_year_raster) <- "+proj=longlat +datum=WGS84"

years <- as.factor(seq(2003,2020,1))

#Summer VPD
vpd_summer_list <- list()

for(i in years){
  
  vpd_summer_2 <- subset(vpd_summer,year==i)  
  
  
  vpd_summer_2 <- vpd_summer_2 %>%
    select(x,y,vpd_max)
  
  vpd_summer_2 <- rasterFromXYZ(vpd_summer_2,digits=1)
  vpd_summer_2 <- resample(vpd_summer_2,driest_year_raster)
  driest_year_raster_resampled <- resample(driest_year_raster,vpd_summer_2)
  vpd_summer_2 <- data.frame(rasterToPoints(vpd_summer_2))
  vpd_summer_2$year <- i
  
  vpd_summer_list[[i]] <- vpd_summer_2
  
}

vpd_summer_list[10]

plot(driest_year_raster_resampled)
vpd_summer_df <- list_to_df(vpd_summer_list)
head(vpd_summer_df)

vpd_summer_df_mean <- aggregate(vpd_max ~ x + y,mean,data=vpd_summer_df)

#driest year resampled back to df
driest_year_raster_resampled_df <- 
  data.frame(rasterToPoints(driest_year_raster_resampled))

vpd_summer_drought <- 
  merge(driest_year_raster_resampled_df,vpd_summer_df,by=c('x','y','year'))
colnames(vpd_summer_drought) <- c('x','y','year','vpd_max_drought')

vpd_summer_drought <- merge(vpd_summer_drought,vpd_summer_df_mean,
                            by=c('x','y'))
head(vpd_summer_drought)

#get % change
vpd_summer_drought$perc_change <- round(((vpd_summer_drought$vpd_max_drought - 
                                            vpd_summer_drought$vpd_max)/vpd_summer_drought$vpd_max),2)*100

summary(vpd_summer_drought$perc_change)

#get abs change
vpd_summer_drought$abs_change <- vpd_summer_drought$vpd_max_drought - 
  vpd_summer_drought$vpd_max

summary(vpd_summer_drought)

#spring VPD loop
vpd_spring_list <- list()

for(i in years){
  
  vpd_spring_2 <- subset(vpd_spring,year==i)  
  
  
  vpd_spring_2 <- vpd_spring_2 %>%
    select(x,y,vpd_max)
  
  vpd_spring_2 <- rasterFromXYZ(vpd_spring_2,digits=1)
  vpd_spring_2 <- resample(vpd_spring_2,driest_year_raster)
  driest_year_raster_resampled <- resample(driest_year_raster,vpd_spring_2)
  vpd_spring_2 <- data.frame(rasterToPoints(vpd_spring_2))
  vpd_spring_2$year <- i
  
  vpd_spring_list[[i]] <- vpd_spring_2
  
}



plot(driest_year_raster_resampled)
vpd_spring_df <- list_to_df(vpd_spring_list)
head(vpd_spring_df)

vpd_spring_df_mean <- aggregate(vpd_max ~ x + y,mean,data=vpd_spring_df)

#driest year resampled back to df
driest_year_raster_resampled_df <- 
  data.frame(rasterToPoints(driest_year_raster_resampled))

vpd_spring_drought <- 
  merge(driest_year_raster_resampled_df,vpd_spring_df,by=c('x','y','year'))
colnames(vpd_spring_drought) <- c('x','y','year','vpd_max_drought')

vpd_spring_drought <- merge(vpd_spring_drought,vpd_spring_df_mean,
                            by=c('x','y'))
head(vpd_spring_drought)

#get % change
vpd_spring_drought$perc_change <- round(((vpd_spring_drought$vpd_max_drought - 
                                            vpd_spring_drought$vpd_max)/vpd_spring_drought$vpd_max),2)*100

#get absolute change
vpd_spring_drought$abs_change <- vpd_spring_drought$vpd_max_drought - 
  vpd_spring_drought$vpd_max

summary(vpd_spring_drought)

#compare differences in spring versus summer
test_dff <- vpd_summer_drought$perc_change - vpd_spring_drought$perc_change
hist(test_dff)
summary(test_dff)

abs_diff <- vpd_summer_drought$abs_change - vpd_spring_drought$abs_change
hist(abs_diff)

hist(vpd_summer_drought$abs_change)
hist(vpd_spring_drought$abs_change,col='red',add=T)

#combine datasets
vpd_summer_drought$season <- 'summer'
vpd_spring_drought$season <- 'spring'
spring_summer_vpd <- rbind(vpd_summer_drought,vpd_spring_drought)
spring_summer_vpd$ecoregion <- Ecoregion

#save to file
write.csv(spring_summer_vpd,paste0(
  './../../Data/Climate/Ecoregion/',
  Ecoregion,
  '/PRISM/VPD_change.csv'
)) 

