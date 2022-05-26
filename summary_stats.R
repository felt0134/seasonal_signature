
# key summary statistics


# Overview -----

# range of % reduction in growing season precipitation during drought across sites (from mean)
Ecoregion <- 'shortgrass_steppe'
driest_year_sgs <- 
  read.csv(paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/growing_season/drought_precip_year_',Ecoregion,'.csv'))

#basic math
# length(seq(2003:2020)) #18 years
# nrow(driest_year_sgs) #11315 sites
# 18*11315 site-year combinations

#plot(rasterFromXYZ(driest_year_sgs))

#import annual ppt, merge, and get % reduction during drought years
mean_precip_sgs <- 
  raster(paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/growing_season/mean_precip_',Ecoregion,'.tif'))
mean_precip_sgs <- data.frame(rasterToPoints(mean_precip_sgs))
drought_reduction_sgs <- merge(driest_year_sgs,mean_precip_sgs,by=c('x','y'))
head(drought_reduction_sgs,1)

#relative
drought_reduction_sgs$perc_reduction <- 
  (drought_reduction_sgs$ppt - drought_reduction_sgs$mean_precip_shortgrass_steppe)/
  drought_reduction_sgs$mean_precip_shortgrass_steppe

quantile(drought_reduction_sgs$perc_reduction,c(.01,0.5,0.99))

#absolute
drought_reduction_sgs$abs_reduction <- 
  (drought_reduction_sgs$ppt - drought_reduction_sgs$mean_precip_shortgrass_steppe)

quantile(drought_reduction_sgs$abs_reduction,c(.01,0.5,0.99))


Ecoregion <- 'northern_mixed_prairies'
driest_year_nmp <- 
  read.csv(paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/growing_season/drought_precip_year_',Ecoregion,'.csv'))

#basic math
# length(seq(2003:2020)) #18 years
# nrow(driest_year_nmp) #11315 sites
#29348*18 - # site-year combos

#import annual ppt, merge, and get % reduction during drought years
mean_precip_nmp <- 
  raster(paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/growing_season/mean_precip_',Ecoregion,'.tif'))
mean_precip_nmp <- data.frame(rasterToPoints(mean_precip_nmp))
drought_reduction_nmp <- merge(driest_year_nmp,mean_precip_nmp,by=c('x','y'))
head(drought_reduction_nmp,1)

#relative
drought_reduction_nmp$perc_reduction <- 
  (drought_reduction_nmp$ppt - drought_reduction_nmp$mean_precip_northern_mixed_prairies)/
  drought_reduction_nmp$mean_precip_northern_mixed_prairies

quantile(drought_reduction_nmp$perc_reduction,c(.01,0.5,0.99))

#absolute
drought_reduction_nmp$abs_reduction <- 
  (drought_reduction_nmp$ppt - drought_reduction_nmp$mean_precip_northern_mixed_prairies)

quantile(drought_reduction_nmp$abs_reduction,c(.01,0.5,0.99))


# % change in temperature during years of drought #

#import temperature data

Ecoregion <- 'shortgrass_steppe'
filepath_temp <-
  dir(
    paste0(
      './../../Data/Climate/Ecoregion/',
      Ecoregion,
      '/Temperature/tmean/'
  ), full.names = T)
temp_data_sgs <- lapply(filepath_temp[c(5:22)], format_temp_df) #get 2003 through 2020
temp_data_sgs <- data.frame(do.call('rbind', temp_data_sgs))
head(temp_data_sgs,1)

#merge with driest year and trim columns
temp_drought_sgs <- merge(temp_data_sgs,driest_year_sgs,by=c('x','y','year'))
temp_drought_sgs <- temp_drought_sgs %>%
  select(x,y,average_temp)

#get average temp, merge with driest year temp, and get % change during drought
average_temp_sgs <- aggregate(average_temp ~ x + y,mean,data=temp_data_sgs)
colnames(average_temp_sgs) <- c('x','y','long_term_temp')
temp_drought_sgs <- merge(average_temp_sgs,temp_drought_sgs,by=c('x','y'))

#relative change
temp_drought_sgs$perc_change <- 
  ((temp_drought_sgs$average_temp - temp_drought_sgs$long_term_temp)/temp_drought_sgs$long_term_temp)*100

quantile(temp_drought_sgs$perc_change,c(0.01,0.5,0.99))

#absolute change
temp_drought_sgs$abs_change <- 
  (temp_drought_sgs$average_temp - temp_drought_sgs$long_term_temp)

quantile(temp_drought_sgs$abs_change,c(0.01,0.5,0.99))

#

Ecoregion <- 'northern_mixed_prairies'
filepath_temp <-
  dir(
    paste0(
      './../../Data/Climate/Ecoregion/',
      Ecoregion,
      '/Temperature/tmean/'
    ), full.names = T)
temp_data_nmp <- lapply(filepath_temp[c(14:31)], format_temp_df) #get 2003 through 2020
temp_data_nmp <- data.frame(do.call('rbind', temp_data_nmp))
head(temp_data_nmp,1)

#merge with driest year and trim columns
temp_drought_nmp <- merge(temp_data_nmp,driest_year_nmp,by=c('x','y','year'))
temp_drought_nmp <- temp_drought_nmp %>%
  select(x,y,average_temp)

#get average temp, merge with driest year temp, and get % change during drought
average_temp_nmp <- aggregate(average_temp ~ x + y,mean,data=temp_data_nmp)
colnames(average_temp_nmp) <- c('x','y','long_term_temp')
temp_drought_nmp <- merge(average_temp_nmp,temp_drought_nmp,by=c('x','y'))

#relative change
temp_drought_nmp$perc_change <- 
  ((temp_drought_nmp$average_temp - temp_drought_nmp$long_term_temp)/temp_drought_nmp$long_term_temp)*100

quantile(temp_drought_nmp$perc_change,c(0.01,0.5,0.99))

#absolute change
temp_drought_nmp$abs_change <- 
  (temp_drought_nmp$average_temp - temp_drought_nmp$long_term_temp)

quantile(temp_drought_nmp$abs_change,c(0.01,0.5,0.99))

# 3 most common drought years for each ecoregion (% and number of pixels)

Ecoregion <- 'shortgrass_steppe'
driest_year_sgs <- 
  read.csv(paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/growing_season/drought_precip_year_',Ecoregion,'.csv'))

driest_sgs <- aggregate(ppt~year,length,data=driest_year_sgs)
#2011, 2012, and 2020
driest_sgs$perc <- driest_sgs$ppt/nrow(driest_year_sgs)*100

Ecoregion <- 'northern_mixed_prairies'
driest_year_nmp <- 
  read.csv(paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/growing_season/drought_precip_year_',Ecoregion,'.csv'))

driest_nmp <- aggregate(ppt~year,length,data=driest_year_nmp)
#2012, 2017, and 2020
driest_nmp$perc <- driest_nmp$ppt/nrow(driest_year_nmp)*100


#-------------------------------------------------------------------------------
# Growth curves -----

#estimated end of season % and absolute reduction in cumulative GPP

Ecoregion <- 'shortgrass_steppe'
drought_growth_curve_sgs <- read.csv(paste0('./../../Data/growth_curves/drought_growth_curve_',Ecoregion,'.csv'))
growth_curve_sgs <- read.csv(paste0('./../../Data/growth_curves/average_growth_curve_',Ecoregion,'.csv'))

#absolute
max(growth_curve_sgs$mean) - max(drought_growth_curve_sgs$mean)
#143.6

#relative
(max(drought_growth_curve_sgs$mean)- max(growth_curve_sgs$mean))/max(growth_curve_sgs$mean)
#40.3%


Ecoregion <- 'northern_mixed_prairies'
drought_growth_curve_nmp <- read.csv(paste0('./../../Data/growth_curves/drought_growth_curve_',Ecoregion,'.csv'))
growth_curve_nmp <- read.csv(paste0('./../../Data/growth_curves/average_growth_curve_',Ecoregion,'.csv'))

#absolute
max(growth_curve_nmp$mean) - max(drought_growth_curve_nmp$mean)
#80.8

#relative
(max(drought_growth_curve_nmp$mean)- max(growth_curve_nmp$mean))/max(growth_curve_nmp$mean)
#18.3%



#potentially add others that are in

rm(drought_growth_curve_sgs,growth_curve_sgs,drought_growth_curve_nmp,
   growth_curve_nmp)


#-------------------------------------------------------------------------------
# impacts through time ------

#maximum estimated gpp enhancement for NMP (% and absolute), and when this happens (Julian day)

Ecoregion <- 'northern_mixed_prairies'
drought_growth_impact_nmp <- read.csv(paste0('./../../Data/growth_dynamics/drought_gpp_reduction_',Ecoregion,'.csv'))
drought_growth_impact_nmp %>% filter(perc_change == max(drought_growth_impact_nmp$perc_change))
#day 87 68.4%

drought_growth_impact_absolute_nmp <- read.csv( paste0('./../../Data/growth_dynamics/drought_gpp_reduction_absolute_',Ecoregion,'.csv'))
drought_growth_impact_absolute_nmp %>% filter(abs_change == max(drought_growth_impact_absolute_nmp$abs_change))
#day 129 5.3

drought_growth_impact_absolute_nmp %>% filter(abs_change == min(drought_growth_impact_absolute_nmp$abs_change))
#day 185 -21.2

#maximum GPP reduction for SGS and NMP (% and absolute) and when this happens (day)
drought_growth_impact_nmp %>% filter(perc_change == min(drought_growth_impact_nmp$perc_change))
#day 193 43.9%

Ecoregion <- 'shortgrass_steppe'
drought_growth_impact_sgs <- read.csv(paste0('./../../Data/growth_dynamics/drought_gpp_reduction_',Ecoregion,'.csv'))
drought_growth_impact_sgs %>% filter(perc_change == min(drought_growth_impact_sgs$perc_change))

#look at absolute differences in GPP through time

Ecoregion <- 'shortgrass_steppe'
drought_growth_impact_absolute_sgs <- 
  read.csv(paste0('./../../Data/growth_dynamics/drought_gpp_reduction_absolute_',Ecoregion,'.csv'))
drought_growth_impact_absolute_sgs %>% filter(abs_change == min(drought_growth_impact_absolute_sgs$abs_change))
plot(abs_change~doy,data=drought_growth_impact_absolute_sgs)


#split up by season

#SGS

#meteorological spring for GPP
drought_growth_impact_sgs %>%
  dplyr::filter(doy > 59) %>%
  dplyr::filter(doy < 152) %>%
  summarise(median(perc_change))
#-6.4%

#summer for GPP
drought_growth_impact_sgs %>%
  dplyr::filter(doy > 151) %>%
  dplyr::filter(doy < 244) %>%
  summarise(median(perc_change))
# -62.4%

#NMP

#meteorological spring for GPP
drought_growth_impact_nmp %>%
  dplyr::filter(doy > 59) %>%
  dplyr::filter(doy < 152) %>%
  summarise(median(perc_change))
#-26.4%

#summer for GPP
drought_growth_impact_nmp %>%
  dplyr::filter(doy > 151) %>%
  dplyr::filter(doy < 244) %>%
  summarise(median(perc_change))
# -62.4%

#Now do NDVI

#SGS
drought_growth_impact_NDVI_sgs <- 
  read_csv('./../../Data/growth_dynamics/drought_ndvi_reduction_shortgrass_steppe.csv')

#meteorological spring for NDVI
drought_growth_impact_NDVI_sgs %>%
  dplyr::filter(doy > 59) %>%
  dplyr::filter(doy < 152) %>%
  summarise(median(perc_change))
#-12.7%

#meteorological summer for NDVI
drought_growth_impact_NDVI_sgs %>%
  dplyr::filter(doy > 151) %>%
  dplyr::filter(doy < 244) %>%
  summarise(median(perc_change))
#-31.2%


#-------------------------------------------------------------------------------
# climate seasonality -------

#shortgrass steppe
Ecoregion <- 'shortgrass_steppe'
source('seasonal_precip_temp_analysis.R')
#942/2
#abs and relative change in spring ppt
quantile(test_spring_ppt_drought$perc_decrease,c(0.01,0.5,0.99))
#50th = 70.8% reduction

quantile(test_spring_ppt_drought$abs_decrease,c(0.01,0.5,0.99))
# 50th = 75.4 mm

#abs and relative decline in summer ppt
quantile(test_summer_ppt_drought$perc_decrease,c(0.01,0.5,0.99))
#50th = 59.2% reduction

quantile(test_summer_ppt_drought$abs_decrease,c(0.01,0.5,0.99))
#50th = 106.22 mm 

#change in the % of spring precip during drought
quantile(spring_summer_precip_drought$change_in_perc_spring,c(0.01,0.5,0.99),na.rm=T)
#-7.6%

#abs and relative change in spring temp
quantile(test_spring_temp_drought$perc_change,c(0.01,0.5,0.99))
#50th = 7.7%

quantile(test_spring_temp_drought$abs_change,c(0.01,0.5,0.99))
#50th = 1 degree

#abs and relative change in summer temp
quantile(test_summer_temp_drought$perc_change,c(0.01,0.5,0.99))
#50th = 9.1%

quantile(test_summer_temp_drought$abs_change,c(0.01,0.5,0.99))
#50th = 2 degree

#filter summer and spring abs and relative for VPD
vpd_change %>%
  filter(season=='summer') %>%
  summarise(quantile(perc_change,c(0.01,0.50,0.99)))
#50th = 28

vpd_change %>%
  filter(season=='spring') %>%
  summarise(quantile(perc_change,c(0.01,0.50,0.99)))
#50th = 22

vpd_change %>%
  filter(season=='summer') %>%
  summarise(quantile(abs_change,c(0.01,0.50,0.99)))
#50th = 9.7

vpd_change %>%
  filter(season=='spring') %>%
  summarise(quantile(abs_change,c(0.01,0.50,0.99)))
#50th = 4.4



##

#northern mixed prairies
Ecoregion = 'northern_mixed_prairies'
source('seasonal_precip_temp_analysis.R')

#abs and relative change in spring ppt
quantile(test_spring_ppt_drought$perc_decrease,c(0.01,0.5,0.99))
#50th = -47.9% reduction

quantile(test_spring_ppt_drought$abs_decrease,c(0.01,0.5,0.99))
# 50th = -71.2 mm

#shift in proportion of spring precip
quantile(spring_summer_precip_drought$change_in_perc_spring,c(0.01,0.5,0.99),na.rm=T)
# 50th = 6.5
#hist(spring_summer_precip_drought$change_in_perc_spring)

#abs and relative decline in summer ppt
quantile(test_summer_ppt_drought$perc_decrease,c(0.01,0.5,0.99))
#50th = -57.6% reduction

quantile(test_summer_ppt_drought$abs_decrease,c(0.01,0.5,0.99))
#50th = -80.9 mmm
#80.9 - 71.2 = 9.7 mm

#abs and relative change in spring temp
quantile(test_spring_temp_drought$perc_change,c(0.01,0.5,0.99))
#50th = 21.4%

quantile(test_spring_temp_drought$abs_change,c(0.01,0.5,0.99))
#50th = 1.2 degree

#abs and relative change in summer temp
quantile(test_summer_temp_drought$perc_change,c(0.01,0.5,0.99))
#50th = 6.8%

quantile(test_summer_temp_drought$abs_change,c(0.01,0.5,0.99))
#50th = 1.3 degree

vpd_change %>%
  filter(season=='summer') %>%
  summarise(quantile(perc_change,c(0.01,0.50,0.99)))
#50th = 22

vpd_change %>%
  filter(season=='spring') %>%
  summarise(quantile(perc_change,c(0.01,0.50,0.99)))
#50th = 14

vpd_change %>%
  filter(season=='summer') %>%
  summarise(quantile(abs_change,c(0.01,0.50,0.99)))
#50th = 6.2

vpd_change %>%
  filter(season=='spring') %>%
  summarise(quantile(abs_change,c(0.01,0.50,0.99)))
#50th=1.5

#6.2/1.5


#look at the % of summer and spring precip during dry and normal years

#merge spring and summer precip
head(test_spring_ppt_drought)
spring_summer_precip_drought <- 
  merge(test_spring_ppt_drought[c(1:6)],test_summer_ppt_drought[c(1:6)],
        by=c('x','y','year','annual_precip'))
head(spring_summer_precip_drought,1)

#get % spring during normal year and dry year
spring_summer_precip_drought$perc_spring_normal <-
  ((spring_summer_precip_drought$spring_precip)/
  (spring_summer_precip_drought$spring_precip + spring_summer_precip_drought$summer_precip))*100

spring_summer_precip_drought$perc_spring_drought <-
  ((spring_summer_precip_drought$spring_precip_drought)/
     (spring_summer_precip_drought$spring_precip_drought + spring_summer_precip_drought$summer_precip_drought))*100

hist(spring_summer_precip_drought$perc_spring_drought)
hist(spring_summer_precip_drought$perc_spring_normal,add=T,col='blue')

spring_summer_precip_drought$perc_spring_normal <-
  ((spring_summer_precip_drought$spring_precip)/
     (spring_summer_precip_drought$spring_precip + spring_summer_precip_drought$summer_precip))*100

#get % summer precip during normal and dry year
spring_summer_precip_drought$perc_summer_drought <-
  ((spring_summer_precip_drought$summer_precip_drought)/
     (spring_summer_precip_drought$spring_precip_drought + spring_summer_precip_drought$summer_precip_drought))*100

spring_summer_precip_drought$perc_summer_normal <-
  ((spring_summer_precip_drought$summer_precip)/
     (spring_summer_precip_drought$spring_precip + spring_summer_precip_drought$summer_precip))*100

#changes in % of spring precip
hist(spring_summer_precip_drought$perc_spring_drought)
hist(spring_summer_precip_drought$perc_spring_normal,add=T,col='blue')

#changes in seasonality of spring precip
spring_summer_precip_drought$change_in_perc_spring <-
  spring_summer_precip_drought$perc_spring_drought - 
  spring_summer_precip_drought$perc_spring_normal

hist(spring_summer_precip_drought$change_in_perc_spring)
summary(spring_summer_precip_drought$change_in_perc_spring)

#changes in % of summer precip
hist(spring_summer_precip_drought$perc_summer_drought)
hist(spring_summer_precip_drought$perc_summer_normal,add=T,col='blue')

#changes in seasonality of summer precip
spring_summer_precip_drought$change_in_perc_summer <-
  spring_summer_precip_drought$perc_summer_drought - 
  spring_summer_precip_drought$perc_summer_normal

hist(spring_summer_precip_drought$change_in_perc_summer)
summary(spring_summer_precip_drought$change_in_perc_summer)



rm(driest_year, test_spring_ppt_drought, test_spring_temp_drought,
   test_summer_ppt_drought, test_summer_temp_drought,vpd_change)


#-------------------------------------------------------------------------------
# follow-up analysis of amount of GPP by season and max GPP ------

#shortgrass steppe

Ecoregion <- "shortgrass_steppe"
growth_curve_sgs <- read.csv(paste0('./../../Data/growth_curves/average_growth_curve_',Ecoregion,'.csv'))

#median GPP during spring
end_spring <- growth_curve_sgs %>%
  dplyr::filter(doy=='151') #march 6
#119.2
spring_gpp = end_spring$mean

end_summer <- growth_curve_sgs %>%
  dplyr::filter(doy=='243')
summer_gpp = end_summer$mean - spring_gpp
(summer_gpp - spring_gpp)/spring_gpp
#summer GPP 40% higher
summer_gpp/spring_gpp
#1.4

drought_growth_curve_sgs <- read.csv(paste0('./../../Data/growth_curves/drought_growth_curve_',Ecoregion,'.csv'))
end_spring_drought <- drought_growth_curve_sgs %>%
  dplyr::filter(doy=='151') #march 6
#95.7
spring_gpp_drought = end_spring_drought$mean

end_summer_drought <- drought_growth_curve_sgs %>%
  dplyr::filter(doy=='243')
summer_gpp_drought = end_summer_drought$mean - spring_gpp_drought
(spring_gpp_drought - summer_gpp_drought)/summer_gpp_drought
#summer GPP 40% higher
summer_gpp_drought/spring_gpp_drought

growth_curve_sgs <- read.csv(paste0('./../../Data/growth_curves/average_growth_curve_',Ecoregion,'.csv'))

#median GPP during spring
end_spring <- growth_curve_sgs %>%
  dplyr::filter(doy=='151') #march 6
#119.2
spring_gpp = end_spring$mean

end_summer <- growth_curve_sgs %>%
  dplyr::filter(doy=='243')
summer_gpp = end_summer$mean - spring_gpp
(summer_gpp - spring_gpp)/spring_gpp
#summer GPP 40% higher
summer_gpp/spring_gpp
#1.4

#northern mixed prairies

Ecoregion <- "northern_mixed_prairies"

growth_curve_nmp <- read.csv(paste0('./../../Data/growth_curves/average_growth_curve_',Ecoregion,'.csv'))

#median GPP during spring
end_spring <- growth_curve_nmp %>%
  dplyr::filter(doy=='151') #march 6
spring_gpp = end_spring$mean
#119.3

end_summer <- growth_curve_nmp %>%
  dplyr::filter(doy=='243')
summer_gpp = end_summer$mean - spring_gpp
#266.8
(summer_gpp - spring_gpp)/spring_gpp
#summer GPP 124% higher
summer_gpp/spring_gpp
#2.2

drought_growth_curve_nmp <- read.csv(paste0('./../../Data/growth_curves/drought_growth_curve_',Ecoregion,'.csv'))
end_spring_drought <- drought_growth_curve_nmp %>%
  dplyr::filter(doy=='151') #march 6
#133
spring_gpp_drought = end_spring_drought$mean

end_summer_drought <- drought_growth_curve_nmp %>%
  dplyr::filter(doy=='243')
summer_gpp_drought = end_summer_drought$mean - spring_gpp_drought
(summer_gpp_drought - spring_gpp_drought)/spring_gpp_drought
#summer GPP 40% higher
summer_gpp_drought/spring_gpp_drought

growth_curve_nmp <- read.csv(paste0('./../../Data/growth_curves/average_growth_curve_',Ecoregion,'.csv'))

#median GPP during spring
end_spring <- growth_curve_nmp %>%
  dplyr::filter(doy=='151') #march 6
#119.2
spring_gpp = end_spring$mean

end_summer <- growth_curve_nmp %>%
  dplyr::filter(doy=='243')
summer_gpp = end_summer$mean - spring_gpp
(summer_gpp - spring_gpp)/spring_gpp
#summer GPP 40% higher
summer_gpp/spring_gpp
#1.4
#-------------------------------------------------------------------------------
#compare distributions of day 90 by ecoregion ------

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

#turn each to a dataframe
day_90_drought_sgs_2_lat <- data.frame(rasterToPoints(day_90_drought_sgs_2))
day_90_drought_nmp_2_lat <- data.frame(rasterToPoints(day_90_drought_nmp_2))

ks.test(day_90_drought_sgs_2_lat$layer,day_90_drought_nmp_2_lat$layer)
hist(day_90_drought_nmp_2_lat$layer)
hist(day_90_drought_sgs_2_lat$layer,add=T,col='blue')

# data:  day_90_drought_sgs_2_lat$layer and day_90_drought_nmp_2_lat$layer
# D = 0.42584, p-value < 2.2e-16
# alternative hypothesis: two-sided
#so distributions are different...what is this really telling us?

library(gstat)
#bind them and make into one variogram 
binded_day_90 <- rbind(day_90_drought_nmp_2_lat,day_90_drought_sgs_2_lat)
coordinates(day_90_drought_nmp_2_lat) = ~ x+y
TheVariogram=variogram(layer~1, data=day_90_drought_nmp_2_lat)
plot(TheVariogram)
summary(TheVariogram)
#psill = asymptote on y axis
#nugget = intercept
#range = asymptote on x axis
TheVariogramModel <- vgm(psill=10, model="Sph", nugget=4, range=5)
plot(TheVariogram, model=TheVariogramModel,xlab='Distance (km)',cex.lab=5,
     main='Effect of drought on day at which 90% of carbon uptake occurs') 
abline(h=5)
?abline
summary(TheVariogramModel)

#-------------------------------------------------------------------------------
# compare distribution of day 50 (to do) ------
#-------------------------------------------------------------------------------
# compare distribution of day 25 (to do) ------
#-------------------------------------------------------------------------------
# compare distribution of seasonality of precip ------

Ecoregion <- 'shortgrass_steppe'
source('seasonal_precip_temp_analysis.R')

#ks.test(spring_summer_precip_drought$change_in_perc_spring),'pnorm'))

n_samples <- as.numeric(nrow(spring_summer_precip_drought))
below_zero <- spring_summer_precip_drought %>%
  dplyr::filter(change_in_perc_spring < 0)
  nrow(below_zero)/n_samples
  

t.test(spring_summer_precip_drought$change_in_perc_summer)

hist(spring_summer_precip_drought$change_in_perc_spring)
hist(spring_summer_precip_drought$change_in_perc_summer,add=T,col='red')

library(rethinking)
library(splitstackshape)


strat <- stratified()

#stratify 1% of pixels bty latitude
strat <- stratified(spring_summer_precip_drought, c("x"), 0.01)

#one sides t.test
t_test <- t.test(strat$change_in_perc_spring)
t_test$method
hist(strat$change_in_perc_spring)

plot(y~x,data=strat)

start_raster <- spring_summer_precip_drought %>%
  select(x,y,change_in_perc_spring)
start_raster <- na.omit(start_raster)

start_raster <- rasterFromXYZ(start_raster)
Moran(start_raster, w=matrix(c(1,1,1,1,0,1,1,1,1), 3,3))
plot(start_raster)


#variogram
coordinates(start_raster)= ~ x+y

# create a bubble plot with the random values
bubble(start_raster, change_in_perc_spring='change in spring ppt', fill=TRUE, do.sqrt=FALSE, maxsize=3)

library(gstat)
TheVariogram=variogram(change_in_perc_spring ~1, data=start_raster)
plot(TheVariogram)

?variogram

#-------------------------------------------------------------------------------
# % of GPP in months not analyzed ----

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



head(sgs_a_annual_gpp_subset,1)

#-------------------------------------------------------------------------------
# size of the study area -----

#import 
rangeland_npp_covariates <- readRDS('./../../Data/Herbaceous_NPP_1986_2015_V2.rds')
head(rangeland_npp_covariates)
mean_production<-aggregate(npp~ x + y + region,mean,data=rangeland_npp_covariates)
study_area<-subset(mean_production,region==c('shortgrass_steppe', 'northern_mixed_prairies'))
study_area_raster <- rasterFromXYZ(study_area[c(1,2,4)])
crs(study_area_raster) <- CRS("+proj=longlat")

cell_size<-area(study_area_raster, na.rm=TRUE, weights=FALSE)
#delete NAs from vector of all raster cells
##NAs lie outside of the rastered region, can thus be omitted
cell_size<-cell_size[!is.na(cell_size)]
#compute area [km2] of all cells in geo_raster1000
area<-length(cell_size)*median(cell_size)

