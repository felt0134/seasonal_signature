
# key summary statistics


# Overview -----
#-------------------------------------------------------------------------------
# driest years ------

#Three most common 'driest years'

#shortgrass steppe
Ecoregion <- 'shortgrass_steppe'
driest_year_sgs <- 
  read.csv(paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/growing_season/drought_precip_year_',Ecoregion,'.csv'))

driest_year_sgs_count <- aggregate(ppt_min ~ year,length,data = driest_year_sgs)
driest_year_sgs_count <- driest_year_sgs_count %>% arrange(desc(ppt_min)) 
driest_year_sgs_count <- driest_year_sgs_count[c(1:3),]
driest_year_sgs_count$perc <- round((driest_year_sgs_count$ppt_min/nrow(driest_year_sgs))*100,1)

rm(driest_year_sgs)

#
#

#northern mixed prairies
Ecoregion <- 'northern_mixed_prairies'
driest_year_nmp <- 
  read.csv(paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/growing_season/drought_precip_year_',Ecoregion,'.csv'))

driest_year_nmp_count <- aggregate(ppt_min ~ year,length,data = driest_year_nmp)
driest_year_nmp_count <- driest_year_nmp_count %>% arrange(desc(ppt_min)) 
driest_year_nmp_count <- driest_year_nmp_count[c(1:3),]
driest_year_nmp_count$perc <- round((driest_year_nmp_count$ppt_min/nrow(driest_year_nmp))*100,1)

rm(driest_year_nmp)

#-------------------------------------------------------------------------------
# change in growing season precipitation during drought  -----
Ecoregion <- 'shortgrass_steppe'
driest_year_sgs <- 
  read.csv(paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/growing_season/drought_precip_year_',Ecoregion,'.csv'))
driest_year_sgs <- driest_year_sgs %>%
  dplyr::select(x,y,ppt_min)
head(driest_year_sgs,1)

#pixel-year time series
as.numeric(length(seq(2003:2020)))*as.numeric(nrow(driest_year_sgs))

#plot(rasterFromXYZ(driest_year_sgs))

#import annual ppt, merge, and get % reduction during drought years
mean_precip_sgs <- 
  read.csv(paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/growing_season/mean_precip_drought_removed',Ecoregion,'.csv'))
mean_precip_sgs <- aggregate(ppt ~ x + y,mean,data = mean_precip_sgs)
drought_reduction_sgs <- merge(driest_year_sgs,mean_precip_sgs,by=c('x','y'))
head(drought_reduction_sgs,1)

#relative (% reduction)
drought_reduction_sgs$perc_reduction <- 
  ((drought_reduction_sgs$ppt_min - drought_reduction_sgs$ppt)/
  drought_reduction_sgs$ppt)*100

sgs_precip_change_rel <- quantile(drought_reduction_sgs$perc_reduction,c(.25,0.5,0.75))

#absolute
drought_reduction_sgs$abs_reduction <- 
  (drought_reduction_sgs$ppt_min - drought_reduction_sgs$ppt)

sgs_precip_change_abs <- quantile(drought_reduction_sgs$abs_reduction,c(.25,0.5,0.75))

#cleanup
rm(mean_precip_sgs,driest_year_sgs,drought_reduction_sgs)

#
#

Ecoregion <- 'northern_mixed_prairies'
driest_year_nmp <- 
  read.csv(paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/growing_season/drought_precip_year_',Ecoregion,'.csv'))
driest_year_nmp <- driest_year_nmp %>%
  dplyr::select(x,y,ppt_min)
head(driest_year_nmp,1)

#pixel-year time series
as.numeric(length(seq(2003:2020)))*as.numeric(nrow(driest_year_nmp))

#import annual ppt, merge, and get % reduction during drought years
mean_precip_nmp <- 
  read.csv(paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/growing_season/mean_precip_drought_removed',Ecoregion,'.csv'))
mean_precip_nmp <- aggregate(ppt ~ x + y,mean,data = mean_precip_nmp)
drought_reduction_nmp <- merge(driest_year_nmp,mean_precip_nmp,by=c('x','y'))
head(drought_reduction_nmp,1)

#relative
drought_reduction_nmp$perc_reduction <- 
  ((drought_reduction_nmp$ppt_min - drought_reduction_nmp$ppt)/
  drought_reduction_nmp$ppt)*100

nmp_precip_change_rel <- quantile(drought_reduction_nmp$perc_reduction,c(.25,0.5,0.75))

#absolute
drought_reduction_nmp$abs_reduction <- 
  (drought_reduction_nmp$ppt_min - drought_reduction_nmp$ppt)

nmp_precip_change_abs <- quantile(drought_reduction_nmp$abs_reduction,c(.25,0.5,0.75))

#cleanup
rm(mean_precip_nmp,driest_year_nmp,drought_reduction_nmp)

#-------------------------------------------------------------------------------
# change in growing season temperature during years of drought -----

#shortgrass steppe

Ecoregion <- 'shortgrass_steppe'
average_temp_sgs <- 
  read.csv(paste0('./../../Data/Climate/Ecoregion/',Ecoregion,
                   '/Temperature/growing_season/mean_temp_drought_removed',Ecoregion,'.csv'))
average_temp_sgs <- average_temp_sgs %>%
  dplyr::select(x,y,average_temp)

temp_drought_sgs <-
  read.csv(paste0('./../../Data/Climate/Ecoregion/',Ecoregion,
                  '/Temperature/growing_season/drought_temp_year_',Ecoregion,'.csv'))
temp_drought_sgs <- temp_drought_sgs %>%
  dplyr::select(x,y,temp_drought)

temp_drought_sgs <- merge(average_temp_sgs,temp_drought_sgs,by=c('x','y'))
rm(average_temp_sgs)

#relative change
temp_drought_sgs$perc_change <- 
  ((temp_drought_sgs$temp_drought - temp_drought_sgs$average_temp)/temp_drought_sgs$average_temp)*100

sgs_temp_change_rel <- quantile(temp_drought_sgs$perc_change,c(0.25,0.5,0.75))

#absolute change
temp_drought_sgs$abs_change <- 
  (temp_drought_sgs$temp_drought - temp_drought_sgs$average_temp)

sgs_temp_change_abs <- quantile(temp_drought_sgs$abs_change,c(0.25,0.5,0.75))

rm(temp_drought_sgs)

#
#

Ecoregion <- 'northern_mixed_prairies'
average_temp_nmp <- 
  read.csv(paste0('./../../Data/Climate/Ecoregion/',Ecoregion,
                  '/Temperature/growing_season/mean_temp_drought_removed',Ecoregion,'.csv'))
average_temp_nmp <- average_temp_nmp %>%
  dplyr::select(x,y,average_temp)

temp_drought_nmp <-
  read.csv(paste0('./../../Data/Climate/Ecoregion/',Ecoregion,
                  '/Temperature/growing_season/drought_temp_year_',Ecoregion,'.csv'))
temp_drought_nmp <- temp_drought_nmp %>%
  dplyr::select(x,y,temp_drought)

temp_drought_nmp <- merge(average_temp_nmp,temp_drought_nmp,by=c('x','y'))
rm(average_temp_nmp)

#relative change
temp_drought_nmp$perc_change <- 
  ((temp_drought_nmp$temp_drought - temp_drought_nmp$average_temp)/temp_drought_nmp$average_temp)*100

nmp_temp_change_rel <- quantile(temp_drought_nmp$perc_change,c(0.25,0.5,0.75))

#absolute change
temp_drought_nmp$abs_change <- 
  (temp_drought_nmp$temp_drought - temp_drought_nmp$average_temp)

nmp_temp_change_abs <- quantile(temp_drought_nmp$abs_change,c(0.25,0.5,0.75))

rm(temp_drought_nmp)

#-------------------------------------------------------------------------------
# Changes to cumulative and maximum C uptake (new) ------

#shortgrass steppe
max_total_reduction_sgs_df <- 
  read.csv('./../../Data/growth_dynamics/max_total_reduction_shortgrass_steppe.csv')
head(max_total_reduction_sgs_df,1)

#total reduction
total_reduction_sgs <- max_total_reduction_sgs_df %>%
  dplyr::filter(type=='total')
quantile(total_reduction_sgs$reduction,c(0.25,0.5,0.75))
quantile(total_reduction_sgs$perc_reduction,c(0.25,0.5,0.75))

#max reduction
max_reduction_sgs <- max_total_reduction_sgs_df %>%
  dplyr::filter(type=='max')
quantile(max_reduction_sgs$reduction,c(0.25,0.5,0.75))
quantile(max_reduction_sgs$perc_reduction,c(0.25,0.5,0.75))
quantile(max_reduction_sgs$doy,c(0.25,0.5,0.75))

#max increase
min_reduction_sgs <- max_total_reduction_sgs_df %>%
  dplyr::filter(type=='min')
quantile(min_reduction_sgs$reduction,c(0.25,0.5,0.75))
quantile(min_reduction_sgs$perc_reduction,c(0.25,0.5,0.75))

#northern mixed prairies
max_total_reduction_nmp_df <- 
  read.csv('./../../Data/growth_dynamics/max_total_reduction_northern_mixed_prairies.csv')
head(max_total_reduction_nmp_df,1)

#total reduction
total_reduction_nmp <- max_total_reduction_nmp_df %>%
  dplyr::filter(type=='total')
quantile(total_reduction_nmp$reduction,c(0.25,0.5,0.75))
quantile(total_reduction_nmp$perc_reduction,c(0.25,0.5,0.75))

#max reduction
max_reduction_nmp <- max_total_reduction_nmp_df %>%
  dplyr::filter(type=='max')
quantile(max_reduction_nmp$reduction,c(0.25,0.5,0.75))
quantile(max_reduction_nmp$perc_reduction,c(0.25,0.5,0.75))
quantile(max_reduction_nmp$doy,c(0.25,0.5,0.75))

#max increase
min_reduction_nmp <- max_total_reduction_nmp_df %>%
  dplyr::filter(type=='min')
quantile(min_reduction_nmp$reduction,c(0.25,0.5,0.75))
quantile(min_reduction_nmp$perc_reduction,c(0.25,0.5,0.75))
quantile(min_reduction_nmp$doy,c(0.25,0.5,0.75))

#compare distributions
d_list_reduction <- list()
for(i in 1:1000){
  
  #get random subset of 100 for each
  total_reduction_nmp_rand <- total_reduction_nmp %>%
    dplyr::sample_n(100)

  total_reduction_sgs_rand <- total_reduction_sgs %>%
    dplyr::sample_n(100)

  test <- ks.test(total_reduction_sgs_rand$reduction,total_reduction_nmp_rand$reduction,exact=F)
  D <- data.frame(test$statistic)
  
  d_list_reduction[[i]] <- D
  
}

d_list_reduction_df <- do.call('rbind',d_list_reduction)
hist(d_list_reduction_df$test.statistic)
mean(d_list_reduction_df$test.statistic)
ci_99(d_list_reduction_df$test.statistic)

#-------------------------------------------------------------------------------
# C uptake through time (old) ------

#maximum estimated gpp enhancement for NMP (% and absolute), and when this happens (Julian day)

#northern mixed prairies
Ecoregion <- 'northern_mixed_prairies'
drought_growth_impact_nmp <- read.csv(paste0('./../../Data/growth_dynamics/drought_gpp_reduction_',Ecoregion,'.csv'))
drought_growth_impact_nmp %>% dplyr::filter(perc_change == max(drought_growth_impact_nmp$perc_change))
#day 85 83.32%

drought_growth_impact_absolute_nmp <- read.csv( paste0('./../../Data/growth_dynamics/drought_gpp_reduction_absolute_',Ecoregion,'.csv'))
drought_growth_impact_absolute_nmp %>% dplyr::filter(abs_change == max(drought_growth_impact_absolute_nmp$abs_change))
#day 128 6

drought_growth_impact_absolute_nmp %>% dplyr::filter(abs_change == min(drought_growth_impact_absolute_nmp$abs_change))
#day 185 -22.9

#maximum GPP reduction for SGS and NMP (% and absolute) and when this happens (day)
drought_growth_impact_nmp %>% dplyr::filter(perc_change == min(drought_growth_impact_nmp$perc_change))
#day 193 43.9%

#
#

#shortgrass steppe
Ecoregion <- 'shortgrass_steppe'
drought_growth_impact_sgs <- read.csv(paste0('./../../Data/growth_dynamics/drought_gpp_reduction_',Ecoregion,'.csv'))
drought_growth_impact_sgs %>% dplyr::filter(perc_change == min(drought_growth_impact_sgs$perc_change))
#day 184 -66.4

#look at absolute differences in GPP through time
drought_growth_impact_absolute_sgs <- 
  read.csv(paste0('./../../Data/growth_dynamics/drought_gpp_reduction_absolute_',Ecoregion,'.csv'))
drought_growth_impact_absolute_sgs %>% dplyr::filter(abs_change == min(drought_growth_impact_absolute_sgs$abs_change))
#day 176: -18.6

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
# seasonal changes in precipitation  -------

#shortgrass steppe
Ecoregion <- 'shortgrass_steppe'
seasonal_precip_sgs <- 
  read.csv(paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/seasonal_change_PPT.csv'))
head(seasonal_precip_sgs,1)

#abs change in spring ppt
quantile(seasonal_precip_sgs$abs_change_spring_precipitation,c(0.25,0.5,0.75))

#abs change in summer ppt
quantile(seasonal_precip_sgs$abs_change_summer_precipitation,c(0.25,0.5,0.75))

#percent change in spring ppt
quantile(seasonal_precip_sgs$perc_change_spring_precipitation,c(0.25,0.5,0.75))

#percent change in spring ppt
quantile(seasonal_precip_sgs$perc_change_summer_precipitation,c(0.25,0.5,0.75))

#change in prop of spring ppt
quantile(seasonal_precip_sgs$change_spring_prop,c(0.25,0.5,0.75),na.rm=T)
test <- t.test(seasonal_precip_sgs$change_spring_pro,alternative='greater')
test$statistic

#boostrapped t.test
sgs_t_vals <- list()
for(i in 1:1000){
  
  
#get random subset of 100
  seasonal_precip_sgs_rand <- seasonal_precip_sgs %>%
    dplyr::sample_n(100)
  
  t_test <- t.test(seasonal_precip_sgs_rand$change_spring_pro)
  t_value <- t_test$statistic
  
  sgs_t_vals[[i]] <- t_value
  
}

sgs_t_vals_df <- list_to_df(sgs_t_vals)
hist(sgs_t_vals_df$t)
ci_99(sgs_t_vals_df$t)
mean(sgs_t_vals_df$t)

#
#


#northern mixed prairies
Ecoregion <- 'northern_mixed_prairies'
seasonal_precip_nmp <- 
  read.csv(paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/seasonal_change_PPT.csv'))
head(seasonal_precip_nmp,1)

#abs change in spring ppt
quantile(seasonal_precip_nmp$abs_change_spring_precipitation,c(0.25,0.5,0.75))

#abs change in summer ppt
quantile(seasonal_precip_nmp$abs_change_summer_precipitation,c(0.25,0.5,0.75))

#percent change in spring ppt
quantile(seasonal_precip_nmp$perc_change_spring_precipitation,c(0.25,0.5,0.75))

#percent change in spring ppt
quantile(seasonal_precip_nmp$perc_change_summer_precipitation,c(0.25,0.5,0.75))

#change in prop of spring ppt
quantile(seasonal_precip_nmp$change_spring_prop,c(0.25,0.5,0.75),na.rm=T)


nmp_t_vals <- list()
for(i in 1:1000){
  
  
  #get random subset of 100
  seasonal_precip_nmp_rand <- seasonal_precip_nmp %>%
    dplyr::sample_n(100)
  
  t_test <- t.test(seasonal_precip_nmp_rand$change_spring_pro)
  t_value <- t_test$statistic
  
  nmp_t_vals[[i]] <- t_value
  
}

nmp_t_vals_df <- list_to_df(nmp_t_vals)
hist(nmp_t_vals_df$t)
ci_99(nmp_t_vals_df$t)
mean(nmp_t_vals_df$t)

library(plotrix)

#-------------------------------------------------------------------------------
# seasonal changes in temperature -----

#shortgrass steppe
Ecoregion <- 'shortgrass_steppe'
seasonal_temp_sgs <- 
  read.csv(paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/temperature/seasonal_change_temperature.csv'))
head(seasonal_temp_sgs,1)

#abs change in spring temp
quantile(seasonal_temp_sgs$abs_change_spring_temperature,c(0.25,0.5,0.75))

#abs change in summer temp
quantile(seasonal_temp_sgs$abs_change_summer_temperature,c(0.25,0.5,0.75))

#percent change in spring temp
quantile(seasonal_temp_sgs$perc_change_spring_temperature,c(0.25,0.5,0.75))

#percent change in spring temp
quantile(seasonal_temp_sgs$perc_change_summer_temperature,c(0.25,0.5,0.75))


#
#


#northern mixed prairies
Ecoregion <- 'northern_mixed_prairies'
seasonal_temp_nmp <- 
  read.csv(paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/temperature/seasonal_change_temperature.csv'))
head(seasonal_temp_nmp,1)

#abs change in spring temp
quantile(seasonal_temp_nmp$abs_change_spring_temperature,c(0.25,0.5,0.75))

#abs change in summer temp
quantile(seasonal_temp_nmp$abs_change_summer_temperature,c(0.25,0.5,0.75))

#percent change in spring temp
quantile(seasonal_temp_nmp$perc_change_spring_temperature,c(0.25,0.5,0.75))

#percent change in spring temp
quantile(seasonal_temp_nmp$perc_change_summer_temperature,c(0.25,0.5,0.75))

#-------------------------------------------------------------------------------
#seasonal change in VPD -------

#shortgrass steppe
Ecoregion <- 'shortgrass_steppe'
seasonal_vpd_sgs <- 
  read.csv(paste0( './../../Data/Climate/Ecoregion/',
                   Ecoregion,
                   '/PRISM/VPD_change.csv'))
head(seasonal_vpd_sgs,1)

#subset by season
seasonal_vpd_sgs_spring <- subset(seasonal_vpd_sgs,season == 'spring')
seasonal_vpd_sgs_summer <- subset(seasonal_vpd_sgs,season == 'summer')

#abs change in spring vpd
quantile(seasonal_vpd_sgs_spring$abs_change,c(0.25,0.5,0.75))

#abs change in summer vpd
quantile(seasonal_vpd_sgs_summer$abs_change,c(0.25,0.5,0.75))

#percent change in spring vpd
quantile(seasonal_vpd_sgs_spring$perc_change,c(0.25,0.5,0.75))

#percent change in spring vpd
quantile(seasonal_vpd_sgs_summer$perc_change,c(0.25,0.5,0.75))


#
#


#northern mixed prairies
Ecoregion <- 'northern_mixed_prairies'
seasonal_vpd_nmp <- 
  read.csv(paste0( './../../Data/Climate/Ecoregion/',
                   Ecoregion,
                   '/PRISM/VPD_change.csv'))
head(seasonal_vpd_nmp,1)

#subset by season
seasonal_vpd_nmp_spring <- subset(seasonal_vpd_nmp,season == 'spring')
seasonal_vpd_nmp_summer <- subset(seasonal_vpd_nmp,season == 'summer')

#abs change in spring vpd
quantile(seasonal_vpd_nmp_spring$abs_change,c(0.25,0.5,0.75))

#abs change in summer vpd
quantile(seasonal_vpd_nmp_summer$abs_change,c(0.25,0.5,0.75))

#percent change in spring vpd
quantile(seasonal_vpd_nmp_spring$perc_change,c(0.25,0.5,0.75))

#percent change in spring vpd
quantile(seasonal_vpd_nmp_summer$perc_change,c(0.25,0.5,0.75))

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

#see how this changes during drought
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
# compare distributions of day 75 by ecoregion ------

#shortgrass stppe
day_75_drought_sgs <-
  raster('./../../Data/CDD/day_of_75/day_75_drought_impact_shortgrass_steppe.tif')
day_75_drought_sgs <- data.frame(rasterToPoints(day_75_drought_sgs))
day_75_drought_sgs <- day_75_drought_sgs %>%
  dplyr::rename('day_75' = 'day_75_drought_impact_shortgrass_steppe')
#day_75_drought_sgs$ecoregion <- 'shortgrass_steppe'

quantile(day_75_drought_sgs$day_75,probs = c(0.25,0.5,0.75))

#
#

#northern mixed prairies
day_75_drought_nmp <-
  raster('./../../Data/CDD/day_of_75/day_75_drought_impact_northern_mixed_prairies.tif')
day_75_drought_nmp <- data.frame(rasterToPoints(day_75_drought_nmp))
day_75_drought_nmp <- day_75_drought_nmp %>%
  dplyr::rename('day_75' = 'day_75_drought_impact_northern_mixed_prairies')
#day_75_drought_nmp$ecoregion <- 'northern_mixed_prairies'

quantile(day_75_drought_nmp$day_75,probs = c(0.25,0.5,0.75))

#compare distributions
compare_day_75 <- ks_test_bootstrap(day_75_drought_nmp,day_75_drought_sgs)
hist(compare_day_75$test.statistic)
quantile(compare_day_75$test.statistic,probs= c(0.01,0.5))

#look at correlation
day_75_drought_sgs_nmp <- rbind(day_75_drought_nmp,day_75_drought_sgs)
cor.test(day_75_drought_sgs_nmp$y,day_75_drought_sgs_nmp$day_75 ,method='spearman',exact=FALSE)
#-0.24


#-------------------------------------------------------------------------------
# compare distribution of day 50 by ecoregion ------


#shortgrass stppe
day_50_drought_sgs <-
  raster('./../../Data/CDD/day_of_50/day_50_drought_impact_shortgrass_steppe.tif')
day_50_drought_sgs <- data.frame(rasterToPoints(day_50_drought_sgs))
day_50_drought_sgs <- day_50_drought_sgs %>%
  dplyr::rename('day_50' = 'day_50_drought_impact_shortgrass_steppe')
#day_50_drought_sgs$ecoregion <- 'shortgrass_steppe'

quantile(day_50_drought_sgs$day_50,probs = c(0.25,0.5,0.75))

#
#

#northern mixed prairies
day_50_drought_nmp <-
  raster('./../../Data/CDD/day_of_50/day_50_drought_impact_northern_mixed_prairies.tif')
day_50_drought_nmp <- data.frame(rasterToPoints(day_50_drought_nmp))
day_50_drought_nmp <- day_50_drought_nmp %>%
  dplyr::rename('day_50' = 'day_50_drought_impact_northern_mixed_prairies')
#day_50_drought_nmp$ecoregion <- 'northern_mixed_prairies'

quantile(day_50_drought_nmp$day_50,probs = c(0.25,0.5,0.75))

#compare distributions
compare_day_50 <- ks_test_bootstrap(day_50_drought_nmp,day_50_drought_sgs)
hist(compare_day_50$test.statistic)
quantile(compare_day_50$test.statistic,probs= c(0.01,0.5))

#look at correlation
day_50_all <- rbind(day_50_drought_nmp,day_50_drought_sgs)
cor.test(day_50_all$y,day_50_all$day_50 ,method='spearman',exact=FALSE)
#0.34

#variogram analysis
library(ape)

#sgs
day_50_drought_sgs <-
  raster('./../../Data/CDD/day_of_50/day_50_drought_impact_shortgrass_steppe.tif')
day_50_drought_sgs_df <- data.frame(rasterToPoints(day_50_drought_sgs))
point_data_50_sgs <- as(day_50_drought_sgs_df,
                        'SpatialPointsDataFrame')

sgs.dists <- as.matrix(dist(cbind(day_50_drought_sgs_df$x,day_50_drought_sgs_df$y)))

sgs.dists.inv <- 1/sgs.dists
diag(sgs.dists.inv) <- 0

Moran.I(day_50_drought_sgs_df$day_50_drought_impact_shortgrass_steppe,sgs.dists.inv,
        alternative = "two.sided")

#significant
#coefficient = 0.15

rm(day_50_drought_sgs_df,sgs.dists,sgs.dists.inv)

TheVariogram_50_sgs = variogram(day_50_drought_impact_shortgrass_steppe ~1,
                                data = point_data_50_sgs,width = 10)

summary(TheVariogram_50_sgs)
plot(TheVariogram_50_sgs)

TheVariogramModel_50_sgs <- vgm(psill=300, model="Exp", nugget=50, range=100)
plot(TheVariogram_50_sgs, model=TheVariogramModel_50_sgs) 
FittedModel_50_sgs <- fit.variogram(TheVariogram_50_sgs, model=TheVariogramModel_50_sgs)
FittedModel_50_sgs
#Range = 50.1 km

# plot(TheVariogram_50_sgs, model=FittedModel_50_sgs,xlab='Distance (km)',
#      ylab = 'Semivariance',cex=1,lwd=2,col='black')

#northern mixed prairies
day_50_drought_nmp <-
  raster('./../../Data/CDD/day_of_50/day_50_drought_impact_northern_mixed_prairies.tif')
day_50_drought_nmp_df <- data.frame(rasterToPoints(day_50_drought_nmp))
point_data_50_nmp <- as(day_50_drought_nmp, 'SpatialPointsDataFrame')

nmp.dists <- as.matrix(dist(cbind(day_50_drought_nmp_df$x,day_50_drought_nmp_df$y)))

nmp.dists.inv <- 1/nmp.dists
diag(nmp.dists.inv) <- 0

Moran.I(day_50_drought_nmp_df$day_50_drought_impact_northern_mixed_prairies,nmp.dists.inv,
        alternative = "two.sided")

#significant
#coefficient = 0.20

rm(day_50_drought_sgs_df,sgs.dists,sgs.dists.inv)

TheVariogram_50_nmp = variogram(day_50_drought_impact_northern_mixed_prairies ~1,
                                data = point_data_50_nmp,width = 10)

summary(TheVariogram_50_nmp)
plot(TheVariogram_50_nmp)

TheVariogramModel_50_nmp <- vgm(psill=300, model="Exp", nugget=50, range=100)
plot(TheVariogram_50_nmp, model=TheVariogramModel_50_nmp) 
FittedModel_50_nmp <- fit.variogram(TheVariogram_50_nmp, model=TheVariogramModel_50_nmp)
FittedModel_50_nmp

# plot(TheVariogram_50_nmp, model=FittedModel_50_nmp,xlab='Distance (km)',
#      ylab = 'Semivariance',cex=1,lwd=2,col='black')

# #make a quick figure of this here
# par(mfrow=c(2,1),cex = 0.5,lwd = 0.5,oma=c(3.2,9,1,1),mar = c(3,2.25,3,3))
# 
# plot(TheVariogram_50_sgs, model=FittedModel_50_sgs,xlab='Distance (km)',
#      ylab = 'Semivariance',cex=1,lwd=2,col='black')
# plot(TheVariogram_50_nmp, model=FittedModel_50_nmp,xlab='Distance (km)',
#      ylab = 'Semivariance',cex=1,lwd=2,col='black')


#-------------------------------------------------------------------------------
# compare distribution of day 25 (to do) ------


#shortgrass stppe
day_25_drought_sgs <-
  raster('./../../Data/CDD/day_of_25/day_25_drought_impact_shortgrass_steppe.tif')
day_25_drought_sgs <- data.frame(rasterToPoints(day_25_drought_sgs))
day_25_drought_sgs <- day_25_drought_sgs %>%
  dplyr::rename('day_25' = 'day_25_drought_impact_shortgrass_steppe')
#day_25_drought_sgs$ecoregion <- 'shortgrass_steppe'

quantile(day_25_drought_sgs$day_25,probs = c(0.25,0.5,0.75))

#
#

#northern mixed prairies
day_25_drought_nmp <-
  raster('./../../Data/CDD/day_of_25/day_25_drought_impact_northern_mixed_prairies.tif')
day_25_drought_nmp <- data.frame(rasterToPoints(day_25_drought_nmp))
day_25_drought_nmp <- day_25_drought_nmp %>%
  dplyr::rename('day_25' = 'day_25_drought_impact_northern_mixed_prairies')
#day_25_drought_nmp$ecoregion <- 'northern_mixed_prairies'

quantile(day_25_drought_nmp$day_25,probs = c(0.25,0.5,0.75))

#compare distributions
compare_day_25 <- ks_test_bootstrap(day_25_drought_nmp,day_25_drought_sgs)
hist(compare_day_25$test.statistic)
quantile(compare_day_25$test.statistic,probs= c(0.01,0.5))

#look at correlation
day_25_all <- rbind(day_25_drought_nmp,day_25_drought_sgs)
cor.test(day_25_all$y,day_25_all$day_25,method='spearman',exact=FALSE)


#-------------------------------------------------------------------------------
# compare distribution of seasonality of precip ------

Ecoregion <- 'shortgrass_steppe'
source('seasonal_precip_temp_analysis.R')

#ks.test(spring_summer_precip_drought$change_in_perc_spring),'pnorm'))

n_samples <- as.numeric(nrow(spring_summer_precip_drought))
below_zero <- spring_summer_precip_drought %>%
  dplyr::filter(change_in_perc_spring < 0)
  nrow(below_zero)/n_samples
  

t_test <- t.test(spring_summer_precip_drought$change_in_perc_summer)

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
sp::coordinates(start_raster)= ~ x+y

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

#for SGS
study_area<-subset(mean_production,region==c('shortgrass_steppe'))
study_area_raster <- rasterFromXYZ(study_area[c(1,2,4)])
crs(study_area_raster) <- CRS("+proj=longlat")

cell_size<-area(study_area_raster, na.rm=TRUE, weights=FALSE)
#delete NAs from vector of all raster cells
##NAs lie outside of the rastered region, can thus be omitted
cell_size<-cell_size[!is.na(cell_size)]
#compute area [km2] of all cells in geo_raster1000
area_sgs<-length(cell_size)*median(cell_size)
# 188651.2

#for NMP
study_area<-subset(mean_production,region==c('northern_mixed_prairies'))
study_area_raster <- rasterFromXYZ(study_area[c(1,2,4)])
crs(study_area_raster) <- CRS("+proj=longlat")

cell_size<-area(study_area_raster, na.rm=TRUE, weights=FALSE)
#delete NAs from vector of all raster cells
##NAs lie outside of the rastered region, can thus be omitted
cell_size<-cell_size[!is.na(cell_size)]
#compute area [km2] of all cells in geo_raster1000
area_nmp<-length(cell_size)*median(cell_size)
#440832.6

#total area
area_nmp + area_sgs
#629483.8

