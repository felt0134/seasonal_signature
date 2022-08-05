

#NDVI correlation with this change

ecoregion_list <- c('shortgrass_steppe','northern_mixed_prairies')
ndvi_climate_list <- list()
for(i in ecoregion_list){
  
  Ecoregion = i
  
  #import VPD
  seasonal_vpd <- 
    read.csv(paste0( './../../Data/Climate/Ecoregion/',
                     Ecoregion,
                     '/PRISM/VPD_change.csv'))
  
  seasonal_vpd <- seasonal_vpd %>%
    dplyr::filter(season == 'summer') %>%
    dplyr::select(x,y,abs_change)
  
  #import precip 
  seasonal_precip <- 
    read.csv(paste0( './../../Data/Climate/Ecoregion/',
                     Ecoregion,
                     '/Precipitation/seasonal_change_PPT.csv'))
  #head(seasonal_precip,1)

  seasonal_precip <- seasonal_precip %>%
    dplyr::select(x,y,abs_change_summer_precipitation)
  
  
  #import temp 
  seasonal_temp <- 
    read.csv(paste0( './../../Data/Climate/Ecoregion/',
                     Ecoregion,
                     '/Temperature/seasonal_change_temperature.csv'))
  #head(seasonal_temp,1)
  
  seasonal_temp <- seasonal_temp %>%
    dplyr::select(x,y,abs_change_summer_temperature) 
  
  
  #import NDVI datasets
  max_total_reduction <- 
    read.csv(paste0('./../../Data/growth_dynamics/max_total_reduction_NDVI_',Ecoregion,'.csv'))
  #head(max_total_reduction_sgs_df,1)
  
  #import and get total for sgs
  peak_abs_reduction <- max_total_reduction %>%
    dplyr::filter(type == 'max') %>%
    dplyr::filter(doy < 297) %>%
    dplyr::filter(doy > 73) %>%
    dplyr::select(x,y,reduction)
  
  #convert to rasters
  peak_abs_reduction <- rasterFromXYZ(peak_abs_reduction)
  seasonal_vpd <- rasterFromXYZ(seasonal_vpd)
  seasonal_temp <- rasterFromXYZ(seasonal_temp)
  seasonal_precip <- rasterFromXYZ(seasonal_precip)
  
  #resample to VPD resolution
  peak_abs_reduction <- resample(peak_abs_reduction,seasonal_vpd)
  seasonal_temp <- resample(seasonal_temp,seasonal_vpd)
  seasonal_precip <- resample(seasonal_precip,seasonal_vpd)
  
  #convert back to dataframe and merge
  peak_abs_reduction <- data.frame(rasterToPoints(peak_abs_reduction))
  seasonal_vpd  <- data.frame(rasterToPoints(seasonal_vpd))
  seasonal_temp  <- data.frame(rasterToPoints(seasonal_temp))
  seasonal_precip  <- data.frame(rasterToPoints(seasonal_precip))
  
  #merge together
  max_total_reduction_vpd <- merge(peak_abs_reduction,
                                   seasonal_vpd,by = c('x','y'))
  
  seasonal_precip_temp <- merge(seasonal_temp,
                                seasonal_precip,by = c('x','y'))
  
  seasonal_climate_ndvi <- merge(max_total_reduction_vpd,seasonal_precip_temp,
                                 by = c('x','y'))
  
  
  
  # if(Ecoregion=='shortgrass_steppe'){
  #   
  #   model_output_sgs
  #   
  # }else{
  #   
  #   max_total_reduction_vpd$ecoregion <- gsub('northern_mixed_prairies','Northern mixed prairies',
  #                                             max_total_reduction_vpd$ecoregion)
  #   max_total_reduction_vpd$ecoregion_2 <- gsub('Northern mixed prairies','b',max_total_reduction_vpd$ecoregion)
  #   
  # }
  
  print(Ecoregion)
  print('vpd')
  print(summary(lm(reduction~abs_change,data=seasonal_climate_ndvi)))
  print('temp')
  print(summary(lm(reduction~abs_change_summer_temperature,data=seasonal_climate_ndvi)))
  print('precip')
  print(summary(lm(reduction~abs_change_summer_precipitation,data=seasonal_climate_ndvi)))
  

  seasonal_climate_ndvi$ecoregion <- Ecoregion
  
  ndvi_climate_list[[i]] <- seasonal_climate_ndvi
  
}


ndvi_climate_df <- list_to_df(ndvi_climate_list)
head(ndvi_climate_df,1)

cor_climate <- ndvi_climate_df %>%
  dplyr::select(reduction,abs_change, abs_change_summer_temperature, abs_change_summer_precipitation)
cor(cor_climate)

summary(lm(reduction ~ abs_change*ecoregion,data=ndvi_climate_df))
summary(lm(reduction ~ abs_change_summer_precipitation*ecoregion,data=ndvi_climate_df))

cor(ndvi_climate_df$abs_change,ndvi_climate_df$abs_change_summer_temperature,
    method = 'spearman')

cor(ndvi_climate_df$abs_change,ndvi_climate_df$abs_change_summer_precipitation,
    method = 'spearman')

0.08/0.39
