

#get precip from driest year and mean precipitation for all other years ------

ppt_gpp <- readr::read_csv(paste0('./../../Data/GPP/Ecoregion/',Ecoregion,'/ppt_gpp_combined.csv'))
head(ppt_gpp,1)


id_list <- unique(ppt_gpp$id_value)

min_ppt_list <- list()
mean_ppt_list_drought_removed <- list()

for(i in id_list){

gpp_ppt_id <- subset(ppt_gpp, id_value == i)
#gpp_id <- subset(gpp_df, id_value == i)

ppt_id <- aggregate(ppt ~ x + y + id_value + year, sum, data = gpp_ppt_id)

#ID lat/lon values
x <- unique(ppt_id %>% pull(x))
y <- unique(ppt_id %>% pull(y))

#get year with lowest precip
min_ppt <- min(ppt_id$ppt) + 0.1

#subset to years below this value
ppt_id_min  <- ppt_id %>%
  filter(ppt < min_ppt) %>%
  rename('ppt_min' = 'ppt') %>%
  select(x,y,year,ppt_min)

min_ppt_list[[i]] <- ppt_id_min

#subset to years above this value
ppt_id_other <- ppt_id %>%
  filter(ppt > min_ppt) %>%
  select(x,y,year,ppt)

ppt_mean <- aggregate(ppt ~ x + y, mean, data = ppt_id_other)

mean_ppt_list_drought_removed[[i]] <- ppt_id_other


}

#get and save minimum ppt year
min_ppt_df <- data.frame(do.call('rbind',min_ppt_list))
write.csv(min_ppt_df,
          paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/growing_season/drought_precip_year_',Ecoregion,'.csv'))

#get and save mean ppt with driest year removed
mean_ppt_df_drought_removed <- data.frame(do.call('rbind',mean_ppt_list_drought_removed))
write.csv(mean_ppt_df_drought_removed,
          paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/growing_season/mean_precip_drought_removed',Ecoregion,'.csv'))

#get and save mean precipitation
mean_ppt_all_years <- aggregate(ppt ~ x + y + year, sum, data = ppt_gpp)
mean_ppt_all_years <- aggregate(ppt ~ x + y, mean, data = mean_ppt_all_years)
write.csv(mean_ppt_all_years,
          paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/growing_season/mean_precip_',Ecoregion,'.csv'))



#-------------------------------------------------------------------------------
# get temperature from driest year and mean temperature from all other years (to do) ----


#import temperature data

#Ecoregion <- 'shortgrass_steppe'
Ecoregion <- 'northern_mixed_prairies'

filepath_temp <-
  dir(
    paste0(
      './../../Data/Climate/Ecoregion/',
      Ecoregion,
      '/Temperature/tmean/'
    ), full.names = T)

temp_data <- lapply(filepath_temp, format_temp_df) #get 2003 through 2020
temp_data <- list_to_df(temp_data)
#head(temp_data,1)

#import driest year dataframe 
driest_year <- 
  read.csv(paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/growing_season/drought_precip_year_',Ecoregion,'.csv'))
driest_year <- driest_year %>%
  dplyr::select(x,y,year) %>%
  dplyr::rename('drought_year' = 'year')
#head(driest_year,1)

#get ID
driest_year$id_value <- seq.int(nrow(driest_year))
id_list <- unique(driest_year$id_value)

#merge with temp
temp_data <- merge(temp_data,driest_year,by = c('x','y'))
rm(driest_year)

#loop
drought_temp_list <- list()
mean_temp_list_drought_removed <- list()

for(i in id_list){
  
  temp_id <- subset(temp_data, id_value == i)
  
  #ID lat/lon values
  x <- unique(temp_id %>% pull(x))
  y <- unique(temp_id %>% pull(y))
  
  #minimum ppt year
  dry_year <- as.numeric(unique(temp_id$drought_year))
  
  #subset to years below this value
  drought_temp  <- temp_id %>%
    dplyr::filter(year == dry_year) %>%
    dplyr::rename('temp_drought' = 'average_temp') %>%
    dplyr::select(x,y,year,temp_drought)
  
  drought_temp_list[[i]] <- drought_temp
  
  #subset to years above this value
  mean_temp <- temp_id %>%
    dplyr::filter(year != dry_year) %>%
    dplyr::select(x,y,year,average_temp)
  
  mean_temp  <- aggregate(average_temp ~ x + y, mean, data = mean_temp)
  
  mean_temp_list_drought_removed[[i]] <- mean_temp 
  
  
}

rm(mean_temp,drought_temp,temp_id)


#get and save drought year
drought_temp_df <- list_to_df(drought_temp_list)
rm(drought_temp_list)
write.csv(drought_temp_df,
          paste0('./../../Data/Climate/Ecoregion/',Ecoregion,
                 '/Temperature/growing_season/drought_temp_year_',Ecoregion,'.csv'))

rm(drought_temp_df)

#get and save mean ppt with driest year removed
mean_temp_df_drought_removed <- list_to_df(mean_temp_list_drought_removed)
rm(mean_temp_list_drought_removed)
write.csv(mean_temp_df_drought_removed,
          paste0('./../../Data/Climate/Ecoregion/',Ecoregion,
                 '/Temperature/growing_season/mean_temp_drought_removed',Ecoregion,'.csv'))

rm(mean_temp_df_drought_removed)

#get and save mean precipitation
mean_temp_all_years <- aggregate(average_temp ~ x + y, mean, data = temp_data)
write.csv(mean_temp_all_years,
          paste0('./../../Data/Climate/Ecoregion/',Ecoregion,
                 '/Temperature/growing_season/mean_temp_',Ecoregion,'.csv'))

rm(temp_data,mean_temp_all_years)




