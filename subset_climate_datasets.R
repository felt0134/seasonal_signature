

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
ppt_id_other  <- ppt_id %>%
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