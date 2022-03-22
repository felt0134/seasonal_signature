
# key summary statistics


# Overview -----

# range of % reduction in precipitation during drought across sites (from mean)


# median % reduction in precipitation during drought for each ecoregion


# % change in temperature during years of drought


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
#impacts through time ------

#maximum estimated gpp enhancement for NMP (% and absolute), and when this happens (Julian day)

Ecoregion <- 'northern_mixed_prairies'
drought_growth_impact_nmp <- read.csv(paste0('./../../Data/growth_dynamics/drought_gpp_reduction_',Ecoregion,'.csv'))
drought_growth_impact_nmp %>% filter(perc_change == max(drought_growth_impact_nmp$perc_change))
#day 87 68.4%


#maximum GPP reduction for SGS and NMP (% and absolute) and when this happens (day)
drought_growth_impact_nmp %>% filter(perc_change == min(drought_growth_impact_nmp$perc_change))
#day 193 43.9%

Ecoregion <- 'shortgrass_steppe'
drought_growth_impact_sgs <- read.csv(paste0('./../../Data/growth_dynamics/drought_gpp_reduction_',Ecoregion,'.csv'))
drought_growth_impact_sgs %>% filter(perc_change == min(drought_growth_impact_sgs$perc_change))

#look at absolute differences in addition to relative (%) differences


#-------------------------------------------------------------------------------
# climate seasonality -------




