

#master analysis script that calls on other scripts 

#get data and track progress
library(future.apply)
library(progressr)

# day at which 25, 50, and 90% of growth has occurred during average and drought years -----

# generate and save to file estimates of the day of 90% growth and how this
# changes during years of drought



#Ecoregion = 'shortgrass_steppe'
Ecoregion = 'northern_mixed_prairies' 
#Ecoregion = 'nebraska_sandhills'



source('day_of_90.R')
source('day_of_50.R')
source('day_of_25.R')


#------------------------
# growth curves -----

#Ecoregion = 'nebraska_sandhills'
#Ecoregion = 'shortgrass_steppe'
#Ecoregion = 'northern_mixed_prairies' 


source('growth_curves.R')

#------------------------
# growth curve splines -----


#Ecoregion = 'nebraska_sandhills'
#Ecoregion = 'shortgrass_steppe'
#Ecoregion = 'northern_mixed_prairies' 

source('growth_curve_splines.R')


#------------------------
# day of maximum gpp ------

Ecoregion = 'shortgrass_steppe'
#Ecoregion = 'northern_mixed_prairies' 
#Ecoregion = 'nebraska_sandhills'



source('day_of_max_gpp.R')


#------------------------
# seasonal climate ------

Ecoregion = 'northern_mixed_prairies'

source('seasonal_precip_temp_analysis.R')

#------------------------
# 1km subset analyses ------


#Ecoregion = 'shortgrass_steppe'
#Ecoregion = 'northern_mixed_prairies' 


source('drought_reduction_spline_1km_subset.R')
source('growth_curve_splines_1km_subset.R')


#------------------------
# NDVI subset analysis ------


#Ecoregion = 'shortgrass_steppe'
Ecoregion = 'northern_mixed_prairies' 
#Ecoregion = 'nebraska_sandhills'


source('drought_reduction_spline_NDVI.R')