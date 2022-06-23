

#master analysis script that calls on other scripts 

#get data and track progress
library(future.apply)
library(progressr)

#pick which ecoregion to runthe script for. You could loop and run each ecoregion
#in one batch but some of these take a while and it may be nice to the output for each.

Ecoregion = 'shortgrass_steppe'
#Ecoregion = 'northern_mixed_prairies' 
#Ecoregion = 'nebraska_sandhills'

# day at which 25%, 50%, and 75% of growth has occurred during average and drought years -----

# generate and save to file estimates of the day of 90% growth and how this
# changes during years of drought


source('day_of_75.R')
source('day_of_50.R')
source('day_of_25.R')


#-------------------------------------------------------------------------------
# growth curve splines -----


source('growth_curve_splines.R')

source('drought_reduction_spline.R')

#-------------------------------------------------------------------------------
# day of maximum gpp ------


source('day_of_max_gpp.R')


#-------------------------------------------------------------------------------
# seasonal climate ------

source('seasonal_precip_temp_analysis.R')

#-------------------------------------------------------------------------------
# 1km subset analyses ------


source('growth_curve_splines_1km_subset.R')

source('drought_reduction_spline_1km_subset.R')


#-------------------------------------------------------------------------------
# NDVI subset analysis ------


source('drought_reduction_spline_NDVI.R')

#-------------------------------------------------------------------------------