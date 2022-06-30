# Project Functions

#http://uop.whoi.edu/UOPinstruments/frodo/aer/julian-day-table.html

#-------------------------------------------------------------------------------
# function to help with converting each GPP raster into a dataframe -----

format_gpp_df <- function(x) {
  #convert to raster
  raster_file <- raster(x)
  
  #extract year from the name of the raster to later add to dataframe
  year_val <- substr(names(raster_file), 5, 8)
  
  #convert to dataframe and add year and period columns
  df <- data.frame(rasterToPoints(raster_file))
  df$year <- year_val
  df$period <-
    gsub(paste0("GPP_", year_val, '_'), '', names(raster_file))
  colnames(df) <- c('x', 'y', 'gpp', 'year', 'period')
  
  #return formatted dataframe
  return(df)
  
  
}
#-------------------------------------------------------------------------------

#function to help with converting NDVI raster into a dataframe ------

format_ndvi_df <- function(x) {
  #convert to raster
  raster_file <- raster(x)
  
  #extract year from the name of the raster to later add to dataframe
  year_val <- substr(names(raster_file), 6, 9)
  
  #convert to dataframe and add year and period columns
  df <- data.frame(rasterToPoints(raster_file))
  #plot(raster_file)
  df$year <- year_val
  df$period <-
    gsub(paste0("NDVI_", year_val, '_'), '', names(raster_file))
  colnames(df) <- c('x', 'y', 'ndvi', 'year', 'period')
  
  #return formatted dataframe
  return(df)
  
  
}


#-------------------------------------------------------------------------------
# function to help with converting each PPT raster into a dataframe (unclear if used) ----

format_ppt_df <- function(x) {
  #convert to raster
  raster_file <- raster(x)
  
  #extract year from the name of the raster
  year_val <- substr(names(raster_file), 8, 11)
  
  #convert to dataframe and add year and period columns
  df <- data.frame(rasterToPoints(raster_file))
  df$year <- year_val
  df$period <-
    gsub(paste0("Precip_", year_val, '_'), '', names(raster_file))
  colnames(df) <- c('x', 'y', 'ppt', 'year', 'period')
  return(df)
  
}

format_temp_df <- function(x) {
  
  #convert to raster
  raster_file <- raster(x)
  
  #extract year from the name of the raster
  year_val <- substr(names(raster_file), 7, 10)
  
  #convert to dataframe and add year and period columns
  df <- data.frame(rasterToPoints(raster_file))
  df$year <- year_val
  df$period <-
    gsub(paste0("Temp_", year_val, '_'), '', names(raster_file))
  colnames(df) <- c('x', 'y', 'average_temp', 'year', 'period')
  return(df)
  
}


#-------------------------------------------------------------------------------
# calculate day of 25%, 50%, and 90% growth (% of cumulative GPP) for all years ------

day_75_gpp_no_drought <- function(i) {
  
  #subset to a given pixel
  growth_id <- ppt_gpp %>%
    dplyr::filter(id_value == i)
  
  #ID lat/lon up front
  x <- unique(growth_id %>% pull(x))
  y <- unique(growth_id %>% pull(y))
  
  ppt_sum <- aggregate(ppt ~ year,sum,data=growth_id)
  colnames(ppt_sum) <- c('year','ppt_sum')
  
  growth_id <- merge(growth_id,ppt_sum,by=c('year'))
  
  #get year with lowest precip
  min_ppt <- min(growth_id$ppt_sum) 
  
  #subset to years above this value
  growth_id  <- growth_id %>%
    filter(ppt_sum > min_ppt)
  
  growth_id <- aggregate(gpp ~ doy, median, data = growth_id)
  
  #for that pixel, get cumulative GPP throughout the year
  growth_id_cmulative <-
    data.frame(growth_id, gpp_2 = cumsum(growth_id$gpp))
  growth_id_cmulative$gpp_3 <-
    100*(growth_id_cmulative$gpp_2 / max(growth_id_cmulative$gpp_2))
  
  rm(growth_id)
  
  #create spline model of growth curve
  gpp.doy.spl <-
    with(growth_id_cmulative, smooth.spline(doy, gpp_3))
  #lines(gpp.doy.spl, col = "blue")
  
  rm(growth_id_cmulative)
  
  #run model through a sequence of days
  doy <- data.frame(seq(from = 65, to = 297, by = 1))
  gpp_predicted <- data.frame(predict(gpp.doy.spl, doy))
  colnames(gpp_predicted) <- c('day', 'gpp_perc')
  
  #get day of year where roughly 75% of cumulative growth has occurred and turn into dataframe
  gpp_predicted_75 <- gpp_predicted %>%
    dplyr::filter(gpp_perc < 75.01)
  
  rm(gpp_predicted)
  
  doy_75 <-
    max(gpp_predicted_75$day) #this is a rough approximation
  
  doy_75_df <- data.frame(doy_75)
  colnames(doy_75_df) <- c('doy_75')
  doy_75_df$x <- x
  doy_75_df$y <- y
  
  doy_75_df <- doy_75_df[c(2, 3, 1)]
  
  return(doy_75_df)
  
  
  
}

#
#

#day of 50% growth
day_50_gpp <- function(i) {
  
  #subset to a given pixel
  growth_id <- gpp_df %>%
    dplyr::filter(id_value == i)
  
  x <- unique(growth_id %>% pull(x))
  y <- unique(growth_id %>% pull(y))
  
  growth_id <- aggregate(gpp ~ doy, mean, data = growth_id)
  
  #for that pixel, get cumulative GPP throughout the year
  growth_id_cmulative <-
    data.frame(growth_id, gpp_2 = cumsum(growth_id$gpp))
  growth_id_cmulative$gpp_3 <-
    100 * (growth_id_cmulative$gpp_2 / max(growth_id_cmulative$gpp_2))
  
  rm(growth_id)
  
  #create spline model of growth curve
  gpp.doy.spl <-
    with(growth_id_cmulative, smooth.spline(doy, gpp_3))
  #lines(gpp.doy.spl, col = "blue")
  
  rm(growth_id_cmulative)
  
  #run model through a sequence of days
  doy <- data.frame(seq(from = 65, to = 297, by = 1))
  gpp_predicted <- data.frame(predict(gpp.doy.spl, doy))
  colnames(gpp_predicted) <- c('day', 'gpp_perc')
  
  #get day of year where roughly 90% of cumulative growth has occurred and turn into dataframe
  gpp_predicted_50 <- gpp_predicted %>%
    dplyr::filter(gpp_perc < 50.1)
  
  rm(gpp_predicted)
  
  doy_50 <-
    max(gpp_predicted_50$day) #this is a rough approximation
  
  doy_50_df <- data.frame(doy_50)
  colnames(doy_50_df) <- c('doy_50')
  doy_50_df$x <- x
  doy_50_df$y <- y
  
  doy_50_df <- doy_50_df[c(2, 3, 1)]
  
  return(doy_50_df)
  
  
  
}

day_50_gpp_no_drought <- function(i) {
  
  #subset to a given pixel
  growth_id <- ppt_gpp %>%
    dplyr::filter(id_value == i)
  
  #ID lat/lon up front
  x <- unique(growth_id %>% pull(x))
  y <- unique(growth_id %>% pull(y))
  
  ppt_sum <- aggregate(ppt ~ year,sum,data=growth_id)
  colnames(ppt_sum) <- c('year','ppt_sum')
  
  growth_id <- merge(growth_id,ppt_sum,by=c('year'))
  
  #get year with lowest precip
  min_ppt <- min(growth_id$ppt_sum) 
  
  #subset to years above this value
  growth_id  <- growth_id %>%
    filter(ppt_sum > min_ppt)
  
  growth_id <- aggregate(gpp ~ doy, median, data = growth_id)
  
  #for that pixel, get cumulative GPP throughout the year
  growth_id_cmulative <-
    data.frame(growth_id, gpp_2 = cumsum(growth_id$gpp))
  growth_id_cmulative$gpp_3 <-
    100*(growth_id_cmulative$gpp_2 / max(growth_id_cmulative$gpp_2))
  
  rm(growth_id)
  
  #create spline model of growth curve
  gpp.doy.spl <-
    with(growth_id_cmulative, smooth.spline(doy, gpp_3))
  #lines(gpp.doy.spl, col = "blue")
  
  rm(growth_id_cmulative)
  
  #run model through a sequence of days
  doy <- data.frame(seq(from = 65, to = 297, by = 1))
  gpp_predicted <- data.frame(predict(gpp.doy.spl, doy))
  colnames(gpp_predicted) <- c('day', 'gpp_perc')
  
  #get day of year where roughly 50% of cumulative growth has occurred and turn into dataframe
  gpp_predicted_50 <- gpp_predicted %>%
    dplyr::filter(gpp_perc < 50.01)
  
  rm(gpp_predicted)
  
  doy_50 <-
    max(gpp_predicted_50$day) #this is a rough approximation
  
  doy_50_df <- data.frame(doy_50)
  colnames(doy_50_df) <- c('doy_50')
  doy_50_df$x <- x
  doy_50_df$y <- y
  
  doy_50_df <- doy_50_df[c(2, 3, 1)]
  
  return(doy_50_df)
  
  
}

#
#

#day of 25% growth
get_25_gpp <- function(i) {
  #subset to a given pixel
  growth_id <- gpp_df %>%
    dplyr::filter(id_value == i)
  
  x <- unique(growth_id %>% pull(x))
  y <- unique(growth_id %>% pull(y))
  
  growth_id <- aggregate(gpp ~ doy, mean, data = growth_id)
  
  #for that pixel, get cumulative GPP throughout the year
  growth_id_cmulative <-
    data.frame(growth_id, gpp_2 = cumsum(growth_id$gpp))
  growth_id_cmulative$gpp_3 <-
    100 * (growth_id_cmulative$gpp_2 / max(growth_id_cmulative$gpp_2))
  
  rm(growth_id)
  
  #create spline model of growth curve
  gpp.doy.spl <-
    with(growth_id_cmulative, smooth.spline(doy, gpp_3))
  #lines(gpp.doy.spl, col = "blue")
  
  rm(growth_id_cmulative)
  
  #run model through a sequence of days
  doy <- data.frame(seq(from = 65, to = 297, by = 1))
  gpp_predicted <- data.frame(predict(gpp.doy.spl, doy))
  colnames(gpp_predicted) <- c('day', 'gpp_perc')
  
  #get day of year where roughly 90% of cumulative growth has occurred and turn into dataframe
  gpp_predicted_25 <- gpp_predicted %>%
    dplyr::filter(gpp_perc < 25.1)
  
  rm(gpp_predicted)
  
  doy_25 <-
    max(gpp_predicted_25$day) #this is a rough approximation
  
  doy_25_df <- data.frame(doy_25)
  colnames(doy_25_df) <- c('doy_25')
  doy_25_df$x <- x
  doy_25_df$y <- y
  
  doy_25_df <- doy_25_df[c(2, 3, 1)]
  
  return(doy_25_df)
  
  
  
}

day_25_gpp_no_drought <- function(i) {
  
  #subset to a given pixel
  growth_id <- ppt_gpp %>%
    dplyr::filter(id_value == i)
  
  #ID lat/lon up front
  x <- unique(growth_id %>% pull(x))
  y <- unique(growth_id %>% pull(y))
  
  ppt_sum <- aggregate(ppt ~ year,sum,data=growth_id)
  colnames(ppt_sum) <- c('year','ppt_sum')
  
  growth_id <- merge(growth_id,ppt_sum,by=c('year'))
  
  #get year with lowest precip
  min_ppt <- min(growth_id$ppt_sum) 
  
  #subset to years above this value
  growth_id  <- growth_id %>%
    filter(ppt_sum > min_ppt)
  
  growth_id <- aggregate(gpp ~ doy, median, data = growth_id)
  
  #for that pixel, get cumulative GPP throughout the year
  growth_id_cmulative <-
    data.frame(growth_id, gpp_2 = cumsum(growth_id$gpp))
  growth_id_cmulative$gpp_3 <-
    100*(growth_id_cmulative$gpp_2 / max(growth_id_cmulative$gpp_2))
  
  rm(growth_id)
  
  #create spline model of growth curve
  gpp.doy.spl <-
    with(growth_id_cmulative, smooth.spline(doy, gpp_3))
  #lines(gpp.doy.spl, col = "blue")
  
  rm(growth_id_cmulative)
  
  #run model through a sequence of days
  doy <- data.frame(seq(from = 65, to = 297, by = 1))
  gpp_predicted <- data.frame(predict(gpp.doy.spl, doy))
  colnames(gpp_predicted) <- c('day', 'gpp_perc')
  
  #get day of year where roughly 25% of cumulative growth has occurred and turn into dataframe
  gpp_predicted_25 <- gpp_predicted %>%
    dplyr::filter(gpp_perc < 25.01)
  
  rm(gpp_predicted)
  
  doy_25 <-
    max(gpp_predicted_25$day) #this is a rough approximation
  
  doy_25_df <- data.frame(doy_25)
  colnames(doy_25_df) <- c('doy_25')
  doy_25_df$x <- x
  doy_25_df$y <- y
  
  doy_25_df <- doy_25_df[c(2, 3, 1)]
  
  return(doy_25_df)
  
  
  
}

#
#


#-------------------------------------------------------------------------------
# calculate day of 25%, 50%, and 75% of total GPP for drought years ------

#75%
day_75_gpp_drought <- function(i) {
  
  growth_id <- ppt_gpp %>%
    dplyr::filter(id_value == i)
  
  #ID lat/lon up front
  x <- unique(growth_id %>% pull(x))
  y <- unique(growth_id %>% pull(y))
  
  ppt_sum <- aggregate(ppt ~ year,sum,data=growth_id)
  colnames(ppt_sum) <- c('year','ppt_sum')
  
  growth_id <- merge(growth_id,ppt_sum,by=c('year'))
  
  
  #get year with lowest precip
  min_ppt <- min(growth_id$ppt_sum) + 0.1
  
  #subset to years above this value
  growth_id  <- growth_id %>%
    filter(ppt_sum < min_ppt)
  
  growth_id <- growth_id %>%
    arrange(doy)
  
  #growth_id <- aggregate(gpp ~ doy, median, data = growth_id)
  
  #for that pixel, get cumulative GPP throughout the year
  growth_id<- data.frame(growth_id, gpp_2 = cumsum(growth_id$gpp))
  growth_id$gpp_3 <- 100*(growth_id$gpp_2 / max(growth_id$gpp_2))
  
  #reduce data size by selecting specific columns
  growth_id <- growth_id %>%
    dplyr::select(c('doy', 'gpp_3'))
  
  #create spline model of growth curve
  gpp.doy.drought.spl <- with(growth_id, smooth.spline(doy, gpp_3))
  #lines(gpp.doy.spl, col = "blue")
  
  #run model through a sequence of days
  doy <- data.frame(seq(from = 65, to = 297, by = 1))
  
  gpp_drought_predicted <-
    data.frame(predict(gpp.doy.drought.spl, doy))
  colnames(gpp_drought_predicted) <- c('day', 'gpp_perc')
  
  rm(growth_id, doy, gpp.doy.drought.spl)
  
  #get day of year where roughly 90% of cumulative growth has occurred and turn into dataframe
  gpp_drought_predicted_75 <- gpp_drought_predicted %>%
    dplyr::filter(gpp_perc < 75.1)
  
  doy_75 <-
    max(gpp_drought_predicted_75$day) #this is a rough approximation
  
  rm(gpp_drought_predicted_75, gpp_drought_predicted)
  
  doy_75_df <- data.frame(doy_75)
  colnames(doy_75_df) <- c('doy_75_drought')
  doy_75_df$x <- x
  doy_75_df$y <- y
  
  doy_75_df <- doy_75_df[c(2, 3, 1)] #re-order to x,y,z
  
  return(doy_75_df)
  
  
}

#
#

#50%
day_50_gpp_drought <- function(i) {
  
  growth_id <- ppt_gpp %>%
    dplyr::filter(id_value == i)
  
  #ID lat/lon up front
  x <- unique(growth_id %>% pull(x))
  y <- unique(growth_id %>% pull(y))
  
  ppt_sum <- aggregate(ppt ~ year,sum,data=growth_id)
  colnames(ppt_sum) <- c('year','ppt_sum')
  
  growth_id <- merge(growth_id,ppt_sum,by=c('year'))
  
  
  #get year with lowest precip
  min_ppt <- min(growth_id$ppt_sum) + 0.1
  
  #subset to years above this value
  growth_id  <- growth_id %>%
    filter(ppt_sum < min_ppt)
  
  growth_id <- growth_id %>%
    arrange(doy)
  
 # growth_id <- aggregate(gpp ~ doy, median, data = growth_id)
  
  #for that pixel, get cumulative GPP throughout the year
  growth_id <- data.frame(growth_id, gpp_2 = cumsum(growth_id$gpp))
  growth_id$gpp_3 <- 100*(growth_id$gpp_2 / max(growth_id$gpp_2))
  
  #reduce data size by selecting specific columns
  growth_id <- growth_id %>%
    dplyr::select(c('doy', 'gpp_3'))
  
  #create spline model of growth curve
  gpp.doy.drought.spl <- with(growth_id, smooth.spline(doy, gpp_3))
  #lines(gpp.doy.spl, col = "blue")
  
  #run model through a sequence of days
  doy <- data.frame(seq(from = 65, to = 297, by = 1))
  
  gpp_drought_predicted <-
    data.frame(predict(gpp.doy.drought.spl, doy))
  colnames(gpp_drought_predicted) <- c('day', 'gpp_perc')
  
  rm(growth_id, doy, gpp.doy.drought.spl)
  
  #get day of year where roughly 90% of cumulative growth has occurred and turn into dataframe
  gpp_drought_predicted_50 <- gpp_drought_predicted %>%
    dplyr::filter(gpp_perc < 50.1)
  
  doy_50 <-
    max(gpp_drought_predicted_50$day) #this is a rough approximation
  
  rm(gpp_drought_predicted_50, gpp_drought_predicted)
  
  doy_50_df <- data.frame(doy_50)
  colnames(doy_50_df) <- c('doy_50_drought')
  doy_50_df$x <- x
  doy_50_df$y <- y
  
  doy_50_df <- doy_50_df[c(2, 3, 1)] #re-order to x,y,z
  
  return(doy_50_df)
  
  
}

#
#

#25%
day_25_gpp_drought <- function(i) {
  
  growth_id <- ppt_gpp %>%
    dplyr::filter(id_value == i)
  
  #ID lat/lon up front
  x <- unique(growth_id %>% pull(x))
  y <- unique(growth_id %>% pull(y))
  
  ppt_sum <- aggregate(ppt ~ year,sum,data=growth_id)
  colnames(ppt_sum) <- c('year','ppt_sum')
  
  growth_id <- merge(growth_id,ppt_sum,by=c('year'))
  
  
  #get year with lowest precip
  min_ppt <- min(growth_id$ppt_sum) + 0.1
  
  #subset to years above this value
  growth_id  <- growth_id %>%
    filter(ppt_sum < min_ppt)
  
  growth_id <- growth_id %>%
    arrange(doy)
  
  #growth_id <- aggregate(gpp ~ doy, median, data = growth_id)
  
  #for that pixel, get cumulative GPP throughout the year
  growth_id<- data.frame(growth_id, gpp_2 = cumsum(growth_id$gpp))
  growth_id$gpp_3 <- 100*(growth_id$gpp_2 / max(growth_id$gpp_2))
  
  #reduce data size by selecting specific columns
  growth_id <- growth_id %>%
    dplyr::select(c('doy', 'gpp_3'))
  
  #create spline model of growth curve
  gpp.doy.drought.spl <- with(growth_id, smooth.spline(doy, gpp_3))
  #lines(gpp.doy.spl, col = "blue")
  
  #run model through a sequence of days
  doy <- data.frame(seq(from = 65, to = 297, by = 1))
  
  gpp_drought_predicted <-
    data.frame(predict(gpp.doy.drought.spl, doy))
  colnames(gpp_drought_predicted) <- c('day', 'gpp_perc')
  
  rm(growth_id, doy, gpp.doy.drought.spl)
  
  #get day of year where roughly 90% of cumulative growth has occurred and turn into dataframe
  gpp_drought_predicted_25 <- gpp_drought_predicted %>%
    dplyr::filter(gpp_perc < 25.1)
  
  doy_25 <-
    max(gpp_drought_predicted_25$day) #this is a rough approximation
  
  rm(gpp_drought_predicted_25, gpp_drought_predicted)
  
  doy_25_df <- data.frame(doy_25)
  colnames(doy_25_df) <- c('doy_25_drought')
  doy_25_df$x <- x
  doy_25_df$y <- y
  
  doy_25_df <- doy_25_df[c(2, 3, 1)] #re-order to x,y,z
  
  return(doy_25_df)
  
  
}

#
#

#-------------------------------------------------------------------------------
# turn a list of the dataframes with same columns into one single dataframe ----


list_to_df <- function(x) {
  df <- data.frame(do.call("rbind", x))
  
  return(df)
  
  
}



#-------------------------------------------------------------------------------
# import and format temp raster into dataframe (unclear if used) ------

#format the temp rasters
# format_temp_df <- function(x, max_min) {
#   #test out what will be the 'format_temp_df' function
#   #convert to raster
#   raster_file <- raster(x)
#   
#   #extract year from the name of the raster to later add to dataframe
#   if (max_min == T) {
#     year_val <- substr(names(raster_file), 6, 9)
#   } else{
#     year_val <- substr(names(raster_file), 7, 10)
#   }
#   
#   #convert to dataframe and add year and period columns
#   df <- data.frame(rasterToPoints(raster_file))
#   df$year <- year_val
#   colnames(df) <- c('x', 'y', 'temp', 'year')
#   
#   #return formatted dataframe
#   return(df)
#   
#   
# }

#import the temp rasters from file, using the format function
import_temp <- function(Ecoregion, temp, value) {
  #i = 2003
  
  filepath_2 <-
    dir(
      paste0(
        './../../Data/Climate/Ecoregion/',
        Ecoregion,
        '/Temperature/',
        temp,
        '/'
      ),
      full.names = T
    )
  if (value == T) {
    test <- lapply(filepath_2, format_temp_df, max_min = T)
  } else{
    test <- lapply(filepath_2, format_temp_df, max_min = F)
  }
  
  test <- list_to_df(test)
  test$ecoregion <- Ecoregion
  test$temp_var <- temp
  
  return(test)
  
  
}

#-------------------------------------------------------------------------------
# growth curves ------

#relative (% of growth)
get_average_growth_curve  <- function(i) {
  
  #subset to a given pixel
  growth_id <- gpp_df %>%
    dplyr::filter(id_value == i)
  
  x <- unique(growth_id %>% pull(x))
  y <- unique(growth_id %>% pull(y))
  
  growth_id <- aggregate(gpp ~ doy, mean, data = growth_id)
  #plot(gpp~doy,growth_id)
  
  #for that pixel, get cumulative GPP throughout the year
  growth_id_cmulative <-
    data.frame(growth_id, gpp_2 = cumsum(growth_id$gpp))
  growth_id_cmulative$gpp_3 <-
    100 * (growth_id_cmulative$gpp_2 / max(growth_id_cmulative$gpp_2))
  #plot(gpp_3 ~ doy,data=growth_id_cmulative)
  
  rm(growth_id)
  
  #create spline model of growth curve
  gpp.doy.spl <-
    with(growth_id_cmulative, smooth.spline(doy, gpp_3))
  #lines(gpp.doy.spl, col = "blue")
  
  rm(growth_id_cmulative)
  
  #run model through a sequence of days
  doy <- data.frame(seq(from = 65, to = 297, by = 1))
  gpp_predicted <- data.frame(predict(gpp.doy.spl, doy))
  colnames(gpp_predicted) <- c('day', 'gpp_perc')
  #plot(gpp_perc ~ day,gpp_predicted)
  
  #get day of year where roughly 25% of cumulative growth has occurred and turn into dataframe
  # gpp_predicted_90 <- gpp_predicted %>%
  #   dplyr::filter(gpp_perc < 25.1)
  
  #rm(gpp_predicted)
  
  gpp_predicted$x <- x
  gpp_predicted$y <- y
  
  gpp_predicted <- gpp_predicted %>%
    select(x,y,day,gpp_perc)
  
  
  return(gpp_predicted)
  
  
  
}

#

#relative (% of growth)
get_drought_growth_curve <- function(i) {
  
  ppt_id <- subset(ppt_gpp, id_value == i)
  gpp_id <- subset(gpp_df, id_value == i)
  
  ppt_id <- aggregate(ppt ~ x + y + id_value + year, sum, data = ppt_id)
  
  x <- unique(ppt_id %>% pull(x))
  y <- unique(ppt_id %>% pull(y))
  
  #get year with lowest precip
  quantile_25 <- min(ppt_id$ppt) + 0.1
  
  #subset to years below this value
  ppt_id  <- ppt_id %>%
    filter(ppt < quantile_25)
  
  ppt_id  <- merge(ppt_id, gpp_id, by = c('x', 'y', 'id_value', 'year'))
  
  ppt_id  <- aggregate(gpp ~ doy, mean, data = ppt_id)
  
  #for that pixel, get cumulative GPP throughout the year
  ppt_id <- data.frame(ppt_id, gpp_2 = cumsum(ppt_id$gpp))
  ppt_id$gpp_3 <- 100 * (ppt_id$gpp_2 / max(ppt_id$gpp_2))
  
  #reduce data size by selecting specific columns
  ppt_id <- ppt_id %>%
    select(c('doy', 'gpp_3'))
  
  #create spline model of growth curve
  gpp.doy.drought.spl <- with(ppt_id, smooth.spline(doy, gpp_3))
  #lines(gpp.doy.spl, col = "blue")
  
  #run model through a sequence of days
  doy <- data.frame(seq(from = 65, to = 297, by = 1))
  gpp_drought_predicted <-
    data.frame(predict(gpp.doy.drought.spl, doy))
  colnames(gpp_drought_predicted) <- c('day', 'gpp_perc')
  
  rm(ppt_id, doy, gpp.doy.drought.spl)
  
  gpp_drought_predicted$x <- x
  gpp_drought_predicted$y <- y
  
  gpp_drought_predicted <- gpp_drought_predicted %>%
    select(x,y,day,gpp_perc)
  
  
  return(gpp_drought_predicted)
  
  
}

#

#absolute (% of growth) (maybe delete june 2022)
# get_average_growth_curve_absolute  <- function(i) {
#   
#   #subset to a given pixel
#   growth_id <- gpp_df %>%
#     dplyr::filter(id_value == i)
#   
#   x <- unique(growth_id %>% pull(x))
#   y <- unique(growth_id %>% pull(y))
#   
#   growth_id <- aggregate(gpp ~ doy, mean, data = growth_id)
#   #plot(gpp~doy,growth_id)
#   
#   #for that pixel, get cumulative GPP throughout the year
#   growth_id_cmulative <-
#     data.frame(growth_id, gpp_2 = cumsum(growth_id$gpp))
#   
#   head(growth_id_cmulative)
#   #plot(gpp_2 ~ doy,data= growth_id_cmulative)
#   
#   rm(growth_id)
#   
#   #create spline model of growth curve
#   gpp.doy.spl <-
#     with(growth_id_cmulative, smooth.spline(doy, gpp_2))
#   #lines(gpp.doy.spl, col = "blue")
#   
#   rm(growth_id_cmulative)
#   
#   #run model through a sequence of days
#   doy <- data.frame(seq(from = 65, to = 297, by = 1))
#   gpp_predicted <- data.frame(predict(gpp.doy.spl, doy))
#   colnames(gpp_predicted) <- c('day', 'gpp')
#   #plot(gpp ~ day,gpp_predicted)
#   
#   #rm(gpp_predicted)
#   
#   gpp_predicted$x <- x
#   gpp_predicted$y <- y
#   
#   gpp_predicted <- gpp_predicted %>%
#     select(x,y,day,gpp)
#   
#   
#   return(gpp_predicted)
#   
#   
#   
# }

#

#absolute (% of growth) (maybe delete - June 2022)
# get_drought_growth_curve_absolute <- function(i) {
#   
#   ppt_id <- subset(ppt_gpp, id_value == i)
#   gpp_id <- subset(gpp_df, id_value == i)
#   
#   ppt_id <- aggregate(ppt ~ x + y + id_value + year, sum, data = ppt_id)
#   
#   x <- unique(ppt_id %>% pull(x))
#   y <- unique(ppt_id %>% pull(y))
#   
#   #get year with lowest precip
#   quantile_25 <- min(ppt_id$ppt) + 0.1
#   
#   #subset to years below this value
#   ppt_id  <- ppt_id %>%
#     filter(ppt < quantile_25)
#   
#   ppt_id  <- merge(ppt_id, gpp_id, by = c('x', 'y', 'id_value', 'year'))
#   
#   ppt_id  <- aggregate(gpp ~ doy, mean, data = ppt_id)
#   
#   #for that pixel, get cumulative GPP throughout the year
#   ppt_id <- data.frame(ppt_id, gpp_2 = cumsum(ppt_id$gpp))
#   
#   #reduce data size by selecting specific columns
#   ppt_id <- ppt_id %>%
#     select(c('doy', 'gpp_2'))
#   
#   #create spline model of growth curve
#   gpp.doy.drought.spl <- with(ppt_id, smooth.spline(doy, gpp_2))
#   #lines(gpp.doy.drought.spl, col = "blue")
#   
#   #run model through a sequence of days
#   doy <- data.frame(seq(from = 65, to = 297, by = 1))
#   gpp_drought_predicted <-
#     data.frame(predict(gpp.doy.drought.spl, doy))
#   colnames(gpp_drought_predicted) <- c('day', 'gpp')
#   
#   rm(ppt_id, doy, gpp.doy.drought.spl)
#   
#   gpp_drought_predicted$x <- x
#   gpp_drought_predicted$y <- y
#   
#   gpp_drought_predicted <- gpp_drought_predicted %>%
#     select(x,y,day,gpp)
#   
#   
#   return(gpp_drought_predicted)
#   
#   
# }

#

#spline model of growth during average year (maybe delete - June 2022)
# get_average_growth_curve_absolute_spline  <- function(i) {
#   
#   #subset to a given pixel
#   growth_id <- gpp_df %>%
#     dplyr::filter(id_value == i)
#   
#   x <- unique(growth_id %>% pull(x))
#   y <- unique(growth_id %>% pull(y))
#   
#   growth_id <- aggregate(gpp ~ doy, mean, data = growth_id)
#   #plot(gpp~doy,growth_id)
#   
#   #for that pixel, get cumulative GPP throughout the year
#   growth_id_cmulative <-
#     data.frame(growth_id, gpp_2 = cumsum(growth_id$gpp))
#   
#   head(growth_id_cmulative)
#   #plot(gpp_2 ~ doy,data= growth_id_cmulative)
#   
#   rm(growth_id)
#   
#   #create spline model of growth curve
#   gpp.doy.spl <-
#     with(growth_id_cmulative, smooth.spline(doy, gpp_2))
#   #lines(gpp.doy.spl, col = "blue")
#   
#   rm(growth_id_cmulative)
#   
#   return(gpp.doy.spl)
#   
#   
#   
# }



#spline models of both average gpp and the temporal CI (updated june 2022)
get_average_growth_curve_absolute_spline_ci  <- function(i) {
  
  #subset to a given pixel
  growth_id <- ppt_gpp %>%
    dplyr::filter(id_value == i)
  
  #ID lat/lon up front
  x <- unique(growth_id %>% pull(x))
  y <- unique(growth_id %>% pull(y))
  
  ppt_sum <- aggregate(ppt ~ year,sum,data=growth_id)
  colnames(ppt_sum) <- c('year','ppt_sum')
  
  growth_id <- merge(growth_id,ppt_sum,by=c('year'))
  

  #get year with lowest precip
  min_ppt <- min(growth_id$ppt_sum) 
  
  #subset to years above this value
  growth_id  <- growth_id %>%
    filter(ppt_sum > min_ppt)
  
  #unique(growth_id$year) #works
  
  #first get temporal variation
  year_list <- list()
  for(j in unique(growth_id$year)){
    
    subset <- subset(growth_id,year==j)
    
    subset <- subset %>% arrange(doy)
    
    subset <-
      data.frame(subset, gpp_ci = cumsum(subset$gpp))
    
    year_list[[j]] <- subset
    
    #str(subset)
    
  }
  
  year_df <- do.call('rbind',year_list)
  #plot(gpp_2~doy,data=year_df)
  rm(year_list)
  
  #sd approach
  #growth_id_ci <- aggregate(gpp_ci ~ doy, sd, data = year_df)
  
  #IQR approach
  growth_id_ci_75 <- aggregate(gpp_ci ~ doy, iqr_75, data = year_df)
  growth_id_ci_25 <- aggregate(gpp_ci ~ doy, iqr_25, data = year_df)
  
  rm(year_df)
  
  #iqr 75
   gpp_doy_spl_ci_75 <-
    with(growth_id_ci_75, smooth.spline(doy, gpp_ci))
   
   #iqr 25
   gpp_doy_spl_ci_25 <-
     with(growth_id_ci_25, smooth.spline(doy, gpp_ci))
  
  
  #now do average growth curve
  growth_id <- aggregate(gpp ~ doy, median, data = growth_id)
  #plot(gpp~doy,growth_id)
  
  #for that pixel, get cumulative GPP throughout the year
  growth_id_cmulative <-
    data.frame(growth_id, gpp_2 = cumsum(growth_id$gpp))
  
  #create spline model of growth curve
  gpp_doy_spl <-
    with(growth_id_cmulative, smooth.spline(doy, gpp_2))

  
  return(list(gpp_doy_spl,gpp_doy_spl_ci_25,gpp_doy_spl_ci_75))
  
  
}

#
#

#spline model of gpp during drought year (updated june 2022)
get_drought_growth_curve_absolute_spline <- function(i) {
  
  gpp_ppt_id <- subset(ppt_gpp, id_value == i)
  #gpp_id <- subset(gpp_df, id_value == i)
  
  ppt_id <- aggregate(ppt ~ x + y + id_value + year, sum, data = gpp_ppt_id)
  
  #ID lat/lon values
  x <- unique(ppt_id %>% pull(x))
  y <- unique(ppt_id %>% pull(y))
  
  #get year with lowest precip
  min_ppt <- min(ppt_id$ppt) + 0.1
  
  #subset to years below this value
  ppt_id  <- ppt_id %>%
    filter(ppt < min_ppt) %>%
    rename('ppt_min' = 'ppt')

  gpp_ppt_id  <- merge(ppt_id, gpp_ppt_id, by = c('x', 'y', 'id_value', 'year'))
  gpp_ppt_id <- gpp_ppt_id %>%
    arrange(doy)
  
  #gpp_ppt_id  <- aggregate(gpp ~ doy, median, data = gpp_ppt_id) #this messes with daily estimates
  
  #for that pixel, get cumulative GPP throughout the year
  gpp_ppt_id <- data.frame(gpp_ppt_id, gpp_2 = cumsum(gpp_ppt_id$gpp))
  
  #reduce data size by selecting specific columns
  gpp_ppt_id <- gpp_ppt_id %>%
    dplyr::select(c('doy', 'gpp_2'))
  
  #create spline model of growth curve
  gpp_doy_drought_spl <- with(gpp_ppt_id, smooth.spline(doy, gpp_2))
  #lines(gpp.doy.drought.spl, col = "blue")
  
  
  return(gpp_doy_drought_spl)
  
  
}



#-------------------------------------------------------------------------------
# maximum production (unclear if used) ------

#day of max production during a typical year
get_average_max_production  <- function(i) {
  
  #subset to a given pixel
  growth_id <- gpp_df %>%
    dplyr::filter(id_value == i)
  
  x <- unique(growth_id %>% pull(x))
  y <- unique(growth_id %>% pull(y))
  
  growth_id <- aggregate(gpp ~ doy, mean, data = growth_id)
  #plot(gpp~doy,growth_id)
  
  #create spline model of growth curve
  gpp.doy.spl <-
    with(growth_id, smooth.spline(doy, gpp))
  #lines(gpp.doy.spl, col = "blue")
  
  
  #run model through a sequence of days
  doy <- data.frame(seq(from = 65, to = 297, by = 1))
  gpp_predicted <- data.frame(predict(gpp.doy.spl, doy))
  colnames(gpp_predicted) <- c('day', 'gpp')
  #plot(gpp ~ day,gpp_predicted)
  
  max_gpp <- max(gpp_predicted$gpp)
  
  #get day of year when maximum GPP ocurrs
  gpp_predicted <- gpp_predicted %>%
    dplyr::filter(gpp == max_gpp)
  
  #rm(gpp_predicted)
  
  gpp_predicted$x <- x
  gpp_predicted$y <- y
  
  gpp_predicted <- gpp_predicted %>%
    select(x,y,day)
  
  
  return(gpp_predicted)
  
  
  
}
#

#day of max production during a drought year
get_drought_max_production <- function(i) {
  
  ppt_id <- subset(ppt_gpp, id_value == i)
  gpp_id <- subset(gpp_df, id_value == i)
  
  ppt_id <- aggregate(ppt ~ x + y + id_value + year, sum, data = ppt_id)
  
  x <- unique(ppt_id %>% pull(x))
  y <- unique(ppt_id %>% pull(y))
  
  #get year with lowest precip
  quantile_25 <- min(ppt_id$ppt) + 0.1
  
  #subset to years below this value
  ppt_id  <- ppt_id %>%
    filter(ppt < quantile_25)
  
  ppt_id  <- merge(ppt_id, gpp_id, by = c('x', 'y', 'id_value', 'year'))
  
  ppt_id  <- aggregate(gpp ~ doy, mean, data = ppt_id)
  
  
  #reduce data size by selecting specific columns
  ppt_id <- ppt_id %>%
    select(c('doy', 'gpp'))
  
  #create spline model of growth curve
  gpp.doy.drought.spl <- with(ppt_id, smooth.spline(doy, gpp))
  #lines(gpp.doy.spl, col = "blue")
  
  #run model through a sequence of days
  doy <- data.frame(seq(from = 65, to = 297, by = 1))
  gpp_drought_predicted <-
    data.frame(predict(gpp.doy.drought.spl, doy))
  colnames(gpp_drought_predicted) <- c('day', 'gpp')
  
  rm(ppt_id, doy, gpp.doy.drought.spl)
  
  max_gpp <- max(gpp_drought_predicted$gpp)
  
  #get day of year when maximum GPP ocurrs
  gpp_drought_predicted <- gpp_drought_predicted %>%
    dplyr::filter(gpp == max_gpp)
  
  gpp_drought_predicted$x <- x
  gpp_drought_predicted$y <- y
  
  gpp_drought_predicted <- gpp_drought_predicted %>%
    select(x,y,day)
  
  
  return(gpp_drought_predicted)
  
  
}

#-------------------------------------------------------------------------------

# % reduction in maximum production during drought (unclear if used) -------


get_drought_pecent_reduction <- function(i) {
  
  ppt_id <- subset(ppt_gpp, id_value == i)
  gpp_id <- subset(gpp_df, id_value == i)
  
  ppt_id <- aggregate(ppt ~ x + y + id_value + year, sum, data = ppt_id)
  
  x <- unique(ppt_id %>% pull(x))
  y <- unique(ppt_id %>% pull(y))
  
  #Get max gpp during years of drought:
  
  #get year with lowest precip
  quantile_25 <- min(ppt_id$ppt) + 0.1
  
  #subset to years below this value
  ppt_id_drought  <- ppt_id %>%
    filter(ppt < quantile_25)
  
  ppt_id_drought  <- merge(ppt_id_drought, gpp_id, by = c('x', 'y', 'id_value', 'year'))
  
  ppt_id_drought  <- aggregate(gpp ~ doy, mean, data = ppt_id_drought)
  #head(ppt_id_drought,1)
  
  #reduce data size by selecting specific columns
  ppt_id_drought <- ppt_id_drought %>%
    select(c('doy', 'gpp'))
  
  #create spline model of growth curve
  gpp.doy.drought.spl <- with(ppt_id_drought, smooth.spline(doy, gpp))
  #lines(gpp.doy.spl, col = "blue")
  
  #run model through a sequence of days
  doy <- data.frame(seq(from = 65, to = 297, by = 1))
  gpp_drought_predicted <-
    data.frame(predict(gpp.doy.drought.spl, doy))
  colnames(gpp_drought_predicted) <- c('day', 'gpp_drought')
  
  max_gpp_drought <- max(gpp_drought_predicted$gpp)
  
  #get day of year when maximum GPP ocurrs
  gpp_drought_predicted <- gpp_drought_predicted %>%
    dplyr::filter(gpp_drought == max_gpp_drought)
  
  gpp_drought_predicted$x <- x
  gpp_drought_predicted$y <- y
  
  gpp_drought_predicted <- gpp_drought_predicted %>%
    select(x,y,gpp_drought)
  
  #now do typical maximum production:
  
  ppt_id  <- merge(ppt_id, gpp_id, by = c('x', 'y', 'id_value', 'year'))
  
  ppt_id  <- aggregate(gpp ~ doy, mean, data = ppt_id)
  #head(ppt_id_drought,1)
  
  #reduce data size by selecting specific columns
  ppt_id <- ppt_id %>%
    select(c('doy', 'gpp'))
  
  #create spline model of growth curve
  gpp.doy.spl <- with(ppt_id, smooth.spline(doy, gpp))
  #lines(gpp.doy.spl, col = "blue")
  
  #run model through a sequence of days
  doy <- data.frame(seq(from = 65, to = 297, by = 1))
  gpp_predicted <-
    data.frame(predict(gpp.doy.spl, doy))
  colnames(gpp_predicted) <- c('day', 'gpp_average')
  
  max_gpp <- max(gpp_predicted$gpp)
  
  #get day of year when maximum GPP ocurrs
  gpp_predicted <- gpp_predicted %>%
    dplyr::filter(gpp_average == max_gpp)
  
  gpp_predicted$x <- x
  gpp_predicted$y <- y
  
  gpp_predicted <- gpp_predicted %>%
    select(x,y,gpp_average)
  
  gpp_predicted_2 <- merge(gpp_predicted,gpp_drought_predicted,by=c('x','y'))
  gpp_predicted_2$perc_reduction <- 
    round(((gpp_predicted_2$gpp_average - gpp_predicted_2$gpp_drought)/gpp_predicted_2$gpp_average)*100,2)
  
  rm(ppt_id, doy, gpp.doy.drought.spl,ppt_id_drought)
  
  gpp_predicted_2 <- gpp_predicted_2 %>%
    select(x,y,perc_reduction)
  
  return(gpp_predicted_2)
  
  
}



#-------------------------------------------------------------------------------
# get the years of drought and their precipitation ----


get_drought_years <- function(i) {
  
  ppt_id <- subset(ppt_gpp, id_value == i)
  gpp_id <- subset(gpp_df, id_value == i)
  
  ppt_id <- aggregate(ppt ~ x + y + id_value + year, sum, data = ppt_id)
  
  x <- unique(ppt_id %>% pull(x))
  y <- unique(ppt_id %>% pull(y))
  
  #Get max gpp during years of drought:
  
  #get year with lowest precip
  quantile_25 <- min(ppt_id$ppt) + 0.1
  
  #subset to years below this value
  ppt_id_drought  <- ppt_id %>%
    filter(ppt < quantile_25) %>%
    select(x,y,year,ppt)
  
  return(ppt_id_drought)
  
  
}



#-------------------------------------------------------------------------------
# GPP splines ------

#16-day cumulative GPP throughout growing season (not the cumulative total GPP growth curve)

# updated June 2022
gpp_spline  <- function(i) {
  
  #subset to a given pixel
  growth_id <- ppt_gpp %>%
    dplyr::filter(id_value == i)
  
  #ID lat/lon up front
  x <- unique(growth_id %>% pull(x))
  y <- unique(growth_id %>% pull(y))
  
  #get total precip
  ppt_sum <- aggregate(ppt ~ year,sum,data=growth_id)
  colnames(ppt_sum) <- c('year','ppt_sum')
  
  growth_id <- merge(growth_id,ppt_sum,by=c('year'))
  
  #get year with lowest precip
  min_ppt <- min(growth_id$ppt_sum) 
  
  #subset to years above this value
  growth_id  <- growth_id %>%
    filter(ppt_sum > min_ppt)
  
  #average
  growth_id <- aggregate(gpp ~ doy, median, data = growth_id)
  #plot(gpp~doy,growth_id)
  
  #create spline model of growth curve
  gpp_doy_spl <-
    with(growth_id, smooth.spline(doy, gpp))
  #lines(gpp_doy_spl, col = "blue")
  
  return(gpp_doy_spl)
  
  
}

# updated June 2022
gpp_spline_drought <- function(i) {
  
  growth_id <- ppt_gpp %>%
    dplyr::filter(id_value == i)
  
  #ID lat/lon up front
  x <- unique(growth_id %>% pull(x))
  y <- unique(growth_id %>% pull(y))
  
  #get total precip
  ppt_sum <- aggregate(ppt ~ year,sum,data=growth_id)
  colnames(ppt_sum) <- c('year','ppt_sum')
  
  growth_id <- merge(growth_id,ppt_sum,by=c('year'))
  
  #get year with lowest precip
  min_ppt <- min(growth_id$ppt_sum) + 0.1
  
  #subset to years above this value
  growth_id  <- growth_id %>%
    filter(ppt_sum < min_ppt)
  
  #create spline model of growth curve
  gpp_doy_drought_spl <- with(growth_id, smooth.spline(doy, gpp))
  plot(gpp ~ doy, data = growth_id)
  #lines(gpp_doy_drought_spl, col = "red")
  
  
  return(gpp_doy_drought_spl)
  
  
}

#-------------------------------------------------------------------------------

#NDVI spline  ------

#16-day NDVI throughout growing season 

#average year
ndvi_spline  <- function(i) {
  
  #subset to a given pixel
  growth_id <- ndvi_df %>%
    dplyr::filter(id_value == i)
  
  x <- unique(growth_id %>% pull(x))
  y <- unique(growth_id %>% pull(y))
  
  growth_id <- aggregate(ndvi ~ doy, mean, data = growth_id)
  #plot(gpp~doy,growth_id,ylim=c(0,30))
  
  #create spline model of growth curve
  ndvi.doy.spl <-
    with(growth_id, smooth.spline(doy, ndvi))
  #lines(gpp.doy.spl, col = "blue")
  
  return(ndvi.doy.spl)
  
  
}

#updated June 2022
ndvi_spline_2  <- function(i) {
  
  #subset to a given pixel
  growth_id <- ppt_ndvi %>%
    dplyr::filter(id_value == i)
  
  #ID lat/lon up front
  x <- unique(growth_id %>% pull(x))
  y <- unique(growth_id %>% pull(y))
  
  #get total precip
  ppt_sum <- aggregate(ndvi ~ year,sum,data=growth_id)
  colnames(ppt_sum) <- c('year','ppt_sum')
  
  growth_id <- merge(growth_id,ppt_sum,by=c('year'))
  
  #get year with lowest precip
  min_ppt <- min(growth_id$ppt_sum) 
  
  #subset to years above this value
  growth_id  <- growth_id %>%
    filter(ppt_sum > min_ppt)
  
  #average
  growth_id <- aggregate(ndvi ~ doy, median, data = growth_id)
  #plot(gpp~doy,growth_id)
  
  #create spline model of growth curve
  ndvi_doy_spl <-
    with(growth_id, smooth.spline(doy, ndvi))
  #lines(gpp_doy_spl, col = "blue")
  
  return(ndvi_doy_spl)
  
  
}

#
#

#drought year
ndvi_spline_drought <- function(i) {
  
  ppt_id <- subset(ppt_ndvi, id_value == i)
  gpp_id <- subset(ndvi_df, id_value == i)
  
  ppt_id <- aggregate(ppt ~ x + y + id_value + year, sum, data = ppt_id)
  
  x <- unique(ppt_id %>% pull(x))
  y <- unique(ppt_id %>% pull(y))
  
  #get year with lowest precip
  quantile_25 <- min(ppt_id$ppt) + 0.1
  
  #subset to years below this value
  ppt_id  <- ppt_id %>%
    filter(ppt < quantile_25)
  
  ppt_id  <- merge(ppt_id, gpp_id, by = c('x', 'y', 'id_value', 'year'))
  
  ppt_id  <- aggregate(ndvi ~ doy, mean, data = ppt_id)
  
  
  #create spline model of growth curve
  ndvi.doy.drought.spl <- with(ppt_id, smooth.spline(doy, ndvi))
  plot(ndvi ~ doy, data = ppt_id)
  #lines(gpp.doy.drought.spl, col = "red")
  
  
  return(ndvi.doy.drought.spl)
  
  
}

#updated June 2022
ndvi_spline_drought_2 <- function(i){
  
  growth_id <- ppt_ndvi %>%
    dplyr::filter(id_value == i)
  
  #ID lat/lon up front
  x <- unique(growth_id %>% pull(x))
  y <- unique(growth_id %>% pull(y))
  
  #get total precip
  ppt_sum <- aggregate(ppt ~ year,sum,data=growth_id)
  colnames(ppt_sum) <- c('year','ppt_sum')
  
  growth_id <- merge(growth_id,ppt_sum,by=c('year'))
  
  #get year with lowest precip
  min_ppt <- min(growth_id$ppt_sum) + 0.1
  
  #subset to years above this value
  growth_id  <- growth_id %>%
    filter(ppt_sum < min_ppt)
  
  #create spline model of growth curve
  ndvi_doy_drought_spl <- with(growth_id, smooth.spline(doy, ndvi))
  #plot(ndvi ~ doy, data = growth_id)
  #lines(ndvi_doy_drought_spl, col = "red")
  
  
  return(ndvi_doy_drought_spl)
  
}


#-------------------------------------------------------------------------------
# wrangle seasonal precip and temperature data (unclear if used)------

#Precipitation
# summer_precip_function <- function(x){
#   
#   #import and turn into dataframe
#   seasonal_df <- raster(x)
#   seasonal_df <- data.frame(rasterToPoints(seasonal_df)) 
#   
#   #extract the year and change column names
#   seasonal_df$year <- substr(colnames(seasonal_df[3]), 15, 18)
#   colnames(seasonal_df) <- c('x','y','summer_precip','year')
#   
#   return(seasonal_df)
#   
# }
# 
# 
# spring_precip_function <- function(x){
#   
#   #import and turn into dataframe
#   seasonal_df <- raster(x)
#   seasonal_df <- data.frame(rasterToPoints(seasonal_df)) 
#   
#   #extract the year and change column names
#   seasonal_df$year <- substr(colnames(seasonal_df[3]), 15, 18)
#   colnames(seasonal_df) <- c('x','y','spring_precip','year')
#   
#   return(seasonal_df)
#   
# }
# 
# 
# #Temperature
# summer_temp_function <- function(x){
#   
#   #import and turn into dataframe
#   seasonal_df <- raster(x)
#   seasonal_df <- data.frame(rasterToPoints(seasonal_df)) 
#   
#   #extract the year and change column names
#   seasonal_df$year <- substr(colnames(seasonal_df[3]), 13, 16)
#   colnames(seasonal_df) <- c('x','y','summer_temp','year')
#   
#   return(seasonal_df)
#   
# }
# 
# 
# spring_temp_function <- function(x){
#   
#   #import and turn into dataframe
#   seasonal_df <- raster(x)
#   seasonal_df <- data.frame(rasterToPoints(seasonal_df)) 
#   
#   #extract the year and change column names
#   seasonal_df$year <- substr(colnames(seasonal_df[3]), 13, 16)
#   colnames(seasonal_df) <- c('x','y','spring_temp','year')
#   
#   return(seasonal_df)
#   
# }

#tweak these so they generalize
seasonal_temp_import_function <- function(x){
  
  #import and turn into dataframe
  seasonal_df <- raster(x)
  seasonal_df <- data.frame(rasterToPoints(seasonal_df)) 
  
  #extract the year and change column names
  seasonal_df$year <- substr(colnames(seasonal_df[3]), 13, 16)
  colnames(seasonal_df) <- c('x','y','temp','year')
  
  return(seasonal_df)
  
}

seasonal_precip_import_function <- function(x){
  
  #import and turn into dataframe
  seasonal_df <- raster(x)
  seasonal_df <- data.frame(rasterToPoints(seasonal_df)) 
  
  #extract the year and change column names
  seasonal_df$year <- substr(colnames(seasonal_df[3]), 15, 18)
  colnames(seasonal_df) <- c('x','y','precip','year')
  
  return(seasonal_df)
  
}

#-------------------------------------------------------------------------------
# get driest years -----

get_driest_year <- function(i){
  
  # i = 1
  
  ppt_df_mean_sub <- subset(ppt_df_mean,id_value==i) 
  
  ppt_df_2 <- merge(ppt_df,ppt_df_mean_sub[c(1,2,4)],by=c('x','y'))
  
  
  x <- unique(ppt_df_2 %>% pull(x))
  y <- unique(ppt_df_2 %>% pull(y))
  
  #get year with lowest precip
  quantile_25 <- min(ppt_id$ppt) + 0.1
  
  #subset to years below this value
  ppt_id  <- ppt_df_2 %>%
    filter(ppt < quantile_25)
  
  ppt_id$x <- x
  ppt_df$y <- y
  
  ppt_id <- ppt_id %>%
    select(x,y,ppt,year)
  
  return(ppt_id)
  
}

#-------------------------------------------------------------------------------

#95 CI -----

ci_99 <- function(x){
  
  ci <- std.error(x)*2.58
  
  return(ci)
  
}

#-------------------------------------------------------------------------------

# IQR ------

iqr_75 <- function(x){

upper_75 <- quantile(x,probs=0.75)

return(upper_75)

}


iqr_25 <- function(x){
  
  upper_75 <- quantile(x,probs=0.25)
  
  return(upper_75)
  
}

#-------------------------------------------------------------------------------
# import and wrangle seasonal precipitation and temperature data ------

seasonality_summary <- function(Ecoregion,climate,season){
  
  #import driest year
  driest_year <- 
    read.csv(paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/growing_season/drought_precip_year_',Ecoregion,'.csv'))
  driest_year <- driest_year %>%
    dplyr::select(x,y,year,ppt_min)
  #head(driest_year,1)
  
  
  # climate = 'temperature'
  # season = 'spring'
  # Ecoregion = 'shortgrass_steppe'
  
  
  #import summer precip data
  filepath_summer_precip <-
    dir(
      paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/',climate,'/',season,'/'   
      ),
      full.names = T
    )
  
  if(climate=='precipitation'){
    
    import_function <- seasonal_precip_import_function
    
  }else{
    
    import_function <- seasonal_temp_import_function
    
  }
  
  test_seasonal <- lapply(filepath_summer_precip, import_function)
  test_seasonal <- list_to_df(test_seasonal)
  #head(test_seasonal,1)
  
  
  colnames(test_seasonal) <- c('x','y','climate','year')
  
  #get unique ID for each coordinate
  id_value_df <- aggregate(climate ~ x + y,mean,data = test_seasonal)
  id_value_df$id_value <- seq.int(nrow(id_value_df))
  id_value_df <- id_value_df %>%
    dplyr::select(x,y,id_value)
  test_seasonal <- merge(id_value_df,test_seasonal,by=c('x','y'))
  
  #merge summer precip with driest year
  test_seasonal_drought <- merge(test_seasonal,driest_year,by=c('x','y','year')) #will use this later for merging
  test_seasonal_drought_2 <- test_seasonal_drought %>%
    dplyr::select(x,y,year,id_value) %>%
    rename('dry_year' = 'year')
  
  id_list <- unique(test_seasonal_drought_2$id_value)
  
  #filter out the driest year from the summer ppt data
  seasonal_list <- list()
  for(i in id_list){
    
    dry_year_subset <- subset(test_seasonal_drought_2,id_value==i)
    dry_year = as.numeric(dry_year_subset$dry_year)
    
    seasonal_id <- subset(test_seasonal,id_value==i)
    
    seasonal_id <- seasonal_id %>%
      dplyr::filter(year != dry_year)
    
    seasonal_list[[i]] <- seasonal_id 
    
    #print(as.numeric(i)/as.numeric(length(id_list)))
    
  }
  
  seasonal_df <- do.call('rbind',seasonal_list)
  head(seasonal_df,1)
  
  #get mean and merge with drought year
  seasonal_mean <- aggregate(climate ~ x + y,mean,data = seasonal_df)
  seasonal_mean <- merge(seasonal_mean,test_seasonal_drought[c(1,2,5)],by = c('x','y'))
  
  colnames(seasonal_mean) <- c('x','y','mean','drought')
  # head(seasonal_mean,1)
  
  #percent change 
  seasonal_mean$perc_change <- 
    round(((seasonal_mean$drought - seasonal_mean$mean)/
             seasonal_mean$mean)*100,2)
  #hist(seasonal_mean$perc_change_seasonal)
  
  #absolute change 
  seasonal_mean$abs_change <- 
    seasonal_mean$drought - seasonal_mean$mean
  
  head(seasonal_mean,1)
  
  #make meaningful column names
  colnames(seasonal_mean) <-
    c('x','y',paste0(season,'_',climate,'_mean'),
      paste0(season,'_',climate,'_drought'),paste0('perc_change_',season,'_',climate),
      paste0('abs_change_',season,'_',climate))
  
  seasonal_mean$ecoregion <- Ecoregion
  
  return(seasonal_mean)
  
}




#-------------------------------------------------------------------------------
# boostrapped ks test of distributions -----


ks_test_bootstrap <- function(data_1,data_2){
  
  #assumes the two dataframes have an XYZ structure
  
  d_list <- list()
  for(i in 1:1000){
    
    #get random subset of 100
    
    #first dataset
    dataset_1 <- data_1 %>%
      dplyr::sample_n(100)
    colnames(dataset_1) <- c('x','y','z')
    
    #second dataset
    dataset_2 <- data_2 %>%
      dplyr::sample_n(100)
    colnames(dataset_2) <- c('x','y','z')

    
    test <- ks.test(dataset_1$z,dataset_2$z,exact=F)
    D <- data.frame(test$statistic)
    
    d_list[[i]] <- D
    
  }
  
  d_df <- do.call('rbind',d_list)
  
  return(d_df)
  
}
#-------------------------------------------------------------------------------
# get max and total C uptake reductions ------



get_max_total_reduction  <- function(i) {
  
  #subset to a given pixel
  growth_id <- ppt_gpp %>%
    dplyr::filter(id_value == i)
  
  #ID lat/lon up front
  x <- unique(growth_id %>% pull(x))
  y <- unique(growth_id %>% pull(y))
  
  ppt_sum <- aggregate(ppt ~ year,sum,data=growth_id)
  colnames(ppt_sum) <- c('year','ppt_sum')
  
  growth_id <- merge(growth_id,ppt_sum,by=c('year'))
  
  
  #get year with lowest precip
  min_ppt <- min(ppt_sum$ppt_sum) 
  
  #subset to years above this value
  growth_id_2  <- growth_id %>%
    dplyr::filter(ppt_sum > min_ppt)
  
  #subset to years above below value
  growth_id_drought  <- growth_id %>%
    dplyr::filter(ppt_sum == min_ppt)
  
  growth_id_drought <- growth_id_drought %>%
    arrange(doy)
  
  #average year cumulative
  
  #now do average growth curve
  growth_id_2 <- aggregate(gpp ~ doy, median, data = growth_id_2)
  #plot(gpp~doy,growth_id)
  
  #growth_id_drought <- aggregate(gpp ~ doy, median, data = growth_id_drought)
  
  #for that pixel, get cumulative GPP throughout the year
  growth_id_cmulative <-
    data.frame(growth_id_2, gpp_2 = cumsum(growth_id_2$gpp))
  
  #create spline model of average cumulative growth curve
  gpp_doy_spl_cumulative <-
    with(growth_id_cmulative, smooth.spline(doy, gpp_2))
  
  #create a spline model for growth dynamics
  gpp_doy_spl <-
    with(growth_id_2, smooth.spline(doy, gpp))
  
  #now do same for drought year
  growth_id_cmulative_drought <-
    data.frame(growth_id_drought, gpp_2 = cumsum(growth_id_drought$gpp))
  
  gpp_doy_spl_cumulative_drought <-
    with(growth_id_cmulative_drought, smooth.spline(doy, gpp_2))
  
  gpp_doy_spl_drought <-
    with(growth_id_drought, smooth.spline(doy, gpp))
  
  
  #get abs and % difference between drought and normal years for total GPP/C uptake
  doy_df_cumulative_difference <- data.frame(predict(gpp_doy_spl_cumulative_drought,297)$y - 
                                               predict(gpp_doy_spl_cumulative,297)$y)
  colnames(doy_df_cumulative_difference) <- 'reduction'
  doy_df_cumulative_difference$perc_reduction <- 
    ((predict(gpp_doy_spl_cumulative_drought,297)$y - predict(gpp_doy_spl_cumulative,297)$y)/
    predict(gpp_doy_spl_cumulative,297)$y)*100
  doy_df_cumulative_difference$doy <- 297
  doy_df_cumulative_difference$type <- 'total'
  doy_df_cumulative_difference <- doy_df_cumulative_difference %>%
    dplyr::select(doy,reduction,perc_reduction,type)
  
  #get abs and % maximum reduction
  doy_df_max_diff_average <- data.frame(predict(gpp_doy_spl,seq(73,297,by=1)))
  colnames(doy_df_max_diff_average) <- c('doy','average_gpp')
  doy_df_max_diff_drought <- data.frame(predict(gpp_doy_spl_drought,seq(73,297,by=1)))
  colnames(doy_df_max_diff_drought) <- c('doy','drought_gpp')
  doy_df_max_diff_average_drought <- merge(doy_df_max_diff_average,doy_df_max_diff_drought,
                                           by = 'doy')
  doy_df_max_diff_average_drought$reduction <- 
    doy_df_max_diff_average_drought$drought_gpp - doy_df_max_diff_average_drought$average_gpp
  
  doy_df_max_diff_average_drought$perc_reduction <- 
    ((doy_df_max_diff_average_drought$drought_gpp - doy_df_max_diff_average_drought$average_gpp)/
    doy_df_max_diff_average_drought$average_gpp)*100
  
  max <- max(doy_df_max_diff_average_drought$reduction)
  min <- min(doy_df_max_diff_average_drought$reduction)
  
  min_reduction <- doy_df_max_diff_average_drought %>%
    select(doy,reduction,perc_reduction) %>%
    dplyr::filter(reduction==max) 
  min_reduction$type <- 'min'
  
  max_reduction <- doy_df_max_diff_average_drought %>%
    select(doy,reduction,perc_reduction) %>%
    dplyr::filter(reduction==min) 
  max_reduction$type <- 'max'
  
  max_min_reduction <- rbind(max_reduction,min_reduction)
  max_min_total_reduction <- rbind(max_min_reduction, doy_df_cumulative_difference)
  max_min_total_reduction$x <- x
  max_min_total_reduction$y <- y
  
  max_min_total_reduction <- max_min_total_reduction %>%
    dplyr::select(x,y,doy,type,reduction,perc_reduction)
  
  #drought year cumulative
  
  
  #create dataframe of cumulative C uptake by day for aerage year
  
  
  return(max_min_total_reduction)
  
  
}

#-------------------------------------------------------------------------------
# get max NDVI reductions ------



get_max_total_reduction_NDVI  <- function(i) {
  
  #subset to a given pixel
  growth_id <- ppt_ndvi %>%
    dplyr::filter(id_value == i)
  
  #ID lat/lon up front
  x <- unique(growth_id %>% pull(x))
  y <- unique(growth_id %>% pull(y))
  
  ppt_sum <- aggregate(ppt ~ year,sum,data=growth_id)
  colnames(ppt_sum) <- c('year','ppt_sum')
  
  growth_id <- merge(growth_id,ppt_sum,by=c('year'))
  
  #get year with lowest precip
  min_ppt <- min(ppt_sum$ppt_sum) 
  
  #subset to years above this value
  growth_id_2  <- growth_id %>%
    dplyr::filter(ppt_sum > min_ppt)
  
  #subset to years above below value
  growth_id_drought  <- growth_id %>%
    dplyr::filter(ppt_sum == min_ppt)
  
  growth_id_drought <- growth_id_drought %>%
    arrange(doy)
  
  #create a spline model for growth dynamics
  ndvi_doy_spl <-
    with(growth_id_2, smooth.spline(doy, ndvi))
  
  #now do same for drought year
  growth_id_cmulative_drought <-
    data.frame(growth_id_drought, ndvi_2 = cumsum(growth_id_drought$ndvi))
  
  ndvi_doy_spl_cumulative_drought <-
    with(growth_id_cmulative_drought, smooth.spline(doy, ndvi_2))
  
  ndvi_doy_spl_drought <-
    with(growth_id_drought, smooth.spline(doy, ndvi))
  
  #get abs and % maximum reduction
  doy_df_max_diff_average <- data.frame(predict(ndvi_doy_spl,seq(73,297,by=1)))
  colnames(doy_df_max_diff_average) <- c('doy','average_ndvi')
  doy_df_max_diff_drought <- data.frame(predict(ndvi_doy_spl_drought,seq(73,297,by=1)))
  colnames(doy_df_max_diff_drought) <- c('doy','drought_ndvi')
  doy_df_max_diff_average_drought <- merge(doy_df_max_diff_average,doy_df_max_diff_drought,
                                           by = 'doy')
  doy_df_max_diff_average_drought$reduction <- 
    doy_df_max_diff_average_drought$drought_ndvi - doy_df_max_diff_average_drought$average_ndvi
  
  doy_df_max_diff_average_drought$perc_reduction <- 
    ((doy_df_max_diff_average_drought$drought_ndvi - doy_df_max_diff_average_drought$average_ndvi)/
       doy_df_max_diff_average_drought$average_ndvi)*100
  
  max <- max(doy_df_max_diff_average_drought$reduction)
  min <- min(doy_df_max_diff_average_drought$reduction)
  
  min_reduction <- doy_df_max_diff_average_drought %>%
    select(doy,reduction,perc_reduction) %>%
    dplyr::filter(reduction==max) 
  min_reduction$type <- 'min'
  
  max_reduction <- doy_df_max_diff_average_drought %>%
    select(doy,reduction,perc_reduction) %>%
    dplyr::filter(reduction==min) 
  max_reduction$type <- 'max'
  
  max_min_reduction <- rbind(max_reduction,min_reduction)
  max_min_reduction$x <- x
  max_min_reduction$y <- y
  
  max_min_reduction <- max_min_reduction %>%
    dplyr::select(x,y,doy,type,reduction,perc_reduction)
  
  #drought year cumulative
  
  
  #create dataframe of cumulative C uptake by day for aerage year
  
  
  return(max_min_reduction)
  
  
}

