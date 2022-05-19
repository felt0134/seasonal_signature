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

#day of 90% growth
get_90_gpp <- function(i) {
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
  gpp_predicted_90 <- gpp_predicted %>%
    dplyr::filter(gpp_perc < 90.1)
  
  rm(gpp_predicted)
  
  doy_90 <-
    max(gpp_predicted_90$day) #this is a rough approximation
  
  doy_90_df <- data.frame(doy_90)
  colnames(doy_90_df) <- c('doy_90')
  doy_90_df$x <- x
  doy_90_df$y <- y
  
  doy_90_df <- doy_90_df[c(2, 3, 1)]
  
  return(doy_90_df)
  
  
  
}
#

#day of 50% growth
get_50_gpp <- function(i) {
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
#-------------------------------------------------------------------------------
# calculate day of 25%, 50%, and 90% of growth for drought years ------

#90%
get_90_gpp_drought <- function(i) {
  ppt_id <- subset(ppt_gpp, id_value == i)
  gpp_id <- subset(gpp_df, id_value == i)
  
  ppt_id <- aggregate(ppt ~ x + y + id_value + year, sum, data = ppt_id)
  
  x <- unique(ppt_id %>% pull(x))
  y <- unique(ppt_id %>% pull(y))
  
  #identify the quantile of interest
  quantile_25 <- quantile(ppt_id$ppt, probs = 0.05)
  
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
  
  #get day of year where roughly 90% of cumulative growth has occurred and turn into dataframe
  gpp_drought_predicted_90 <- gpp_drought_predicted %>%
    dplyr::filter(gpp_perc < 90.1)
  
  doy_90 <-
    max(gpp_drought_predicted_90$day) #this is a rough approximation
  
  rm(gpp_drought_predicted_90, gpp_drought_predicted)
  
  doy_90_df <- data.frame(doy_90)
  colnames(doy_90_df) <- c('doy_90_drought')
  doy_90_df$x <- x
  doy_90_df$y <- y
  
  doy_90_df <- doy_90_df[c(2, 3, 1)] #re-order to x,y,z
  
  return(doy_90_df)
  
  
}
#

#50%
get_50_gpp_drought <- function(i) {
  ppt_id <- subset(ppt_gpp, id_value == i)
  gpp_id <- subset(gpp_df, id_value == i)
  
  ppt_id <- aggregate(ppt ~ x + y + id_value + year, sum, data = ppt_id)
  
  x <- unique(ppt_id %>% pull(x))
  y <- unique(ppt_id %>% pull(y))
  
  #identify the quantile of interest
  quantile_25 <- quantile(ppt_id$ppt, probs = 0.05)
  
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

#25%
get_25_gpp_drought <- function(i) {
  ppt_id <- subset(ppt_gpp, id_value == i)
  gpp_id <- subset(gpp_df, id_value == i)
  
  ppt_id <- aggregate(ppt ~ x + y + id_value + year, sum, data = ppt_id)
  
  x <- unique(ppt_id %>% pull(x))
  y <- unique(ppt_id %>% pull(y))
  
  #identify the quantile of interest
  quantile_25 <- quantile(ppt_id$ppt, probs = 0.05)
  
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
  
  #identify the quantile of interest
  quantile_25 <- quantile(ppt_id$ppt, probs = 0.05)
  
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

#absolute (% of growth)
get_average_growth_curve_absolute  <- function(i) {
  
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
  
  head(growth_id_cmulative)
  #plot(gpp_2 ~ doy,data= growth_id_cmulative)
  
  rm(growth_id)
  
  #create spline model of growth curve
  gpp.doy.spl <-
    with(growth_id_cmulative, smooth.spline(doy, gpp_2))
  #lines(gpp.doy.spl, col = "blue")
  
  rm(growth_id_cmulative)
  
  #run model through a sequence of days
  doy <- data.frame(seq(from = 65, to = 297, by = 1))
  gpp_predicted <- data.frame(predict(gpp.doy.spl, doy))
  colnames(gpp_predicted) <- c('day', 'gpp')
  #plot(gpp ~ day,gpp_predicted)
  
  #rm(gpp_predicted)
  
  gpp_predicted$x <- x
  gpp_predicted$y <- y
  
  gpp_predicted <- gpp_predicted %>%
    select(x,y,day,gpp)
  
  
  return(gpp_predicted)
  
  
  
}

#

#absolute (% of growth)
get_drought_growth_curve_absolute <- function(i) {
  
  ppt_id <- subset(ppt_gpp, id_value == i)
  gpp_id <- subset(gpp_df, id_value == i)
  
  ppt_id <- aggregate(ppt ~ x + y + id_value + year, sum, data = ppt_id)
  
  x <- unique(ppt_id %>% pull(x))
  y <- unique(ppt_id %>% pull(y))
  
  #identify the quantile of interest
  quantile_25 <- quantile(ppt_id$ppt, probs = 0.05)
  
  #subset to years below this value
  ppt_id  <- ppt_id %>%
    filter(ppt < quantile_25)
  
  ppt_id  <- merge(ppt_id, gpp_id, by = c('x', 'y', 'id_value', 'year'))
  
  ppt_id  <- aggregate(gpp ~ doy, mean, data = ppt_id)
  
  #for that pixel, get cumulative GPP throughout the year
  ppt_id <- data.frame(ppt_id, gpp_2 = cumsum(ppt_id$gpp))
  
  #reduce data size by selecting specific columns
  ppt_id <- ppt_id %>%
    select(c('doy', 'gpp_2'))
  
  #create spline model of growth curve
  gpp.doy.drought.spl <- with(ppt_id, smooth.spline(doy, gpp_2))
  #lines(gpp.doy.drought.spl, col = "blue")
  
  #run model through a sequence of days
  doy <- data.frame(seq(from = 65, to = 297, by = 1))
  gpp_drought_predicted <-
    data.frame(predict(gpp.doy.drought.spl, doy))
  colnames(gpp_drought_predicted) <- c('day', 'gpp')
  
  rm(ppt_id, doy, gpp.doy.drought.spl)
  
  gpp_drought_predicted$x <- x
  gpp_drought_predicted$y <- y
  
  gpp_drought_predicted <- gpp_drought_predicted %>%
    select(x,y,day,gpp)
  
  
  return(gpp_drought_predicted)
  
  
}

#

#spline model of growth during average year
get_average_growth_curve_absolute_spline  <- function(i) {
  
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
  
  head(growth_id_cmulative)
  #plot(gpp_2 ~ doy,data= growth_id_cmulative)
  
  rm(growth_id)
  
  #create spline model of growth curve
  gpp.doy.spl <-
    with(growth_id_cmulative, smooth.spline(doy, gpp_2))
  #lines(gpp.doy.spl, col = "blue")
  
  rm(growth_id_cmulative)
  
  return(gpp.doy.spl)
  
  
  
}

#spline models of both average and the temporal 99CI
get_average_growth_curve_absolute_spline_ci  <- function(i) {
  
  #subset to a given pixel
  growth_id <- gpp_df %>%
    dplyr::filter(id_value == i)
  
  x <- unique(growth_id %>% pull(x))
  y <- unique(growth_id %>% pull(y))
  
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
  
  growth_id_ci <- aggregate(gpp_ci ~ doy, ci_99, data = year_df)
  rm(year_df)
  
  gpp.doy.spl_ci <-
    with(growth_id_ci, smooth.spline(doy, gpp_ci))
  
  
  #now do average growth curve
  growth_id <- aggregate(gpp ~ doy, mean, data = growth_id)
  #plot(gpp~doy,growth_id)
  
  #for that pixel, get cumulative GPP throughout the year
  growth_id_cmulative <-
    data.frame(growth_id, gpp_2 = cumsum(growth_id$gpp))
  
  #create spline model of growth curve
  gpp.doy.spl <-
    with(growth_id_cmulative, smooth.spline(doy, gpp_2))

  
  return(list(gpp.doy.spl,gpp.doy.spl_ci))
  
  
}



#

#spline model of growth during drought year
get_drought_growth_curve_absolute_spline <- function(i) {
  
  ppt_id <- subset(ppt_gpp, id_value == i)
  gpp_id <- subset(gpp_df, id_value == i)
  
  ppt_id <- aggregate(ppt ~ x + y + id_value + year, sum, data = ppt_id)
  
  x <- unique(ppt_id %>% pull(x))
  y <- unique(ppt_id %>% pull(y))
  
  #identify the quantile of interest
  quantile_25 <- quantile(ppt_id$ppt, probs = 0.05)
  
  #subset to years below this value
  ppt_id  <- ppt_id %>%
    filter(ppt < quantile_25)
  
  ppt_id  <- merge(ppt_id, gpp_id, by = c('x', 'y', 'id_value', 'year'))
  
  ppt_id  <- aggregate(gpp ~ doy, mean, data = ppt_id)
  
  #for that pixel, get cumulative GPP throughout the year
  ppt_id <- data.frame(ppt_id, gpp_2 = cumsum(ppt_id$gpp))
  
  #reduce data size by selecting specific columns
  ppt_id <- ppt_id %>%
    select(c('doy', 'gpp_2'))
  
  #create spline model of growth curve
  gpp.doy.drought.spl <- with(ppt_id, smooth.spline(doy, gpp_2))
  #lines(gpp.doy.drought.spl, col = "blue")
  
  
  return(gpp.doy.drought.spl)
  
  
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
  
  #identify the quantile of interest
  quantile_25 <- quantile(ppt_id$ppt, probs = 0.05)
  
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
  
  #identify the quantile of interest
  quantile_25 <- quantile(ppt_id$ppt, probs = 0.05)
  
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
  
  #identify the quantile of interest
  quantile_25 <- quantile(ppt_id$ppt, probs = 0.05)
  
  #subset to years below this value
  ppt_id_drought  <- ppt_id %>%
    filter(ppt < quantile_25) %>%
    select(x,y,year,ppt)
  
  return(ppt_id_drought)
  
  
}



#-------------------------------------------------------------------------------
# GPP splines ------

#16-day cumulative GPP throughout growing season (not the cumulative total GPP growth curve)

gpp_spline  <- function(i) {
  
  #subset to a given pixel
  growth_id <- gpp_df %>%
    dplyr::filter(id_value == i)
  
  x <- unique(growth_id %>% pull(x))
  y <- unique(growth_id %>% pull(y))
  
  growth_id <- aggregate(gpp ~ doy, mean, data = growth_id)
  #plot(gpp~doy,growth_id,ylim=c(0,30))
  
  #create spline model of growth curve
  gpp.doy.spl <-
    with(growth_id, smooth.spline(doy, gpp))
  #lines(gpp.doy.spl, col = "blue")
  
  return(gpp.doy.spl)
  
  
}

gpp_spline_drought <- function(i) {
  
  ppt_id <- subset(ppt_gpp, id_value == i)
  gpp_id <- subset(gpp_df, id_value == i)
  
  ppt_id <- aggregate(ppt ~ x + y + id_value + year, sum, data = ppt_id)
  
  x <- unique(ppt_id %>% pull(x))
  y <- unique(ppt_id %>% pull(y))
  
  #identify the quantile of interest
  quantile_25 <- quantile(ppt_id$ppt, probs = 0.05)
  
  #subset to years below this value
  ppt_id  <- ppt_id %>%
    filter(ppt < quantile_25)
  
  ppt_id  <- merge(ppt_id, gpp_id, by = c('x', 'y', 'id_value', 'year'))
  
  ppt_id  <- aggregate(gpp ~ doy, mean, data = ppt_id)
  
  
  #create spline model of growth curve
  gpp.doy.drought.spl <- with(ppt_id, smooth.spline(doy, gpp))
  plot(gpp ~ doy, data = ppt_id)
  #lines(gpp.doy.drought.spl, col = "red")
  
  
  return(gpp.doy.drought.spl)
  
  
}

#-------------------------------------------------------------------------------

#NDVI spline  ------

#16-day NDVI throughout growing season 

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

ndvi_spline_drought <- function(i) {
  
  ppt_id <- subset(ppt_ndvi, id_value == i)
  gpp_id <- subset(ndvi_df, id_value == i)
  
  ppt_id <- aggregate(ppt ~ x + y + id_value + year, sum, data = ppt_id)
  
  x <- unique(ppt_id %>% pull(x))
  y <- unique(ppt_id %>% pull(y))
  
  #identify the quantile of interest
  quantile_25 <- quantile(ppt_id$ppt, probs = 0.05)
  
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


#-------------------------------------------------------------------------------
# wrangle seasonal precip and temperature data (unclear if used)------

#Precipitation
summer_precip_function <- function(x){
  
  #import and turn into dataframe
  seasonal_df <- raster(x)
  seasonal_df <- data.frame(rasterToPoints(seasonal_df)) 
  
  #extract the year and change column names
  seasonal_df$year <- substr(colnames(seasonal_df[3]), 15, 18)
  colnames(seasonal_df) <- c('x','y','summer_precip','year')
  
  return(seasonal_df)
  
}


spring_precip_function <- function(x){
  
  #import and turn into dataframe
  seasonal_df <- raster(x)
  seasonal_df <- data.frame(rasterToPoints(seasonal_df)) 
  
  #extract the year and change column names
  seasonal_df$year <- substr(colnames(seasonal_df[3]), 15, 18)
  colnames(seasonal_df) <- c('x','y','spring_precip','year')
  
  return(seasonal_df)
  
}


#Temperature
summer_temp_function <- function(x){
  
  #import and turn into dataframe
  seasonal_df <- raster(x)
  seasonal_df <- data.frame(rasterToPoints(seasonal_df)) 
  
  #extract the year and change column names
  seasonal_df$year <- substr(colnames(seasonal_df[3]), 13, 16)
  colnames(seasonal_df) <- c('x','y','summer_temp','year')
  
  return(seasonal_df)
  
}


spring_temp_function <- function(x){
  
  #import and turn into dataframe
  seasonal_df <- raster(x)
  seasonal_df <- data.frame(rasterToPoints(seasonal_df)) 
  
  #extract the year and change column names
  seasonal_df$year <- substr(colnames(seasonal_df[3]), 13, 16)
  colnames(seasonal_df) <- c('x','y','spring_temp','year')
  
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
  
  #identify the quantile of interest
  quantile_25 <- quantile(ppt_df_2$ppt, probs = 0.05)
  
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
  
  ci <- std.error(x)*2.576
  
  return(ci)
  
}

