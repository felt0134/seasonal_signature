

#workshopping code


# NDVI import -----


library(plotrix)
plan(multisession, workers = 10)
options(future.globals.maxSize = 8000 * 1024^2) #https://github.com/satijalab/seurat/issues/1845
period_list <- seq(1, 15, 1) #set periods
period_list <-
  as.character(period_list) #easier when they are characters
year_list <- seq(2003, 2020, 1) #set years
year_list <-
  as.character(year_list) #easier when they are characters


# first do the GPP import----

#loop through each year and period combination
Ecoregion <- 'shortgrass_steppe'

#list to store outputs in
gpp_list <- list()
i = 11
#run the loop
for (i in period_list) {
  filepath <-
    dir(
      paste0(
        './../../Data/NDVI/Ecoregion/',
        Ecoregion,
        '/MODIS_NDVI/Period/',
        i,
        '/'
      ),
      full.names = T
    )
  test <- lapply(filepath, format_gpp_df)
  test <- data.frame(do.call('rbind', test))
  #test <- lapply(test,rasterToPoints)
  gpp_list[[i]] <- test
  
}




#convert to raster
raster_file <- raster(filepath[10])
plot(raster_file)


#extract year from the name of the raster to later add to dataframe
year_val <- substr(names(raster_file), 6, 9)

#convert to dataframe and add year and period columns
df <- data.frame(rasterToPoints(raster_file))
df$year <- year_val
df$period <-
  gsub(paste0("GPP_", year_val, '_'), '', names(raster_file))
colnames(df) <- c('x', 'y', 'gpp', 'year', 'period')

#return formatted dataframe
return(df)


#convert list of dataframes to a single dataframe
gpp_df <- do.call('rbind', gpp_list)
rm(gpp_list, test) #get rid of excess stuff

#make unique id for each site
gpp_df_mean <- aggregate(gpp ~ x + y, mean, data = gpp_df)
gpp_df_mean$id_value <- seq.int(nrow(gpp_df_mean))
#head(gpp_df_mean)

#import conversion of period to day of year to map period on to DOY
doy_conversion <- read.csv('./../../Data/GPP/period_day_match.csv')

#add on day of year and ID columns
gpp_df <- merge(gpp_df, doy_conversion[c(2, 3)], by = c('period'))
gpp_df <- merge(gpp_df, gpp_df_mean[c(1, 2, 4)], by = c('x', 'y'))

#create a vector of unique sites IDs
id_list <- unique(gpp_df$id_value)