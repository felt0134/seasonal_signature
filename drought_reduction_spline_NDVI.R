
# NDVI drought reduction spline


# setup----
library(plotrix)
plan(multisession, workers = 10)
options(future.globals.maxSize = 8000 * 1024^2) #https://github.com/satijalab/seurat/issues/1845
period_list <- seq(1, 15, 1) #set periods
period_list <-
  as.character(period_list) #easier when they are characters
year_list <- seq(2003, 2020, 1) #set years
year_list <-
  as.character(year_list) #easier when they are characters


# first do the ndvi import----

#loop through each year and period combination

#list to store outputs in
ndvi_list <- list()

#run the loop
for (i in period_list) {
  filepath <-
    dir(
      paste0(
        './../../Data/ndvi/Ecoregion/',
        Ecoregion,
        '/MODIS_ndvi/Period/',
        i,
        '/'
      ),
      full.names = T
    )
  test <- lapply(filepath, format_ndvi_df)
  test <- data.frame(do.call('rbind', test))
  #test <- lapply(test,rasterToPoints)
  ndvi_list[[i]] <- test
  
}

#convert list of dataframes to a single dataframe
ndvi_df <- do.call('rbind', ndvi_list)
rm(ndvi_list, test) #get rid of excess stuff

#make unique id for each site
ndvi_df_mean <- aggregate(ndvi ~ x + y, mean, data = ndvi_df)
ndvi_df_mean$id_value <- seq.int(nrow(ndvi_df_mean))
#head(ndvi_df_mean,50)

#import conversion of period to day of year to map period on to DOY
doy_conversion <- read.csv('./../../Data/ndvi/period_day_match.csv')

#add on day of year and ID columns
ndvi_df <- merge(ndvi_df, doy_conversion[c(2, 3)], by = c('period'))
ndvi_df <- merge(ndvi_df, ndvi_df_mean[c(1, 2, 4)], by = c('x', 'y'))

#create a vector of unique sites IDs
id_list <- unique(ndvi_df$id_value)

# import ppt -----
ppt_list <- list()

#loop through each year and period
for (i in period_list) {
  filepath <-
    dir(
      paste0(
        './../../Data/Climate/Ecoregion/',
        Ecoregion,
        '/Precipitation/Period/',
        i,
        '/'
      ),
      full.names = T
    )
  test_ppt <- lapply(filepath, format_ppt_df)
  test_ppt <- data.frame(do.call('rbind', test_ppt))
  ppt_list[[i]] <- test_ppt
  
}

#convert to dataframe
ppt_df <- do.call('rbind', ppt_list)
rm(ppt_list, test_ppt) #remove excess data
#head(ppt_df)

#merge the two dataframes by location, year, and period within each year
ppt_ndvi <- merge(ndvi_df, ppt_df, by = c('x', 'y', 'year', 'period'))
#head(ppt_ndvi)
rm(ppt_df)


# get splines -----

#get average growth 
with_progress({
  p <- progressor(along = id_list)
  growth_spline_list <- future_lapply(id_list, function(i) {
    Sys.sleep(0.1)
    p(sprintf("i=%g", i))
    ndvi_spline(i)
  })
})


#now do drought
with_progress({
  p <- progressor(along = id_list)
  growth_drought_spline_list <- future_lapply(id_list, function(i) {
    Sys.sleep(0.1)
    p(sprintf("i=%g", i))
    ndvi_spline_drought(i)
  })
})

#get each 95% CI for each day of the prediction
doy_list <- c(65:297)

#loop
ndvi_predicted_list_average <- list()
ndvi_predicted_list_drought <- list()
ndvi_reduction_list <- list()
ndvi_reduction_list_2 <- list()
#test_list <- c(1:10)

for(i in doy_list){
  
  for(j in id_list){
    
    #average
    ndvi_predicted_average <- data.frame(predict(growth_spline_list[[j]], i))
    ndvi_predicted_average$id_val <- j
    ndvi_predicted_list_average[[j]] <- ndvi_predicted_average
    
    #drought
    ndvi_predicted_drought <- data.frame(predict(growth_drought_spline_list[[j]], i))
    ndvi_predicted_drought$id_val <- j
    ndvi_predicted_list_drought[[j]] <- ndvi_predicted_drought
    
  }
  
  #convert to dataframe and remove values below zero
  ndvi_predicted_list_average_df <- list_to_df(ndvi_predicted_list_average)
  colnames(ndvi_predicted_list_average_df) <- c('doy','ndvi_average','id_val')
  ndvi_predicted_list_average_df <- ndvi_predicted_list_average_df %>%
    filter(ndvi_average > -0.1)
  # 
  ndvi_predicted_list_drought_df <- list_to_df(ndvi_predicted_list_drought)
  colnames(ndvi_predicted_list_drought_df) <- c('doy','ndvi_drought','id_val')
  ndvi_predicted_list_drought_df <- ndvi_predicted_list_drought_df %>%
    filter(ndvi_drought > -0.1)
  
  # hist(ndvi_predicted_list_average_df$ndvi_average)
  # summary(ndvi_predicted_list_average_df)
  # hist(ndvi_predicted_list_drought_df$ndvi_drought)
  # summary(ndvi_predicted_list_drought_df)
  
  ndvi_predicted_drought_average <- merge(ndvi_predicted_list_drought_df,ndvi_predicted_list_average_df,
                                         by=c('doy','id_val'))
  
  ss <- nrow(ndvi_predicted_drought_average)
  
  ndvi_predicted_drought_average_3 <- ndvi_predicted_drought_average #use for absolute
  
  #relative
  ndvi_predicted_drought_average$perc_change <- ((ndvi_predicted_drought_average$ndvi_drought -
    ndvi_predicted_drought_average$ndvi_average)/ndvi_predicted_drought_average$ndvi_average)*100

  #get median
  ndvi_predicted_drought_average_2 <- aggregate(perc_change~doy,median,data=ndvi_predicted_drought_average)

  #get and add 99% CI
  ndvi_predicted_drought_average_2$ci_99 <- std.error(ndvi_predicted_drought_average$perc_change)*2.576
  ndvi_predicted_drought_average_2$sample_size <- ss
  ndvi_reduction_list[[i]] <- ndvi_predicted_drought_average_2
  
  #absolute
  ndvi_predicted_drought_average_3$abs_change <- ndvi_predicted_drought_average_3$ndvi_drought -
    ndvi_predicted_drought_average$ndvi_average
  
  #get median
  ndvi_predicted_drought_average_4 <- aggregate(abs_change~doy,median,data=ndvi_predicted_drought_average_3)
  
  #get and add 99% CI
  ndvi_predicted_drought_average_4$ci_99 <- std.error(ndvi_predicted_drought_average_3$abs_change)*2.576
  ndvi_predicted_drought_average_4$sample_size <- ss
  ndvi_reduction_list_2[[i]] <- ndvi_predicted_drought_average_4
  
}

ndvi_reduction_list_df <- list_to_df(ndvi_reduction_list)
head(ndvi_reduction_list_df,1)

filename <- paste0('./../../Data/growth_dynamics/drought_ndvi_reduction_',Ecoregion,'.csv')
write.csv(ndvi_reduction_list_df,filename)

ndvi_reduction_list_df_2 <- list_to_df(ndvi_reduction_list_2)
head(ndvi_reduction_list_df_2,1)

filename <- paste0('./../../Data/growth_dynamics/drought_ndvi_reduction_absolute_',Ecoregion,'.csv')
write.csv(ndvi_reduction_list_df_2,filename)

rm(ndvi_df_mean,ndvi_predicted_average,ndvi_predicted_drought,ndvi_predicted_drought_average,
   ndvi_predicted_drought_average_2,ndvi_predicted_list_average,ndvi_predicted_list_average_df,
   ndvi_predicted_list_drought,ndvi_predicted_list_drought_df,ndvi_reduction_list,
   growth_drought_spline_list,growth_spline_list,ndvi_reduction_list_2,ndvi_predicted_drought_average_3,
   ndvi_predicted_drought_average_4)


#plot this out ------
str(ndvi_reduction_list_df)
ndvi_reduction_list_df$upper <- ndvi_reduction_list_df$perc_change + ndvi_reduction_list_df$ci_99
ndvi_reduction_list_df$lower <- ndvi_reduction_list_df$perc_change - ndvi_reduction_list_df$ci_99
plot(perc_change~doy,data=ndvi_reduction_list_df,cex=0.1,
     xlab='Julian day',ylab='Drought impact (% change in ndvi)',ylim=c(-40,1))
lines(perc_change~doy,data=ndvi_reduction_list_df)
lines(upper~as.numeric(as.integer(doy)),ndvi_reduction_list_df)
lines(lower~doy,ndvi_reduction_list_df)
abline(h=0)
# 
# ndvi.doy.spl <-
#   with(ndvi_reduction_list_df, smooth.spline(doy, perc_change))
#lines(ndvi.doy.spl, col = "blue")

# #import and merge
# ndvi_reduction_list_df <- read.csv(paste0('./../../Data/growth_dynamics/drought_ndvi_reduction_',Ecoregion,'.csv'))
# ndvi_mean_list_df <- read.csv(paste0('./../../Data/growth_dynamics/average_ndvi_',Ecoregion,'.csv'))
# 
# normal_drought_df <- merge(ndvi_mean_list_df_2[c(2,3,5)],ndvi_mean_list_df[c(2,3,5)],by='doy')
# head(normal_drought_df)
# 
# normal_drought_df$perc_change <- 
#   ((normal_drought_df$mean.x - normal_drought_df$mean.y)/normal_drought_df$mean.y)*100
# 
# head(normal_drought_df)
# plot(perc_change ~ doy,data=normal_drought_df,xlab='Julian day',ylab='Drought impact (% change in ndvi)')
# abline(h=0)