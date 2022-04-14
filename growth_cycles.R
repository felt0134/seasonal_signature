
# growth cycles betwee drought and average year



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


# first do the GPP import----

#loop through each year and period combination

#list to store outputs in
gpp_list <- list()

#run the loop
for (i in period_list) {
  filepath <-
    dir(
      paste0(
        './../../Data/GPP/Ecoregion/',
        Ecoregion,
        '/MODIS_GPP/Period/',
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
ppt_gpp <- merge(gpp_df, ppt_df, by = c('x', 'y', 'year', 'period'))
#head(ppt_gpp)
rm(ppt_df)


# get splines -----

#get average growth 
with_progress({
  p <- progressor(along = id_list)
  growth_spline_list <- future_lapply(id_list, function(i) {
    Sys.sleep(0.1)
    p(sprintf("i=%g", i))
    gpp_spline(i)
  })
})


#now do drought
with_progress({
  p <- progressor(along = id_list)
  growth_drought_spline_list <- future_lapply(id_list, function(i) {
    Sys.sleep(0.1)
    p(sprintf("i=%g", i))
    gpp_spline_drought(i)
  })
})


# loop through to get through cycles for dry year and median ------

#get each 95% CI for each day of the prediction
doy_list <- c(65:297)


#loop
gpp_normal_list <- list()
gpp_drought_list <- list()
gpp_predicted_list_average <- list()
gpp_predicted_list_drought <- list()
#test_list <- c(1:10)

for(i in doy_list){
  
  for(j in id_list){
    
    #average
    gpp_predicted_average <- data.frame(predict(growth_spline_list[[j]], i))
    gpp_predicted_average$id_val <- j
    gpp_predicted_list_average[[j]] <- gpp_predicted_average
    
    #drought
    gpp_predicted_drought <- data.frame(predict(growth_drought_spline_list[[j]], i))
    gpp_predicted_drought$id_val <- j
    gpp_predicted_list_drought[[j]] <- gpp_predicted_drought
    
  }
  
  #average growth cycle
  
  #convert to data frame and remove values below zero
  gpp_predicted_list_average_df <- list_to_df(gpp_predicted_list_average)
  colnames(gpp_predicted_list_average_df) <- c('doy','gpp_average','id_val')
  gpp_predicted_list_average_df <- gpp_predicted_list_average_df %>%
    filter(gpp_average > 0)
  
  #sample size
  ss <- nrow(gpp_predicted_list_average_df)
  
  #get median
  gpp_predicted_average <- aggregate(gpp_average~doy,median,data=gpp_predicted_list_average_df)
  
  #get 95% CI
  gpp_predicted_average$ci_99 <- std.error(gpp_predicted_list_average_df$gpp_average)*2.576
  gpp_predicted_average$sample_size <- ss
  gpp_normal_list[[i]] <- gpp_predicted_average
  
  
  #drought growth cycle
  
  #convert to data frame and remove values below zero
  gpp_predicted_list_drought_df <- list_to_df(gpp_predicted_list_drought)
  colnames(gpp_predicted_list_drought_df) <- c('doy','gpp_drought','id_val')
  gpp_predicted_list_drought_df <- gpp_predicted_list_drought_df %>%
    filter(gpp_drought > 0)
  
  #sample size
  ss <- nrow(gpp_predicted_list_drought_df)
  
  #get median
  gpp_predicted_drought <- aggregate(gpp_drought~doy,median,data=gpp_predicted_list_drought_df)
  
  #get 95% CI
  gpp_predicted_drought$ci_99 <- std.error(gpp_predicted_list_drought_df$gpp_drought)*2.576
  gpp_predicted_drought$sample_size <- ss
  gpp_drought_list[[i]] <- gpp_predicted_drought
  
  
}

#save normal cycle
gpp_normal_list_df <- list_to_df(gpp_normal_list)
#head(gpp_normal_list_df,1)

filename <- paste0('./../../Data/Growth_Cycle/average_cycle_',Ecoregion,'.csv')
write.csv(gpp_normal_list_df,filename)

#save drought cycle
gpp_drought_list_df <- list_to_df(gpp_drought_list)
filename <- paste0('./../../Data/Growth_Cycle/drought_cycle_',Ecoregion,'.csv')
write.csv(gpp_drought_list_df,filename)

plot(gpp_average~doy,data=gpp_normal_list_df)
lines(gpp_drought~doy,data=gpp_drought_list_df,col='red')


