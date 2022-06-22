
# NDVI drought reduction spline


# setup----
plan(multisession, workers = 10)
options(future.globals.maxSize = 8000 * 1024^2) #https://github.com/satijalab/seurat/issues/1845
period_list <- seq(1, 15, 1) #set periods
period_list <-
  as.character(period_list) #easier when they are characters
year_list <- seq(2003, 2020, 1) #set years
year_list <-
  as.character(year_list) #easier when they are characters

#import
ppt_ndvi <- readr::read_csv(paste0('./../../Data/NDVI/Ecoregion/',Ecoregion,'/ppt_ndvi_combined.csv'))
#head(ppt_ndvi,1)

# get splines -----

#create a vector of unique sites IDs
id_list <- unique(ppt_ndvi$id_value)

#get average growth 
with_progress({
  p <- progressor(along = id_list)
  growth_spline_list <- future_lapply(id_list, function(i) {
    Sys.sleep(0.1)
    p(sprintf("i=%g", i))
    ndvi_spline_2(i)
  })
})


#now do drought
with_progress({
  p <- progressor(along = id_list)
  growth_drought_spline_list <- future_lapply(id_list, function(i) {
    Sys.sleep(0.1)
    p(sprintf("i=%g", i))
    ndvi_spline_drought_2(i)
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

  #get and add 25th and 75 quantiles
  #ndvi_predicted_drought_average_2$ci_99 <- std.error(ndvi_predicted_drought_average$perc_change)*2.576
  ndvi_predicted_drought_average_2$ci_75 <- quantile(ndvi_predicted_drought_average$perc_change, probs = 0.75)
  ndvi_predicted_drought_average_2$ci_25 <- quantile(ndvi_predicted_drought_average$perc_change, probs = 0.25)
  ndvi_predicted_drought_average_2$sample_size <- ss
  ndvi_reduction_list[[i]] <- ndvi_predicted_drought_average_2
  
  #absolute
  ndvi_predicted_drought_average_3$abs_change <- ndvi_predicted_drought_average_3$ndvi_drought -
    ndvi_predicted_drought_average$ndvi_average
  
  #get median
  ndvi_predicted_drought_average_4 <- aggregate(abs_change~doy,median,data=ndvi_predicted_drought_average_3)
  
  #get and add 25th and 75th quantiles
  #ndvi_predicted_drought_average_4$ci_99 <- std.error(ndvi_predicted_drought_average_3$abs_change)*2.576
  ndvi_predicted_drought_average_4$ci_75 <- quantile(ndvi_predicted_drought_average_3$abs_change, probs = 0.75)
  ndvi_predicted_drought_average_4$ci_25 <- quantile(ndvi_predicted_drought_average_3$abs_change, probs = 0.25)
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

rm(ndvi_predicted_average,ndvi_predicted_drought,ndvi_predicted_drought_average,
   ndvi_predicted_drought_average_2,ndvi_predicted_list_average,ndvi_predicted_list_average_df,
   ndvi_predicted_list_drought,ndvi_predicted_list_drought_df,ndvi_reduction_list,
   growth_drought_spline_list,growth_spline_list,ndvi_reduction_list_2,ndvi_predicted_drought_average_3,
   ndvi_predicted_drought_average_4)


#plot this out ------
# str(ndvi_reduction_list_df)
# ndvi_reduction_list_df$upper <- ndvi_reduction_list_df$perc_change + ndvi_reduction_list_df$ci_99
# ndvi_reduction_list_df$lower <- ndvi_reduction_list_df$perc_change - ndvi_reduction_list_df$ci_99
# plot(perc_change~doy,data=ndvi_reduction_list_df,cex=0.1,
#      xlab='Julian day',ylab='Drought impact (% change in ndvi)',ylim=c(-40,1))
# lines(perc_change~doy,data=ndvi_reduction_list_df)
# lines(upper~as.numeric(as.integer(doy)),ndvi_reduction_list_df)
# lines(lower~doy,ndvi_reduction_list_df)
# abline(h=0)
