#get splines of growth curves 

#this script is mostly identical to growth_curve_splines.R, except with the filepaths
#different. You could alternatively tweak things to do ifelse statements between the full and
#1km subset to have this consolidated, but I have decided to have each as separate scripts to run.

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
ppt_gpp <- readr::read_csv(paste0('./../../Data/GPP/Ecoregion/',Ecoregion,'/ppt_gpp_combined_1km.csv'))
#head(ppt_gpp,1)

# get splines -----

#create a vector of unique sites IDs
id_list <- unique(ppt_gpp$id_value)

#get average growth curve and that 95 CI
with_progress({
  p <- progressor(along = id_list)
  growth_curve_spline_list <- future_lapply(id_list, function(i) {
    Sys.sleep(0.1)
    p(sprintf("i=%g", i))
    get_average_growth_curve_absolute_spline_ci(i)
  })
})


#get each median and IQR for each day of the prediction
doy_list <- c(65:297)
gpp_predicted_list <- list()
gpp_predicted_list_2 <- list()
gpp_predicted_list_3 <- list()
gpp_mean_list <- list()

for(i in c(doy_list)){ 
  
  for(j in id_list){
    
    #predicted mean curve
    gpp_predicted <- data.frame(predict(growth_curve_spline_list[[j]][[1]], i)) #extracts the median cumulative gpp of a pixel
    gpp_predicted_list[[j]] <- gpp_predicted
    
    #predicted temporal iqr 25
    gpp_predicted_ci_25 <- data.frame(predict(growth_curve_spline_list[[j]][[2]], i)) #extracts 25th percentile of a pixel
    gpp_predicted_list_2[[j]] <- gpp_predicted_ci_25
    
    #predicted temporal iqr 75
    gpp_predicted_ci_75 <- data.frame(predict(growth_curve_spline_list[[j]][[3]], i)) #extracts 75th percentile of a pixel
    gpp_predicted_list_3[[j]] <- gpp_predicted_ci_75
    
  }
  
  #spatial variation
  gpp_predicted_list_df <- list_to_df(gpp_predicted_list)
  gpp_predicted_list_mean <- aggregate(y~x,median,data=gpp_predicted_list_df)
  gpp_predicted_list_mean$spatial_ci_25 <- quantile(gpp_predicted_list_df$y,probs=0.25)
  gpp_predicted_list_mean$spatial_ci_75 <- quantile(gpp_predicted_list_df$y,probs=0.75)
  #gpp_predicted_list_mean$upper <- mean(gpp_predicted_list_mean$y) + std.error(gpp_predicted_list_df_2$y)*2.58
  # gpp_predicted_list_mean$lower <- mean(gpp_predicted_list_df$y) - sd(gpp_predicted_list_df$y)
  # gpp_predicted_list_mean$upper <- mean(gpp_predicted_list_mean$y) + sd(gpp_predicted_list_df$y)
  colnames(gpp_predicted_list_mean) <- c('doy','mean','spatial_ci_25','spatial_ci_75')
  
  #temporal variation 25th percentiles
  gpp_predicted_list_2_df <- list_to_df(gpp_predicted_list_2)
  gpp_predicted_list_2_mean <- aggregate(y~x,median,data=gpp_predicted_list_2_df)
  colnames(gpp_predicted_list_2_mean) <-c('doy','temporal_ci_25')
  
  #temporal variation 75th percentiles
  gpp_predicted_list_3_df <- list_to_df(gpp_predicted_list_3)
  gpp_predicted_list_3_mean <- aggregate(y~x,median,data=gpp_predicted_list_3_df)
  colnames(gpp_predicted_list_3_mean) <-c('doy','temporal_ci_75')
  
  gpp_predicted_list_mean <- merge(gpp_predicted_list_mean,gpp_predicted_list_2_mean,by=c('doy'))
  gpp_predicted_list_mean <- merge(gpp_predicted_list_mean,gpp_predicted_list_3_mean,by=c('doy'))
  
  gpp_mean_list[[i]] <- gpp_predicted_list_mean
  
  
}

#turn into dataframe
gpp_mean_list_df <- list_to_df(gpp_mean_list)
#head(gpp_mean_list_df,1)

#save
filename <- paste0('./../../Data/growth_curves/one_km_subset/average_growth_curve_',Ecoregion,'.csv')
write.csv(gpp_mean_list_df,filename)

#cleanup
rm(gpp_mean_list,gpp_mean_list_df,gpp_predicted,gpp_predicted_list,
   gpp_predicted_list_df,gpp_predicted_list_mean,growth_curve_spline_list)


#get drought growth curve
with_progress({
  p <- progressor(along = id_list)
  growth_curve_drought_spline_list <- future_lapply(id_list, function(i) {
    Sys.sleep(0.1)
    p(sprintf("i=%g", i))
    get_drought_growth_curve_absolute_spline(i)
  })
})

# for each day, get a prediction from each spline, then calculate the IQR
gpp_predicted_list_2 <- list()
gpp_mean_list_2 <- list()

for(i in doy_list){
  
  for(j in id_list){
    
    gpp_predicted <- data.frame(predict(growth_curve_drought_spline_list[[j]], i))
    gpp_predicted_list_2[[j]] <- gpp_predicted
    
  }
  
  #get mean and spatial variation
  gpp_predicted_list_df <- list_to_df(gpp_predicted_list_2)
  gpp_predicted_list_mean <- aggregate(y~x,median,data=gpp_predicted_list_df)
  #gpp_predicted_list_mean$lower <- median(gpp_predicted_list_df$y) - std.error(gpp_predicted_list_df$y)*2.576 #99% CI
  #gpp_predicted_list_mean$upper <- median(gpp_predicted_list_mean$y) + std.error(gpp_predicted_list_df$y)*2.576
  gpp_predicted_list_mean$ci_25 <- quantile(gpp_predicted_list_df$y,probs=0.25)
  gpp_predicted_list_mean$ci_75 <- quantile(gpp_predicted_list_df$y,probs=0.75)
  colnames(gpp_predicted_list_mean) <- c('doy','mean','spatial_25','spatial_75')
  gpp_mean_list_2[[i]] <- gpp_predicted_list_mean
  
  
}

#turn into dataframe
gpp_mean_list_df_2 <- list_to_df(gpp_mean_list_2)
#head(gpp_mean_list_df_2,1)

#save
filename <- paste0('./../../Data/growth_curves/one_km_subset/drought_growth_curve_',Ecoregion,'.csv')
write.csv(gpp_mean_list_df_2,filename)

#cleanup
rm(gpp_mean_list_2,gpp_mean_list_df_2,gpp_predicted,gpp_predicted_ci_25,
   gpp_predicted_ci_75,gpp_predicted_list_2,gpp_predicted_list_2_df,
   gpp_predicted_list_2_mean,gpp_predicted_list_3,gpp_predicted_list_3_df,
   gpp_predicted_list_3_mean,gpp_predicted_list_df,gpp_predicted_list_mean,
   growth_curve_drought_spline_list,ppt_gpp)


# ------


