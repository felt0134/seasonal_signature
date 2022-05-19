
#get splines of growth curves 

library(plotrix)


# setup----
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

# test_list <- c(1:100)
# test_store <- list()


#create a vector of unique sites IDs
id_list <- unique(gpp_df$id_value)

#get average growth curve
with_progress({
  p <- progressor(along = id_list)
  growth_curve_spline_list <- future_lapply(id_list, function(i) {
    Sys.sleep(0.1)
    p(sprintf("i=%g", i))
    get_average_growth_curve_absolute_spline_ci(i)
  })
})

#get each spline model
# for(i in test_list){
#   
#   test_spline <- get_average_growth_curve_absolute_spline(i)
#   
#   test_store[[i]] <- test_spline
#   
# }

#get each 95% CI for each day of the prediction
#library(plotrix)
doy_list <- c(65:297)
gpp_predicted_list <- list()
gpp_predicted_list_2 <- list()
gpp_mean_list <- list()

for(i in c(doy_list)){ #doy_list
  
  for(j in id_list){
    
    #predicted mean curve
    gpp_predicted <- data.frame(predict(growth_curve_spline_list[[j]][[1]], i)) #extracts the mean cumulative gpp
    gpp_predicted_list[[j]] <- gpp_predicted
    
    #predicted temporal ci
    gpp_predicted_ci <- data.frame(predict(growth_curve_spline_list[[j]][[2]], i)) #extracts the CI around the curve
    gpp_predicted_list_2[[j]] <- gpp_predicted_ci
    
  }
  
  #spatial variation
  gpp_predicted_list_df <- list_to_df(gpp_predicted_list)
  gpp_predicted_list_mean <- aggregate(y~x,mean,data=gpp_predicted_list_df)
  gpp_predicted_list_mean$lower <- median(gpp_predicted_list_df$y) - std.error(gpp_predicted_list_df$y)*2.576 #99% CI
  gpp_predicted_list_mean$upper <- median(gpp_predicted_list_mean$y) + std.error(gpp_predicted_list_df$y)*2.576
  colnames(gpp_predicted_list_mean) <- c('doy','mean','lower_spatial','upper_spatial')
  
  #temporal variation
  gpp_predicted_list_2_df <- list_to_df(gpp_predicted_list_2)
  gpp_predicted_list_2_mean <- aggregate(y~x,mean,data=gpp_predicted_list_2_df)
  colnames(gpp_predicted_list_2_mean) <-c('doy','temporal_ci')
  
  gpp_predicted_list_mean <- merge(gpp_predicted_list_mean,gpp_predicted_list_2_mean,by=c('doy'))
  
  gpp_mean_list[[i]] <- gpp_predicted_list_mean
  
  
}

#turn into dataframe
gpp_mean_list_df <- list_to_df(gpp_mean_list)
head(gpp_mean_list_df,1)

gpp_mean_list_df$lower_temporal <- gpp_mean_list_df$lower_spatial - gpp_mean_list_df$temporal_ci
gpp_mean_list_df$upper_temporal <- gpp_mean_list_df$upper_spatial + gpp_mean_list_df$temporal_ci

plot(mean~doy,data=gpp_mean_list_df)
lines(lower_temporal~doy,data=gpp_mean_list_df)
lines(upper_temporal~doy,data=gpp_mean_list_df)

filename <- paste0('./../../Data/growth_curves/average_growth_curve_',Ecoregion,'.csv')
write.csv(gpp_mean_list_df,filename)

rm(gpp_df_mean,gpp_mean_list,gpp_mean_list_df,gpp_predicted,gpp_predicted_list,
   gpp_predicted_list_df,gpp_predicted_list_mean,growth_curve_spline_list)

?std.error

#get drought growth curve
with_progress({
  p <- progressor(along = id_list)
  growth_curve_drought_spline_list <- future_lapply(id_list, function(i) {
    Sys.sleep(0.1)
    p(sprintf("i=%g", i))
    get_drought_growth_curve_absolute_spline(i)
  })
})


# test_store_2 <- list()
# for(i in test_list){
#   
#   test_spline_2 <- get_drought_growth_curve_absolute_spline(i)
#   
#   test_store_2[[i]] <- test_spline_2
#   
# }

# for(i in test_list){
#   
#   lines(test_store_2[[i]],col='red')
#   
#   
# }
# 
# 
# model <- test_store_2[[i]]
# coef(model)  

# for each day, get a prediction from each spline, then calculate the 95%CI (mean +/- 2*SE)
gpp_predicted_list_2 <- list()
gpp_mean_list_2 <- list()

for(i in doy_list){
  
  for(j in id_list){
    
    gpp_predicted <- data.frame(predict(growth_curve_drought_spline_list[[j]], i))
    gpp_predicted_list_2[[j]] <- gpp_predicted
    
  }
  
  gpp_predicted_list_df <- list_to_df(gpp_predicted_list_2)
  gpp_predicted_list_mean <- aggregate(y~x,mean,data=gpp_predicted_list_df)
  gpp_predicted_list_mean$lower <- median(gpp_predicted_list_df$y) - std.error(gpp_predicted_list_df$y)*2.576 #99% CI
  gpp_predicted_list_mean$upper <- median(gpp_predicted_list_mean$y) + std.error(gpp_predicted_list_df$y)*2.576
  colnames(gpp_predicted_list_mean) <- c('doy','mean','lower','upper')
  gpp_mean_list_2[[i]] <- gpp_predicted_list_mean
  
  
}

#turn into dataframe
gpp_mean_list_df_2 <- list_to_df(gpp_mean_list_2)
head(gpp_mean_list_df_2,1)
#?shade

filename <- paste0('./../../Data/growth_curves/drought_growth_curve_',Ecoregion,'.csv')
write.csv(gpp_mean_list_df_2,filename)

rm(gpp_df_mean,gpp_mean_list,gpp_mean_list_df_2,gpp_predicted,gpp_predicted_list,
   gpp_predicted_list_df,gpp_predicted_list_mean,growth_curve_spline_list)


# plot it out -------

gpp_mean_list_df <- read.csv(paste0('./../../Data/growth_curves/average_growth_curve_',Ecoregion,'.csv'))
gpp_mean_list_df_2 <- read.csv(paste0('./../../Data/growth_curves/drought_growth_curve_',Ecoregion,'.csv'))

# #plot it out (will vary by ecoregion)
plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = 'Cumulative GPP', xlab = 'Julian day of year',
     xlim=c(65,297), ylim=c(0,500))

#normal year
# lines(lower~doy,data=gpp_mean_list_df,lwd=1,lty='dashed')
# lines(upper~doy,data=gpp_mean_list_df,lwd=1,lty='dashed')
polygon(c(gpp_mean_list_df$doy, rev(gpp_mean_list_df$doy)),
        c(gpp_mean_list_df$lower,rev(gpp_mean_list_df$upper)), density = 200, col ='grey90')
lines(mean~doy,data=gpp_mean_list_df,lwd=3)
#
# # #?adjustcolor
# # 
# #droughtyear
polygon(c(gpp_mean_list_df_2$doy, rev(gpp_mean_list_df_2$doy)),
        c(gpp_mean_list_df_2$lower,rev(gpp_mean_list_df_2$upper)), density = 200, col='red')
        #adjustcolor("red",alpha.f=0.5))
lines(mean~doy,data=gpp_mean_list_df_2,lwd=3,col='black',lty='dashed')
# lines(lower~doy,data=gpp_mean_list_df_2,lwd=1,col='red',lty='dashed')
# lines(upper~doy,data=gpp_mean_list_df_2,lwd=1,col='red',lty='dashed')
# axis(side=1)
# axis(side=2)
# # axis(side=3)
# # axis(side=4)
# # 
# # #get GPP difference by day (need to add confidence interval)
# # gpp_mean_list_df$year <- 'average'
# # gpp_mean_list_df_2$year <- 'drought'
# # 
# # gpp_mean_list_df_binded <- merge(gpp_mean_list_df[c(1,2,5)],gpp_mean_list_df_2[c(1,2,5)],by=c('doy'))
# # head(gpp_mean_list_df_binded)
# # gpp_mean_list_df_binded$diff <- gpp_mean_list_df_binded$mean.y - gpp_mean_list_df_binded$mean.x
# # head(gpp_mean_list_df_binded)
# # 
# # #plot it out
# # plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = 'GPP difference between drought and normal year'
# #      , xlab = 'Julian day of year',
# #      xlim=c(65,297), ylim=c(-200,25))
# # lines(diff ~ doy, data=gpp_mean_list_df_binded,lwd=5)
# # abline(h=0,lty='dashed')
# # axis(side=1)
# # axis(side=2)
# 
# 
