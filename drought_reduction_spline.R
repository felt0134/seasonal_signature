

#get % reduction in GPP throughout the growing season

# setup----
#library(plotrix)
plan(multisession, workers = 10)
options(future.globals.maxSize = 8000 * 1024^2) #https://github.com/satijalab/seurat/issues/1845
period_list <- seq(1, 15, 1) #set periods
period_list <-
  as.character(period_list) #easier when they are characters
year_list <- seq(2003, 2020, 1) #set years
year_list <-
  as.character(year_list) #easier when they are characters

#save this file so can just pull it out when re-running this code
ppt_gpp <- readr::read_csv(paste0('./../../Data/GPP/Ecoregion/',Ecoregion,'/ppt_gpp_combined.csv'))
head(ppt_gpp,1)

#aggregate(gpp~period,max,data=ppt_gpp)

#plot(gpp~period,data=ppt_gpp)

# get splines -----

#create a vector of unique sites IDs
id_list <- unique(ppt_gpp$id_value)

# average growth 
with_progress({
  p <- progressor(along = id_list)
  growth_spline_list <- future_lapply(id_list, function(i) {
    Sys.sleep(0.1)
    p(sprintf("i=%g", i))
    gpp_spline(i)
  })
})


# drought growth curve
with_progress({
  p <- progressor(along = id_list)
  growth_drought_spline_list <- future_lapply(id_list, function(i) {
    Sys.sleep(0.1)
    p(sprintf("i=%g", i))
    gpp_spline_drought(i)
  })
})

#get each CI for each day of the prediction
doy_list <- c(65:297)

#loop
gpp_predicted_list_average <- list()
gpp_predicted_list_drought <- list()
gpp_reduction_list <- list()
gpp_reduction_list_2 <- list()

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
  
  #convert to dataframe and remove values/pixels below zero and extreme high values/outliers
  
  #average
  gpp_predicted_list_average_df <- list_to_df(gpp_predicted_list_average)
  colnames(gpp_predicted_list_average_df) <- c('doy','gpp_average','id_val')
  high_average <- quantile(gpp_predicted_list_average_df$gpp_average,probs = 0.99)
  gpp_predicted_list_average_df <- gpp_predicted_list_average_df %>%
    filter(gpp_average > 0) #%>%
    #filter(gpp_average < high_average) 
  
  #drought
  gpp_predicted_list_drought_df <- list_to_df(gpp_predicted_list_drought)
  colnames(gpp_predicted_list_drought_df) <- c('doy','gpp_drought','id_val')
  high_drought <- quantile(gpp_predicted_list_drought_df$gpp_drought,probs = 0.99)
  gpp_predicted_list_drought_df <- gpp_predicted_list_drought_df %>%
    filter(gpp_drought > 0) #%>%
    #filter(gpp_drought < high_drought)
  
  hist(gpp_predicted_list_drought_df$gpp_drought,main = i)
  # summary(gpp_predicted_list_average_df)
  # hist(gpp_predicted_list_drought_df$gpp_drought)
  # summary(gpp_predicted_list_drought_df)
  
  gpp_predicted_drought_average <- merge(gpp_predicted_list_drought_df,gpp_predicted_list_average_df,
                                         by=c('doy','id_val'))
  
  ss <- nrow(gpp_predicted_drought_average)
  
  gpp_predicted_drought_average_3 <- gpp_predicted_drought_average #use for absolute change calculation
  
  #relative
  gpp_predicted_drought_average$perc_change <- ((gpp_predicted_drought_average$gpp_drought -
    gpp_predicted_drought_average$gpp_average)/gpp_predicted_drought_average$gpp_average)*100

  #get average
  gpp_predicted_drought_average_2 <- aggregate(perc_change~doy,median,data=gpp_predicted_drought_average)

  #get and add CI
  #gpp_predicted_drought_average_2$ci_99 <- std.error(gpp_predicted_drought_average$perc_change)*2.576
  #gpp_predicted_drought_average_2$ci_99 <- sd(gpp_predicted_drought_average$perc_change)
  gpp_predicted_drought_average_2$ci_95 <- quantile(gpp_predicted_drought_average$perc_change,probs=0.95)
  gpp_predicted_drought_average_2$ci_05 <- quantile(gpp_predicted_drought_average$perc_change,probs=0.05)
  gpp_predicted_drought_average_2$sample_size <- ss
  gpp_reduction_list[[i]] <- gpp_predicted_drought_average_2
  
  #absolute
  gpp_predicted_drought_average_3$abs_change <- gpp_predicted_drought_average_3$gpp_drought -
                                                   gpp_predicted_drought_average$gpp_average
  
  #get mean
  gpp_predicted_drought_average_4 <- aggregate(abs_change~doy,median,data=gpp_predicted_drought_average_3)
  
  #get and add CI
  #gpp_predicted_drought_average_4$ci_99 <- std.error(gpp_predicted_drought_average_3$abs_change)*2.576
  #gpp_predicted_drought_average_4$ci_99 <- sd(gpp_predicted_drought_average_3$abs_change)
  gpp_predicted_drought_average_4$ci_75 <- quantile(gpp_predicted_drought_average_3$abs_change,probs=0.75)
  gpp_predicted_drought_average_4$ci_25 <- quantile(gpp_predicted_drought_average_3$abs_change,probs=0.25)
  gpp_predicted_drought_average_4$sample_size <- ss
  gpp_reduction_list_2[[i]] <- gpp_predicted_drought_average_4
  
}

gpp_reduction_list_df <- list_to_df(gpp_reduction_list)
head(gpp_reduction_list_df,1)

filename <- paste0('./../../Data/growth_dynamics/drought_gpp_reduction_',Ecoregion,'.csv')
write.csv(gpp_reduction_list_df,filename)

gpp_reduction_list_df_2 <- list_to_df(gpp_reduction_list_2)
head(gpp_reduction_list_df_2,1)

filename <- paste0('./../../Data/growth_dynamics/drought_gpp_reduction_absolute_',Ecoregion,'.csv')
write.csv(gpp_reduction_list_df_2,filename)

rm(gpp_predicted_average,gpp_predicted_drought,gpp_predicted_drought_average,
   gpp_predicted_drought_average_2,gpp_predicted_list_average,gpp_predicted_list_average_df,
   gpp_predicted_list_drought,gpp_predicted_list_drought_df,gpp_reduction_list,
   growth_drought_spline_list,growth_spline_list,gpp_reduction_list_2,gpp_predicted_drought_average_3,
   gpp_predicted_drought_average_4)


#plot this out ------
# str(gpp_reduction_list_df)
# gpp_reduction_list_df$upper <- gpp_reduction_list_df$perc_change + gpp_reduction_list_df$ci_99
# gpp_reduction_list_df$lower <- gpp_reduction_list_df$perc_change - gpp_reduction_list_df$ci_99
# plot(perc_change~doy,data=gpp_reduction_list_df,cex=0.1,
#      xlab='Julian day',ylab='Drought impact (% change in GPP)')
# lines(perc_change~doy,data=gpp_reduction_list_df)
# lines(upper~as.numeric(as.integer(doy)),gpp_reduction_list_df)
# lines(lower~doy,gpp_reduction_list_df)
# abline(h=0)
# 
# gpp.doy.spl <-
#   with(gpp_reduction_list_df, smooth.spline(doy, perc_change))
#lines(gpp.doy.spl, col = "blue")

# #import and merge
# gpp_reduction_list_df <- read.csv(paste0('./../../Data/growth_dynamics/drought_gpp_reduction_',Ecoregion,'.csv'))
# gpp_mean_list_df <- read.csv(paste0('./../../Data/growth_dynamics/average_gpp_',Ecoregion,'.csv'))
# 
# normal_drought_df <- merge(gpp_mean_list_df_2[c(2,3,5)],gpp_mean_list_df[c(2,3,5)],by='doy')
# head(normal_drought_df)
# 
# normal_drought_df$perc_change <- 
#   ((normal_drought_df$mean.x - normal_drought_df$mean.y)/normal_drought_df$mean.y)*100
# 
# head(normal_drought_df)
# plot(perc_change ~ doy,data=normal_drought_df,xlab='Julian day',ylab='Drought impact (% change in GPP)')
# abline(h=0)

