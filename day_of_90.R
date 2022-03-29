
# this scrips estimates the day by which ~90% of total productivity
# has been reached between day of year 57 and 297 and also explores
# how that changes during years of low precipitation

#setup----
plan(multisession, workers = 10)
#options(future.globals.maxSize = 8000 * 1024^2) #https://github.com/satijalab/seurat/issues/1845
period_list <- seq(1, 15, 1) #set periods
period_list <-
  as.character(period_list) #easier when they are characters
year_list <- seq(2003, 2020, 1) #set years
year_list <-
  as.character(year_list) #easier when they are characters


#first do the GPP import----

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
#head(gpp_df)
rm(gpp_df_mean)

#save to temporary file
#write.csv(gpp_df,'./../../Data/Temporary/gpp_df.csv')

#estimate day of 90% of growth----


#loop through list of each site id to get day by which 90% of cumulative growth has occurred
#str(gpp_df)
#id_list <- as.factor(unique(gpp_df$id))
id_list <- unique(gpp_df$id_value)
#doy_90_list <- list()

#get typical day by which 90% of growth has occurred
with_progress({
  p <- progressor(along = id_list)
  gpp_90_list <- future_lapply(id_list, function(i) {
    Sys.sleep(0.1)
    p(sprintf("i=%g", i))
    get_90_gpp(i)
  })
})

gpp_90_df <- do.call('rbind', gpp_90_list)
gpp_90_df <- rasterFromXYZ(gpp_90_df)
crs(gpp_90_df) <- "+proj=longlat +datum=WGS84"
#plot(gpp_90_df)
filename <-
  paste0('./../../Data/CDD/day_of_90/day_90_', Ecoregion, '.tif')
writeRaster(gpp_90_df, filename, overwrite = TRUE)


rm(gpp_90_df, gpp_90_list)


#import precip data  ----

#create list to store values
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

#ppt_df <- merge(ppt_df,gpp_df[c(1,2,7)])

#make unique id for each site
# ppt_df_mean <- aggregate(ppt~x+y,mean,data=ppt_df)
# ppt_df_mean$id <- seq.int(nrow(ppt_df_mean))
# head(ppt_df_mean)

#save to temporary file
#write.csv(ppt_df,'./../../Data/Temporary/ppt_df.csv')


#find drought years for each pixel and see how day of 90% gpp changes ----

#merge the two dataframes by location, year, and period within each year
ppt_gpp <- merge(gpp_df, ppt_df, by = c('x', 'y', 'year', 'period'))
#head(ppt_gpp)
rm(ppt_df)

#save to temporary file
#write.csv(ppt_gpp,'./../../Data/Temporary/ppt_gpp.csv')

#get total growing season precip
# gpp_ppt_annual <- aggregate(ppt~x+y+id_value+year,sum,data=ppt_gpp)
# rm(gpp_ppt_annual)
# #head(gpp_ppt_annual)

#id_list_ppt <- as.factor(unique(gpp_ppt_annual$id))

#get typical day by which 90% of growth has occurred during years of drought

#split workload into two batches (half of dataset each)
midpoint <- round(length(id_list) / 2)
id_list_1 <- 1:midpoint

#import
# gpp_df <- read.csv('./../../Data/Temporary/gpp_df.csv')
# gpp_df$id_value <- as.numeric(gpp_df$id_value)
# ppt_df <- read.csv('./../../Data/Temporary/ppt_df.csv')
# ppt_gpp <- read.csv('./../../Data/Temporary/ppt_gpp.csv')
# ppt_gpp$id_value <- as.numeric(ppt_gpp$id_value)

#subset data
# head(gpp_df)
# gpp_df <- gpp_df %>% arrange(id_value) %>%
#   filter(id_value <= midpoint)
#
# ppt_gpp <- ppt_gpp %>% arrange(id_value) %>%
#   filter(id_value <= midpoint)



#subset all the large dataframes to the halves, save to file, and then read in when using them
#see if that

#options(future.globals.maxSize= 1000)
#first half of data:
with_progress({
  p <- progressor(along = id_list_1)
  gpp_90_drought_list <- future_lapply(id_list_1, function(i) {
    Sys.sleep(0.1)
    p(sprintf("i=%g", i))
    get_90_gpp_drought(i)
  })
})

gpp_90_drought_df <- do.call('rbind', gpp_90_drought_list)
# gpp_90_drought_df <- gpp_90_drought_df[c(3,4,1,2)]
# gpp_90_drought_df$ecoregion <- Ecoregion
rm(gpp_90_drought_list)

#second half of data:
id_list_2 <- (midpoint + 1):length(id_list)

with_progress({
  p <- progressor(along = id_list_2)
  gpp_90_drought_list_2 <- future_lapply(id_list_2, function(i) {
    Sys.sleep(0.1)
    p(sprintf("i=%g", i))
    get_90_gpp_drought(i)
  })
})

gpp_90_drought_df_2 <- do.call('rbind', gpp_90_drought_list_2)
rm(gpp_90_drought_list_2)

gpp_90_drought_df_3 <- rbind(gpp_90_drought_df_2, gpp_90_drought_df)
#head(gpp_90_drought_df_3)

gpp_90_drought <- rasterFromXYZ(gpp_90_drought_df_3)
crs(gpp_90_drought) <- "+proj=longlat +datum=WGS84"
#plot(gpp_90_df)
filename <-
  paste0('./../../Data/CDD/day_of_90/day_90_drought',
         Ecoregion,
         '.tif')
writeRaster(gpp_90_drought, filename,overwrite=TRUE)
#plot(gpp_90_drought)

#plot(raster(filename))

#-----first look ------

# Ecoregion = 'shortgrass_steppe'
# Ecoregion = 'northern_mixed_prairies'
# 
# #day of 90
# filename <-
#   paste0('./../../Data/CDD/day_of_90/day_90_', Ecoregion, '.tif')
# day_90 <- raster(filename)
# plot(day_90)
# 
# day_90_df <- data.frame(rasterToPoints(day_90))
# head(day_90_df)
# plot(day_90_northern_mixed_prairies ~ y, data = day_90_df)
# plot(day_90_shortgrass_steppe ~ y, data = day_90_df)
# 
# #day of 90 during drought
# filename_2 <-
#   paste0('./../../Data/CDD/day_of_90/day_90_drought',
#          Ecoregion,
#          '.tif')
# day_90_drought <- raster(filename_2)
# plot(day_90_drought)
# 
# stack_test <- stack(day_90, day_90_drought)
# plot(stack_test)
# 
# #sgs
# new <-
#   stack_test$day_90_droughtshortgrass_steppe - stack_test$day_90_shortgrass_steppe
# plot(new)
# new_df <- data.frame(rasterToPoints(new))
# head(new_df)
# plot(layer ~ y, data = new_df)
# 
# #nmp
# new <-
#   stack_test$day_90_droughtnorthern_mixed_prairies - stack_test$day_90_northern_mixed_prairies
# plot(new)
# hist(new$layer)
# mean(new$layer)
# summary(new)
# new_df <- data.frame(rasterToPoints(new))
# head(new_df)
# plot(layer ~ y, data = new_df)
