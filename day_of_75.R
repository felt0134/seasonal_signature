# this scrips estimates the day by which ~75% of total productivity
# has been reached between day of year 57 and 297 and also explores
# how that changes during years of low precipitation

#setup----
plan(multisession, workers = 10)
options(future.globals.maxSize= 8000 * 1024^2)
#https://github.com/satijalab/seurat/issues/1845
#options(future.globals.maxSize = 8000 * 1024^2) #https://github.com/satijalab/seurat/issues/1845
period_list <- seq(1, 15, 1) #set periods
period_list <-
  as.character(period_list) #easier when they are characters
year_list <- seq(2003, 2020, 1) #set years
year_list <-
  as.character(year_list) #easier when they are characters


#import data
ppt_gpp <- readr::read_csv(paste0('./../../Data/GPP/Ecoregion/',Ecoregion,'/ppt_gpp_combined.csv'))
head(ppt_gpp,1)

#create a vector of unique sites IDs
id_list <- unique(ppt_gpp$id_value)

#get typical day at which 75% of total gpp has ocurred -----

with_progress({
  p <- progressor(along = id_list)
  gpp_75_list <- future_lapply(id_list, function(i) {
    Sys.sleep(0.1)
    p(sprintf("i=%g", i))
    day_75_gpp_no_drought(i)
  })
})


#collapse to dataframe
gpp_75 <- list_to_df(gpp_75_list)
rm(gpp_75_list)

#filter out extreme high and low values
gpp_75 <- gpp_75 %>%
  dplyr::filter(doy_75 < 297) %>%
  dplyr::filter(doy_75 > 65)

#save as raster
gpp_75 <- rasterFromXYZ(gpp_75)
crs(gpp_75) <- "+proj=longlat +datum=WGS84"
#plot(gpp_75)
filename <-
  paste0('./../../Data/CDD/day_of_75/day_75_', Ecoregion, '.tif')
writeRaster(gpp_75, filename, overwrite = TRUE)


#find drought years for each pixel and see how day of 75% gpp changes ----

#split workload into two batches (half of dataset each)
midpoint <- round(length(id_list) / 2)
id_list_1 <- 1:midpoint

#options(future.globals.maxSize= 1000)
#first half of data:
with_progress({
  p <- progressor(along = id_list_1)
  gpp_75_drought_list <- future_lapply(id_list_1, function(i) {
    Sys.sleep(0.1)
    p(sprintf("i=%g", i))
    day_75_gpp_drought(i)
  })
})

#collapse to dataframe
gpp_75_drought <- list_to_df(gpp_75_drought_list)
# gpp_75_drought_df <- gpp_75_drought_df[c(3,4,1,2)]
# gpp_75_drought_df$ecoregion <- Ecoregion
rm(gpp_75_drought_list)

#second half of data:
id_list_2 <- (midpoint + 1):length(id_list)

with_progress({
  p <- progressor(along = id_list_2)
  gpp_75_drought_list_2 <- future_lapply(id_list_2, function(i) {
    Sys.sleep(0.1)
    p(sprintf("i=%g", i))
    day_75_gpp_drought(i)
  })
})

#collapse to dataframe
gpp_75_drought_2 <- list_to_df(gpp_75_drought_list_2)
rm(gpp_75_drought_list_2)

#combine two dataframes
gpp_75_drought_3 <- rbind(gpp_75_drought_2, gpp_75_drought)
rm(gpp_75_drought_2, gpp_75_drought)
#head(gpp_75_drought_df_3)

#filter out extreme high and low values
gpp_75_drought_3 <- gpp_75_drought_3 %>%
  dplyr::filter(doy_75_drought < 297) %>%
  dplyr::filter(doy_75_drought > 65)

#save as raster
gpp_75_drought <- rasterFromXYZ(gpp_75_drought_3)
crs(gpp_75_drought) <- "+proj=longlat +datum=WGS84"
#plot(gpp_75_drought)
filename <-
  paste0('./../../Data/CDD/day_of_75/day_75_drought_',
         Ecoregion,
         '.tif')
writeRaster(gpp_75_drought, filename,overwrite=TRUE)
#plot(gpp_75_drought)

#plot(raster(filename))

#calculate the difference between the two
day_75_diff <- raster::stack(gpp_75_drought, gpp_75)
day_75_diff_2 <- day_75_diff$doy_75_drought - day_75_diff$doy_75
rm(day_75_diff)
# plot(day_75_diff_2)
# hist(day_75_diff_2$layer)

#save to file as raster 
filename <-
  paste0('./../../Data/CDD/day_of_75/day_75_drought_impact_',
         Ecoregion,
         '.tif')
writeRaster(day_75_diff_2, filename,overwrite=TRUE)

#done ------

