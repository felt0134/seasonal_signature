
#get max and total c upktake reductions during drought for each pixel

# setup
plan(multisession, workers = 10)
options(future.globals.maxSize = 8000 * 1024^2) #https://github.com/satijalab/seurat/issues/1845
period_list <- seq(1, 15, 1) #set periods
period_list <-
  as.character(period_list) #easier when they are characters
year_list <- seq(2003, 2020, 1) #set years
year_list <-
  as.character(year_list) #easier when they are characters

#import
ppt_gpp <- readr::read_csv(paste0('./../../Data/GPP/Ecoregion/',Ecoregion,'/ppt_gpp_combined.csv'))
head(ppt_gpp,1)

id_list <- unique(ppt_gpp$id_value)

# run function on each pixel
with_progress({
  p <- progressor(along = id_list)
  max_total_reduction_list <- future_lapply(id_list, function(i) {
    Sys.sleep(0.1)
    p(sprintf("i=%g", i))
    get_max_total_reduction(i)
  })
})

max_total_reduction_df <- list_to_df(max_total_reduction_list)
rm(max_total_reduction_list)

filename <- paste0('./../../Data/growth_dynamics/max_total_reduction_',Ecoregion,'.csv')
write.csv(max_total_reduction_df, filename)