# This function calculate the percentile N for all the pixels inside the rasterbrick introduce as an input 

# Input parameters:

#   N                          = integer [0-100] with the percentile  you want to  calcualte for each pixel

#   rasterbrick_for_statistics = the rasterbrick  with dialy rainfall data the rasters for the different dates

# function outputs
 
#   daily_statistics           = Collection with the main statistics associated with the date 
#                                introduce as an input parameter
#                                (mean,min,max,sum) 

# this function does not create any file nor saves any element 

calculate_percentiles_for_daily_CR2_data  <- function(N,rasterbrick_for_statistics) 
{
  
  extraction_day_index<-return_CR2_index(date,rasterbrick_for_statistics) 
  
  # Extracting a slice out of a rasterbrick 
  
  # this is the way to extract 1 raster out of a rasterbrick according to the index number
  raster_with_one_day_of_data <-rasterbrick_for_statistics[[extraction_day_index]]
  
  mean<-cellStats(raster_with_one_day_of_data, 'mean')
  min<-cellStats(raster_with_one_day_of_data, 'min')
  max<-cellStats(raster_with_one_day_of_data, 'max')
  sum<-cellStats(raster_with_one_day_of_data, 'sum')
  
  daily_statistics<-c(mean,min, max, sum)
  
  return(daily_statistics)
  # clean console
  cat("\014")
  
}
# clean console
cat("\014")
