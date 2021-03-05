
# This function calculate the basic statistics for the date introduce as an input parameter 

# input parameters:

#   date                       = text thata indicates the date you want to extract:
#                                 yyyy-mm-dd:
#   rasterbrick_for_statistics = the rasterbrick element with all the rasters for the different dates

# function outputs

#   daily_statistics           = Collection with the main statistics associated with the date 
#                                introduce as an input parameter
#                                (mean,min,max,sum) 

# this function does not create any file nor saves any element in secondary memmory

calculate_basic_statistics_for_1_date <- function(date,rasterbrick_for_statistics) 
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

