
# This function calculate the basic statistics for the date introduce as an input parameter 

# input parameters:


#   raster_for_statistics       = the raster element 

# function outputs

#   daily_statistics           = Collection with the main statistics associated with the date 
#                                introduce as an input parameter
#                                (mean,min,max,sum) 

# this function does not create any file nor saves any element in secondary memmory

calculate_basic_statistics_for_1_raster <- function(raster_for_statistics) 
{

  mean<-cellStats(raster_for_statistics, 'mean')
  min<-cellStats(raster_for_statistics, 'min')
  max<-cellStats(raster_for_statistics, 'max')
  sum<-cellStats(raster_for_statistics, 'sum')
  
  daily_statistics<-c(mean,min, max, sum)
  
  return(daily_statistics)
}
# clean console
cat("\014")

