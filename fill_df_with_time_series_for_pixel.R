# This function generates a dataframe with the time series of variables for a pixel inside a rasterbrick 

#input parameters

#  pixel_index

#  rasterbrick

#  origin_date

fill_df_with_time_series_for_pixel<-function(pixel_index,rasterbrick,df_to_fill) 
{
  raster_rows<-dim(rasterbrick[[1]])[1]
  raster_columns<-dim(rasterbrick[[1]])[2]
    
  total_number_of_pixels<-raster_rows*raster_columns
  
  # This loop goes through all the times of the rasterbrick to populate the dataframe

  start_index_rasterbrick_time_dimension<-1
  final_index_rasterbrick_time_dimension<-dim(rasterbrick)[3]
  
  for (i in start_index_rasterbrick_time_dimension:final_index_rasterbrick_time_dimension) 
    { 
      print(paste("Filling dataframe for pixel",pixel_index,sep=" "))
      df_to_fill[i,"var"]<-rasterbrick[[i]][pixel_index]
      print(paste("current time possition",i,sep=" "))
    }
  print(paste("Dataframe for pixel",pixel_index,"created sucessfully",sep=" "))
  return(df_to_fill)
}
#clean console
cat("\014")

