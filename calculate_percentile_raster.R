# This function generates a raster with percentile values for a desire percentile value using as an input
# an netcdf file

#input parameters:

# percentile

# rasterbrick

# origin_date

calculate_percentile_raster<-function(percentile,rasterbrick,origin_date) 
{
  # The main loop over the number of pixels that represent the catchment area:
  
  # we use one of the rasters inside the rasterbrick as a template 
  #(any raster will do the job coz you will re-write it):
  
  raster_with_percentile_values<-rasterbrick[[13232]]
  
  # Loop for all the catchment pixels
  
  start_index_pixel_loop<-1
  ncells_catchemnt_area<-dim(raster_with_percentile_values)[1]*dim(raster_with_percentile_values)[2]
  
  limit_for_evaluation<-ncells_catchemnt_area #  can be any other number for testing this code
  
  for (i in start_index_pixel_loop:limit_for_evaluation) { 
    
    i_pixel<-rasterbrick[[1]][i]
    
    #if the pixel is different than NA you can create the dataframe with the time series otehrwise you 
    # don't waste computational resources trying to calculate a dataframe full of NA values
    
    if(!is.na(i_pixel)){
      
      print(paste("creating dataframe for pixel",i,"out of",ncells_catchemnt_area,"pixels",sep=" "))
      
      dates<-as.Date(1:dim(rasterbrick)[3], origin= origin_date)
      pixel_df<- data.frame("Date" = dates, "var"=as.double(0))
      
      i_pixel_df<-fill_df_with_time_series_for_pixel(i,rasterbrick,pixel_df)
      
      # The original CR2 datset has holes in some areas (missing values perhaps?) for certain dates. 
      # Therefore, when covnerting the ncdf file into a rasterbrick (for some catchment areas)  
      # some pixel values take values of 32000 (default no data value). 
      
      # The following function goes through all the time series and replace all the 32000 with "NA" 
      # We cannot assume that the pixel value is 32000 nor 0 coz that affects the percentile value 
      # of the pixel
      
      corrected_pixel_df<-fix_CR2_pixel_df(i,i_pixel_df)
      
      #here you calculate the percentile of your dataframe removing the NA values before :
      pix_per<- quantile(corrected_pixel_df[,"var"], probs=(percentile/100), na.rm=TRUE)
      raster_with_percentile_values[i]<-pix_per
    }
    else{raster_with_percentile_values[i]<-NA }
    
    print(paste("Percentile for pixel:",
                i,
                "out of",
                ncells_catchemnt_area,
                "calucalted succesfully !",
                sep =" " ))
    
  }
  
  return(raster_with_percentile_values)
  
  
  saving_directory <- paste(main_directory,"08-Result-rasters", sep="/")
  area_name <- "Copiapo-Catchment"
  file_name_for_saving_data <- paste("percentile",percentile,area_name,sep="-")
  
  save_a_raster_object_in_a_specified_location(saving_directory,
                                               main_directory,
                                               file_name_for_saving_data,
                                               raster_with_percentile_values)
  
  plot(raster_with_percentile_values)
  print(paste(percentile,"% percentile raster generated sucessfully",sep=" "))
   
}
#clean console
cat("\014")

