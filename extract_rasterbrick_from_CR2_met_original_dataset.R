# This function extracts a rasterbrick for a specified catchment area 

# input parameters:

# main_directory
# variable_short_name
# mask_raster_name_file
# CR2_original_ncdf_filename
# location_to_load_ncdf_file
# location_to_load_mask_file



extract_rasterbrick_from_CR2_met_original_dataset<-function(main_directory,
                                                            variable_short_name,
                                                            mask_raster_name_file,
                                                            CR2_original_ncdf_filename,
                                                            location_to_load_ncdf_file,
                                                            location_to_load_mask_file)
{
  starting_execution_time<-Sys.time()
  load_libraries()
  
  setwd(location_to_load_ncdf_file)
  rasterbrick_with_all_chile <- brick(CR2_original_ncdf_filename, var=variable_short_name)
  setwd(main_directory)
  
  loading_route<- paste(location_to_load_mask_file,mask_raster_name_file, sep="/")
  mask_raster<-raster(loading_route)


  # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! WARNING
  # THIS TAKE SOME TIME TO RUN !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
  # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! WARNING
  
  # this is an example of the efficiency of the rasterbrick approach to operate over thousands of 
  # rasters with  just a few code lines and no loops 
  
  mask_extent <- extent(mask_raster)
  
  cropped_brick<-crop(rasterbrick_with_all_chile,mask_extent)
  
  extent(cropped_brick)<- mask_extent
  
  masked_brick <- mask(cropped_brick,mask_raster)
  
  # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! WARNING
  # THIS TAKE SOME TIME TO RUN !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
  # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! WARNING
  
  return(masked_brick)
  
  ending_execution_time<-Sys.time()
  execution_time<-ending_execution_time-starting_execution_time
  print("Execution time:")
  print(execution_time)
}
# clean console
cat("\014")

