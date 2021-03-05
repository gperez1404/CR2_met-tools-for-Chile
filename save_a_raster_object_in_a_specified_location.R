# This function saves a GIS raster object in a specific location introuced as a parameter

# input parameters:

#   saving_folder_name
#   main_directory
#   file_name
#
  
save_a_raster_object_in_a_specified_location<-function(saving_folder_name,
                                                       main_directory,
                                                       file_name,
                                                       raster_to_save)

{
  saving_location<-paste(main_directory,saving_folder_name, sep="/")
  
  rasterfile_extension <- ".tif"
  file_name_with_extension <- paste(file_name,rasterfile_extension, sep ="")
  
  setwd(saving_location)
  writeRaster(raster_to_save, filename=file_name_with_extension, overwrite=TRUE)
  setwd(main_directory)
  
  rm(saving_location)
  rm(rasterfile_extension)
  rm(file_name_with_extension)
  
  # Clean the consle
  cat("\014")
}
# Clean the consle
cat("\014")

