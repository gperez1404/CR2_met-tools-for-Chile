# This function saves a rasterbrick as a ncdf object using the specific location introuced as a parameter

# input parameters:

# name_of_saving_directory     String/char element with the 
# main_directory,              String/char element with the main directory 
# ncdf_filename_for_saving,
# rasterbrick_to_save,
# variable_short_name,
# variable_long_name,
# time_units

save_a_ncdf_object_in_a_specified_location<-function(name_of_saving_directory,
                                                     main_directory,
                                                     ncdf_filename_for_saving,
                                                     rasterbrick_to_save,
                                                     variable_short_name,
                                                     variable_long_name,
                                                     varunits_to_save,
                                                     time_units)
  
{
  saving_location<-paste(main_directory,name_of_saving_directory, sep="/")
  
  saving_route_with_filename<-paste(saving_location,ncdf_filename_for_saving, sep="/")
  
  writeRaster(rasterbrick_to_save, 
              saving_route_with_filename, 
              overwrite=TRUE, 
              format="CDF",     
              varname=variable_short_name, 
              varunit=varunits_to_save, 
              longname=variable_long_name, 
              xname="Longitude",   
              yname="Latitude", 
              zname=time_units)
  
  # Clean the consle
  cat("\014")
}
# Clean the consle
cat("\014")
