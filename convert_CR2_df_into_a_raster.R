# This function transforms a dataframe object containing CR2_met data for 1 day 
# into a GIS compatible raster  

#input parameters:

#   CR2_df
#   CRS_code

# This function returns a raster containing Cr2 data that is compatible with ArcGIS and GDAL

convert_CR2_df_into_a_raster<-function(CR2_df,CRS_code)
{  
# here you create a raster containing the CR2_met data for 1 day
# caution: this raster is missing spatial reference !
raster_for_1_day_of_data <- rasterFromXYZ(CR2_df)

# visual check of raster properties
#print(raster_for_1_day_of_data) # originalversion

# how to assign CRS to a raster created from a NetCDF file?

# For the particular case of CR2_met data, we already know that the CRS is geographic-> WGS84
# the function CRS() requires as an input argument a string value with the code name of the Coordinate 
# Reference System (CRS)

projection(raster_for_1_day_of_data) <- CRS(CRS_code)

return(raster_for_1_day_of_data)

# Clean the consle
cat("\014")
}
# Clean the consle
cat("\014")
