# This function creates a daframe object for 1 date using as an input the following parameters:

# ncdfname
# main_directory
# loading_directory
# extraction_day_index
# variable_short_name

# This function returns a dataframe object with all the CR2_met data fot 1 day

extract_1_df_out_of_a_CR2_ncdf_file_for_1_day<-function(ncdfname,
                                                        main_directory,
                                                        loading_directory,
                                                        extraction_day_index,
                                                        variable_short_name)
{
setwd(loading_directory)
ncdf_file <- nc_open(ncdfname)
setwd(main_directory)

# Here you create a spatial grid with the coordinates of the CR2 data for the entire country

longitude_ncdf<- ncvar_get(ncdf_file, "lon")
latitude_ncdf<- ncvar_get(ncdf_file, "lat")

lonlat_grid <- expand.grid(longitude_ncdf, latitude_ncdf)

starting_point_time <- c(1,1,extraction_day_index) #start []
time_constrain <- c(-1,-1,1)                       #count []


CR2_ncdf_1_day_matrix <- ncvar_get(ncdf_file,variable_short_name,start=starting_point_time, 
                                      count=time_constrain)


vector_1_day<- as.vector(CR2_ncdf_1_day_matrix)

# Here you create a dataframe for 1 date (random day) to extarct the spatial extent for the 
# CR2 dataset 

df_1_day<- data.frame(cbind(lonlat_grid, vector_1_day))

# here you create an array with the names of the labels of the graph
names(df_1_day) <- c("lon", "lat", variable_short_name)

return(df_1_day)

rm(ncdf_file)
rm(longitude_ncdf)
rm(latitude_ncdf)
rm(lonlat_grid)
rm(starting_point_time)
rm(time_constrain)
rm(CR2_ncdf_1_day_matrix)
rm(vector_1_day)

#clean the console
cat("\014")
}
#clean the console
cat("\014")
