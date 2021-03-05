# CR2_met Data extraction for a particualr catchment

# this fucntion must be pre-compiled
load_libraries()

# before you load files you need to make sure you have a copy of the current working directory just in case
pc_default_route<-getwd()

# If you wanna plot in a separated Window:
options(device = "windows")

#if your are running in the hard drive of your PC:
setwd("C:/00-C-GPM_INFO/04-C-RDM/04-C-01-R/04-C-01-02-CR2-DAP")
main_directory<- getwd()

# Folder paths
input_ncdf_path<-"C:/00-C-GPM_INFO/04-C-RDM/04-C-01-R/04-C-01-02-CR2-DAP/04-Input-files"
input_raster_path<-"C:/00-C-GPM_INFO/04-C-RDM/04-C-01-R/04-C-01-02-CR2-DAP/05-Input-rasters"
input_shapes_path<-"C:/00-C-GPM_INFO/04-C-RDM/04-C-01-R/04-C-01-02-CR2-DAP/06-Input-shapes"

saving_directory_rasters <- "08-Result-rasters"

#file names
ncdf_filename<- "CR2MET_v1.4.2_pr.nc" 

mask_raster_file<-"Vinita_Azul_SWIFTCR2.tif"

# mask_raster_file<-"Vallenar_SWIFTCR2.tif"
# mask_raster_file<-"Paipote_SWIFTCR2.tif"
# mask_raster_file<-"Copiapo_SWIFTCR2.tif"
mask_raster_file<-"Pastillo_CR2.tif"

#variables
variable_short_name<-"pr"
area_name <- "Copiapo-SWIFT"

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!IMPORTANT
#                                     MANDATORY TO EXECUTE THIS SECTION
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!IMPORTANT

setwd(input_ncdf_path)
ncdf_element<- nc_open(ncdf_filename)
variable_long_name<-as.character(ncatt_get( ncdf_element, "pr", "long_name") [2][1])
variable_units<-as.character(ncatt_get( ncdf_element, "pr", "units")[2] [1])
time_units<-as.character(ncatt_get( ncdf_element, "time", "units") [2] [1])
time_origin_date<-strsplit(time_units,split=" ") [1] [[1]] [3]
setwd(main_directory)

rm(ncdf_element)

rasterbrick_with_all_chile <- brick(ncdf_filepath, var=variable_short_name)

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!IMPORTANT
#                                     MANDATORY TO EXECUTE THIS SECTION
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!IMPORTANT


# ^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^
#               Extraction of all  CR2_met rasters for all dates for an entire catchment/region
# ^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^

loading_route<- paste(input_raster_path,mask_raster_file, sep="/")
mask_raster<-raster(loading_route)
mask_extent <- extent(mask_raster)

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! WARNING
# THIS TAKE SOME TIME TO RUN !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! WARNING

# Here you crop the rasterbrick with all chile to the interest area
cropped_brick<-crop(rasterbrick_with_all_chile,mask_extent)
extent(cropped_brick)<- mask_extent
rasterbrick <- mask(cropped_brick,mask_raster)

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! WARNING
# THIS TAKE SOME TIME TO RUN !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! WARNING

# ^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^
#               Extraction of all  CR2_met rasters for all dates for an entire catchment/region
# ^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^


# x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|
#                       Spatial statistics using spatialpixeldataframe elements 
# x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|

extraction_date_text<- "2015-03-29"   # "yyy-mm-dd" 

# this function returns the index of the data matching the search criteria 
extraction_day_index<-return_CR2_index(extraction_date_text, rasterbrick_with_all_chile)


raster_with_one_day_of_data<-rasterbrick[[extraction_day_index]]

spdf_with_One_day_of_data <- as( raster_with_one_day_of_data,"SpatialPixelsDataFrame")

# with the following comand you can have a summary of the basic statistics for the raster
summary(spdf_with_One_day_of_data)

# how to visually assess the spatialpixelsdataframe element:
plot(spdf_with_One_day_of_data)

mean<-cellStats(raster_with_one_day_of_data, 'mean')
min<-cellStats(raster_with_one_day_of_data, 'min')
max<-cellStats(raster_with_one_day_of_data, 'max')
sum<-cellStats(raster_with_one_day_of_data, 'sum')

print(paste("mean value:",mean, sep=" "))
print(paste("min value:", min, sep=" "))
print(paste("max value:", max, sep=" "))
print(paste("sum value:", sum, sep=" "))

# raster's size (number of cells):
print(number_of_cells <- ncell(raster_with_one_day_of_data))

# raster's resolution (Degress):
res(raster_with_one_day_of_data)

# if you need the frequency distribution of the pixel values you can use the function freq()
# if you just want a simple one you sholud choose 0 decimal positions for the class breaks
freq_matrix_for_1_raster<-freq(raster_with_one_day_of_data, digits=0, value=NULL, useNA='no')

#if you want to plot the histogram of the pixwel values for 1 raster:
hist(raster_with_one_day_of_data)

# x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|
#                  End of spatial statistics using spatialpixeldataframe elements 
# x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|x_x|

