# Climate Change Risk Analysis for Chilean mining regions using CR2 datasets 

# Data extraction script
# this is the first Scrip to use/analyze climate data


# ========================================================================================================
#  Script Index
# ========================================================================================================

#  24-74............. Read me section 
#  76-119............ Definition of working directory and mandatory variables declarations 
#  121-349 .......... Exploring CR2 datasets
#  351-421 .......... Opening  CR2 data in NEtCDFfile file using Rasterbricks 
#  -........... Extracting CR2 data for a particular date Converting a dataframe object into a GIS compatible raster 
#  645-668........... Extracting 1 raster out of a rasterbrick for a particular area: catchment  
#  670-707........... Saving data
#  710-761..........  Saving plots in a specific folder

# ========================================================================================================
#  Script Index
# ========================================================================================================

#################################################################################################### README 
#                  IF YOU HAVE NEVER USED R NOR Rstudio PLEASE READ THIS SECTION
#################################################################################################### README

# The following lines and comments provide a basic explanation of how to execute R code and how to use 
# this script

# press Ctrl + enter to run code line by line

# how to check the version of R ?
R <-version   
R # displays the details of the R version installed in your computer

# Installing neccesary packages....

# useful packages for  plots and spatiald data analysis
# uncomment only if the packages are not isntalled in your computer 
# (this is executed just once in your machine)

#ggmap: extends the plotting package ggplot2 for maps
#rgdal: extends interface to the popular C/C++ spatial data processing library gdal
#rgeos: extends interface to the powerful vector processing library geos
#maptools: provides various mapping functions
#dplyr and tidyr: fast and concise data manipulation packages
#tmap: a new packages for rapidly creating beautiful maps

# how to manually install just 1 package ?
# Generic code line for manual installation of 1 package:

#install.packages("package_name")

#Examples:
#install.packages("gganimate")
#install.packages("magick")
#install.packages('broom')

#packages_to_install <- c("ggmap", "rgdal", "rgeos", "maptools", "dplyr", "tidyr","tmap","tmaptools",
#                         "OpenStreetMap","Rcpp","rJava","rlang","osmar","chron", "ncdf4","sf","tiff",
#                         "tidyverse","resample")


# The following code is used to Download the required packages (you just need to do this once)
# warning: Executing the following line may take several minutes and you need internet conenction:
# install.packages(packages_to_install) 

# Example of an advance way of download a package :
# if (!require(devtools)) install.packages("devtools")

#################################################################################################### README 
#                ^^ IF YOU HAVE NEVER USED R NOR Rstudio PLEASE READ THIS SECTION ^^
#################################################################################################### README

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!IMPORTANT
#                                     MANDATORY TO EXECUTE THIS SECTION
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!IMPORTANT

# It is absolutely neccesary to run these lines before doing any analysiis or calling any function !

# Loading the neccesary libraries....
# The following function is called to load the required libraries and packages

# this fucntion must be pre-compiled
load_libraries()

# before you load files you need to make sure you have a copy of the current working directory just in case
pc_default_route<-getwd()

# before running this code in your computer for the first time you need to change this route by hand 
# (it differs for each user):

#if your are running in the hard drive of your PC:
setwd("C:/00-C-GPM_INFO/04-C-RDM/04-C-01-R/04-C-01-02-CR2-DAP")

# if you are running in the UQ-RDM server:
#setwd("R:/PHD042018-Q1021/09-R-CCRMC2019/09-R-06-Scripts/06-01-R/CR2-DAP")

main_directory<- getwd()

# CR2_met daily products (current cliamnte conditions):

ncdfname_pr   <- "CR2MET_v1.4.2_pr.nc" 
ncdfname_tmin <- "CR2MET_v1.3_tmin.nc"
ncdfname_tmax <- "CR2MET_v1.3_tmax.nc"

# Here you define the variable you want to analize

# variable_short_name<-"tmin"
# variable_short_name<-"tmax"
variable_short_name<-"pr"

# Clean the consle
cat("\014")

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!IMPORTANT
#                                     MANDATORY TO EXECUTE THIS SECTION
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!IMPORTANT

# >
# >>>
# >>>>>>>>
# >>>>>>>>>>>>>>>>>
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Exploring the CR2_met products


# This script allows you to do a detailed analysis of CR2_met daily pr data at a regional/catchment scale. 

# statistics will be executed for a group of catchments within Chile that have been predefined by the user

# the CR2_met pr is saved in NetCDF format (network Common Data Form).This is a self-documenting, 
# machine-independent format for creating and distributing arrays of gridded data. It was originally 
# developed for storing and distributing climate data.

# a netCDF file  has dimensions, variables and attributes. In this particular case, CR2 pr data has 3 
# dimensions (x,y and Time). The R packages 'ncdf', 'ncdf4' and 'raster' provide the  necessary support 
# for reading and  writing NetCDF files. the package ncdf is not supported in the new versions of R, so 
# all the code will be developed using ncdf4.

# For more info about the fucntions of the package "ncdf4" visit the following website :
# https://www.rdocumentation.org/packages/ncdf4/versions/1.16

# to consult the version of your ncdf package execute the following line:
# nc_version()

# Opening the NetCDF files with CR2_met daily precipitation and temperature data for all Chile

loading_directory<-paste(main_directory,"04-Input-files", sep="/")
setwd(loading_directory)


if (variable_short_name=="pr") {
  Chile_daily_pr_ncdf       <- nc_open(ncdfname_pr)
  chile_daily_ncdf<-Chile_daily_pr_ncdf
} else if (variable_short_name=="tmin"){
  chile_daily_tmin_ncdf     <- nc_open(ncdfname_tmin)
  chile_daily_ncdf<-chile_daily_tmin_ncdf
} else if (variable_short_name=="tmax"){
  chile_daily_tmax_ncdf     <- nc_open(ncdfname_tmax)
  chile_daily_ncdf<-chile_daily_tmax_ncdf
}
    
setwd(main_directory)

# Some basic information can be obtained using the print() function 

# print() displays the information in CDL notation (Common Data Language)
# CDL is the ASCII format used to describe the content of a NetCDF file
# the names of dimensions, attributes and variables are case sensitive

# The dimensions of the ncdf refer to the estructure used to store 
# the information in each pixel of the raster. For the particular case 
# of CR2 data there are 3 dimnensions:

#    lon : longitude = x coordiantes
#    lat : latitude = y coordiantes
#    time: number of days with data in the format yyyy-mm-dd

# the variables are the information that is related to an especic set of
# of dimensions. For this particular case, each set of (lon,lat,time) 
# has a precipitation value associated, so "Preipitation" is the only variable in 
# the CR2_met dataset

# How to display general information of the NetCDF file ?
print(chile_daily_ncdf)

# more verbose information can be obtained with the str() function
str(chile_daily_ncdf)

# How to extract the attributes of a variable from a NEtCDF file ?
# There is only 1 variable inside CR2_met data file (daily precipitation 
# to extract the attributes you have to use the function ncatt_get()
# the second parameter of this function is The variable whose attributes you want to know

if (variable_short_name=="pr") {
  pr_attributes<- ncatt_get( Chile_daily_pr_ncdf, "pr")
  pr_units<- ncatt_get( Chile_daily_pr_ncdf, "pr", "units") [2]
  pr_lon_name<- ncatt_get( Chile_daily_pr_ncdf, "pr", "long_name") [2]
  pr_fill_value <- ncatt_get(Chile_daily_pr_ncdf, "pr", "_FillValue")
  
  varunits<-pr_units
  variable_long_name<-pr_lon_name
  time_units<-ncatt_get( Chile_daily_pr_ncdf, "time", "units") [2]
  
  time_units_text<-paste( unlist(time_units), collapse='')
  parts_of_text_time_units<-strsplit(time_units_text, split = " ")
  time_origin<-parts_of_text_time_units[[1]][3]
  
  print(pr_attributes)   # main attributes of precipitation variable
  print(pr_units)        # unit of precipitation
  print(pr_lon_name)     # long name of pr variable
  print(pr_fill_value)   # value uso to fill the no data spaces
  
  print(time_units)       # Time units of the ncdf dataset
  
  
  # How to extract a spatial element with the coordinates of all the cells in the NetCDF file 
  # This is an spatial element with the spatial dimension only: 
  longitude_CR2_ncdf<- ncvar_get(Chile_daily_pr_ncdf, "lon")
  lattitude_CR2_ncdf<- ncvar_get(Chile_daily_pr_ncdf, "lat")
  
  # how to extract an array (1D- element) with the list of time values (dates) from the NetCDF file  ?
  # in this particular case CR2_Met dataset has 13880 values of daily precipitation which means there  
  # are 13880 days of data
  
  times_CR2_ncdf<- ncvar_get(Chile_daily_pr_ncdf,"time")
  rm(times_CR2_ncdf)
  
  #The name of the ncdf file to load the data
  ncdfname <- "CR2MET_v1.4.2_pr.nc"
  
} else if (variable_short_name=="tmin"){
  tmin_attributes<- ncatt_get( chile_daily_tmin_ncdf, "tmin")
  tmin_units<- ncatt_get( chile_daily_tmin_ncdf, "tmin", "units") [2]
  tmin_lon_name<- ncatt_get( chile_daily_tmin_ncdf, "tmin", "long_name") [2]
  tmin_fill_value <- ncatt_get(chile_daily_tmin_ncdf, "tmin", "_FillValue")
  
  varunits<-tmin_units
  variable_long_name<-tmin_lon_name
  time_units<-ncatt_get( chile_daily_tmin_ncdf, "time", "units") [2]
  
  time_units_text<-paste( unlist(time_units), collapse='')
  parts_of_text_time_units<-strsplit(time_units_text, split = " ")
  time_origin<-parts_of_text_time_units[[1]][3]
  
  print(tmin_attributes)   # main attributes of precipitation variable
  print(tmin_units)        # unit of precipitation
  print(tmin_lon_name)     # long name of pr variable
  print(tmin_fill_value)   # value uso to fill the no data spaces
  
  print(time_units)       # Time units of the ncdf dataset
  
  # How to extract a spatial element with the coordinates of all the cells in the NetCDF file 
  # This is an spatial element with the spatial dimension only: 
  longitude_CR2_ncdf<- ncvar_get(chile_daily_tmin_ncdf, "lon")
  lattitude_CR2_ncdf<- ncvar_get(chile_daily_tmin_ncdf, "lat")
  
  # how to extract an array (1D- element) with the list of time values (dates) from the NetCDF file  ?
  # in this particular case CR2_Met dataset has 13880 values of daily precipitation which means there  
  # are 13880 days of data
  times_CR2_ncdf<-ncvar_get(chile_daily_tmin_ncdf,"time")
  
  #The name of the ncdf file to load the data
  ncdfname <- "CR2MET_v1.3_tmin.nc"
  
} else if (variable_short_name=="tmax"){
  tmax_attributes<- ncatt_get( chile_daily_tmax_ncdf, "tmax")
  tmax_units<- ncatt_get( chile_daily_tmax_ncdf, "tmax", "units") [2]
  tmax_lon_name<- ncatt_get( chile_daily_tmax_ncdf, "tmax", "long_name") [2]
  tmax_fill_value <- ncatt_get(chile_daily_tmax_ncdf, "tmax", "_FillValue")
  
  varunits<-tmax_units
  variable_long_name<-tmax_lon_name
  time_units<-ncatt_get( chile_daily_tmax_ncdf, "time", "units") [2]
  
  time_units_text<-paste( unlist(time_units), collapse='')
  parts_of_text_time_units<-strsplit(time_units_text, split = " ")
  time_origin<-parts_of_text_time_units[[1]][3]
  
  print(tmax_attributes)   # main attributes of precipitation variable
  print(tmax_units)        # unit of precipitation
  print(tmax_lon_name)     # long name of pr variable
  print(tmax_fill_value)   # value uso to fill the no data spaces
  
  print(time_units)       # Time units of the ncdf dataset
  
  # How to extract a spatial element with the coordinates of all the cells in the NetCDF file 
  # This is an spatial element with the spatial dimension only: 
  longitude_CR2_ncdf<- ncvar_get(chile_daily_tmax_ncdf, "lon")
  lattitude_CR2_ncdf<- ncvar_get(chile_daily_tmax_ncdf, "lat")
  
  # how to extract an array (1D- element) with the list of time values (dates) from the NetCDF file  ?
  # in this particular case CR2_Met dataset has 13880 values of daily precipitation which means there  
  # are 13880 days of data
  
  times_CR2_ncdf<-ncvar_get(chile_daily_tmax_ncdf,"time")
  
  #The name of the ncdf file to load the data
  ncdfname <- "CR2MET_v1.3_tmax.nc"
  
}

variable_units<-as.character(varunits[1])
variable_long_name<-as.character(variable_long_name[1])
time_units<-as.character(time_units[1])

# Clean the consOle
cat("\014")

# number of columns (squares in the x direction):
number_longitude_CR2_ncdf <- dim(longitude_CR2_ncdf)

# number of rows (squares in the y direction):
number_latitude_CR2_ncdf <- dim(lattitude_CR2_ncdf)

# confirm to the user the spatial resolution of the dataset
print(paste("Dimensions of the spatial grid containing the NetCDF data in the x direction:",
            number_longitude_CR2_ncdf, sep=" "))

print(paste("Dimensions of the spatial grid containing the NetCDF data in the y direction:",
            number_latitude_CR2_ncdf ,sep=" "))

# Dimensions of a CR2  product should be interpretated as:

#        nrows=800      -> latitude   (spatial  dimension on the y direction)   
#        ncols=220      -> longitude  (spatial  dimension on the x direction)
#        ncells=17600   -> grid_size  (data base dimension or the number of cells that organize the data)
#        nlayers=13880  -> date       (temproal dimension or the z direction)

# number of time elements stored in the loaded ncdf file:
number_times_CR2_ncdf <- dim(times_CR2_ncdf)

#if you don't need the ncdf file you should delete it from the workspace to save RAM memory:
rm(chile_daily_tmin_ncdf)
rm(chile_daily_tmax_ncdf)
rm(Chile_daily_pr_ncdf)

# Clean the consle
cat("\014")

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Exploring the CR2_met products
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# >>>>>>>>>>>>>>>>>
# >>>>>>>>
# >>>
# >

#___________________________________________________________________________________________________________
#___________________________________________________________________________________________________________
#_____________________Extracting  data out of a NEtCDFfile using Rasterbricks_______________________________
#___________________________________________________________________________________________________________
#___________________________________________________________________________________________________________

# When dealing with spatial data is a very bad idea to use a nested iterative fucntions (for() loops...) 
# to do  statistics over a large number of spatial data like NetCDF files with thousands of rasters inside

# By combining functional style with the use of the raster() library, we eliminate the need for nested loops 
# and boil the problem down to a streamlined "apply+function" structure:


# The first tstep to handle ncdf files is to convert them  into a "rasterbrick"  element ...

# Once you have a rasterbrick element you can get spatial staistics very easily

# Alternative 1 :
# lapply( rasterbrick_element , function to apply to each raster inside the rasterbrick)

# Alternative 2
#calc(rasterbrick_element , function to apply to each raster inside the rasterbrick)

# the following lines are used to load the NetCDF file in rasterbrick format

# In most cases, we can work with  a RasterBrick in the same way we might work with a RasterStack. 
# However a RasterBrick is often more efficient and faster to process - which is important when working 
# with large files.

# Rasterbricks are multi-dimensional structures that hold information in the way of (time,[longitude, 
# latitude, variable]). in other words, inside a rasterbrick each time has associated a raster. 

# We eliminate the neccesity of a loop with the help of RasterBrick manipulations. That is, instead of 
# looping  over the individual raster layers within a NetCDF file, we transform the NetCDF into a 
# RasterBrick object and manipulate the collection of layers (raster for each day) as a single object.

# Appealing to RasterBrick instead of cycling through individual raster layers, is a lot like the 
# "vectorization", where instead of iteraring over individual members of a vector, one-by-one, we work 
# directly with  the vector. 

# This is a common line of attack for writing more efficient code.

# the main problem with the rasterbrick format is that the rows, columns and layers are switched compare 
# whith a normal GIS raster, which results in an incorrect interpretation of data when trying to pull out 
# a specific raster for a specific date (one particular day). 

#The name of the ncdf file to load the data
# ncdfname <- "CR2MET_v1.4.2_pr.nc"

loading_directory<-paste(main_directory,"04-Input-files", sep="/")
setwd(loading_directory)
rasterbrick_with_all_chile <- brick(ncdfname, var=variable_short_name)
setwd(main_directory)

# the following line can tell you the list of methods and functions available to deal with rasterbrick 
#objects:

methods(class=class(rasterbrick_with_all_chile))

# The following lines display on console the main info and properties of the brick raster file
print(dim(rasterbrick_with_all_chile))    #dimension :nrows ncols nlayers 
object.size(rasterbrick_with_all_chile)   #object size in memmory
rasterbrick_with_all_chile                #general info

# Clean the consle
cat("\014")

#____________________________________________________________________________________________________________
#____________________________________________________________________________________________________________
#____________________ End of Extracting data out of a NEtCDFfile using Rasterbricks_________________________
#____________________________________________________________________________________________________________
#____________________________________________________________________________________________________________


# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
#           Useful information to extract data out of a netcdf file using the function ncvar_get()
# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

# parameters and syntaxis of ncvar_get() function:

# ncvar_get(nc, varid=NA, start=NA, count=NA, verbose=FALSE, signedbyte=TRUE, collapse_degen=TRUE, 
#           raw_datavals=FALSE )

# Arguments of this function

# varid = name of the variable you want to pull out from the NEtCDF file
# start = A vector of indices indicating the starting point of the reading. 
#         the vector size must be equal to the NEtCDF number of dimensions
#         for the particular case of CR2_met dataset the number of dimensions is 3
#         if not specified reading starting at (1,1,1) 
# count = A vector of integers indicating the count of values to read along each 
#         dimension (order is X-Y-Z-T). As a special case, the value "-1" indicates that all entries
#         along that dimension should be read


# If you want to pull out 1 day of data for the entire country you should use the following  
# constrain vectors:

# start = [1,1,X]   -> means you want to extract all the spatial dimnension but you are just interest  
#                      in the time dimension starting at the time "x" 

# count = [-1,-1,1] -> -1 means you want to extract all elements that comprises the dimension after 
#                      the starting point. In this case we are saying we want to extract all the 
#                      the elements in x and y dimension but only 1 day (1 value in the time dimension)

# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
#         Useful information to extract data out of a netcdf file using the function ncvar_get()
# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#                   Extracting CR2_met original data from a NetCDF file for a particular date
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@          

#with the following instruction we ask the user to type the date to extract the data :

#extraction_date_text <- readline(prompt="Enter the extraction day in the following format yyyy-mm-dd: ")
#print(extraction_date_text)

# type the date to extract the data  # yyy-mm-dd

#extraction_date_text<-"1984-07-06"
#extraction_date_text<-"2015-03-23"

extraction_date_text<-"2015-03-25"

# this function returns the index of the data matching the search criteria inside the rasterbrick
extraction_day_index <- return_CR2_index(extraction_date_text,rasterbrick_with_all_chile) 

# If you need to create a R date element using an index for CR2 data you can use this line:
extraction_date_as_R_date<-as.Date(extraction_day_index, origin= time_origin)

# Visualising the date of the data extraction
print("the day of data extraction is [ day / month/year]")
print(extraction_date_as_R_date) # day / month / year

raster_for_interest_day<- rasterbrick_with_all_chile[[extraction_day_index]]

# Clean the consle
cat("\014")

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#               End of extraction of CR2_met original data from a NetCDF file for a particular date
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@          

# ============================================================================================================
# =======================Converting a dataframe object into a GIS compatible raster  =========================
# ============================================================================================================

# You need to know the Coordinate Reference System (CRS) of the CR2 data before executing this functions:
# The CRS has been checked before using ArcGIS:

CRS_code<-"+init=epsg:4326"

df_for_1_day <- extract_1_df_out_of_a_CR2_ncdf_file_for_1_day(ncdfname,main_directory,loading_directory,
                                                            extraction_day_index,variable_short_name)

raster_for_1_day_of_data<-convert_CR2_df_into_a_raster(df_for_1_day,CRS_code)


# Clean the consle
cat("\014")

# ============================================================================================================
# =================End of convertion of dataframe object into a GIS compatible raster  =======================
# ============================================================================================================


# ^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^
#                                      Visual assessment of CR2 data  
# ^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^

# The following line is used to change the default configuration of Rstudio to plot in a separated window:
# This is useful when the "plot" section of the GUI is too small (working on a laptop computer)
# This is also useful if you want to resize your plots or copy them into the clipboard 
options(device = "windows")

# The following lone is used to 
options(device = "RStudioGD")

# if you apply the function plot() to a rasterbrick you will have a graphical object with multiple rasters 
# (up to 16)  each panel will be a date and the panels will be arranged chronologically from left to right, 
# from top to  bottom

plot(rasterbrick_with_all_chile)

# This function helps you to create good looking plot of a rasterbrick element:
plot_raster_brick_elements(rasterbrick_with_all_chile,variable_short_name)

# This function helps you to create a good looking plot of raster elements 
raster_to_plot<-rasterbrick_with_all_chile[[extraction_day_index]]

# This fucntion plots a raster elemet with a personalized cosntinous scale in a new pop-up window
plot_raster_elements(raster_to_plot,variable_short_name)

# Clean the consle
cat("\014")

# Delete all plots 
dev.off(dev.list()["RStudioGD"])

# ^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^
#                                      Visual assessment of CR2 data  
# ^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^

# ???????????????????????????????????????????????????????????????????????????????????????????????????????????
#                        Extracting CR2_met data for a particular area: catchment 
# ???????????????????????????????????????????????????????????????????????????????????????????????????????????

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Pre-extraction
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# To run this section of the code you must have a rasterbrick element with the CR2_met data for 
# all chile loaded in the Global Environment (GE):

#    rasterbrick_with_all_chile

# You need to load the raster that is gonna be used as a mask to extract data out of the raster 
# for all Chile. In this particular case the mask raster will represent a catchment 

loading_directory<-paste(main_directory,"05-Input-rasters", sep="/")

# There are multiple alternatives for this file (multiple catchments)

# raster_name_file <-'copiapo_comp.tif'
# raster_name_file <-'Demo_regions.tif'
# raster_name_file <- 'New_demo_regions_CR2_extent.tif'

raster_name_file<-'Copiapo_SWIFTCR2.tif'
raster_name_file<-'Paipote_SWIFTCR2.tif'
raster_name_file<-'Vallenar_SWIFTCR2.tif'
raster_name_file<-'Vinita_Azul_SWIFTCR2.tif'
  

loading_route<- paste(loading_directory,raster_name_file, sep="/")
mask_raster<-raster(loading_route)

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# End of pre-extraction
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


# ???????????????????????????????????????????????????????????????????????????????????????????????
#                    Extracting CR2_met data for a particular area for 1 day 
# ???????????????????????????????????????????????????????????????????????????????????????????????

# if you already have a raster element  with 1 day of CR2 data for the entire country and you want 
# to extract another raster with the shape of a catchment or a interest area you can do it using 
# the following function  

input_raster<-raster_for_1_day_of_data

masked_raster<-extract_by_mask_from_a_raster(mask_raster,input_raster)

# This lines are used to visually assess the individual rasters:

plot(mask_raster)            # mask (catchment)
plot(masked_raster)          # cropped data (rainfall for the catchment) for a especified date

# Delete all plots 
dev.off(dev.list()["RStudioGD"])

# Clean the consle
cat("\014")

# ???????????????????????????????????????????????????????????????????????????????????????????????
#               End of extraction of CR2_met data for a particular area for 1 day 
# ???????????????????????????????????????????????????????????????????????????????????????????????

# ^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^
#         Extraction of all  CR2_met rasters for all dates for an entire catchment/region
# ^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^

# if the mask layer is not perfectly aligned with the CR2 data 
# you need to resample your layer 

#resample the mask to align with the CR2
#mask_new = resample(rasterbrick_with_all_chile[[1]], mask_raster, "bilinear")

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! WARNING
# THIS TAKE SOME TIME TO RUN !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! WARNING

# this is an example of the efficiency of the rasterbrick approach to 
# operate over thousands of rasters with  just a few code lines and no 
# loops 

mask_extent <- extent(mask_raster)

cropped_brick<-crop(rasterbrick_with_all_chile,mask_extent)

extent(cropped_brick)<- mask_extent

masked_brick <- mask(cropped_brick,mask_raster)

# Clean the consle
cat("\014")

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! WARNING
# THIS TAKE SOME TIME TO RUN !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! WARNING

# ^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^
#       End of extraction of all  CR2_met rasters for all dates for an entire catchment/region
# ^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^

# ???????????????????????????????????????????????????????????????????????????????????????????????????????????
#                     End of extraction of CR2_met data for a particular area: catchment 
# ???????????????????????????????????????????????????????????????????????????????????????????????????????????

# ^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^
#                   Extracting 1 raster out of a rasterbrick for a particular area: catchment
# ^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^

# with the following instruction we ask the user to type in the console a date to extract the data 
# extraction_date_text <- readline(prompt="Enter the extraction day yyyy-mm-dd: ")

# extraction_date_text<- "1984-07-04"
# extraction_date_text<- "1991-06-17"
# extraction_date_text<- "1991-06-19"
# extraction_date_text<- "2015-03-28"
extraction_date_text<- "2015-03-24"

# this function returns the index of the data matching the search criteria inside the rasterbrick
extraction_day_index<-return_CR2_index(extraction_date_text, masked_brick)

masked_raster<-masked_brick[[extraction_day_index]]

# Clean the consle
cat("\014")

# ^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^
#                End Extracting 1 raster out of a rasterbrick for a particular area: catchment
# ^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^>^

# sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
# sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss saving data
# sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss


# Definition of saving variables:

name_of_saving_directory <- "08-Result-rasters"
area_name <- "Copiapo-SWIFT"

# In this part of the code you save the raster file for a particular day for a particular raster in a 
# ".tif" file

raster_filename_for_saving <- paste(variable_short_name,"-",area_name,extraction_date_text)

save_a_raster_object_in_a_specified_location(name_of_saving_directory,
                                             main_directory,
                                             raster_filename_for_saving,
                                             masked_raster)

# In this part of the code you save the extracted data for the catchment in a separated ncdf file

ncdf_filename_for_saving=paste(variable_short_name,"_CR2_",area_name,".nc")

save_a_ncdf_object_in_a_specified_location(name_of_saving_directory,
                                           main_directory,
                                           ncdf_filename_for_saving,
                                           masked_raster,
                                           variable_short_name,
                                           variable_long_name,
                                           varunits,
                                           time_units)
# Clean the consle
cat("\014")

# sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
# sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss saving data
# sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss


#*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.
#*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.* Saving plots in a specific folder *.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*
#*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.

# Strng with the date fo the day you want to extract:
extraction_date_text<- "2015-03-25"

# this function returns the index of the data matching the search criteria inside the rasterbrick
extraction_day_index<-return_CR2_index(extraction_date_text, masked_brick)

# This is the raster you want to plot:
raster_to_plot<-masked_brick[[extraction_day_index]]

#name of the catchment: 
area_name <- "Copiapo-catchment"

#Name of folder to save all the plots:
name_of_saving_folder <- "10-Results-graphs"

# Since R runs on so many different operating systems, and supports so many different image formats, it is 
# not surprising that there are multiple ways of saving your plots.

# The first step in deciding how to save your plots is to decide on the output format that you want to use. 
# The following list contains some of the available formats, along with guidance as to when they may be 
# useful.


# JPG	       -> Can be used anywhere, but doesn't resize
# PNG	       ->	Can be used anywhere, but doesn't resize
# WMF	       -> best choice with Word; easily resizable
# PDF	       -> Best choice with pdflatex; easily resizable
# Postscript ->	Best choice with latex and Open Office; easily resizable

file_type<-"pdf"

# the following fucntion  reates a plot in the especified location 

save_plot_in_a_specified_location(raster_to_plot,
                                  variable_short_name,
                                  variable_long_name,
                                  variable_units,
                                  area_name,
                                  file_type,
                                  extraction_date_text,
                                  name_of_saving_folder,
                                  main_directory)
# Clean the consle
cat("\014")

#*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.
#*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.* End of Saving plots in a specific folder *.*.*.*.*.*..*.*.*.*.*.*.*.*.*.*
#*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.