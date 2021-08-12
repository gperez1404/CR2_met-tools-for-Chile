# Analysis of CR2_met daily products available at:

# https://www.cr2.cl/datos-productos-grillados/?cp_cr2met=3 

# daily precipitation: pr
# maximum temperature : tmax
# minimum temperature : tmin

# Important !
# Before executing this Script you need to download the ncdf file with data here:

# C:\00-C-GPM_INFO\04-C-RDM\04-C-01-R\04-C-01-02-CR2-DAP\04-Inputs-files
# C:\00_PhD\R\04-Inputs-files

# Inputs:
#----------------------------------------------------------------------------------------------------------

# these are the first versions of the product:
# these are known to have some issues with the names
#ncdfname    <- "CR2MET_v1.3_tmin.nc"
#ncdfname    <- "CR2MET_v1.3_tmax.nc"

#the are the most recent versions of this product as of 2021
#ncdfname <-"CR2MET_tmax_v2.0_day.nc"
#ncdfname <-"CR2MET_tmin_v2.0_day.nc"
ncdfname <-"CR2MET_tmean_v2.0_day.nc"

# Do you need to create the tmean file ?
# tmean is the average of daily tmax and daily tmin
# the tmean product is not available for downlaod in the website of CR2
# you just neede to create this once and save it on the hard drive
# this takes several hours

create_tmean_ncdf<-FALSE

# this is teh variable you wnt to analyse : in this case precipitation
variable_short_name<-"tmean"   # options: pr / tmin / tmax / tmean

# date to extract 1 day of data in the following format : "yyy-mm-dd" 
extraction_date_text<- "2015-03-25"

# this is the area you want to analyse (an especific river basin)
area_name<- "Copiapo-River-Basin"

raster_name_file <-'copiapo_comp.tif'

shape_file_name <-'Copiapo_catchment_for_plots.shp'

# delete data from RAM after analysis ? [Yes/No]

delete_ncdf_from_RAM<-"Yes"

#----------------------------------------------------------------------------------------------------------

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#
#                                                                                          Nested functions
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#

# load necessary libraries:
#----------------------------------------------------------------------------------------------------------

load_libraries<- function()
{
  
  #libraries for GIS and spatial data-handling
  library(ncdf4)
  library(raster)
  library(sf)
  library(sp)
  library(grid)
  library(gridExtra)
  library(spData)
  library(dplyr)
  library(rgdal)
  library(osmar)
  library(tiff)
  library(resample)
  library(rasterVis)
  
  #loading the libraries for plots
  library(ggplot2)
  library(ggExtra)
  library(MASS)
  library(RColorBrewer)
  library(viridis)
  library(ggthemes)
  library(colorRamps)
  library(wesanderson)
  
  
  #loading libraries for animations:
  
  library(gganimate)
  library(magick)
  library(animation)
  
  # Library for daily precipitation data analysis:
  
  library(hydroTSM)
  require(lattice)
  
  # Other libraries 
  libraries_to_load <- c("tmap","chron", "parallel","broom", "lubridate")
  
  lapply(libraries_to_load , library, character.only = TRUE) 
  
  rm(libraries_to_load)
  
  # clean console 
  cat("\014")
}
# clean console 
cat("\014")

#----------------------------------------------------------------------------------------------------------

# This function return a string with  a time difference:  
#----------------------------------------------------------------------------------------------------------
# This is used to control execution times and print on console
# input parameters:
# start_time

time_diff <- function(start_time) {
  diff = as.numeric(difftime(Sys.time(), start_time, units="mins"))
  hr <- diff%/%60
  min <- floor(diff - hr * 60)
  sec <- round(diff%%1 * 60,digits=2)
  
  return(paste(hr,min,sec,sep=':'))
}
#clean console
cat("\014")
#----------------------------------------------------------------------------------------------------------

# This function converts a string with a month name value in lowercase into a month number [1-12]
#----------------------------------------------------------------------------------------------------------
# Inputs:
# Any fo the following:
#                      "january"
#                      "february"
#                      "march"    
#                      "april"
#                      "may"
#                      "june"
#                      "july"     
#                      "august"
#                      "september"
#                      "october"
#                      "november"        
#                      "december"                


# into a date value accoridnt to the ISO date standart 

# this function returns a 'numeric' element

convert_month_name_into_number<-function(month_name_text)
{
  
  if(month_name_text=="january"){
    month_number<-1
  } else if (month_name_text=="february"){
    month_number<-2
  } else if (month_name_text=="march"){
    month_number<-3
  } else if (month_name_text=="april"){
    month_number<-4
  } else if (month_name_text=="may"){
    month_number<-5
  } else if (month_name_text=="june"){
    month_number<-6
  } else if (month_name_text=="july"){
    month_number<-7
  } else if (month_name_text=="august"){
    month_number<-8
  } else if (month_name_text=="september"){
    month_number<-9
  } else if (month_name_text=="october"){
    month_number<-10
  } else if (month_name_text=="november"){
    month_number<-11
  } else if (month_name_text=="december"){
    month_number<-12
  }
  
  return(month_number)
  
}  
# clean console
cat("\014")
#----------------------------------------------------------------------------------------------------------

# This function converts a string with a month name value in lowercase into a month number [1-12]
#----------------------------------------------------------------------------------------------------------
# Inputs:
# Any fo the following:
#                      1   for "january"
#                      2   for "february"
#                      3   for "march"    
#                      4   for "april"
#                      5   for "may"
#                      6   for "june"
#                      7   for "july"     
#                      8   for "august"
#                      9   for "september"
#                      10  for "october"
#                      11  for "november"        
#                      12  for "december"                


# into a date value accoridnt to the ISO date standart 

# this function returns a 'numeric' element

convert_month_number_into_char<-function(month_number)
{
  
  if(month_number==1){
    month_number_as_char<-"01"
  } else if (month_number==2){
    month_number_as_char<-"02"
  } else if (month_number==3){
    month_number_as_char<-"03"
  } else if (month_number==4){
    month_number_as_char<-"04"
  } else if (month_number==5){
    month_number_as_char<-"05"
  } else if (month_number==6){
    month_number_as_char<-"06"
  } else if (month_number==7){
    month_number_as_char<-"07"
  } else if (month_number==8){
    month_number_as_char<-"08"
  } else if (month_number==9){
    month_number_as_char<-"09"
  } else if (month_number==10){
    month_number_as_char<-"10"
  } else if (month_number==11){
    month_number_as_char<-"11"
  } else if (month_number==12){
    month_number_as_char<-"12"
  }
  
  return(month_number_as_char)
}
# clean console
cat("\014")
#----------------------------------------------------------------------------------------------------------

# This function return a string with a month name based on a number
#----------------------------------------------------------------------------------------------------------
# Inputs:
# Any fo the following:
#                      1   for "january"
#                      2   for "february"
#                      3   for "march"    
#                      4   for "april"
#                      5   for "may"
#                      6   for "june"
#                      7   for "july"     
#                      8   for "august"
#                      9   for "september"
#                      10  for "october"
#                      11  for "november"        
#                      12  for "december"                


# into a date value accoridnt to the ISO date standart 

# this function returns a 'numeric' element

convert_month_number_into_string<-function(month_number)
{
  
  if(month_number==1){
    month<-"january"
  } else if (month_number==2){
    month<-"february"
  } else if (month_number==3){
    month<-"march"
  } else if (month_number==4){
    month<-"april"
  } else if (month_number==5){
    month<-"may"
  } else if (month_number==6){
    month<-"june"
  } else if (month_number==7){
    month<-"july" 
  } else if (month_number==8){
    month<-"august"
  } else if (month_number==9){
    month<-"september"
  } else if (month_number==10){
    month<-"october"
  } else if (month_number==11){
    month<-"november"
  } else if (month_number==12){
    month<-"december"
  }
  
  return(month)
}
# clean console
cat("\014")
#----------------------------------------------------------------------------------------------------------

# This function calculate the basic statistics for 1 raster
#----------------------------------------------------------------------------------------------------------
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
#----------------------------------------------------------------------------------------------------------

# This function calculate the basic statistics for 1 date introduce as an input parameter 
#----------------------------------------------------------------------------------------------------------

# input parameters:

#   date                       = text thata indicates the date you want to extract:
#                                 yyyy-mm-dd:
#   rasterbrick_for_statistics = the rasterbrick element with all the rasters for the different dates

# function outputs

#   daily_statistics           = Collection with the main statistics associated with the date 
#                                introduce as an input parameter
#                                (mean,min,max,sum) 

# this function does not create any file nor saves any element in secondary memmory

calculate_basic_statistics_for_1_date <- function(date,rasterbrick_for_statistics) 
{
  
  extraction_day_index<-return_CR2_index(date,rasterbrick_for_statistics) 
  
  # Extracting a slice out of a rasterbrick 
  
  # this is the way to extract 1 raster out of a rasterbrick according to the index number
  raster_with_one_day_of_data <-rasterbrick_for_statistics[[extraction_day_index]]
  
  mean<-cellStats(raster_with_one_day_of_data, 'mean')
  min<-cellStats(raster_with_one_day_of_data, 'min')
  max<-cellStats(raster_with_one_day_of_data, 'max')
  sum<-cellStats(raster_with_one_day_of_data, 'sum')
  
  daily_statistics<-c(mean,min, max, sum)
  
  return(daily_statistics)
  # clean console
  cat("\014")
  
}
# clean console
cat("\014")
#----------------------------------------------------------------------------------------------------------

# This function creates a list of years with 12 months of data
#----------------------------------------------------------------------------------------------------------

return_list_of_years_CR2_met<-function(rasterbrick_with_daily_data,date_origin){
  
  number_of_rasters<-nlayers(rasterbrick_with_daily_data)
  
  if(date_origin=="1979-01-01"){
    
    first_year_text<-"1979"
    
    relative_seconds_last_raster<-as.numeric(gsub("X","",names(rasterbrick_with_daily_data)[number_of_rasters]))
    realtive_days_last_raster<-relative_seconds_last_raster/86400
    
    date_last_data_raster<-dmy("1/1/1979") + days(realtive_days_last_raster)
    
    data_last_raster_as_str<-as.character(date_last_data_raster)
    
    list_with_date_components<-strsplit(data_last_raster_as_str,"-")
    
    year_number<- as.numeric(list_with_date_components[[1]][1])
    month_number<- as.numeric(list_with_date_components[[1]][2])
    day_number<- as.numeric(list_with_date_components[[1]][3])
    
    if(month_number==12 && day_number== 31){
      last_year_text<-as.character(year_number)
    } else { 
      last_year_text<-as.character(year_number-1)
    }
    
  } else if (date_origin=="1978-12-31"){
    
    first_year_text<-"1979"
    
    last_raster_str_name<-names(rasterbrick_with_daily_data[[number_of_rasters]])
    last_raster_date_str<-gsub("X","",last_raster_str_name)
    list_with_date_components<-strsplit(last_raster_date_str,".",fixed=TRUE)
    
    year_number<- as.numeric(list_with_date_components[[1]][1])
    month_number<- as.numeric(list_with_date_components[[1]][2])
    day_number<- as.numeric(list_with_date_components[[1]][3])
    
    if(month_number==12 && day_number== 31){
      last_year_text<-as.character(year_number)
    } else { 
      last_year_text<-as.character(year_number-1)
    }
    
  }
  
  first_year<-as.numeric(first_year_text)
  last_year<-as.numeric(last_year_text)
  
  #list of years with complete data (12 months)
  list_of_years<- seq(first_year,last_year, by=1)
  
  return(list_of_years)
}
# clean console
cat("\014")
#----------------------------------------------------------------------------------------------------------

# This function receives as an input a df with two columns : Ids and temp 
# and returns the index of the highest value 
#----------------------------------------------------------------------------------------------------------
find_location_highest_value<-function(df_temps){
  
  names(df_temps)<- c("ids","values")
  
  highest_value<-max(df_temps$values)
  
  location_highest_value<-match(highest_value,df_temps$values)
  
  return(location_highest_value)
}
#----------------------------------------------------------------------------------------------------------

# This function receives as an input a df with two columns : Ids and temp 
# and returns the index of the lowest value 
#----------------------------------------------------------------------------------------------------------
find_location_lowest_value<-function(df_temps){
  
  names(df_temps)<- c("ids","values")
  
  lowest_value<-min(df_temps$values)
  
  location_lowest_value<-match(lowest_value,df_temps$values)
  return(location_lowest_value)
}
#----------------------------------------------------------------------------------------------------------

# This function creates a list/vector of strings with the IDs for a particular month of a 
# particular year
#----------------------------------------------------------------------------------------------------------
create_list_of_IDs_month<-function(first_date_as_text,number_of_days){
  
  list_of_days<- seq(as.Date(first_date_as_text),length=number_of_days,by="day")
  list_of_days_as_string<-unlist(as.character(list_of_days))
  return(list_of_days_as_string)
  
}
#----------------------------------------------------------------------------------------------------------

# This function creates a raster brick element with a time series of monthly values for a specific month 
#----------------------------------------------------------------------------------------------------------

# This function creates 1 raster for each year with the sum of precipitation for the entire month

# This function uses as input a raster brick element containing  CR2_met daily data  

# This function is very inefficient. for 40 years of data it can take up to  60 seconds per month

# good news are you just need to run this once per month

# input parameters:

# month_number                  = numeric value  that indicates the month for the analysis
#                                 


#  type_of_data                  = text with the initials or short name of the type of data you 
#                                   are analyzing:
#                                                  "pr"
#                                                  "tmeam"
#                                                  "tmin"
#                                                  "tmax"

#   rasterbrick_with_daily_data   = the rasterbrick element with all the rasters for the different dates
#                                    this is assume be a daily rsterbrick element  

#  date_origin                  = string with the date of the first raster format : yyy-mm-dd

#   function outputs

#   rasterbrick_monthly_values  = A rasterbrick element with 1 raster associated to every year


# This function does not save any element 

generate_monthly_rasterbrick <- function(month_number,
                                         type_of_data,
                                         rasterbrick_with_daily_data,
                                         date_origin) 
{
  
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  # debugging:
  #month_number<-1
  #type_of_data<-"tmean"
  #rasterbrick_with_daily_data<-rasterbrick
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  
  # this variable control the execution time
  time_before_executing<- Sys.time()
  
  # number of days in the input rasterbrick
  total_number_of_days_input_data<-nlayers(rasterbrick_with_daily_data)
  
  # Here you create the values for each month
  
  if(month_number==1){
    monht_text<-"january"
    month_number_as_char<-"01"
    number_of_days_of_a_typical_month<-31
  } else if (month_number==2){
    monht_text<-"february"
    month_number_as_char<-"02"
    number_of_days_of_a_typical_month<-28
  } else if (month_number==3){
    monht_text<-"march"
    month_number_as_char<-"03"
    number_of_days_of_a_typical_month<-31
  } else if (month_number==4){
    monht_text<-"april"
    month_number_as_char<-"04"
    number_of_days_of_a_typical_month<-30
  } else if (month_number==5){
    monht_text<-"may"
    month_number_as_char<-"05"
    number_of_days_of_a_typical_month<-31
  } else if (month_number==6){
    monht_text<-"june"
    month_number_as_char<-"06"
    number_of_days_of_a_typical_month<-30  
  } else if (month_number==7){
    monht_text<-"july"
    month_number_as_char<-"07"
    number_of_days_of_a_typical_month<-31
  } else if (month_number==8){
    monht_text<-"august"
    month_number_as_char<-"08"
    number_of_days_of_a_typical_month<-31
  } else if (month_number==9){
    monht_text<-"september"
    month_number_as_char<-"09"
    number_of_days_of_a_typical_month<-30  
  } else if (month_number==10){
    monht_text<-"october"
    month_number_as_char<-"10"
    number_of_days_of_a_typical_month<-31
  } else if (month_number==11){
    monht_text<-"november"
    month_number_as_char<-"11"
    number_of_days_of_a_typical_month<-30   
  } else if (month_number==12){
    monht_text<-"december"
    month_number_as_char<-"12"
    number_of_days_of_a_typical_month<-31   
  }
  
  
  list_of_leap_years<-c(1904, 1908, 1912, 1916, 1920, 1924, 1928, 1932, 1936, 1940,
                        1944, 1948, 1952, 1956, 1960, 1964, 1968, 1972, 1976, 1980, 
                        1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016, 2020)
  
  # Here you generate a list of years with complete data based on the input rasterbrick:
  
  list_of_years<-return_list_of_years_CR2_met(rasterbrick_with_daily_data,date_origin)
  
  number_of_years<-length(list_of_years)
  first_year_text<-list_of_years[1]
  last_year_text<-list_of_years[number_of_years]
  
  print("Creating monthly raster brick for: ")

  if(type_of_data =="tmean"){
    print("mean daily temperature")
    print("individual rasters for each day  will be used to calculate the average")
  } else if (type_of_data =="tmin"){
    print("min daily temperature")
    print("Each raster will represent the coldest day of the month for each year")
  } else if (type_of_data =="tmax"){
    print("max daily temperature")
    print("Each raster will represent the hottest day of the month for each year")
  }
  
  
  print(paste("Month: ",monht_text, sep=""))
  
  print(paste("Finding monthly values for period: " , first_year_text, "-", last_year_text,sep=""))
  
  print("This takes up to 2 minutes per month")
  print("please wait.........................")
  
  current_day_index<-1
  
  # Here you initialise the ratserbrick:
  # This first raster will be substitude in the main loop so it doesn't matter
  first_raster<-rasterbrick_with_daily_data[[current_day_index]]
  
  brick_with_multi_annual_monthly_rasters<-brick(first_raster)
  
  #  Main loop 
  #  This loop is over the years of the list with years:
  
  #number of years covered by the rasterbrick
  limit_main_loop<-length(list_of_years)
  
  #index for main loop
  current_year_index<-1
  
  while (current_year_index < limit_main_loop) {
    
    if(type_of_data =="pr"){
      
      # if the month is February you need to take especial considerations for the leap years
      if(month_number_as_char=="02"){
        
        if (!is.na(match(list_of_years[current_year_index], list_of_leap_years))){
          # if you enter here is beacuse you have a leap year
          print("analysing leap year...")
          number_of_days_of_a_typical_month<-29
        } else {
          number_of_days_of_a_typical_month<-28
        }
        
      }
      
      #print("Creating monthly time series for precipitation data")
      print(paste("current year: ", list_of_years[current_year_index], sep=""))
      
      if (month_number_as_char=="02"){
        print(paste("Number of days of current month: ",number_of_days_of_a_typical_month ,sep=""))
        
      }
      
      
      # here you create a string with the first day of the month
      # date to extract the first day of the month: "yyy-mm-dd" 
      
      current_year_as_char<-as.character(list_of_years[current_year_index])
      
      date_text_first_day_current_year<-paste(current_year_as_char,
                                              month_number_as_char,
                                              "01",
                                              sep="-")
      
      # here you create vector with the IDs of the days of the current month and year
      
      
      list_of_IDs_month<-create_list_of_IDs_month(date_text_first_day_current_year,
                                                  number_of_days_of_a_typical_month)
      
      
      # for some reason you cannot return the index of the first day of september 2004
      if(date_text_first_day_current_year=="2004-09-01" &&  date_origin=="1979-01-01"){
        current_day_index<-9376
      } else{
        current_day_index<-return_CR2_index(date_text_first_day_current_year,
                                            rasterbrick_with_daily_data)
      }
      
      
      # here you create the raster that will holds the sum for the month
      # there is NO rasterbrick if the data type is pr !
      current_day=1
      
      if(current_day_index <= nlayers(rasterbrick_with_daily_data)){
        
        first_raster_of_the_current_year<-rasterbrick_with_daily_data[[current_day_index]]
        raster_with_monthly_pr<-first_raster_of_the_current_year
        
        # loop advance:
        current_day_index<-current_day_index+1
        current_day=current_day+1
        
      }
      
      month_accumulated_succesfully<-FALSE
      
      # This loop accumulates pr for 1 month
      # This loop creates 1 raster with the total sum value for 1 month = total precipitation
      
      while (month_accumulated_succesfully==FALSE && current_day <=  number_of_days_of_a_typical_month ) {
        
        # progress printing on console for debugging :
        # current_day_as_char<-as.character(current_day)
        # current_day_date_text<-paste(current_year_as_char,month_number_as_char,current_day_as_char,sep="-")
        
        # print(paste("current day:",current_day_date_text,sep=" "))
        # print(paste("current day index:",current_day_index, "out of",total_number_of_days_input_data, sep=" "))
        
        # this conditional controls the addition of the raster for the current day
        if(current_day_index <= nlayers(rasterbrick_with_daily_data)) {
          
          raster_current_day<-rasterbrick_with_daily_data[[current_day_index]]
          raster_with_monthly_pr<-raster_with_monthly_pr+raster_current_day # you numerically add the values
          
        }  
        
        # loop advance:
        current_day_index<-current_day_index+1
        current_day=current_day+1
        
        
        # this conditional controls the end of the loop to accumulate the month
        if(current_day>number_of_days_of_a_typical_month){
          
          # Once you change to the next year you stop the accumulation for the monthly value
          month_accumulated_succesfully==TRUE
        }
        
      } # This is the end of the accumulation loop for pr data
      
    } else if(type_of_data =="tmean" | type_of_data =="tmin" | type_of_data =="tmax"){
      
      # if the month is February you need to take especial considerations for the leap years
      if(month_number_as_char=="02"){
        
        if (!is.na(match(list_of_years[current_year_index], list_of_leap_years))){
          # if you enter here is because you have a leap year
          print("analysing leap year...")
          number_of_days_of_a_typical_month<-29
        } else {
          number_of_days_of_a_typical_month<-28
        }
        
      }
        
      #print("Creating monthly time series for temperature data")
      print(paste("current year: ", list_of_years[current_year_index], sep=""))
      
      # here you create a string with the first day of the month
      # date to extract the first day of the month: "yyy-mm-dd" 
      
      current_year_as_char<-as.character(list_of_years[current_year_index])
      
      date_text_first_day_current_year<-paste(current_year_as_char,
                                              month_number_as_char,
                                              "01",
                                              sep="-")
      
      
      if (month_number_as_char=="02"){
        print(paste("Number of days of current month: ",number_of_days_of_a_typical_month ,sep=""))
        
      }
      
      # here you create vector with the IDs of the days of the current month and year
      
      
      list_of_IDs_month<-create_list_of_IDs_month(date_text_first_day_current_year,
                                                  number_of_days_of_a_typical_month)
      
      # here you initialize the raster brick that holds individual daily rasters for the entire month
      
      current_day<-1
      
      if(current_day_index <= nlayers(rasterbrick_with_daily_data)){
        
        first_raster_of_the_current_year<-rasterbrick_with_daily_data[[current_day_index]]
        rasterbrick_monthly_values<-brick(first_raster_of_the_current_year)
        
        # loop advance:
        current_day_index<-current_day_index+1
        current_day=current_day+1
        
      }
      
      month_bricked_succesfully<-FALSE
      
      # This loop bricks multiple rasters with temperature for all the days of 1 month
      
      while (month_bricked_succesfully==FALSE && current_day <=  number_of_days_of_a_typical_month ) {
        
        
        # progress printing on console
        
        #current_day_as_char<-as.character(current_day)
        #current_day_date_text<-paste(current_year_as_char,month_number_as_char,current_day_as_char,sep="-")
        
        #print(paste("current day:",current_day_date_text,sep=" "))
        #print(paste("current day index:",current_day_index, "out of",total_number_of_days_input_data, sep=" "))
        
        
        
        # here you add a raster for 1 day to the brick
        
        if(current_day_index <= nlayers(rasterbrick_with_daily_data)) {
          
          raster_current_day<-rasterbrick_with_daily_data[[current_day_index]]
          
          rasterbrick_monthly_values<- addLayer(rasterbrick_monthly_values,raster_current_day)
          
        }  
        
        # loop advance:
        current_day_index<-current_day_index+1
        current_day=current_day+1
        
        # this if() crontrols the exit of the loop:
        # if the day number is > of the numebr of days of the month you exit the loop
        if(current_day>number_of_days_of_a_typical_month){
          
          # You need to exit this loop
          month_bricked_succesfully<-TRUE
          
        } # this is the end of the if() to handle the end of the loop
        
      } # this is the end of the loop to brick temp data for all the days of 1 month
      
    } else if(type_of_data!="tmean" | type_of_data!="tmin" | type_of_data!="tmax" | type_of_data!="pr"){
      print("the code for this type of data is not define yet")
    } # This is the end of the if() for creating the brick of monthly values for  tmean | tmin | tmax  or the raster for total monthly pr 
    
    # here you generate the raster for the current year:
    
    if (type_of_data=="pr"){
      
      monthly_raster<-raster_with_monthly_pr
      
    } else if (type_of_data=="tmean"){
      
      monthly_raster<-mean(rasterbrick_monthly_values)
      
    } else if (type_of_data=="tmin"){
      
      # here you calculate a df with the values of average temp for each day
      
      df_average_temps<-generate_df_with_average_for_rasterbrick(list_of_IDs_month,
                                                                 rasterbrick_monthly_values)
      
      # here you find the index of the day with the minium daily temperature
      
      location_coldest_day<-find_location_lowest_value(df_average_temps)
      #print("coldest day of the month located successfully ")
      #print(location_coldest_day)
      
      # here you extract the raster with the coldest day of the month
      
      monthly_raster<-rasterbrick_monthly_values[[location_coldest_day]]
      
    } else if (type_of_data=="tmax") {
      
      # here you calculate a df with the values of average temp for each day
      
      df_average_temps<-generate_df_with_average_for_rasterbrick(list_of_IDs_month,
                                                                 rasterbrick_monthly_values)
      
      # here you find the index of the day with the minium daily temperature
      
      location_hottest_day<-find_location_highest_value(df_average_temps)
      #print("Hottest day of the month located successfully ")
      #print(location_hottest_day)
      
      # here you extract the raster with the coldest day of the month
      
      monthly_raster<-rasterbrick_monthly_values[[location_hottest_day]]
    }
    
    # here you add the monthly value raster to the main rasterbrick
    brick_with_multi_annual_monthly_rasters<-addLayer(brick_with_multi_annual_monthly_rasters,
                                                      monthly_raster,
                                                      current_year_index)
    
    # here you continue with the next year
    current_year_index=current_year_index+1
    
  } # This is the end of the main loop  over the years
  
  names(brick_with_multi_annual_monthly_rasters)<-list_of_years
  print("rasterbrick with multi-annual monthly values generated succesfully !!")
  
  elapsed_time<-time_diff(time_before_executing)
  print(paste("Total execution time [hh:mm:ss]",elapsed_time,sep=" "))
  
  return(brick_with_multi_annual_monthly_rasters)
}  
# clean console
cat("\014")
#----------------------------------------------------------------------------------------------------------

# This function generates a vector of char/string/text values with IDs for rasterbrick
#----------------------------------------------------------------------------------------------------------

# This vector will be use to populate the ID field of a dataframe

# input parameters:


#  type_of_data                  = text with the type of data you are analysing:
#                                                  "daily_CR2"
#                                                  "monthly_CR2"
#                                                  "annual_CR2"


# additional_info               = text with the additional info for identifying the obs 
#                                 in case you have data of monthly_CR2 or annual_CR2 types. 
#                                 Could be anything you want if you have "daily_CR2" 


# rasterbrick_to_index          = The rasterbrick element with the data you want to index

Generate_list_with_consecutive_identifiers<-function(type_of_data,
                                                     additional_info,
                                                     rasterbrick_to_index,
                                                     date_first_raster){
  
  # this variable control the execution time
  time_before_executing<- Sys.time()
  
  #this lines controls that the main input value is valid
  if(!(type_of_data=="daily_CR2" | type_of_data=="monthly_CR2" | type_of_data=="annual_CR2")){
    stop("data type not supported. Change it to any of the followings: daily_CR2 | monthly_CR2  | annual_CR2")
  }
  
  # This variable will control the main loop 
  current_index<-0
  
  # this valriable controls the stop of main loop to fill up the id values vector
  number_of_rasters_inside_rasterbrick<-nlayers(rasterbrick_to_index)
  
  # Here you initialise the vector:
  obs_id_vector<-c()
  
  if(type_of_data=="daily_CR2"){
    
    # if the type_of_data  is  "daily_CR2"the id value contains a char/text/string 
    # with the date in the following format: 
    #                                       yyyy-mm-dd
    
    # this line creates a R date element using an index for CR2 data:
    current_obs_date<-as.Date(current_index, origin = date_first_raster)
    current_obs_date_text<-as.character(current_obs_date)
    
    current_id_value<-current_obs_date_text
    
    print(paste("generating id value",current_index,"out of",number_of_rasters_inside_rasterbrick,sep=" "))
    
    current_index<-1
    
    #Main loop to fill up the vector with ids
    while(current_index<=number_of_rasters_inside_rasterbrick){
      
      # this line creates a R date element using an index for CR2 data:
      current_obs_date<-as.Date(current_index, origin = date_first_raster)
      current_obs_date_text<-as.character(current_obs_date)
      current_id_value<-current_obs_date_text
      
      obs_id_vector<-c(obs_id_vector, current_id_value)
      print(paste("generating id value",current_index,"out of",number_of_rasters_inside_rasterbrick,sep=" "))
      current_index<-current_index+1  
    }
    
  } else if (type_of_data=="monthly_CR2") { 
    
    # if the type_of_data  is  "monthly_CR2" the id value contains a char/text/string 
    # with the month and the year in the following format: 
    #                                                     month_name-yyyy
    
    first_year<-as.numeric(strsplit(date_first_raster,"-")[[1]][1])
    
    last_year<-first_year+number_of_rasters_inside_rasterbrick-1
    
    #list of years
    list_of_years<- seq(first_year,last_year, by=1)
    
    current_year_text<-as.character(list_of_years[current_index])
    current_id_value<-paste(additional_info,current_year_text,sep="-")
    
    current_index<-1
    
    #Main loop to fill up the vector with ids
    while(current_index<=number_of_rasters_inside_rasterbrick){
      
      # this line creates a R date element using an index for CR2 data:
      current_year_text<-as.character(list_of_years[current_index])
      current_id_value<-paste(additional_info,current_year_text,sep="-")
      print(current_id_value)
      obs_id_vector<-c(obs_id_vector, current_id_value)
      print(paste("generating id value",current_index,"out of",number_of_rasters_inside_rasterbrick,sep=" "))
      current_index<-current_index+1  
    }
    
  } else if (type_of_data=="annual_CR2") {
    
    # if the type_of_data  is "annual_CR2" the id value contains a char/text/string 
    # with the year of the observation in the following format:
    #                                                          yyyy
    
    first_year<-as.numeric(strsplit(date_first_raster,"-")[[1]][1])
    
    last_year<-first_year+number_of_rasters_inside_rasterbrick-1
    
    #list of years
    list_of_years<- seq(first_year,last_year, by=1)
    
    current_year_text<-as.character(list_of_years[current_index])
    current_id_value<-current_year_text
    
    current_index<-1
    
    #Main loop to fill up the vector with ids
    while(current_index<=number_of_rasters_inside_rasterbrick){
      
      # this line creates a R date element using an index for CR2 data:
      current_year_text<-as.character(list_of_years[current_index])
      current_id_value<-current_year_text
      
      obs_id_vector<-c(obs_id_vector, current_id_value)
      print(paste("generating id value",current_index,"out of",number_of_rasters_inside_rasterbrick,sep=" "))
      current_index<-current_index+1  
    }
    
  }
  
  print("IDs vector generated succesfully !!")
  
  elapsed_time<-time_diff(time_before_executing)
  print(paste("Total execution time [hh:mm:ss]",elapsed_time,sep=" "))
  
  
  return(obs_id_vector)
}  
# clean console
cat("\014")




#----------------------------------------------------------------------------------------------------------

# This function creates a list of Dates corresponding to each raster in a rasterbrick with daily data
#----------------------------------------------------------------------------------------------------------

Generate_list_identifiers_daily_rasterbrick<-function(rasterbrick_to_index,time_units,date_first_raster){
  
  date_origin<-date_first_raster
  
  number_of_rasters<-nlayers(rasterbrick_to_index)
  
  list_IDs<-c()
  
  print("creating list of IDs")
  print("This takes MORE THAN 15 minutes, please be patient")
  print("executing R code, please do not close Rstudio......")
  print("executing R code, please do not execute aditional lines....")
  
  # this variable control the execution time
  time_before_executing<- Sys.time()
  
  if(time_units=="seconds since 1979-01-01 00:00:00"){
    
    start<-1
    
    for(i in start:number_of_rasters){
      
      if(i==(round(number_of_rasters/4,0))*1){
        print("25% of IDs created....")
        elapsed_time<-time_diff(time_before_executing)
        print(paste("Execution time [hh:mm:ss]",elapsed_time,sep=" "))
      } else if(i==(round(number_of_rasters/4,0))*2){
        print("50% of IDs created....")
        elapsed_time<-time_diff(time_before_executing)
        print(paste("Execution time [hh:mm:ss]",elapsed_time,sep=" "))
      } else if (i==(round(number_of_rasters/4,0))*3){
        print("75% of IDs created....")
        elapsed_time<-time_diff(time_before_executing)
        print(paste("Execution time [hh:mm:ss]",elapsed_time,sep=" "))
      } else if(i == number_of_rasters-1) {
        print("100% of IDs created....")
      }
      
      string_ID<-names(rasterbrick_to_index)[i]
      relative_seconds<-as.numeric(strsplit(string_ID,"X")[[1]][2])
      
      relative_days<-relative_seconds/86400
      
      date_element<-as.Date(relative_days, origin = date_origin)
      date_as_string<-as.character(date_element)
      list_IDs<-append(list_IDs,date_as_string,i)
      
    }
    
  } else if (time_units=="days since 1978-12-31"){
    
    start<-1
    
    for(i in start:number_of_rasters){
      string<-names(rasterbrick_to_index)[i]
      string_ID<-as.character(strsplit(string_ID,"X")[[1]][2])
      date <-sub(".", "-", string)
      list_IDs<-append(list_IDs,date,i)
    }
    
  }
  
  # here you look for NA values if any and you fix them one by one using the previous value 
  
  # list of index position with NA values
  
  list_index_with_NAs<-which(is.na(list_IDs), arr.ind=TRUE)
  number_of_rows_with_NAs<-length(list_index_with_NAs)
  
  if(number_of_rows_with_NAs>0){
    for (current_index_position in list_index_with_NAs) {
      previous_date<-list_IDs[current_index_position-1]
      previous_date_as_date_element<- as.Date(previous_date)
      date_to_replace_NA<-as.character(previous_date_as_date_element+1)
      list_IDs[current_index_position]<-date_to_replace_NA
    }
  }

  print("list of IDs as character created succesfully")
  
  elapsed_time<-time_diff(time_before_executing)
  print(paste("Total execution time [hh:mm:ss]",elapsed_time,sep=" "))
  
  return(list_IDs)
  
}
#----------------------------------------------------------------------------------------------------------

# This function return a dataframe with all the spatial statistics of a rasterbrick
#----------------------------------------------------------------------------------------------------------

# input parameters:


#  type_of_data                  = text with the type of data you are analysing:
#                                                  "daily_CR2"
#                                                  "monthly_CR2"
#                                                  "annual_CR2"
#                   

#   obs_identifier_vector        =
#                                           "yyy-mm-dd"
#                                           "month_name-year"
#                                           "yyyy"                

#   rasterbrick                   = the rasterbrick element with all the rasters for different 
#                                   dates (1 date is associated with 1 day)

#   function outputs

#   df_spatial_stats               = dataframe element with the main spatial statistics 
#                                    calcualted over the rasterbrick ntroduce as an input parameter
#                                   
#                                    Thess statistics are:  
#                                                         (mean,min,max,sum) 

# this function does not create any file nor saves any element in secondary memmory

generate_dataframe_with_spatial_stats<- function(type_of_data,
                                                 obs_identifier_vector,
                                                 area_name,
                                                 variable_short_name,
                                                 rasterbrick) 
{
  
  # this variable control the execution time
  time_before_executing<- Sys.time()
  
  number_of_rasters_inside_rasterbrick<-nlayers(rasterbrick)
  
  #this lines controls that the inputs are valid
  if(!((length(obs_identifier_vector))==number_of_rasters_inside_rasterbrick)){
    stop("vector with observation's identifiers has wrong dimension")
  }
  
  if(!(type_of_data=="daily_CR2" | type_of_data=="monthly_CR2" | type_of_data=="annual_CR2")){
    stop("data type not supported. Change it to any of the followings: daily_CR2 | monthly_CR2  | annual_CR2")
  }
  
  
  # This variable will control the loop to go trough the rasterbrick
  current_extraction_index<-1
  
  # this is the way to extract 1 raster out of a rasterbrick according to the index number
  current_raster<-rasterbrick[[current_extraction_index]]
  
  # mean,min,max,sum
  statistics_current_raster<-calculate_basic_statistics_for_1_raster(current_raster)
  
  #Here you initialise all the vectors that will contain  all the observations:  
  
  vector_area_names<-c(area_name)
  vector_of_type_data<-c(variable_short_name)
  
  vector_of_mean_values <- c(statistics_current_raster[1])
  vector_of_min_values  <- c(statistics_current_raster[2])
  vector_of_max_values  <- c(statistics_current_raster[3])
  vector_of_sum_values  <- c(statistics_current_raster[4])
  
  if(type_of_data=="daily_CR2"){
    
    # if the input rasterbrick contains daily data then the indentifier
    # contains a char/text/string with the date in the following 
    # format: 
    #         yyyy-mm-dd
    
    text_with_id_current_obs<-obs_identifier_vector[1]
    
    list_with_date_components<- strsplit(text_with_id_current_obs,"-")
    
    year_number_current_obs<- as.numeric(list_with_date_components[[1]][1])
    month_number_current_obs<- as.numeric(list_with_date_components[[1]][2])
    day_number_current_obs<- as.numeric(list_with_date_components[[1]][3])
    
    year_char_current_obs<-as.character(year_number_current_obs)
    month_char_current_obs<-as.character(month_number_current_obs)
    day_char_current_obs<-as.character(day_number_current_obs)
    
    vector_year_values  <-c(year_char_current_obs)
    vector_month_values <-c(month_char_current_obs)
    vector_day_values   <-c(day_char_current_obs)
    
  } else if (type_of_data=="monthly_CR2") {  
    
    # if the input rasterbrick contains monthly data then the indentifier
    # contains a char/text/string with the month and the year in the following 
    # format: 
    #        month_name-yyyy
    
    text_with_id_current_obs<-obs_identifier_vector[1]
    
    list_with_month_year_components<- strsplit(text_with_id_current_obs,"-")
    
    month_name_current_obs<- list_with_month_year_components[[1]][1]
    month_number_current_obs<-convert_month_name_into_number(month_name_current_obs)
    month_char_current_obs<-convert_month_number_into_char(month_number_current_obs)
    
    year_number_current_obs<- as.numeric(list_with_month_year_components[[1]][2])
    year_char_current_obs<-as.character(year_number_current_obs)
    
    vector_year_values  <-c(year_char_current_obs)
    vector_month_values <-c(month_char_current_obs)
    vector_day_values   <-c(1)
    
  } else if (type_of_data=="annual_CR2") {
    
    # if the input rasterbrick contains annual data then the indentifier
    # itself is a char/text/string with the year of the observation in the 
    # following format: 
    #                  yyyy
    
    vector_year_values  <-c(obs_identifier_vector[1])
    vector_month_values <-c(NA)
    vector_day_values   <-c(NA)
    
  }
  
  #print(paste("generating dataframe obs",current_extraction_index,"out of",number_of_rasters_inside_rasterbrick,sep=" "))
  current_extraction_index<-current_extraction_index+1
  # This is the main loop that fills up all the rows in the dataframe:
  while (current_extraction_index <= number_of_rasters_inside_rasterbrick) {
    
    vector_area_names<-c(vector_area_names,area_name)
    vector_of_type_data<-c(vector_of_type_data,variable_short_name)
    
    # In the following conditional you fill out the following vectors:
    
    # day
    # month
    # year 
    
    # accoding to the type of data that 
    
    if(type_of_data=="daily_CR2"){
      
      # if the input rasterbrick contains daily data then the indentifier
      # contains a char/text/string with the date in the following 
      # format: 
      #         yyyy-mm-dd
      
      text_with_id_current_obs<-obs_identifier_vector[current_extraction_index]
      
      list_with_date_components<- strsplit(text_with_id_current_obs,"-")
      
      year_number_current_obs<- as.numeric(list_with_date_components[[1]][1])
      month_number_current_obs<- as.numeric(list_with_date_components[[1]][2])
      day_number_current_obs<- as.numeric(list_with_date_components[[1]][3])
      
      year_char_current_obs<-as.character(year_number_current_obs)
      month_char_current_obs<-as.character(month_number_current_obs)
      day_char_current_obs<-as.character(day_number_current_obs)
      
      vector_year_values  <-c(vector_year_values, year_char_current_obs)
      vector_month_values <-c(vector_month_values, month_char_current_obs)
      vector_day_values   <-c(vector_day_values,day_char_current_obs)
      
    } else if (type_of_data=="monthly_CR2") {  
      
      # if the input rasterbrick contains monthly data then the indentifier
      # contains a char/text/string with the month and the year in the following 
      # format: 
      #        month_name-yyyy
      
      text_with_id_current_obs<-obs_identifier_vector[current_extraction_index]
      
      list_with_month_year_components<- strsplit(text_with_id_current_obs,"-")
      
      month_name_current_obs<- list_with_month_year_components[[1]][1]
      month_number_current_obs<-convert_month_name_into_number(month_name_current_obs)
      month_char_current_obs<-convert_month_number_into_char(month_number_current_obs)
      
      year_number_current_obs<- as.numeric(list_with_month_year_components[[1]][2])
      year_char_current_obs<-as.character(year_number_current_obs)
      
      vector_year_values  <-c(vector_year_values,year_char_current_obs)
      vector_month_values <-c(vector_month_values,month_char_current_obs)
      vector_day_values   <-c(vector_day_values,1)
      
    } else if (type_of_data=="annual_CR2") {
      
      # if the input rasterbrick contains annual data then the indentifier
      # itself is a char/text/string with the year of the observation in the 
      # following format: 
      #                  yyyy
      
      vector_year_values  <-c(vector_year_values,obs_identifier_vector[current_extraction_index])
      vector_month_values <-c(vector_month_values,NA)
      vector_day_values   <-c(vector_day_values,NA)
      
    }
    
    # in the following lines you fill up the vectors that don't depend on the data type:
    
    current_raster<-rasterbrick[[current_extraction_index]]
    statistics_current_raster<-calculate_basic_statistics_for_1_raster(current_raster)
    
    vector_of_mean_values <- c(vector_of_mean_values,statistics_current_raster[1])
    vector_of_min_values  <- c(vector_of_min_values,statistics_current_raster[2])
    vector_of_max_values  <- c(vector_of_max_values,statistics_current_raster[3])
    vector_of_sum_values  <- c(vector_of_sum_values,statistics_current_raster[4])
    
    print(paste("generating dataframe for raster ",current_extraction_index,"out of",number_of_rasters_inside_rasterbrick,sep=" "))
    current_extraction_index<-current_extraction_index+1
  }
  
  
  df_spatial_stats <- data.frame(id=obs_identifier_vector,
                                 area_name=vector_area_names,
                                 data_type=vector_of_type_data,
                                 year=vector_year_values,
                                 month=vector_month_values,
                                 day=vector_day_values,
                                 mean=vector_of_mean_values,
                                 min=vector_of_min_values,
                                 max=vector_of_max_values,
                                 sum=vector_of_sum_values,
                                 stringsAsFactors=FALSE)
  
  
  print("dataframe generated succesfully !!")
  
  elapsed_time<-time_diff(time_before_executing)
  print(paste("Total execution time [hh:mm:ss]",elapsed_time,sep=" "))
  
  return(df_spatial_stats)

}
# clean console
cat("\014")
#----------------------------------------------------------------------------------------------------------

# This function plots single box-plot for multi-annual m monthly pr values 
#----------------------------------------------------------------------------------------------------------

# input parameters:

# month_number    a numeric value with the minth number [1-12]

plot_monthly_boxplot_and_freq_distri<-function(month_number)
{
  
  # This display pltos in adifferetn screen
  options(device = "windows")
  
  if(month_number==1){
    plot_title<-"january"
    variable_to_plot<-df_stats_MAMVIA_janaury$mean
    
    number_of_breaks_for_hist<-floor(length(variable_to_plot)/2)+1
    
    # Layout to split the screen
    layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
    
    # Draw the boxplot and the histogram 
    par(mar=c(0, 3.1, 1.1, 2.1))
    boxplot(variable_to_plot,
            horizontal=TRUE,
            ylim=c(min(variable_to_plot),(floor(max(variable_to_plot))+1)),
            xaxt="n" ,
            col='deepskyblue',
            frame=F)
    par(mar=c(4, 3.1, 1.1, 2.1))
    hist(variable_to_plot,
         breaks=number_of_breaks_for_hist,
         col=rgb(0.2,0.8,0.5,0.5) ,
         border=F ,
         main=plot_title,
         ylim=c(0,(length(variable_to_plot))),
         xlab="cummulative monthly pr [mm]",
         xlim=c(min(variable_to_plot),(floor(max(variable_to_plot))+1)))
    
  } else if (month_number==2){
    plot_title<-"february"
    variable_to_plot<-df_stats_MAMVIA_february$mean
    
    number_of_breaks_for_hist<-floor(length(variable_to_plot)/2)+1
    
    # Layout to split the screen
    layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
    
    # Draw the boxplot and the histogram 
    par(mar=c(0, 3.1, 1.1, 2.1))
    boxplot(variable_to_plot,
            horizontal=TRUE,
            ylim=c(min(variable_to_plot),(floor(max(variable_to_plot))+1)),
            xaxt="n" ,
            col='deepskyblue',
            frame=F)
    par(mar=c(4, 3.1, 1.1, 2.1))
    hist(variable_to_plot,
         breaks=number_of_breaks_for_hist,
         col=rgb(0.2,0.8,0.5,0.5) ,
         border=F ,
         main=plot_title,
         ylim=c(0,(length(variable_to_plot))),
         xlab="cummulative monthly pr [mm]",
         xlim=c(min(variable_to_plot),(floor(max(variable_to_plot))+1)))
    
    
  } else if (month_number==3){
    plot_title<-"march"
    variable_to_plot<-df_stats_MAMVIA_march$mean
    
    number_of_breaks_for_hist<-floor(length(variable_to_plot)/2)+1
    
    # Layout to split the screen
    layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
    
    # Draw the boxplot and the histogram 
    par(mar=c(0, 3.1, 1.1, 2.1))
    boxplot(variable_to_plot,
            horizontal=TRUE,
            ylim=c(min(variable_to_plot),(floor(max(variable_to_plot))+1)),
            xaxt="n" ,
            col='deepskyblue',
            frame=F)
    par(mar=c(4, 3.1, 1.1, 2.1))
    hist(variable_to_plot,
         breaks=number_of_breaks_for_hist,
         col=rgb(0.2,0.8,0.5,0.5) ,
         border=F ,
         main=plot_title,
         ylim=c(0,(length(variable_to_plot))),
         xlab="cummulative monthly pr [mm]",
         xlim=c(min(variable_to_plot),(floor(max(variable_to_plot))+1)))
    
  } else if (month_number==4){
    plot_title<-"april"
    variable_to_plot<-df_stats_MAMVIA_april$mean
    
    number_of_breaks_for_hist<-floor(length(variable_to_plot)/2)+1
    
    # Layout to split the screen
    layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
    
    # Draw the boxplot and the histogram 
    par(mar=c(0, 3.1, 1.1, 2.1))
    boxplot(variable_to_plot,
            horizontal=TRUE,
            ylim=c(min(variable_to_plot),(floor(max(variable_to_plot))+1)),
            xaxt="n" ,
            col='deepskyblue',
            frame=F)
    par(mar=c(4, 3.1, 1.1, 2.1))
    hist(variable_to_plot,
         breaks=number_of_breaks_for_hist,
         col=rgb(0.2,0.8,0.5,0.5) ,
         border=F ,
         main=plot_title,
         ylim=c(0,(length(variable_to_plot))),
         xlab="cummulative monthly pr [mm]",
         xlim=c(min(variable_to_plot),(floor(max(variable_to_plot))+1)))
    
  } else if (month_number==5){
    plot_title<-"may"
    variable_to_plot<-df_stats_MAMVIA_may$mean
    
    number_of_breaks_for_hist<-floor(length(variable_to_plot)/2)+1
    
    # Layout to split the screen
    layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
    
    # Draw the boxplot and the histogram 
    par(mar=c(0, 3.1, 1.1, 2.1))
    boxplot(variable_to_plot,
            horizontal=TRUE,
            ylim=c(min(variable_to_plot),(floor(max(variable_to_plot))+1)),
            xaxt="n" ,
            col='deepskyblue',
            frame=F)
    par(mar=c(4, 3.1, 1.1, 2.1))
    hist(variable_to_plot,
         breaks=number_of_breaks_for_hist,
         col=rgb(0.2,0.8,0.5,0.5) ,
         border=F ,
         main=plot_title,
         ylim=c(0,(length(variable_to_plot)-10)),
         xlab="cummulative monthly pr [mm]",
         xlim=c(min(variable_to_plot),(floor(max(variable_to_plot))+1)))
    
  } else if (month_number==6){
    plot_title<-"june"
    variable_to_plot<-df_stats_MAMVIA_june$mean
    
    number_of_breaks_for_hist<-floor(length(variable_to_plot)/2)+1
    
    # Layout to split the screen
    layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
    
    # Draw the boxplot and the histogram 
    par(mar=c(0, 3.1, 1.1, 2.1))
    boxplot(variable_to_plot,
            horizontal=TRUE,
            ylim=c(min(variable_to_plot),(floor(max(variable_to_plot))+1)),
            xaxt="n" ,
            col='deepskyblue',
            frame=F)
    par(mar=c(4, 3.1, 1.1, 2.1))
    hist(variable_to_plot,
         breaks=number_of_breaks_for_hist,
         col=rgb(0.2,0.8,0.5,0.5) ,
         border=F ,
         main=plot_title,
         ylim=c(0,(length(variable_to_plot))),
         xlab="cummulative monthly pr [mm]",
         xlim=c(min(variable_to_plot),(floor(max(variable_to_plot))+1)))
    
  } else if (month_number==7){
    plot_title<-"july"
    variable_to_plot<-df_stats_MAMVIA_july$mean
    
    number_of_breaks_for_hist<-floor(length(variable_to_plot)/2)+1
    
    # Layout to split the screen
    layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
    
    # Draw the boxplot and the histogram 
    par(mar=c(0, 3.1, 1.1, 2.1))
    boxplot(variable_to_plot,
            horizontal=TRUE,
            ylim=c(min(variable_to_plot),(floor(max(variable_to_plot))+1)),
            xaxt="n" ,
            col='deepskyblue',
            frame=F)
    par(mar=c(4, 3.1, 1.1, 2.1))
    hist(variable_to_plot,
         breaks=number_of_breaks_for_hist,
         col=rgb(0.2,0.8,0.5,0.5) ,
         border=F ,
         main=plot_title,
         ylim=c(0,(length(variable_to_plot)-10)),
         xlab="cummulative monthly pr [mm]",
         xlim=c(min(variable_to_plot),(floor(max(variable_to_plot))+5)))
    
  } else if (month_number==8){
    plot_title<-"august"
    variable_to_plot<-df_stats_MAMVIA_august$mean
    
    number_of_breaks_for_hist<-floor(length(variable_to_plot)/2)+1
    
    # Layout to split the screen
    layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
    
    # Draw the boxplot and the histogram 
    par(mar=c(0, 3.1, 1.1, 2.1))
    boxplot(variable_to_plot,
            horizontal=TRUE,
            ylim=c(min(variable_to_plot),(floor(max(variable_to_plot))+1)),
            xaxt="n" ,
            col='deepskyblue',
            frame=F)
    par(mar=c(4, 3.1, 1.1, 2.1))
    hist(variable_to_plot,
         breaks=number_of_breaks_for_hist,
         col=rgb(0.2,0.8,0.5,0.5) ,
         border=F ,
         main=plot_title,
         ylim=c(0,(length(variable_to_plot))),
         xlab="cummulative monthly pr [mm]",
         xlim=c(min(variable_to_plot),(floor(max(variable_to_plot))+1)))
    
  } else if (month_number==9){
    plot_title<-"september"
    variable_to_plot<-df_stats_MAMVIA_september$mean
    
    number_of_breaks_for_hist<-floor(length(variable_to_plot)/2)+1
    
    # Layout to split the screen
    layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
    
    # Draw the boxplot and the histogram 
    par(mar=c(0, 3.1, 1.1, 2.1))
    boxplot(variable_to_plot,
            horizontal=TRUE,
            ylim=c(min(variable_to_plot),(floor(max(variable_to_plot))+1)),
            xaxt="n" ,
            col='deepskyblue',
            frame=F)
    par(mar=c(4, 3.1, 1.1, 2.1))
    hist(variable_to_plot,
         breaks=number_of_breaks_for_hist,
         col=rgb(0.2,0.8,0.5,0.5) ,
         border=F ,
         main=plot_title,
         ylim=c(0,(length(variable_to_plot)-10)),
         xlab="cummulative monthly pr [mm]",
         xlim=c(min(variable_to_plot),(floor(max(variable_to_plot))+1)))
    
  } else if (month_number==10){
    plot_title<-"october"
    variable_to_plot<-df_stats_MAMVIA_october$mean
    
    number_of_breaks_for_hist<-floor(length(variable_to_plot)/2)+1
    
    # Layout to split the screen
    layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
    
    # Draw the boxplot and the histogram 
    par(mar=c(0, 3.1, 1.1, 2.1))
    boxplot(variable_to_plot,
            horizontal=TRUE,
            ylim=c(min(variable_to_plot),(floor(max(variable_to_plot))+1)),
            xaxt="n" ,
            col='deepskyblue',
            frame=F)
    par(mar=c(4, 3.1, 1.1, 2.1))
    hist(variable_to_plot,
         breaks=number_of_breaks_for_hist,
         col=rgb(0.2,0.8,0.5,0.5) ,
         border=F ,
         main=plot_title,
         ylim=c(0,(length(variable_to_plot))),
         xlab="cummulative monthly pr [mm]",
         xlim=c(min(variable_to_plot),(floor(max(variable_to_plot))+1)))
    
  } else if (month_number==11){
    plot_title<-"november"
    variable_to_plot<-df_stats_MAMVIA_november$mean
    
    number_of_breaks_for_hist<-floor(length(variable_to_plot)/2)+1
    
    # Layout to split the screen
    layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
    
    # Draw the boxplot and the histogram 
    par(mar=c(0, 3.1, 1.1, 2.1))
    boxplot(variable_to_plot,
            horizontal=TRUE,
            ylim=c(min(variable_to_plot),(floor(max(variable_to_plot))+1)),
            xaxt="n" ,
            col='deepskyblue',
            frame=F)
    par(mar=c(4, 3.1, 1.1, 2.1))
    hist(variable_to_plot,
         breaks=number_of_breaks_for_hist,
         col=rgb(0.2,0.8,0.5,0.5) ,
         border=F ,
         main=plot_title,
         ylim=c(0,(length(variable_to_plot))),
         xlab="cummulative monthly pr [mm]",
         xlim=c(min(variable_to_plot),(floor(max(variable_to_plot))+1)))
    
  } else if (month_number==12){
    plot_title<-"december"
    variable_to_plot<-df_stats_MAMVIA_december$mean
    
    number_of_breaks_for_hist<-floor(length(variable_to_plot)/2)+1
    
    # Layout to split the screen
    layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
    
    # Draw the boxplot and the histogram 
    par(mar=c(0, 3.1, 1.1, 2.1))
    boxplot(variable_to_plot,
            horizontal=TRUE,
            ylim=c(min(variable_to_plot),(floor(max(variable_to_plot))+1)),
            xaxt="n" ,
            col='deepskyblue',
            frame=F)
    par(mar=c(4, 3.1, 1.1, 2.1))
    hist(variable_to_plot,
         breaks=number_of_breaks_for_hist,
         col=rgb(0.2,0.8,0.5,0.5) ,
         border=F ,
         main=plot_title,
         ylim=c(0,(length(variable_to_plot)-10)),
         xlab="cummulative monthly pr [mm]",
         xlim=c(min(variable_to_plot),(floor(max(variable_to_plot))+1)))
    
  }  
  # clean console
  cat("\014")
}
# clean console
cat("\014")


#----------------------------------------------------------------------------------------------------------

# This function generates a list containing the index numbers of a  year 
#----------------------------------------------------------------------------------------------------------

# Input parameters:

#   year of interest       = number with the year of interest [1979-2016]
#   rasterbrick            = the rasterbrick element with all the rasters for the CR2 data

# This fucntion assumes that all the years start at the 01-01
# This fucntion assumes that the names for the rasterbrick are in the form of : X.YYYY.mm.dd
# This function assumes all the years are complete and the data organised chronologically

generate_list_of_indices_for_a_year <- function(year_of_interest,rasterbrick) 
{
  
  # date of the first day of the interest year [yyyy-mm-dd]:  
  date_text_first_day_of_interest_year<-paste(toString(year_of_interest),"-01-01",sep="")
  
  index_first_day_of_the_interest_year<-return_CR2_index(date_text_first_day_of_interest_year,rasterbrick) 
  
  #assuming the year is compelte you just need to find the last day of the current year and that's it
  
  date_text_last_day_of_the_current_year<-paste(toString(year_of_interest),"-12-31",sep="")
  
  index_last_day_of_the_interest_year<-return_CR2_index(date_text_last_day_of_the_current_year,rasterbrick) 
  
  # here you generate the list of values for the year:
  list_of_indices<-seq(index_first_day_of_the_interest_year,index_last_day_of_the_interest_year, by=1)
  
  return(list_of_indices)
  
}
# clean console
cat("\014")
#----------------------------------------------------------------------------------------------------------

# This function returns a  corrected time series in case there are errors in teh original CR2 met product
#----------------------------------------------------------------------------------------------------------
# input parameters:

#   df         = the dataframe with the pixel time series


# the NO data values should be NA but R convert those values to 300 or 32000 and I dunno why ?

fix_CR2_df <- function(error_threshold,df) 
{
  print("Fixing dataframe:")
  
  start_index<-1
  end_index<-dim(df)[1]
  
  for (i in start_index:end_index) 
  { 
    current_value<-df[i,2]
    
    if (current_value>error_threshold) 
    {
      df[i,2]<-NA
      print(paste("Value for index number ",i," fixed !",sep=""))
      
    }
  }
  print("Dataframe fixed succesfully !")
  
  return(df)
  
}
#clean console
cat("\014")
#----------------------------------------------------------------------------------------------------------

# This function saves a rasterbrick as a ncdf object using the specific location 
#----------------------------------------------------------------------------------------------------------

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
#----------------------------------------------------------------------------------------------------------

# This function creates a dataframe with the average pixel value for each raster in the input rasterbrick
#----------------------------------------------------------------------------------------------------------

generate_df_with_average_for_rasterbrick<- function(identifier_vector,
                                                    rasterbrick) 
{
  
  # this variable control the execution time
  time_before_executing<- Sys.time()
  
  number_of_rasters_inside_rasterbrick<-nlayers(rasterbrick)
  
  #this lines controls that the inputs are valid
  if(!((length(identifier_vector))==number_of_rasters_inside_rasterbrick)){
    print(identifier_vector)
    print(names(rasterbrick))
    print(paste("Number of IDs:",as.character(length(identifier_vector)),sep=" "))
    print(paste("Number of rasters:",as.character(number_of_rasters_inside_rasterbrick),sep=" "))
    stop("vector with raster's identifiers has wrong dimension")
  }
  
  # This variable will control the loop to go trough the rasterbrick
  current_extraction_index<-1
  
  # this is the way to extract 1 raster out of a rasterbrick according to the index number
  current_raster<-rasterbrick[[current_extraction_index]]
  
  # mean,min,max,sum
  statistics_current_raster<-calculate_basic_statistics_for_1_raster(current_raster)
  
  # Here you initialise  the vector with mean values:  
  
  vector_of_mean_values <- c(statistics_current_raster[current_extraction_index])
  vector_ids<-c(identifier_vector[current_extraction_index])
  
  #print("generating dataframe...")
  #print(paste("generating dataframe for raster ",current_extraction_index ,"out of",number_of_rasters_inside_rasterbrick,sep=" "))
  current_extraction_index<-current_extraction_index+1
  
  # This is the main loop that fills up all the rows in the dataframe:
  while (current_extraction_index <= number_of_rasters_inside_rasterbrick) {
    
    # in the following lines you fill up the vectors:
    current_raster<-rasterbrick[[current_extraction_index]]
    
    statistics_current_raster<-calculate_basic_statistics_for_1_raster(current_raster)
    id_current_raster<-identifier_vector[current_extraction_index]
    
    vector_of_mean_values <- c(vector_of_mean_values,statistics_current_raster[1])
    vector_ids<-c(vector_ids, id_current_raster)
    
    #print(paste("generating dataframe for raster ",current_extraction_index,"out of",number_of_rasters_inside_rasterbrick,sep=" "))
    current_extraction_index<-current_extraction_index+1
  } # This is the end of the  main loop
  
  
  df_mean_vals <- data.frame(ID=vector_ids,
                             mean=vector_of_mean_values,
                             stringsAsFactors=FALSE)
  
  
  #print("dataframe generated succesfully !!")
  
  elapsed_time<-time_diff(time_before_executing)
  #print(paste("Total execution time [hh:mm:ss]",elapsed_time,sep=" "))
  
  return(df_mean_vals)
  
  rm(time_before_executing)
  rm(elapsed_time)
}
#----------------------------------------------------------------------------------------------------------

# This function returns a string/ char with the first day of a month introduced as number 

#----------------------------------------------------------------------------------------------------------
return_first_day_of_the_month_as_text<-function(month_number, first_year_as_char){
  

  if(month_number==1){
    monht_text<-"january"
    month_number_as_char<-"01"
    number_of_days_of_a_typical_month<-31
  } else if (month_number==2){
    monht_text<-"february"
    month_number_as_char<-"02"
    number_of_days_of_a_typical_month<-28
  } else if (month_number==3){
    monht_text<-"march"
    month_number_as_char<-"03"
    number_of_days_of_a_typical_month<-31
  } else if (month_number==4){
    monht_text<-"april"
    month_number_as_char<-"04"
    number_of_days_of_a_typical_month<-30
  } else if (month_number==5){
    monht_text<-"may"
    month_number_as_char<-"05"
    number_of_days_of_a_typical_month<-31
  } else if (month_number==6){
    monht_text<-"june"
    month_number_as_char<-"06"
    number_of_days_of_a_typical_month<-30  
  } else if (month_number==7){
    monht_text<-"july"
    month_number_as_char<-"07"
    number_of_days_of_a_typical_month<-31
  } else if (month_number==8){
    monht_text<-"august"
    month_number_as_char<-"08"
    number_of_days_of_a_typical_month<-31
  } else if (month_number==9){
    monht_text<-"september"
    month_number_as_char<-"09"
    number_of_days_of_a_typical_month<-30  
  } else if (month_number==10){
    monht_text<-"october"
    month_number_as_char<-"10"
    number_of_days_of_a_typical_month<-31
  } else if (month_number==11){
    monht_text<-"november"
    month_number_as_char<-"11"
    number_of_days_of_a_typical_month<-30   
  } else if (month_number==12){
    monht_text<-"december"
    month_number_as_char<-"12"
    number_of_days_of_a_typical_month<-31   
  }
  
  date_text_first_day_current_year<-paste(first_year_as_char,
                                          month_number_as_char,
                                          "01",
                                          sep="-")
  
  return(date_text_first_day_current_year)
  
}
#----------------------------------------------------------------------------------------------------------

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#
# ^^     ^^     ^^     ^^     ^^     ^^     ^^     ^^     ^^     ^^     ^^     ^^          Nested functions
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#


# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!IMPORTANT
#                                               MANDATORY TO EXECUTE THIS SECTION BEFORE EXECUTING ANY LINE
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!IMPORTANT

# here you load the raw data (CR2_met product) and you mask it to the interest area:

# It is absolutely necessary to run these lines before doing any analysis or calling any function !

# main output: raster brick

# This function must be pre-compiled
load_libraries()

# Here you explore the ncdf file or create it if it doesn't exist:
#----------------------------------------------------------------------------------------------------------

#if your are running in the hard drive of your PC or laptop:
#setwd("C:/00-C-GPM_INFO/04-C-RDM/04-C-01-R/04-C-01-02-CR2-DAP")
setwd("C:/00_PhD/R")

# If you wanna plot in a separated Window:
options(device = "windows")

# if you are running in the UQ-RDM server:
#setwd("R:/PHD042018-Q1021/09-R-CCRMC2019/09-R-06-Scripts/06-01-R/CR2-DAP")
main_directory<- getwd()

# Here you load the CR2 dataset and extract the basic attributes:
loading_directory<-paste(main_directory,"04-Inputs-files", sep="/")
setwd(loading_directory)

if (variable_short_name=="pr") {
  ncdf_element<- nc_open(ncdfname)
  variable_long_name<-as.character(ncatt_get( ncdf_element, "pr", "long_name") [2][1])
  variable_units<-as.character(ncatt_get( ncdf_element, "pr", "units")[2] [1])
  
  time_units<-ncatt_get( ncdf_element, "time", "units") [2]
  time_units<-as.character(time_units[1])
  
  time_origin_date<-strsplit(time_units,split=" ") [1] [[1]] [3]
  
  
} else if (variable_short_name=="tmin"){
  ncdf_element<- nc_open(ncdfname)
  variable_long_name<-as.character(ncatt_get( ncdf_element, "tmin", "long_name") [2][1])
  variable_units<-as.character(ncatt_get( ncdf_element, "tmin", "units")[2] [1])
  
  time_units<-ncatt_get( ncdf_element, "time", "units") [2]
  time_units<-as.character(time_units[1])
  
  time_origin_date<-strsplit(time_units,split=" ") [1] [[1]] [3]
  
} else if (variable_short_name=="tmax"){
  ncdf_element<- nc_open(ncdfname)
  variable_long_name<-as.character(ncatt_get( ncdf_element, "tmax", "long_name") [2][1])
  variable_units<-as.character(ncatt_get( ncdf_element, "tmax", "units")[2] [1])
  
  time_units<-ncatt_get( ncdf_element, "time", "units") [2]
  time_units<-as.character(time_units[1])
  
  time_origin_date<-strsplit(time_units,split=" ") [1] [[1]] [3]
  
} else if(variable_short_name=="tmean" & create_tmean_ncdf==TRUE){
  
  ncdf_element_tmin<- nc_open("daily-tmin-CR2_met_v2.0-Copiapo-River-Basin.nc")
  ncdf_element_tmax<- nc_open("daily-tmax-CR2_met_v2.0-Copiapo-River-Basin.nc")
  
  variable_long_name<-"mean temperature"
  variable_units<-"Celsius"
  time_units<-"seconds since 1979-01-01 00:00:00"
  time_origin_date<-"1979-01-01"
  rm(ncdf_element_tmin)
  rm(ncdf_element_tmax)
} else if (variable_short_name=="tmean" & create_tmean_ncdf==FALSE){
  
  ncdf_element<- nc_open(ncdfname)
  variable_long_name<-as.character(ncatt_get( ncdf_element, "tmean", "long_name") [2][1])
  variable_units<-as.character(ncatt_get( ncdf_element, "tmean", "units")[2] [1])
  
  time_units<-ncatt_get( ncdf_element, "time", "units") [2]
  time_units<-as.character(time_units[1])
  
  time_origin_date<-strsplit(time_units,split=" ") [1] [[1]] [3]

  
}
#if you don't need the ncdf file you should delete it from the workspace to save RAM memory:
if(delete_ncdf_from_RAM == "Yes"){
  
  if (variable_short_name=="pr") {
    rm(ncdf_element)
  }
  if (variable_short_name=="tmin"){
    rm(ncdf_element)
    
  }
  if(variable_short_name=="tmax"){
    rm(ncdf_element)
  }
  if(variable_short_name=="tmean" & create_tmean_ncdf==FALSE){
    rm(ncdf_element)
  }
}

# This conditional controls the creation of the tmean rasterbrick

# WARNING !
# This takes several hours (depending on the computer ever days)

if(variable_short_name=="tmean" & create_tmean_ncdf==TRUE ){
  
  loading_directory<-paste(main_directory,"04-Inputs-files", sep="/")
  setwd(loading_directory)
  
  rasterbrick_tmin <- brick("CR2MET_tmin_v2.0_day.nc")
  rasterbrick_tmax <- brick("CR2MET_tmax_v2.0_day.nc")
  
  first_raster_tmin<-rasterbrick_tmin[[1]]
  first_raster_tmax<-rasterbrick_tmax[[1]]
  
  first_raster_tmean<-mean(first_raster_tmin,first_raster_tmax)
  
  rasterbrick_with_all_chile<-brick(first_raster_tmean)
  
  number_rasters_tmin<-nlayers(rasterbrick_tmin)
  number_rasters_tmax<-nlayers(rasterbrick_tmax)
  
  if(number_rasters_tmin == number_rasters_tmax){
    
    number_rasters<-number_rasters_tmax
    
    print("generating raster tmean...")
    print("please wait....")
    print("This takes several hours")
    time_before_executing<- Sys.time()
    
    for( i in 2:number_rasters ){
      
      print(paste("raster ", i ," out of " ,number_rasters,sep=""))
      
      current_raster_tmin<-rasterbrick_tmin[[i]]
      current_raster_tmax<-rasterbrick_tmax[[i]]
      
      current_raster_tmean<-mean(current_raster_tmin,current_raster_tmax)
      rasterbrick_with_all_chile<-addLayer(rasterbrick_with_all_chile,current_raster_tmean)
      
      elapsed_time<-time_diff(time_before_executing)
      print(paste("Elapsed time [hh:mm:ss]",elapsed_time,sep=" "))
      
    } # end of the loop to create tmean 
  
    # here you save the ncdf file
    
    names(rasterbrick_with_all_chile)<-names(rasterbrick_tmax)
    
    ncdf_filename_for_saving=paste("CR2MET_",variable_short_name,"_v2.0_day.nc",sep="")

    name_of_saving_directory<-"08-Results-rasters"
    
    save_a_ncdf_object_in_a_specified_location(name_of_saving_directory,
                                               main_directory,
                                               ncdf_filename_for_saving,
                                               rasterbrick_with_all_chile,
                                               variable_short_name,
                                               variable_long_name,
                                               variable_units,
                                               time_units)
    
  
  } else {  
    print("Error")
    print("Number of rasters of tmin is different than the number of rasters of tmax")
    
  } # end of the conditional to create tmean
  
} else {
  
  # if you don't have to create the tmean it means that the product already exists 
  rasterbrick_with_all_chile <- brick(ncdfname, var=variable_short_name)
  
} # End fo the loop to load or create the raster brick with all chile for tmean

#----------------------------------------------------------------------------------------------------------

# Here you masked the CR2 product to the interest area/catchment you want to analyze
#----------------------------------------------------------------------------------------------------------

loading_directory<-paste(main_directory,"05-Input-rasters", sep="/")
setwd(loading_directory)

loading_route<- paste(loading_directory,raster_name_file, sep="/")
mask_raster<-raster(loading_route)
mask_extent <- extent(mask_raster)

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! WARNING
# THIS TAKE SOME TIME TO RUN !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! WARNING

# Here you crop the raster brick with all Chile to the interest area
cropped_brick<-crop(rasterbrick_with_all_chile,mask_extent)
extent(cropped_brick)<- mask_extent
rasterbrick <- mask(cropped_brick,mask_raster)

setwd(main_directory)

# As we don't know if the CR2 data set has days missing it is necessary to convert each time ID
# into a date one by one: 

if(time_units=="days since 1978-12-31"){
  
  date_first_raster<-"1979-01-01"
  first_year_as_text<-"1979"
  
} else if(time_units=="seconds since 1979-01-01 00:00:00"){  
  
  date_first_raster<-"1979-01-01"
  first_year_as_text<-"1979"
}

# Here you create a list of IDs with dates for the raster brick
list_of_IDs<-Generate_list_identifiers_daily_rasterbrick(rasterbrick,time_units,date_first_raster)

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! WARNING
# THIS TAKE SOME TIME TO RUN !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! WARNING


# the next instruction assigns the contour of the catchment to a variable 
# called "catchment_polygon"

shape_file_loading_directory<-paste(main_directory,"06-Input-shapes", sep="/")
shape_name <- paste(shape_file_loading_directory,shape_file_name, sep="/")

# this instruction opens the shape file with the catchment polygon and store 
# it into a variable called "catchment_polygon"

rgdal::readOGR(shape_name, stringsAsFactors = F) %>% 
  sp::spTransform(CRS(projection(rasterbrick[[1]]))) -> catchment_polygon

rm(cropped_brick)
rm(mask_extent)
rm(loading_directory)
#----------------------------------------------------------------------------------------------------------

# here you check the data set visually
#----------------------------------------------------------------------------------------------------------
 
plot(rasterbrick)

#----------------------------------------------------------------------------------------------------------
 
# Here you save the raster brick with the daily temperature for the interest area:
#----------------------------------------------------------------------------------------------------------

ncdf_filename_for_saving=paste("daily-",variable_short_name,"-CR2_met-",area_name,".nc",sep="")

name_of_saving_directory<-"08-Result-rasters"

save_a_ncdf_object_in_a_specified_location(name_of_saving_directory,
                                           main_directory,
                                           ncdf_filename_for_saving,
                                           rasterbrick,
                                           variable_short_name,
                                           variable_long_name,
                                           variable_units,
                                           time_units)

# Clean the console
cat("\014")

#----------------------------------------------------------------------------------------------------------

# Here you define the function to find the index of an specific date
# this function depends on the time units of the original RAW data therefore you cannot define it until
# you have loaded the input data
#----------------------------------------------------------------------------------------------------------
# Here you create the right function to find a day index based on the names of the ncdf file

if(time_units=="seconds since 1979-01-01 00:00:00"){
  #this can change :
  date_origin<-"1979-01-01"
  
  print("First definition to find CR2 met index will be used....")
  print("raster labels in the format X.relative seconds")
  print(time_units)
  return_CR2_index <- function(date,rasterbrick) 
  {
    current_date<-Sys.time()
    list_with_date_components<-strsplit(date,"-")
    year_number<- as.numeric(list_with_date_components[[1]][1])
    month_number<- as.numeric(list_with_date_components[[1]][2])
    day_number<- as.numeric(list_with_date_components[[1]][3])
    
    date_element<-ISOdate(year = year_number, month = month_number, day = day_number)
    
    date_element_origin<-ISOdate(year = 1979, month = 1, day = 1)
    
    Time_difference<-date_element-date_element_origin
    
    TDF_relative_days<- as.numeric(Time_difference, units="days")
    TDF_relative_seconds<- TDF_relative_days*86400 
    
    if(date_element>current_date) 
    {
      print("Input date:")
      print(date_element)
      stop("The date introduced as the input parameter is referring to the future !")
    }
    
    # in the following lines we create a string(char) with the format required for the match() 
    # function to find the right index
    
    # the ncdf names are in the form : X0, X86400, X172800 ....
    
    string_with_name_to_find_index <-paste("X",TDF_relative_seconds, sep ="")
    
    # this function returns the index of the data matching the search criteria inside the rasterbrick
    index<-match(string_with_name_to_find_index, names(rasterbrick))
    
    # for debugging the code
    try(if(is.na(index)) stop(paste("the rasterbrick column with the dates is corrupted ! ", date, sep=" ")))
    
    #print(paste("The index for the requested date is:",index))
    return(index)
    
    rm(date_element)
    rm(date_element_origin)
    rm(current_date)
    rm(list_with_date_components)
    rm(TDF_relative_days)
    rm(TDF_relative_seconds)
    rm(string_with_name_to_find_index)
    rm(index)
  }
  
}

if(time_units=="days since 1978-12-31"){
  
  #this can change :
  date_origin<-"1978-12-31"
  print("second definition to find the CR2 met index used ...")
  print("raster labels in the format X.YYYY.MM.DD")
  return_CR2_index <- function(date,rasterbrick) 
  {
    current_date<-Sys.time()
    list_with_date_components<-strsplit(date,"-")
    year_number<- as.numeric(list_with_date_components[[1]][1])
    month_number<- as.numeric(list_with_date_components[[1]][2])
    day_number<- as.numeric(list_with_date_components[[1]][3])
    
    date_element<-ISOdate(year = year_number, month = month_number, day = day_number)
    
    if(date_element>current_date) 
    {
      print("Input date:")
      print(date_element)
      stop("The date introduced as the input parameter is referring to the future !")
    }
    
    # in the following lines we create a string(char) with the format required for the match() function to work
    name_for_index_search_part1 <-"X"
    
    # in the following lines we replace the "-" with "." 
    string <-sub("-", ".", date)
    name_for_index_search_part2 <-sub("-", ".", string)
    
    # in this line we create the paramter to run the search within the rasterbrick
    string_with_name_to_find_index <-paste(name_for_index_search_part1,name_for_index_search_part2 ,
                                           sep ="")
    
    # this function returns the index of the data matching the search criteria inside the rasterbrick
    index<-match(string_with_name_to_find_index, names(rasterbrick))
    
    # for debugging the code
    try(if(is.na(index)) stop("the rasterbrick column with the dates is corrupted !"))
    
    #print(paste("The index for the requested date is:",index))
    return(index)
    
    rm(name_for_index_search_part1)
    rm(string)
    rm(name_for_index_search_part2)
    rm(string_with_name_to_find_index)
    rm(index)
  }
}
#----------------------------------------------------------------------------------------------------------

# these lines help to check the index function is working fine:
#----------------------------------------------------------------------------------------------------------
# this function should return the index of the data matching the search criteria

print(paste("interest date: " , extraction_date_text ,sep=""))
extraction_day_index<- return_CR2_index(extraction_date_text,rasterbrick) 
print( paste("the interest index: ", extraction_day_index,sep=""))
#----------------------------------------------------------------------------------------------------------

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!IMPORTANT
#  ^   ^   ^   ^   ^   ^   ^   ^   ^   ^   ^   ^   ^   ^   ^   ^          MANDATORY TO EXECUTE THIS SECTION
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!IMPORTANT

#------------------------------------------------------------------------------------------------------------> start

# ()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()
#                                                                                      Temporal  statistics 
#()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()

# Calculates mean value of the entire time series (all days on record)
raster_with_multiannual_mean_daily_values<- mean(rasterbrick)

# create raster bricks with multi-annual monthly values: 

# if variable_short_name == tmin -> you find the minimum value for each month (coldest day of the month)
# if variable_short_name == tmax -> you find the maximum value for each month (hottest day of the month)
# if variable_short_name == tmean -> you calculate the average of all the raster for each month

#----------------------------------------------------------------------------------------------------------
# Here you create a raster brick with 1 monthly value for each year:


# January
month_number<-1
rasterbrick_MA_monthly_values_january<-generate_monthly_rasterbrick(month_number,
                                                                    variable_short_name,
                                                                    rasterbrick,
                                                                    date_origin)

# February
month_number<-2
rasterbrick_MA_monthly_values_february<-generate_monthly_rasterbrick(month_number,
                                                                     variable_short_name,
                                                                     rasterbrick,
                                                                     date_origin)

# March
month_number<-3
rasterbrick_MA_monthly_values_march<-generate_monthly_rasterbrick(month_number,
                                                               variable_short_name,
                                                               rasterbrick,
                                                               date_origin)

# April
month_number<-4
rasterbrick_MA_monthly_values_april<-generate_monthly_rasterbrick(month_number,
                                                               variable_short_name,
                                                               rasterbrick,
                                                               date_origin)

# May
month_number<-5
rasterbrick_MA_monthly_values_may<-generate_monthly_rasterbrick(month_number,
                                                             variable_short_name,
                                                             rasterbrick,
                                                             date_origin)

# June
month_number<-6
rasterbrick_MA_monthly_values_june<-generate_monthly_rasterbrick(month_number,
                                                              variable_short_name,
                                                              rasterbrick,
                                                              date_origin)

# July
month_number<-7
rasterbrick_MA_monthly_values_july<-generate_monthly_rasterbrick(month_number,
                                                              variable_short_name,
                                                              rasterbrick,
                                                              date_origin)

# August
month_number<-8
rasterbrick_MA_monthly_values_august<-generate_monthly_rasterbrick(month_number,
                                                                variable_short_name,
                                                                rasterbrick,
                                                                date_origin)

# September
month_number<-9
rasterbrick_MA_monthly_values_september<-generate_monthly_rasterbrick(month_number,
                                                                   variable_short_name,
                                                                   rasterbrick,
                                                                   date_origin)

# October
month_number<-10
rasterbrick_MA_monthly_values_october<-generate_monthly_rasterbrick(month_number,
                                                                 variable_short_name,
                                                                 rasterbrick,
                                                                 date_origin)

# November
month_number<-11
rasterbrick_MA_monthly_values_november<-generate_monthly_rasterbrick(month_number,
                                                                  variable_short_name,
                                                                  rasterbrick,
                                                                  date_origin)

# December
month_number<-12
rasterbrick_MA_monthly_values_december<-generate_monthly_rasterbrick(month_number,
                                                                  variable_short_name,
                                                                  rasterbrick,
                                                                  date_origin)

#----------------------------------------------------------------------------------------------------------

# Visual assessment of monthly raster bricks:
#----------------------------------------------------------------------------------------------------------
plot(rasterbrick_MA_monthly_values_january)
plot(rasterbrick_MA_monthly_values_february)
plot(rasterbrick_MA_monthly_values_march)
plot(rasterbrick_MA_monthly_values_april)
plot(rasterbrick_MA_monthly_values_may)
plot(rasterbrick_MA_monthly_values_june)
plot(rasterbrick_MA_monthly_values_july)
plot(rasterbrick_MA_monthly_values_august)
plot(rasterbrick_MA_monthly_values_september)
plot(rasterbrick_MA_monthly_values_october)
plot(rasterbrick_MA_monthly_values_november)
plot(rasterbrick_MA_monthly_values_december)
#----------------------------------------------------------------------------------------------------------

# Here you create 12 dataframes with pixel statistics for each month (mean, min, max, sum): 
#----------------------------------------------------------------------------------------------------------

MAMVIA= "multi-annual monthly values interest area"

data_series_type<-"monthly_CR2"

# Generate dataframe for January

additional_info<-"january"
date_first_raster<-return_first_day_of_the_month_as_text(1,first_year_as_text)

obs_identifier_vector<-Generate_list_with_consecutive_identifiers(data_series_type,
                                                                  additional_info,
                                                                  rasterbrick_MA_monthly_values_january,
                                                                  date_first_raster)

df_stats_MAMVIA_janaury<-generate_dataframe_with_spatial_stats(data_series_type,
                                                               obs_identifier_vector,
                                                               area_name,
                                                               variable_short_name,
                                                               rasterbrick_MA_monthly_values_january)


# Generate dataframe for February

additional_info<-"february"
date_first_raster<-return_first_day_of_the_month_as_text(2,first_year_as_text)

obs_identifier_vector<-Generate_list_with_consecutive_identifiers(data_series_type,
                                                                  additional_info,
                                                                  rasterbrick_MA_monthly_values_february,
                                                                  date_first_raster)

df_stats_MAMVIA_february<-generate_dataframe_with_spatial_stats(data_series_type,
                                                                obs_identifier_vector,
                                                                area_name,
                                                                variable_short_name,
                                                                rasterbrick_MA_monthly_values_february)

# Generate dataframe for March

additional_info<-"march"
date_first_raster<-return_first_day_of_the_month_as_text(3,first_year_as_text)

obs_identifier_vector<-Generate_list_with_consecutive_identifiers(data_series_type,
                                                                  additional_info,
                                                                  rasterbrick_MA_monthly_values_march,
                                                                  date_first_raster)

df_stats_MAMVIA_march<-generate_dataframe_with_spatial_stats(data_series_type,
                                                             obs_identifier_vector,
                                                             area_name,
                                                             variable_short_name,
                                                             rasterbrick_MA_monthly_values_march)


# Generate dataframe for April

additional_info<-"april"
date_first_raster<-return_first_day_of_the_month_as_text(4,first_year_as_text)


obs_identifier_vector<-Generate_list_with_consecutive_identifiers(data_series_type,
                                                                  additional_info,
                                                                  rasterbrick_MA_monthly_values_april,
                                                                  date_first_raster)

df_stats_MAMVIA_april<-generate_dataframe_with_spatial_stats(data_series_type,
                                                             obs_identifier_vector,
                                                             area_name,
                                                             variable_short_name,
                                                             rasterbrick_MA_monthly_values_april)

# Generate dataframe for May

additional_info<-"may"
date_first_raster<-return_first_day_of_the_month_as_text(5,first_year_as_text)


obs_identifier_vector<-Generate_list_with_consecutive_identifiers(data_series_type,
                                                                  additional_info,
                                                                  rasterbrick_MA_monthly_values_may,
                                                                  date_first_raster)

df_stats_MAMVIA_may<-generate_dataframe_with_spatial_stats(data_series_type,
                                                           obs_identifier_vector,
                                                           area_name,
                                                           variable_short_name,
                                                           rasterbrick_MA_monthly_values_may)


# Generate dataframe for June

additional_info<-"june"
date_first_raster<-return_first_day_of_the_month_as_text(6,first_year_as_text)


obs_identifier_vector<-Generate_list_with_consecutive_identifiers(data_series_type,
                                                                  additional_info,
                                                                  rasterbrick_MA_monthly_values_june,
                                                                  date_first_raster)

df_stats_MAMVIA_june<-generate_dataframe_with_spatial_stats(data_series_type,
                                                            obs_identifier_vector,
                                                            area_name,
                                                            variable_short_name,
                                                            rasterbrick_MA_monthly_values_june)

# Generate dataframe for July

additional_info<-"july"
date_first_raster<-return_first_day_of_the_month_as_text(7,first_year_as_text)


obs_identifier_vector<-Generate_list_with_consecutive_identifiers(data_series_type,
                                                                  additional_info,
                                                                  rasterbrick_MA_monthly_values_july,
                                                                  date_first_raster)

df_stats_MAMVIA_july<-generate_dataframe_with_spatial_stats(data_series_type,
                                                            obs_identifier_vector,
                                                            area_name,
                                                            variable_short_name,
                                                            rasterbrick_MA_monthly_values_july)



# Generate dataframe for August

additional_info<-"august"
date_first_raster<-return_first_day_of_the_month_as_text(8,first_year_as_text)


obs_identifier_vector<-Generate_list_with_consecutive_identifiers(data_series_type,
                                                                  additional_info,
                                                                  rasterbrick_MA_monthly_values_august,
                                                                  date_first_raster)

df_stats_MAMVIA_august<-generate_dataframe_with_spatial_stats(data_series_type,
                                                              obs_identifier_vector,
                                                              area_name,
                                                              variable_short_name,
                                                              rasterbrick_MA_monthly_values_august)



# Generate dataframe for September

additional_info<-"september"
date_first_raster<-return_first_day_of_the_month_as_text(9,first_year_as_text)


obs_identifier_vector<-Generate_list_with_consecutive_identifiers(data_series_type,
                                                                  additional_info,
                                                                  rasterbrick_MA_monthly_values_september,
                                                                  date_first_raster)

df_stats_MAMVIA_september<-generate_dataframe_with_spatial_stats(data_series_type,
                                                                 obs_identifier_vector,
                                                                 area_name,
                                                                 variable_short_name,
                                                                 rasterbrick_MA_monthly_values_september)


# Generate dataframe for October

additional_info<-"october"
date_first_raster<-return_first_day_of_the_month_as_text(10,first_year_as_text)

obs_identifier_vector<-Generate_list_with_consecutive_identifiers(data_series_type,
                                                                  additional_info,
                                                                  rasterbrick_MA_monthly_values_october,
                                                                  date_first_raster)

df_stats_MAMVIA_october<-generate_dataframe_with_spatial_stats(data_series_type,
                                                               obs_identifier_vector,
                                                               area_name,
                                                               variable_short_name,
                                                               rasterbrick_MA_monthly_values_october)

# Generate dataframe for november

additional_info<-"november"
date_first_raster<-return_first_day_of_the_month_as_text(11,first_year_as_text)


obs_identifier_vector<-Generate_list_with_consecutive_identifiers(data_series_type,
                                                                  additional_info,
                                                                  rasterbrick_MA_monthly_values_november,
                                                                  date_first_raster)

df_stats_MAMVIA_november<-generate_dataframe_with_spatial_stats(data_series_type,
                                                                obs_identifier_vector,
                                                                area_name,
                                                                variable_short_name,
                                                                rasterbrick_MA_monthly_values_november)

# Generate dataframe for december

additional_info<-"december"
date_first_raster<-return_first_day_of_the_month_as_text(12,first_year_as_text)


obs_identifier_vector<-Generate_list_with_consecutive_identifiers(data_series_type,
                                                                  additional_info,
                                                                  rasterbrick_MA_monthly_values_december,
                                                                  date_first_raster)

df_stats_MAMVIA_december<-generate_dataframe_with_spatial_stats(data_series_type,
                                                                obs_identifier_vector,
                                                                area_name,
                                                                variable_short_name,
                                                                rasterbrick_MA_monthly_values_december)

#----------------------------------------------------------------------------------------------------------

# Here you merge all the dataframes with  the spatial statistics for all the months into 1 single df !
#----------------------------------------------------------------------------------------------------------
# https://medium.com/coinmonks/merging-multiple-dataframes-in-r-72629c4632a3

df_MA_monthly_statistics<-do.call("rbind", list(df_stats_MAMVIA_janaury,
                                                df_stats_MAMVIA_february,
                                                df_stats_MAMVIA_march,
                                                df_stats_MAMVIA_april,
                                                df_stats_MAMVIA_may,
                                                df_stats_MAMVIA_june,
                                                df_stats_MAMVIA_july,
                                                df_stats_MAMVIA_august,
                                                df_stats_MAMVIA_september,
                                                df_stats_MAMVIA_october,
                                                df_stats_MAMVIA_november,
                                                df_stats_MAMVIA_december))

# Visual assessment of data as a table:
View(df_MA_monthly_statistics)

number_Ma_values<-length(df_MA_monthly_statistics$id)

year_numbers<-as.numeric(df_MA_monthly_statistics$year)
df_MA_monthly_statistics$year<-year_numbers

month_numbers<-as.numeric(df_MA_monthly_statistics$month)
df_MA_monthly_statistics$month<-month_numbers

#----------------------------------------------------------------------------------------------------------

# Here you create a multi-annual average raster brick with mean temperature for each month
#----------------------------------------------------------------------------------------------------------

if(variable_short_name =="tmean") {
  
  MA_mean_january<-mean(rasterbrick_MA_monthly_values_january)
  MA_mean_february<-mean(rasterbrick_MA_monthly_values_february)
  MA_mean_march<-mean(rasterbrick_MA_monthly_values_march)
  MA_mean_april<-mean(rasterbrick_MA_monthly_values_april)
  MA_mean_may<-mean(rasterbrick_MA_monthly_values_may)
  MA_mean_june<-mean(rasterbrick_MA_monthly_values_june)
  MA_mean_july<-mean(rasterbrick_MA_monthly_values_july)
  MA_mean_august<-mean(rasterbrick_MA_monthly_values_august)
  MA_mean_september<-mean(rasterbrick_MA_monthly_values_september)
  MA_mean_october<-mean(rasterbrick_MA_monthly_values_october)
  MA_mean_november<-mean(rasterbrick_MA_monthly_values_november)
  MA_mean_december<-mean(rasterbrick_MA_monthly_values_december)
  
  # here you create a rasterbrick with 12 rasters : 1 for each month
  multi_annual_average_monthly_values<-brick(MA_mean_january,
                                             MA_mean_february,
                                             MA_mean_march,
                                             MA_mean_april,
                                             MA_mean_may,
                                             MA_mean_june,
                                             MA_mean_july,
                                             MA_mean_august,
                                             MA_mean_september,
                                             MA_mean_october,
                                             MA_mean_november,
                                             MA_mean_december)
  
  
  
  names(multi_annual_average_monthly_values)<-c("january",
                                                "february",
                                                "march",
                                                "april",
                                                "may",
                                                "june",
                                                "july",
                                                "august",
                                                "september",
                                                "october",
                                                "november",
                                                "december")
  
  ncdf_filename_for_saving=paste("MMAM-",variable_short_name,"-CR2met-",area_name,".nc",sep="")
  
  name_of_saving_directory<-"08-Results-rasters"
  
  saving_route_with_filename<-paste(main_directory,name_of_saving_directory,ncdf_filename_for_saving,sep="/")
  
  writeRaster(multi_annual_average_monthly_values, 
              saving_route_with_filename, 
              overwrite=TRUE, 
              format="CDF",     
              varname=variable_short_name, 
              varunit=variable_units, 
              longname=variable_long_name, 
              xname="Longitude",   
              yname="Latitude", 
              zname=time_units)
  
}

# Clean the console
cat("\014")
#----------------------------------------------------------------------------------------------------------

# Here you create a plot of multi-annual mean temperature for each month using
# a paper like colour palette
#----------------------------------------------------------------------------------------------------------
myTheme <- rasterTheme(region = rep(rev(rev(colorRamps::matlab.like(n=12))),c(1,1,1,1,1,1,1,1,1,1,1,1)))

MMA_tmean<-levelplot(multi_annual_average_monthly_values,par.settings=myTheme)

plot(MMA_tmean)
#----------------------------------------------------------------------------------------------------------

# Here you create a dataframe with a time series of multi-annual  daily values for an interest area:
# This is THE DF (this is the DF you can use to make heat-plots)!
#----------------------------------------------------------------------------------------------------------

# The objective of the following lines is to generate a 
# data frame with the timeseries of numbers corresponding to the mean daily values of a CR2_met product 

#  WARNING ! This can take up to 15 minutes

# in the following line you initialise a dataframe to allocate the values for each day


df_MA_average_daily <- data.frame("Date" = list_of_IDs,
                                  "mean_daily_value"=as.double(0))
start<-1
limit<-dim(rasterbrick)[3]

# Main loop:
time_before_executing<- Sys.time()
for (i in start:limit) { 
  date_i<-list_of_IDs[i]
  raster_current_day<-rasterbrick[[i]]
  
  # In the following line you convert the raster to spatial pixels dataframe
  spdf_current_day <- as( raster_current_day,"SpatialPixelsDataFrame")
  names(spdf_current_day)<-"mean_daily_value"
  mean_current_day<- mean(spdf_current_day$mean_daily_value, na.rm=TRUE)
  
  df_MA_average_daily[i,"mean_daily_value"]<-mean_current_day
  print(paste("day",i,"out of",limit,sep=" "))
  elapsed_time<-time_diff(time_before_executing)
  print(paste("Elapsed time [hh:mm:ss]",elapsed_time,sep=" "))
}
elapsed_time<-time_diff(time_before_executing)
print("Data frame with daily values generated successfully !!")
print(paste("Total elapsed time [hh:mm:ss]",elapsed_time,sep=" "))


# visual inspection of data:

plot(df_MA_average_daily$mean_daily_value)
print(max(df_MA_average_daily$mean_daily_value))
print(min(df_MA_average_daily$mean_daily_value))
#View(df_mean_daily_pr)

if(max(df_MA_average_daily$mean_daily_value)>=300){
  Do_you_need_to_correct_data<-"Yes"
}

df_MA_average_daily_corrected<-df_MA_average_daily
Do_you_need_to_correct_data<-"No"

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! WARNING 
#  WARNING: DO NOT RUN THIS WITHOUT INSPECTING THE TIME SERIES FIRST !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! WARNING

# For some reason, the original CR2 datset has holes in the "pr" values for some areas and certain
# dates. Therefore, when covnerting the ncdf file into a rasterbrick the mean values can be 300 (or higher) 
# which makes no sense. 

# The following function goes through all the mean values in the time series dataframe and   
# correct this error.This weird values should be replace for NA 
# (we cannot assume the mean there is 0):
if(Do_you_need_to_correct_data=="Yes"){
  error_threshold<-299
  df_MA_average_daily_corrected<-fix_CR2_df(error_threshold,df_MA_average_daily)
}

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! WARNING 
#  WARNING: DO NOT RUN THIS WITHOUT INSPECTING THE TIME SERIES FIRST !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! WARNING

# Here you save the time series: 

location_to_save_data<-paste(main_directory,"07-Results-files",sep="/")

mean_daily_time_series_file_name <- paste(paste("mean-daily",variable_short_name,"time-series",area_name, sep ="-"),".csv",sep="")

saving_location <-paste(location_to_save_data,mean_daily_time_series_file_name,sep="/")

write.csv(df_MA_average_daily_corrected,saving_location, row.names = FALSE)

# here you create the extended version of the df with columns for year 

date_parts<-strsplit(list_of_IDs,"-")

number_of_records<-length(list_of_IDs)

list_of_years<-c()
list_of_months<-c()
list_of_months_numbers<-c()
list_of_days<-c()

for(i in 1:number_of_records){
  
  year<-as.numeric(date_parts[[i]][1])
  month<-as.numeric(date_parts[[i]][2])  
  day<-as.numeric(date_parts[[i]][3])
  
  if(!is.na(month)){
    month_text<-convert_month_number_into_string(month)
  } else if (is.na(month)){
    year<-list_of_years[i-1]
    month<-list_of_months[i-1]
    day<-list_of_days[i-1]
  }
  
  list_of_months_numbers<-c(list_of_months_numbers,as.numeric(month))
  list_of_years<-c(list_of_years,year)
  list_of_months<-c(list_of_months,month_text)
  list_of_days<-c(list_of_days,day)
  
}

df_MA_month_values<-df_MA_average_daily_corrected["Date"]

df_MA_month_values$year<-list_of_years
df_MA_month_values$month<-list_of_months
df_MA_month_values$month_number<-list_of_months_numbers
df_MA_month_values$day<-list_of_days
df_MA_month_values$value<-df_MA_average_daily_corrected$mean_daily_value

names(df_MA_month_values)<-c("date","year","month","month_number","day",variable_short_name)

#----------------------------------------------------------------------------------------------------------

# Here you save the dataframe with Monthly values
#----------------------------------------------------------------------------------------------------------
location_to_save_data<-paste(main_directory,"07-Results-files",sep="/")

mean_daily_time_series_file_name <- paste(paste("MMAM",variable_short_name,"time-series",area_name, sep ="-"),".csv",sep="")

saving_location <-paste(location_to_save_data,mean_daily_time_series_file_name,sep="/")

write.csv(df_MA_month_values,saving_location, row.names = FALSE)
#----------------------------------------------------------------------------------------------------------

# ()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()
#     ^    ^    ^    ^    ^    ^    ^    ^    ^    ^    ^    ^    ^    ^    ^    ^      Temporal  statistics 
# ()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
#                                                                                                 Heat plots
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# here you create a heat plot for a particular year
#----------------------------------------------------------------------------------------------------------

year_interest<-1979

temp_df_year<-filter(df_MA_month_values, year == year_interest)

# you plot a heat map of a particualr year 

#plot with text labels for months 
basic_heat_map<-ggplot(data = temp_df_year, aes(x=day,y=month)) + geom_tile(aes(fill=tmax)) + scale_fill_viridis()
print(basic_heat_map)

#plot with number labels for Y
ggplot(data = temp_df_year, aes(x = day,y = month_number)) + 
  geom_tile(aes(fill = tmax)) +
  scale_x_continuous(breaks=c(1:31), expand=c(0,0)) + coord_equal(ratio = 1) + 
  scale_fill_viridis(option="magma") + theme_tufte(base_family="Helvetica")
#----------------------------------------------------------------------------------------------------------

# here you create a heat plot for Multi-annual mean temperature:
#----------------------------------------------------------------------------------------------------------


# colour theme options viridis 
# inferno / magma / viridis /turbo /cividis 

# color theme options wesanderson
#names(wes_palettes)

pal <- wes_palette("Zissou1", 100, type = "continuous")

if (variable_short_name =="tmean"){
  
  first_year<-min(df_MA_month_values$year)
  last_year<-max(df_MA_month_values$year)
  
  base_plot<-ggplot(data = df_MA_month_values, aes(x = year,y = month_number )) + geom_tile(aes(fill = tmean))
  
  heat_plot<-base_plot + scale_x_continuous(breaks=c(first_year:last_year), expand=c(0,0))
  heat_plot<-heat_plot + coord_equal(ratio = 1)
  #heat_plot<-heat_plot + scale_fill_viridis(option="magma") 
  heat_plot<-heat_plot + scale_fill_gradientn(colours = pal)  
  heat_plot<-heat_plot + theme_tufte(base_family="sans")
  heat_plot<- heat_plot + theme(axis.title.y=element_blank(),axis.text.y=element_blank())  # for paper 
  
  print(heat_plot)
  
}


#----------------------------------------------------------------------------------------------------------

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
