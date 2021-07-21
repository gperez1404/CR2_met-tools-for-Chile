# Analysis of  CR2 datasets 

# Important !
# Before executing this Script you need to execute a script called "CR2_met_extraciton.R" 

# ========================================================================================================
#  Script Index
# ========================================================================================================

#  20-1498............MANDATORY To RUN THIS LINES
#  1500- 1600.........Spatial Statistics 1 raster
#           ......... Percentile analysis
#           ......... Generation of time series of mean values for a specific area  

# ========================================================================================================
#  Script Index
# ========================================================================================================


#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#
#                                                                                          Nested functions
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#

# load neccesary libraies:
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
  
  #loading the libraries for plots
  library(ggplot2)
  library(ggExtra)
  library(MASS)
  library(RColorBrewer)
  
  #loading libraries for animations one by one:
  
  library(gganimate)
  library(magick)
  library(animation)
  
  # Library for daily precipitation data analysis:
  
  library(hydroTSM)
  require(lattice)
  
  # Other libraries 
  libraries_to_load <- c("tmap","chron","lattice", "parallel","broom","magick", "lubridate")
  
  lapply(libraries_to_load , library, character.only = TRUE) 
  
  rm(libraries_to_load)
  
  # clean console 
  cat("\014")
}
# clean console 
cat("\014")

#----------------------------------------------------------------------------------------------------------

# This function return a string with the text containign a time difference:  
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

# This function return CR2 data index for an specific date:
#----------------------------------------------------------------------------------------------------------

# This function returns the index of a raster  associated with the date introduced as an input parameter

# input parameters:

#   date                   = text with the date you want to extract in the following format yyyy-mm-dd:
#   rasterbrick            = the rasterbrick element with all the rasters for the CR2 data

# Example of how to call the function
# index<-return_CR2_index("1994-10-10",rasterbrick) 

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
#clean console
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

# This function creates a rasterbrick element with  monthly  values for a specific month 
#----------------------------------------------------------------------------------------------------------

# This function uses as input a rasterbrick element containing  CR2_met daily data  

# This function is  inneficient. for CR2_met produt (13880 rasters) it can takes40 secondsper month

# good news are you just need to run this once per month

# input parameters:

# month_number                  = numeric value  that indicates the month for the analysis
#                                 


#  type_of_data                  = text with the initials or short name of the type of data you 
#                                   are analysing:
#                                                  "pr"
#                                                  "tmeam"
#                                                  "tmin"
#                                                  "tmax"

#   rasterbrick_with_daily_data   = the rasterbrick element with all the rasters for the different dates
#                                    this is assume be a daily rsterbrick element  

#   function outputs

#   rasterbrick_monthly_values  = A rasterbrick element with 1 raster associated to every year


# This function does not save any element 

generate_monthly_rasterbrick <- function(month_number,type_of_data,rasterbrick_with_daily_data) 
{
  # this variable control the execution time
  time_before_executing<- Sys.time()
  
  #this can change :
  date_origin<-"1978-12-31"
  
  #number of days in the input rasterbrick
  total_number_of_days_input_data<-nlayers(rasterbrick_with_daily_data)
  
  print("Creating monthly raster brick")
  print("please wait, this takes up to 1 minute...")
  
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
  
  # Here you generate a list of years
  first_year_text<-gsub("X","",strsplit(names(rasterbrick_with_daily_data[[1]]),
                                        ".",
                                        fixed=TRUE)[[1]][1])
  first_year<-as.numeric(first_year_text)
  
  last_year_text<-gsub("X","",strsplit(names(rasterbrick_with_daily_data[[nlayers(rasterbrick_with_daily_data)]]),
                                       ".",
                                       fixed=TRUE)[[1]][1])
  last_year<-as.numeric(last_year_text)
  
  #list of years
  list_of_years<- seq(first_year,last_year, by=1)
  
  # Here you initialise the ratserbrick with a random raster :
  # This first raster will be substitude
  random_raster_to_initialise_the_rasterbrick<-rasterbrick_with_daily_data[[1]]
  
  rasterbrick_monthly_values<-brick(random_raster_to_initialise_the_rasterbrick)
  
  if(type_of_data =="pr"){
    
    #  Main loop 
    #  This loop is over the years of the list with years:
    
    #number of years covered by the rasterbrick
    limit_main_loop<-length(list_of_years)
    
    #index for main loop
    current_year_index<-1
    
    while (current_year_index < limit_main_loop) { 
      
      # date to extract the first day of the month: 
      #                                             "yyy-mm-dd" 
      
      current_year_as_char<-as.character(list_of_years[current_year_index])
      
      date_text_first_day_current_year<-paste(current_year_as_char,
                                              month_number_as_char,
                                              "01",
                                              sep="-")
      
      current_day_index<-return_CR2_index(date_text_first_day_current_year,
                                          rasterbrick_with_daily_data)
      
      
      if(current_day_index <= nlayers(rasterbrick_with_daily_data)){
        
        current_day<-1
        first_raster_of_the_current_year<-rasterbrick_with_daily_data[[current_day_index]]
        monthly_raster_current_year<-first_raster_of_the_current_year
        
      }
      
      
      current_day_index<-current_day_index+1
      current_day<-current_day+1
      
      month_accumulated_succesfully<-FALSE
      
      # This loop helps to create 1 raster with a total sum value for 1 month
      
      while (month_accumulated_succesfully==FALSE && current_day <=  number_of_days_of_a_typical_month ) {
        
        
        if(current_day_index <= nlayers(rasterbrick_with_daily_data)) {
          
          raster_current_day<-rasterbrick_with_daily_data[[current_day_index]]
          monthly_raster_current_year<-monthly_raster_current_year+raster_current_day
        }  
        
        # progress printing on console
        current_day_as_char<-as.character(current_day)
        current_day_date_text<-paste(current_year_as_char,
                                     month_number_as_char,
                                     current_day_as_char,
                                     sep="-")
        
        
        #print(paste("current day:",current_day_date_text,sep=" "))
        #print(paste("current day index:",current_day_index, "out of",total_number_of_days_input_data, sep=" "))
        
        # loop advance:
        current_day_index<-current_day_index+1
        current_day=current_day+1
        
        if(current_day>number_of_days_of_a_typical_month){
          
          # if the month is February you need to take especial considerations for the leap years
          if(month_number_as_char=="02"){
            if (!is.na(match(list_of_years[current_year_index], list_of_leap_years))){
              # if you enter here is beacuse you have a leap year
              print("analysing leap year...")
              print(paste("current year:",current_year_as_char,sep=" "))
              Last_raster<-rasterbrick_with_daily_data[[current_day_index]]
              monthly_raster_current_year<-monthly_raster_current_year+Last_raster
            }
            
          }
          
          # here you add the  monthly value raster to the rasterbrick
          rasterbrick_monthly_values<-addLayer(rasterbrick_monthly_values,
                                               monthly_raster_current_year,
                                               current_year_index)
          current_year_index=current_year_index+1
          # Once you change to the next year you stop the accumulation for the monthly value
          month_accumulated_succesfully==TRUE
        }
        
      }
    }
  } else if(type_of_data=="tmean" | type_of_data=="tmin" | type_of_data=="tmax" ){
    print("the code for this type of data is not define yet")
  }
  
  names(rasterbrick_monthly_values)<-list_of_years
  print("rasterbrick with monthly values generated succesfully !!")
  
  elapsed_time<-time_diff(time_before_executing)
  print(paste("Total execution time [hh:mm:ss]",elapsed_time,sep=" "))
  
  return(rasterbrick_monthly_values)
  
}
# clean console
cat("\014")
#----------------------------------------------------------------------------------------------------------

# This funtion generates a vector of char/string/text values with the identifier values for a rasterbrick
#----------------------------------------------------------------------------------------------------------

# This vector will be use to populate the ID field of a dataframe

# input parameters:


#  type_of_data                  = text with the type of data you are analysing:
#                                                  "daily_CR2"
#                                                  "monthly_CR2"
#                                                  "annual_CR2"


# additional_info               = text with the additional info for identifying the ibs 
#                                 in case you have data of of monthly_CR2 or annual_CR2 types. 
#                                 Could be anything you want if you have "daily_CR2" 


# rasterbrick_to_index          = The rasterbrick element with the data you want to index

create_obs_identifier_vector<-function(type_of_data,
                                       additional_info,
                                       rasterbrick_to_index)
  
{
  
  # this variable control the execution time
  time_before_executing<- Sys.time()
  
  input_rasterbrick_date_origin<-"1978-12-31"
  
  #this lines controls that the main input value is valid
  if(!(type_of_data=="daily_CR2" | type_of_data=="monthly_CR2" | type_of_data=="annual_CR2")){
    stop("data type not supported. Change it to any of the followings: daily_CR2 | monthly_CR2  | annual_CR2")
  }
  
  # This variable will control the main loop 
  current_index<-1
  
  # this valriable controls the stop of main loop to fill up the id values vector
  number_of_rasters_inside_rasterbrick<-nlayers(rasterbrick_to_index)
  
  
  if(type_of_data=="daily_CR2"){
    
    # if the type_of_data  is  "daily_CR2"the id value contains a char/text/string 
    # with the date in the following format: 
    #                                       yyyy-mm-dd
    
    
    # this line creates a R date element using an index for CR2 data:
    current_obs_date<-as.Date(current_index, origin = input_rasterbrick_date_origin)
    current_obs_date_text<-as.character(current_obs_date)
    
    current_id_value<-current_obs_date_text
    
    # Here you initialise the vector:
    obs_id_vector<-c(current_id_value)
    print(paste("generating id value",current_index,"out of",number_of_rasters_inside_rasterbrick,sep=" "))
    current_index<-current_index+1
    
    #Main loop to fill up the vector with ids
    while(current_index<=number_of_rasters_inside_rasterbrick){
      
      # this line creates a R date element using an index for CR2 data:
      current_obs_date<-as.Date(current_index, origin = input_rasterbrick_date_origin)
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
    
    first_year<-1979
    last_year<-first_year+number_of_rasters_inside_rasterbrick-1
    
    #list of years
    list_of_years<- seq(first_year,last_year, by=1)
    
    current_year_text<-as.character(list_of_years[current_index])
    current_id_value<-paste(additional_info,current_year_text,sep="-")
    
    # Here you initialise the vector:
    obs_id_vector<-c(current_id_value)
    print(paste("generating id value",current_index,"out of",number_of_rasters_inside_rasterbrick,sep=" "))
    current_index<-current_index+1
    
    #Main loop to fill up the vector with ids
    while(current_index<=number_of_rasters_inside_rasterbrick){
      
      # this line creates a R date element using an index for CR2 data:
      current_year_text<-as.character(list_of_years[current_index])
      current_id_value<-paste(additional_info,current_year_text,sep="-")
      
      obs_id_vector<-c(obs_id_vector, current_id_value)
      print(paste("generating id value",current_index,"out of",number_of_rasters_inside_rasterbrick,sep=" "))
      current_index<-current_index+1  
    }
    
  } else if (type_of_data=="annual_CR2") {
    
    # if the type_of_data  is "annual_CR2" the id value contains a char/text/string 
    # with the year of the observation in the following format:
    #                                                          yyyy
    
    first_year<-1979
    last_year<-first_year+number_of_rasters_inside_rasterbrick-1
    
    #list of years
    list_of_years<- seq(first_year,last_year, by=1)
    
    current_year_text<-as.character(list_of_years[current_index])
    current_id_value<-current_year_text
    
    # Here you initialise the vector:
    obs_id_vector<-c(current_id_value)
    print(paste("generating id value",current_index,"out of",number_of_rasters_inside_rasterbrick,sep=" "))
    current_index<-current_index+1
    
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
  
  print("id vector generated succesfully !!")
  
  elapsed_time<-time_diff(time_before_executing)
  print(paste("Total execution time [hh:mm:ss]",elapsed_time,sep=" "))
  
  
  return(obs_id_vector)
}  
# clean console
cat("\014")




#----------------------------------------------------------------------------------------------------------

# This function return a dataframe with all the  spatial statsitics of a rasterbrick
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
    vector_day_values   <-c(NA)
    
  } else if (type_of_data=="annual_CR2") {
    
    # if the input rasterbrick contains annual data then the indentifier
    # itself is a char/text/string with the year of the observation in the 
    # following format: 
    #                  yyyy
    
    vector_year_values  <-c(obs_identifier_vector[1])
    vector_month_values <-c(NA)
    vector_day_values   <-c(NA)
    
  }
  
  print(paste("generating dataframe obs",current_extraction_index,"out of",number_of_rasters_inside_rasterbrick,sep=" "))
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
      vector_day_values   <-c(vector_day_values,NA)
      
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
    
    print(paste("generating dataframe obs",current_extraction_index,"out of",number_of_rasters_inside_rasterbrick,sep=" "))
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
  
  rm(text_with_id_current_obs)
  rm(type_of_data)
  rm(time_before_executing)
  rm(obs_identifier_vector)
  rm(vector_of_type_data)
  rm(vector_month_values)
  rm(vector_day_values)
  rm(vector_of_min_values)
  rm(vector_of_max_values)
  rm(vector_of_sum_values)
  rm(elapsed_time)
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
    variable_to_plot<-df_mean_monthly_values_interest_area_janaury$mean
    
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
    variable_to_plot<-df_mean_monthly_values_interest_area_february$mean
    
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
    variable_to_plot<-df_mean_monthly_values_interest_area_march$mean
    
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
    variable_to_plot<-df_mean_monthly_values_interest_area_april$mean
    
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
    variable_to_plot<-df_mean_monthly_values_interest_area_may$mean
    
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
    variable_to_plot<-df_mean_monthly_values_interest_area_june$mean
    
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
    variable_to_plot<-df_mean_monthly_values_interest_area_july$mean
    
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
    variable_to_plot<-df_mean_monthly_values_interest_area_august$mean
    
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
    variable_to_plot<-df_mean_monthly_values_interest_area_september$mean
    
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
    variable_to_plot<-df_mean_monthly_values_interest_area_october$mean
    
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
    variable_to_plot<-df_mean_monthly_values_interest_area_november$mean
    
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
    variable_to_plot<-df_mean_monthly_values_interest_area_december$mean
    
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

# This fucntion generates a list containing the index numbers of a  year 
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

# This function returns a  corrected time series in case tehre are errors in teh original CR2 met product
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

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#
# ^^     ^^     ^^     ^^     ^^     ^^     ^^     ^^     ^^     ^^     ^^     ^^          Nested functions
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#

#------------------------------------------------------------------------------------------------------------> start

# This function must be pre-compiled
load_libraries()

# Inputs: 

# this is teh variable you wnt to analyse : in this case precipitation
variable_short_name<-"pr"   # options: pr / tmin / tmax


# date to extract 1 day of data in the following format : "yyy-mm-dd" 
extraction_date_text<- "2015-03-25"


# this is the area you want to analyse (an especific river basin)
area_name<- "Copiapo-River-Basin"

raster_name_file <-'copiapo_comp.tif'

shape_file_name <- 'Copiapo_catchment_for_plots.shp'


# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!IMPORTANT
#                                     MANDATORY TO EXECUTE THIS SECTION
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!IMPORTANT

# It is absolutely neccesary to run these lines before doing any analysiis or calling any function !

#----------------------------------------------------------------------------------------------------------

#if your are running in the hard drive of your PC or laptop:
setwd("C:/00-C-GPM_INFO/04-C-RDM/04-C-01-R/04-C-01-02-CR2-DAP")

# If you wanna plot in a separated Window:
options(device = "windows")

# if you are running in the UQ-RDM server:
#setwd("R:/PHD042018-Q1021/09-R-CCRMC2019/09-R-06-Scripts/06-01-R/CR2-DAP")
main_directory<- getwd()

# Here you load the original CR2 dataset and extract the basic attributes:
loading_directory<-paste(main_directory,"04-Input-files", sep="/")
setwd(loading_directory)

if (variable_short_name=="pr") {
  ncdfname    <- "CR2MET_v1.4.2_pr.nc" 
  ncdf_element<- nc_open(ncdfname)
  variable_long_name<-as.character(ncatt_get( ncdf_element, "pr", "long_name") [2][1])
  variable_units<-as.character(ncatt_get( ncdf_element, "pr", "units")[2] [1])
  time_units<-as.character(ncatt_get( ncdf_element, "time", "units") [2] [1])
  time_origin_date<-strsplit(time_units,split=" ") [1] [[1]] [3]
} else if (variable_short_name=="tmin"){
  ncdfname    <- "CR2MET_v1.3_tmin.nc"
  ncdf_element<- nc_open(ncdfname)
  variable_long_name<-as.character(ncatt_get( ncdf_element, "tmin", "long_name") [2][1])
  variable_units<-as.character(ncatt_get( ncdf_element, "tmin", "units")[2] [1])
  time_units<-as.character(ncatt_get( ncdf_element, "time", "units") [2] [1])
  time_origin_date<-strsplit(time_units,split=" ") [1] [[1]] [3]
} else if (variable_short_name=="tmax"){
  ncdfname    <- "CR2MET_v1.3_tmax.nc"
  ncdf_element<- nc_open(ncdfname)
  variable_long_name<-as.character(ncatt_get( ncdf_element, "tmax", "long_name") [2][1])
  variable_units<-as.character(ncatt_get( ncdf_element, "tmax", "units")[2] [1])
  time_units<-as.character(ncatt_get( ncdf_element, "time", "units") [2] [1])
  time_origin_date<-strsplit(time_units,split=" ") [1] [[1]] [3]
}

rm(ncdf_element)

rasterbrick_with_all_chile <- brick(ncdfname, var=variable_short_name)

# Here you load the mask raster for the cahtment you want to analyze

loading_directory<-paste(main_directory,"05-Input-rasters", sep="/")
setwd(loading_directory)

loading_route<- paste(loading_directory,raster_name_file, sep="/")
mask_raster<-raster(loading_route)
mask_extent <- extent(mask_raster)

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! WARNING
# THIS TAKE SOME TIME TO RUN !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! WARNING

# Here you crop the rasterbrick with all chile to the interest area
cropped_brick<-crop(rasterbrick_with_all_chile,mask_extent)
extent(cropped_brick)<- mask_extent
rasterbrick <- mask(cropped_brick,mask_raster)

setwd(main_directory)

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! WARNING
# THIS TAKE SOME TIME TO RUN !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! WARNING


# this function returns the index of the data matching the search criteria 
extraction_day_index<-return_CR2_index(extraction_date_text, rasterbrick)

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

# Clean the consle
cat("\014")

#----------------------------------------------------------------------------------------------------------

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!IMPORTANT
#  ^   ^   ^   ^   ^   ^   ^   ^   ^  MANDATORY TO EXECUTE THIS SECTION   ^   ^   ^   ^   ^   ^   ^   ^   ^
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!IMPORTANT


#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
#                                                                         Spatial  statistics  for 1 raster 
#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

# Spatial statistics for just 1 raster using Cellstas() + hystogram of 1 day
#----------------------------------------------------------------------------------------------------------
# cellStats( raster, type of statistics)
#     ^
#     type of statistics can calculate for a raster:
#       'sum' 
#       'mean' 
#       'min' 
#       'max'
#       'sd' 
#       'skew' (skewness) and 
#       'rms' (Root Mean Square)

# Example:

raster_with_one_day_of_data<-rasterbrick[[extraction_day_index]]

print("mean:")
cellStats(raster_with_one_day_of_data, 'mean')
print("minn:")
cellStats(raster_with_one_day_of_data, 'min')
print("max:")
cellStats(raster_with_one_day_of_data, 'max')
print("sum:")
cellStats(raster_with_one_day_of_data, 'sum')

# raster's size (number of cells):
print(number_of_cells <- ncell(raster_with_one_day_of_data))

# raster's resolution (Degress):
res(raster_with_one_day_of_data)

# if you need the frequency distribution of the pixel values you can use the function freq()
# if you just want a simple one you sholud choose 0 decimal positions for the class breaks
freq_matrix_for_1_raster<-freq(raster_with_one_day_of_data, digits=0, value=NULL, useNA='no')

#if you want to plot the histogram of the pixwel values for 1 raster:
hist(raster_with_one_day_of_data)
#----------------------------------------------------------------------------------------------------------

# Spatial statistics using spatialpixeldataframe elements + plot of 1 day: 
#----------------------------------------------------------------------------------------------------------

# convert an individual raster into a "Pixeldataframe format"

# In the following line you convert the raster to spatial pixels dataframe
spdf_with_One_day_of_data <- as( raster_with_one_day_of_data,"SpatialPixelsDataFrame")

# with the following comand you cna have a summary of the basic statistics for the raster
summary(spdf_with_One_day_of_data)

# how to visually assess the spatialpixelsdataframe element:
plot(spdf_with_One_day_of_data)

# how to automatically generate the basic statistics for a spdf element ?

if (variable_short_name=="pr") {
  names(spdf_with_One_day_of_data)<-"pr"
  print(paste("mean value:", mean(spdf_with_One_day_of_data$pr, na.rm=TRUE), sep=" "))
  print(paste("min value:", min(spdf_with_One_day_of_data$pr, na.rm=TRUE), sep=" "))
  print(paste("max value:", max(spdf_with_One_day_of_data$pr, na.rm=TRUE), sep=" "))
  print(paste("sum value:", sum(spdf_with_One_day_of_data$pr, na.rm=TRUE), sep=" "))
} else if (variable_short_name=="tmin"){
  names(spdf_with_One_day_of_data)<-"tmin"
  print(paste("mean value:", mean(spdf_with_One_day_of_data$tmin, na.rm=TRUE), sep=" "))
  print(paste("min value:", min(spdf_with_One_day_of_data$tmin, na.rm=TRUE), sep=" "))
  print(paste("max value:", max(spdf_with_One_day_of_data$tmin, na.rm=TRUE), sep=" "))
  print(paste("sum value:", sum(spdf_with_One_day_of_data$tmin, na.rm=TRUE), sep=" "))
} else if (variable_short_name=="tmax"){
  names(spdf_with_One_day_of_data)<-"tmax"
  print(paste("mean value:", mean(spdf_with_One_day_of_data$tmax, na.rm=TRUE), sep=" "))
  print(paste("min value:", min(spdf_with_One_day_of_data$tmax, na.rm=TRUE), sep=" "))
  print(paste("max value:", max(spdf_with_One_day_of_data$tmax, na.rm=TRUE), sep=" "))
  print(paste("sum value:", sum(spdf_with_One_day_of_data$tmax, na.rm=TRUE), sep=" "))
}
#----------------------------------------------------------------------------------------------------------

# Spatial statistics using a predefined function:
#----------------------------------------------------------------------------------------------------------

# Here you use a predefined function to calcualte basic statistics for 1 raster:

# calculate_basic_statistics_for_1_date()

basic_stats_for_1_day<-calculate_basic_statistics_for_1_date(extraction_date_text,rasterbrick)

print(class(basic_stats_for_1_day))

# Visual assessment of results 
print("mean | min | max | sum")
print(basic_stats_for_1_day)

#----------------------------------------------------------------------------------------------------------

#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
#                                                                   End of Spatial statistics for 1 raster 
#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*


# ()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()
#                                                                                      Temporal  statistics 
#()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()

# Calcualtes mean value of the entire time series (all days on record)
raster_with_mean_values_per_pixel<- mean(rasterbrick)

# create  rasterbricks with monthly values 
#----------------------------------------------------------------------------------------------------------
# Here you create a rasterbrick with monthly values for a specific month:

# January
month_number<-1
rasterbrick_monthly_values_january<-generate_monthly_rasterbrick(month_number,variable_short_name,rasterbrick)

# february
month_number<-2
rasterbrick_monthly_values_february<-generate_monthly_rasterbrick(month_number,variable_short_name,rasterbrick)

# March
month_number<-3
rasterbrick_monthly_values_march<-generate_monthly_rasterbrick(month_number,variable_short_name,rasterbrick)

# April
month_number<-4
rasterbrick_monthly_values_april<-generate_monthly_rasterbrick(month_number,variable_short_name,rasterbrick)

# May
month_number<-5
rasterbrick_monthly_values_may<-generate_monthly_rasterbrick(month_number,variable_short_name,rasterbrick)

# June
month_number<-6
rasterbrick_monthly_values_june<-generate_monthly_rasterbrick(month_number,variable_short_name,rasterbrick)

# July
month_number<-7
rasterbrick_monthly_values_july<-generate_monthly_rasterbrick(month_number,variable_short_name,rasterbrick)

# August
month_number<-8
rasterbrick_monthly_values_august<-generate_monthly_rasterbrick(month_number,variable_short_name,rasterbrick)

# September
month_number<-9
rasterbrick_monthly_values_september<-generate_monthly_rasterbrick(month_number,variable_short_name,rasterbrick)

# October
month_number<-10
rasterbrick_monthly_values_october<-generate_monthly_rasterbrick(month_number,variable_short_name,rasterbrick)

# November
month_number<-11
rasterbrick_monthly_values_november<-generate_monthly_rasterbrick(month_number,variable_short_name,rasterbrick)

# December
month_number<-12
rasterbrick_monthly_values_december<-generate_monthly_rasterbrick(month_number,variable_short_name,rasterbrick)

#----------------------------------------------------------------------------------------------------------

# Visual assessment of monthly raster bricks:
#----------------------------------------------------------------------------------------------------------
plot(rasterbrick_monthly_values_january)
plot(rasterbrick_monthly_values_february)
plot(rasterbrick_monthly_values_march)
plot(rasterbrick_monthly_values_april)
plot(rasterbrick_monthly_values_may)
plot(rasterbrick_monthly_values_june)
plot(rasterbrick_monthly_values_july)
plot(rasterbrick_monthly_values_august)
plot(rasterbrick_monthly_values_september)
plot(rasterbrick_monthly_values_october)
plot(rasterbrick_monthly_values_november)
plot(rasterbrick_monthly_values_december)
#----------------------------------------------------------------------------------------------------------

# Create dataframes with all spatial statistics for each month:
#----------------------------------------------------------------------------------------------------------

data_series_type<-"monthly_CR2"

# Generate dataframe for january

additional_info<-"january"

obs_identifier_vector<-create_obs_identifier_vector(data_series_type,
                                                    additional_info,
                                                    rasterbrick_monthly_values_january)

df_mean_monthly_values_interest_area_janaury<-generate_dataframe_with_spatial_stats(data_series_type,
                                                                                    obs_identifier_vector,
                                                                                    area_name,
                                                                                    variable_short_name,
                                                                                    rasterbrick_monthly_values_january)


# Generate dataframe for February

additional_info<-"february"

obs_identifier_vector<-create_obs_identifier_vector(data_series_type,
                                                    additional_info,
                                                    rasterbrick_monthly_values_february)

df_mean_monthly_values_interest_area_february<-generate_dataframe_with_spatial_stats(data_series_type,
                                                                                     obs_identifier_vector,
                                                                                     area_name,
                                                                                     variable_short_name,
                                                                                     rasterbrick_monthly_values_february)

# Generate dataframe for March

additional_info<-"march"

obs_identifier_vector<-create_obs_identifier_vector(data_series_type,
                                                    additional_info,
                                                    rasterbrick_monthly_values_march)

df_mean_monthly_values_interest_area_march<-generate_dataframe_with_spatial_stats(data_series_type,
                                                                                  obs_identifier_vector,
                                                                                  area_name,
                                                                                  variable_short_name,
                                                                                  rasterbrick_monthly_values_march)


# Generate dataframe for April

additional_info<-"april"

obs_identifier_vector<-create_obs_identifier_vector(data_series_type,
                                                    additional_info,
                                                    rasterbrick_monthly_values_april)

df_mean_monthly_values_interest_area_april<-generate_dataframe_with_spatial_stats(data_series_type,
                                                                                  obs_identifier_vector,
                                                                                  area_name,
                                                                                  variable_short_name,
                                                                                  rasterbrick_monthly_values_april)

# Generate dataframe for May

additional_info<-"may"

obs_identifier_vector<-create_obs_identifier_vector(data_series_type,
                                                    additional_info,
                                                    rasterbrick_monthly_values_may)

df_mean_monthly_values_interest_area_may<-generate_dataframe_with_spatial_stats(data_series_type,
                                                                                obs_identifier_vector,
                                                                                area_name,
                                                                                variable_short_name,
                                                                                rasterbrick_monthly_values_may)


# Generate dataframe for June

additional_info<-"june"

obs_identifier_vector<-create_obs_identifier_vector(data_series_type,
                                                    additional_info,
                                                    rasterbrick_monthly_values_june)

df_mean_monthly_values_interest_area_june<-generate_dataframe_with_spatial_stats(data_series_type,
                                                                                 obs_identifier_vector,
                                                                                 area_name,
                                                                                 variable_short_name,
                                                                                 rasterbrick_monthly_values_june)

# Generate dataframe for July

additional_info<-"july"

obs_identifier_vector<-create_obs_identifier_vector(data_series_type,
                                                    additional_info,
                                                    rasterbrick_monthly_values_july)

df_mean_monthly_values_interest_area_july<-generate_dataframe_with_spatial_stats(data_series_type,
                                                                                 obs_identifier_vector,
                                                                                 area_name,
                                                                                 variable_short_name,
                                                                                 rasterbrick_monthly_values_july)



# Generate dataframe for August

additional_info<-"august"

obs_identifier_vector<-create_obs_identifier_vector(data_series_type,
                                                    additional_info,
                                                    rasterbrick_monthly_values_august)

df_mean_monthly_values_interest_area_august<-generate_dataframe_with_spatial_stats(data_series_type,
                                                                                   obs_identifier_vector,
                                                                                   area_name,
                                                                                   variable_short_name,
                                                                                   rasterbrick_monthly_values_august)



# Generate dataframe for September

additional_info<-"september"

obs_identifier_vector<-create_obs_identifier_vector(data_series_type,
                                                    additional_info,
                                                    rasterbrick_monthly_values_september)

df_mean_monthly_values_interest_area_september<-generate_dataframe_with_spatial_stats(data_series_type,
                                                                                      obs_identifier_vector,
                                                                                      area_name,
                                                                                      variable_short_name,
                                                                                      rasterbrick_monthly_values_september)


# Generate dataframe for October

additional_info<-"october"

obs_identifier_vector<-create_obs_identifier_vector(data_series_type,
                                                    additional_info,
                                                    rasterbrick_monthly_values_october)

df_mean_monthly_values_interest_area_october<-generate_dataframe_with_spatial_stats(data_series_type,
                                                                                    obs_identifier_vector,
                                                                                    area_name,
                                                                                    variable_short_name,
                                                                                    rasterbrick_monthly_values_october)

# Generate dataframe for november

additional_info<-"november"

obs_identifier_vector<-create_obs_identifier_vector(data_series_type,
                                                    additional_info,
                                                    rasterbrick_monthly_values_november)

df_mean_monthly_values_interest_area_november<-generate_dataframe_with_spatial_stats(data_series_type,
                                                                                     obs_identifier_vector,
                                                                                     area_name,
                                                                                     variable_short_name,
                                                                                     rasterbrick_monthly_values_november)

# Generate dataframe for december

additional_info<-"december"

obs_identifier_vector<-create_obs_identifier_vector(data_series_type,
                                                    additional_info,
                                                    rasterbrick_monthly_values_december)

df_mean_monthly_values_interest_area_december<-generate_dataframe_with_spatial_stats(data_series_type,
                                                                                     obs_identifier_vector,
                                                                                     area_name,
                                                                                     variable_short_name,
                                                                                     rasterbrick_monthly_values_december)

#----------------------------------------------------------------------------------------------------------

# Visual assesment of dfs with monthly statistics:
#----------------------------------------------------------------------------------------------------------

View(df_mean_monthly_values_interest_area_janaury)
View(df_mean_monthly_values_interest_area_february)
View(df_mean_monthly_values_interest_area_march)
View(df_mean_monthly_values_interest_area_april)
View(df_mean_monthly_values_interest_area_may)
View(df_mean_monthly_values_interest_area_june)
View(df_mean_monthly_values_interest_area_july)
View(df_mean_monthly_values_interest_area_august)
View(df_mean_monthly_values_interest_area_september)
View(df_mean_monthly_values_interest_area_october)
View(df_mean_monthly_values_interest_area_november)
View(df_mean_monthly_values_interest_area_december)

#----------------------------------------------------------------------------------------------------------

# Here you merge all the dataframes with  the spatial statistics for all the months into 1 df !
#----------------------------------------------------------------------------------------------------------
# https://medium.com/coinmonks/merging-multiple-dataframes-in-r-72629c4632a3

df_monthly_pr_statistics<-do.call("rbind", list(df_mean_monthly_values_interest_area_janaury,
                                                df_mean_monthly_values_interest_area_february,
                                                df_mean_monthly_values_interest_area_march,
                                                df_mean_monthly_values_interest_area_april,
                                                df_mean_monthly_values_interest_area_may,
                                                df_mean_monthly_values_interest_area_june,
                                                df_mean_monthly_values_interest_area_july,
                                                df_mean_monthly_values_interest_area_august,
                                                df_mean_monthly_values_interest_area_september,
                                                df_mean_monthly_values_interest_area_october,
                                                df_mean_monthly_values_interest_area_november,
                                                df_mean_monthly_values_interest_area_december))

# Visual assesment of data as a table:
View(df_monthly_pr_statistics)

#Exploratory analysis of cummulative monthly pr rasters 

# This display plots in a different window
options(device = "windows")

# Box-whisker plot for mean pr values

bp_monthly_values<-boxplot(mean~month,
                           data=df_monthly_pr_statistics,
                           main=paste("Multi-annual monthly precipitation for",area_name,"(1979-2016)",sep=" "),
                           xlab="Month Number",
                           ylab=" monthly precipitation (accumulated) [mm]",
                           col="blue",
                           border="black")

# If you want to add labels to the boxes

# create the labels with min values
table_lables_min<-table(df_monthly_pr_statistics$month)
table_lables_min[1]<-round(min(df_mean_monthly_values_interest_area_janaury$mean),2)
table_lables_min[2]<-round(min(df_mean_monthly_values_interest_area_february$mean),2)
table_lables_min[3]<-round(min(df_mean_monthly_values_interest_area_march$mean),2)
table_lables_min[4]<-round(min(df_mean_monthly_values_interest_area_april$mean),2)
table_lables_min[5]<-round(min(df_mean_monthly_values_interest_area_may$mean),2)
table_lables_min[6]<-round(min(df_mean_monthly_values_interest_area_june$mean),2)
table_lables_min[7]<-round(min(df_mean_monthly_values_interest_area_july$mean),2)
table_lables_min[8]<-round(min(df_mean_monthly_values_interest_area_august$mean),2)
table_lables_min[9]<-round(min(df_mean_monthly_values_interest_area_september$mean),2)
table_lables_min[10]<-round(min(df_mean_monthly_values_interest_area_october$mean),2)
table_lables_min[11]<-round(min(df_mean_monthly_values_interest_area_november$mean),2)
table_lables_min[12]<-round(min(df_mean_monthly_values_interest_area_december$mean),2)

# create the labels with mean values
table_lables_mean<-table(df_monthly_pr_statistics$month)
table_lables_mean[1]<-round(mean(df_mean_monthly_values_interest_area_janaury$mean),2)
table_lables_mean[2]<-round(mean(df_mean_monthly_values_interest_area_february$mean),2)
table_lables_mean[3]<-round(mean(df_mean_monthly_values_interest_area_march$mean),2)
table_lables_mean[4]<-round(mean(df_mean_monthly_values_interest_area_april$mean),2)
table_lables_mean[5]<-round(mean(df_mean_monthly_values_interest_area_may$mean),2)
table_lables_mean[6]<-round(mean(df_mean_monthly_values_interest_area_june$mean),2)
table_lables_mean[7]<-round(mean(df_mean_monthly_values_interest_area_july$mean),2)
table_lables_mean[8]<-round(mean(df_mean_monthly_values_interest_area_august$mean),2)
table_lables_mean[9]<-round(mean(df_mean_monthly_values_interest_area_september$mean),2)
table_lables_mean[10]<-round(mean(df_mean_monthly_values_interest_area_october$mean),2)
table_lables_mean[11]<-round(mean(df_mean_monthly_values_interest_area_november$mean),2)
table_lables_mean[12]<-round(mean(df_mean_monthly_values_interest_area_december$mean),2)

# create the labels with max values
table_lables_max<-table(df_monthly_pr_statistics$month)
table_lables_max[1]<-round(max(df_mean_monthly_values_interest_area_janaury$mean),2)
table_lables_max[2]<-round(max(df_mean_monthly_values_interest_area_february$mean),2)
table_lables_max[3]<-round(max(df_mean_monthly_values_interest_area_march$mean),2)
table_lables_max[4]<-round(max(df_mean_monthly_values_interest_area_april$mean),2)
table_lables_max[5]<-round(max(df_mean_monthly_values_interest_area_may$mean),2)
table_lables_max[6]<-round(max(df_mean_monthly_values_interest_area_june$mean),2)
table_lables_max[7]<-round(max(df_mean_monthly_values_interest_area_july$mean),2)
table_lables_max[8]<-round(max(df_mean_monthly_values_interest_area_august$mean),2)
table_lables_max[9]<-round(max(df_mean_monthly_values_interest_area_september$mean),2)
table_lables_max[10]<-round(max(df_mean_monthly_values_interest_area_october$mean),2)
table_lables_max[11]<-round(max(df_mean_monthly_values_interest_area_november$mean),2)
table_lables_max[12]<-round(max(df_mean_monthly_values_interest_area_december$mean),2)

# add labels with min values 
text( 
  x=c(1:12), 
  y=bp_monthly_values$stats[nrow(bp_monthly_values$stats),] + 10.5, 
  paste("min value= ",table_lables_min,sep="")  
)

# add labels with mean values 
text( 
  x=c(1:12), 
  y=bp_monthly_values$stats[nrow(bp_monthly_values$stats),] + 12.5, 
  paste("mean value= ",table_lables_mean,sep="")  
)

# add labels with max values 
text( 
  x=c(1:12), 
  y=bp_monthly_values$stats[nrow(bp_monthly_values$stats),] + 14.5, 
  paste("max value= ",table_lables_max,sep="")  
)
#----------------------------------------------------------------------------------------------------------

# Make 1 individual box-plot with frequency distribution for 1 particular month:              
#----------------------------------------------------------------------------------------------------------

month_to_plot<-12

plot_monthly_boxplot_and_freq_distri(month_to_plot)
#----------------------------------------------------------------------------------------------------------

# Here you save the time series
#----------------------------------------------------------------------------------------------------------
  
location_to_save_data<-paste(main_directory,"07-Results-files",sep="/")

mean_pr_monthly_time_series_file_name <- paste(paste("mean-monthly-pr-df",area_name, sep ="-"),".csv",sep="")

saving_location <-paste(location_to_save_data,mean_pr_monthly_time_series_file_name,sep="/")

write.csv(df_monthly_pr_statistics,saving_location, row.names = FALSE)

#----------------------------------------------------------------------------------------------------------

# Here you create a rasterbrick with annual values:
#----------------------------------------------------------------------------------------------------------

# Here you generate a list of years for the annual rasterbrick
first_year_text<-gsub("X","",strsplit(names(rasterbrick[[1]]),".",fixed=TRUE)[[1]][1])
first_year<-as.numeric(first_year_text)

last_year_text<-gsub("X","",strsplit(names(rasterbrick[[nlayers(rasterbrick)]]),".",fixed=TRUE)[[1]][1])
last_year<-as.numeric(last_year_text)

#list of years
list_of_years<- seq(first_year,last_year, by=1)

# Number of years in the original rasterbrick:
number_of_years<-length(list_of_years)

# Here you initialize the rasterbrcik before filling it with the annual rasters using a loop
rasterbrick_annual_values<-brick()
year_assign_index<-1

# this variable controls the execution time
time_before_executing<- Sys.time()

for (current_year in list_of_years) {
  
  indices_for_the_current_year<- generate_list_of_indices_for_a_year(current_year,rasterbrick)
  
  rasterbrick_subset<-subset(rasterbrick,indices_for_the_current_year, drop=TRUE)
  
  # if you run the sum() function on a rasterbick you will add (non-cumulatively) all the rasters
  current_year_annual_raster <-sum(rasterbrick_subset) 
  
  rasterbrick_annual_values<-addLayer(rasterbrick_annual_values,
                                      current_year_annual_raster,
                                      year_assign_index)
  
  year_assign_index<-year_assign_index+1
  
  print(paste("current year:",current_year,"year number",year_assign_index,"out of",number_of_years, sep=" "))
  elapsed_time<-time_diff(time_before_executing)
  print(paste("Elapsed time since start [hh:mm:ss]",elapsed_time,sep=" "))
  
}

names(rasterbrick_annual_values)<-list_of_years
print("Annual values raster generated succesfully !!")
elapsed_time<-time_diff(time_before_executing)
print(paste("Execution time [hh:mm:ss]",elapsed_time,sep=" "))

#----------------------------------------------------------------------------------------------------------

# Here you create a rasterbrick of annual series of maximun dialy pr values:
#----------------------------------------------------------------------------------------------------------

# Here you generate a list of years for the annual rasterbrick
first_year_text<-gsub("X","",strsplit(names(rasterbrick[[1]]),".",fixed=TRUE)[[1]][1])
first_year<-as.numeric(first_year_text)

last_year_text<-gsub("X","",strsplit(names(rasterbrick[[nlayers(rasterbrick)]]),".",fixed=TRUE)[[1]][1])
last_year<-as.numeric(last_year_text)

#list of years
list_of_years<- seq(first_year,last_year, by=1)

# Number of years in the original rasterbrick:
number_of_years<-length(list_of_years)

# Here you initialize the rasterbrcik before filling it with the annual rasters using a loop
rasterbrick_max_annual_values<-brick()
year_assign_index<-1

# this variable control the execution time
time_before_executing<- Sys.time()

for (current_year in list_of_years) {
  
  indices_for_the_current_year<- generate_list_of_indices_for_a_year(current_year,rasterbrick)
  
  rasterbrick_subset<-subset(rasterbrick,indices_for_the_current_year, drop=TRUE)
  
  # if you run the sum() function on a rasterbick you will add (non-cumulatively) all the rasters
  current_year_annual_raster <-max(rasterbrick_subset) 
  
  rasterbrick_max_annual_values<-addLayer(rasterbrick_max_annual_values,
                                          current_year_annual_raster,
                                          year_assign_index)
  
  year_assign_index<-year_assign_index+1
  
  print(paste("current year:",current_year,"year number",year_assign_index,"out of",number_of_years, sep=" "))
  elapsed_time<-time_diff(time_before_executing)
  print(paste("Elapsed time since start [hh:mm:ss]",elapsed_time,sep=" "))
  
}

names(rasterbrick_max_annual_values)<-list_of_years
print("Rasterbirck with maximun annual rasters generated succesfully !!")
elapsed_time<-time_diff(time_before_executing)
print(paste("Execution time [hh:mm:ss]",elapsed_time,sep=" "))


#----------------------------------------------------------------------------------------------------------

# Here you load the  df with the time series of mean daily values (if already exisits !)
#----------------------------------------------------------------------------------------------------------

name_of_loading_folder<-"04-Input-files"
loading_directory<-paste(main_directory,name_of_loading_folder, sep="/")

df_mean_daily_pr_file_name<-paste("mean-daily-pr-time-series-",area_name,".csv",sep="")

file_path<-paste(loading_directory,df_mean_daily_pr_file_name,sep="/")

View(time_series_mean_raster)

#----------------------------------------------------------------------------------------------------------

# Here you create a time series of mean values for an interest area:
#----------------------------------------------------------------------------------------------------------

# The objective of the following lines is to generate a 
# data frame with the timeseries of mean daily  CR2 values 

#  WARNING ! This can take up to 15 minutes

# in the following line you initialise a dataframe to allocate the spatial mean pr values for 
# each day

df_mean_daily_pr <- data.frame("Date" = as.Date(1:dim(rasterbrick)[3],origin = "1978-12-31"),
                               "mean_daily_pr"=as.double(0))
start<-1
limit<-dim(rasterbrick)[3]

# Main loop:
time_before_executing<- Sys.time()
for (i in start:limit) { 
  date_i<-as.Date(i, origin = "1978-12-31")
  raster_current_day<-rasterbrick[[i]]
  
  # In the following line you convert the raster to spatial pixels dataframe
  spdf_current_day <- as( raster_current_day,"SpatialPixelsDataFrame")
  names(spdf_current_day)<-"pr"
  mean_current_day<- mean(spdf_current_day$pr, na.rm=TRUE)
  
  df_mean_daily_pr[i,"mean_daily_pr"]<-mean_current_day
  print(paste("day",i,"out of",limit,sep=" "))
  elapsed_time<-time_diff(time_before_executing)
  print(paste("Elapsed time [hh:mm:ss]",elapsed_time,sep=" "))
}
elapsed_time<-time_diff(time_before_executing)
print("Data frame with daily values generated successfully !!")
print(paste("Total elapsed time [hh:mm:ss]",elapsed_time,sep=" "))


# visual inspection of data:

plot(df_mean_daily_pr$mean_daily_pr)
print(max(df_mean_daily_pr$mean_daily_pr))
print(min(df_mean_daily_pr$mean_daily_pr))
#View(df_mean_daily_pr)

if(max(df_mean_daily_pr$mean_daily_pr)>=300){
  Do_you_need_to_correct_data<-"Yes"
}

df_mean_daily_pr_corrected<-df_mean_daily_pr
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
  df_mean_daily_pr_corrected<-fix_CR2_df(error_threshold,df_mean_daily_pr)
}
  
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! WARNING 
#  WARNING: DO NOT RUN THIS WITHOUT INSPECTING THE TIME SERIES FIRST !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! WARNING

# Here you save the time series: 

location_to_save_data<-paste(main_directory,"07-Results-files",sep="/")

mean_daily_time_series_file_name <- paste(paste("mean-daily-pr-time-series",area_name, sep ="-"),".csv",sep="")

saving_location <-paste(location_to_save_data,mean_daily_time_series_file_name,sep="/")

write.csv(df_mean_daily_pr_corrected,saving_location, row.names = FALSE)

#----------------------------------------------------------------------------------------------------------

# ()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()
#                                                                                        Temporal  statistics 
#()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()


#:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|
#:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|  Precipitation data analysis with hydroTSM :|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|
#:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|


# more info about this package :

# https://cran.r-project.org/web/packages/hydroTSM/hydroTSM.pdf 
# https://cran.r-project.org/web/packages/hydroTSM/vignettes/hydroTSM_Vignette-knitr.pdf


# This library uses "zoo" elements

# input to this section : df_mean_daily_pr_corrected

#----------------------------------------------------------------------------------------------------------

# Here you convert data.frame object with the time series  into a zoo element (zoo package):
x <- read.zoo(df_mean_daily_pr_corrected)
View(x)

# Basic Exploratory Data Analysis

nyears <- yip(from=start(x), to=end(x), out.type="nmbr")
dates <- time(x)
smry(x)

# Hydroplot 
hydroplot(x, var.type="Precipitation", main=area_name, pfreq = "dm", from=as.Date(1,origin = "1979-12-31"))

# Amount of days with information (not NA) per year
dwi(x)

# Amount of days with information (not NA) per month per year
dwi(x, out.unit="mpy")

# Monthly precipitation analysis
m <- daily2monthly(x, FUN=sum, na.rm=TRUE)

M <- matrix(m, ncol=12, byrow=TRUE)
colnames(M) <- month.abb
rownames(M) <- unique(format(time(m), "%Y"))

# Vissual analysis of monthly precipitation values
print(matrixplot(M,ColorRamp="Precipitation",main="Monthly precipitation [mm/month]"))

# Medians of the monthly values for each month
medians<-monthlyfunction(m, FUN=median, na.rm=TRUE)

# Vector with the three-letter abbreviations for the month names
cmonth <- format(time(m), "%b")

# Creating ordered monthly factors
months <- factor(cmonth, levels=unique(cmonth), ordered=TRUE)

boxplot( coredata(m) ~ months, col="lightblue", main="Monthly Precipitation",ylab="Precipitation, [mm]", xlab="Month")

# Extreme Precipitation Indices (EPI) for daily pr data

#  Heavy Precipitation Days (R10mm)
#  Counting and plotting the number of days in the period where precipitation is >= 10 [mm]
R10mm <- length( x[x>10] )

# Very Wet Days (R95p)
# Subgroup of values with daily precipitation >= 1 [mm]
wet.index <- which(x >= 1)

# clean console
cat("\014")

number.of.very.wet.days<-length(wet.index)

# 95th percentile of precipitation on wet days (PRwn95)
PRwn95 <- quantile(x[wet.index], probs=0.95, na.rm=TRUE) 

print("Extreme precipitation indeces")
print(paste("Number of days with heavy precipitation: ",R10mm))
print(paste("Number of very wet days: ", number.of.very.wet.days))
print(" 95th percentile of precipitation on very wet days [mm/day]: ")
PRwn95[1]

#----------------------------------------------------------------------------------------------------------


#:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|
#:|:|:|:|:|:|:|:|:|:|:|:|  Precipitation data analysis with hydroTSM :|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|
#:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|:|

