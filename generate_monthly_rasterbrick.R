# This function creates a rasterbrick element with the monthly  values for a specific month 
# using as input a rasterbrick element containing  CR2_met daily data  

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
      
      print(paste("current year:",current_year_as_char,sep=" "))
      
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
        print(paste("current day:",current_day_date_text,sep=" "))
        print(paste("current day index:",current_day_index, "out of",total_number_of_days_input_data, sep=" "))
        elapsed_time<-time_diff(time_before_executing)
        print(paste("Elapsed time since start [hh:mm:ss]",elapsed_time,sep=" "))
        
        
        # loop advance:
        current_day_index<-current_day_index+1
        current_day=current_day+1
        
        if(current_day>number_of_days_of_a_typical_month){
          
          # if the month is February you need to take especial considerations for the leap years
          if(month_number_as_char=="02"){
            
            if (!is.na(match(list_of_years[current_year_index], list_of_leap_years))){
            # if you enter here is beacuse you have a leap year
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

