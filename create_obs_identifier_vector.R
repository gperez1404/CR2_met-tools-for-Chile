# This funtion generates a vector of char/string/text values with the identifier values for a rasterbrick
# element.

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
