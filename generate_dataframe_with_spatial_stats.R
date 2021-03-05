# This function generates a dataframe with all the  spatial statsitics of a rasterbrick

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

