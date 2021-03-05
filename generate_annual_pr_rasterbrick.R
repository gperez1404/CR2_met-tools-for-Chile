# This function creates a rasterbrick element with the annual precipitation rasters using as an input
# a rasterbrick containing  CR2_met daily prcipitation data

# This function is extremely inneficient  !!

# input parameters:

#   rasterbrick_with_daily_data   = the rasterbrick element with all the rasters for the different dates
#                                    this is assume be a daily rsterbrick element  

#   function outputs

#   rasterbrick_annual_values  = A rasterbrick element with 1 raster associated to the total pr value
#                                for every year


# this function does not save any element 

generate_annual_pr_rasterbrick <- function(rasterbrick_with_daily_data) 
{
  # this variable control the execution time
  time_before_executing<- Sys.time()
  
  date_origin<-"1978-12-31"
  
  #number of dates in the input rasterbrick
  limit<-nlayers(rasterbrick_with_daily_data)
  
  # Here you generate a list of years for the annual rasterbrick

  first_year_text<-gsub("X","",strsplit(names(rasterbrick_with_daily_data[[1]]),".",fixed=TRUE)[[1]][1])
  first_year<-as.numeric(first_year_text)
  
  last_year_text<-gsub("X","",strsplit(names(rasterbrick_with_daily_data[[limit]]),".",fixed=TRUE)[[1]][1])
  last_year<-as.numeric(last_year_text)
  
  #list of years
  list_of_years<- seq(first_year,last_year, by=1)

  #  Main loop 
  # Thi loop is over all the elements of the input rasterbrick:
  i<-1
  current_year_index<-1

  first_raster_of_the_first_year<-rasterbrick_with_daily_data[[i]]
  rasterbrick_annual_values<-brick(first_raster_of_the_first_year)
  
  while (i < limit) { 
    
    date_first_raster_current_year<-as.Date(i, origin =date_origin)
    date_text_first_raster_current_year<-as.character(date_first_raster_current_year)
    
    list_with_date_components_currnet_year<-strsplit(date_text_first_raster_current_year,"-")
    current_year<- as.numeric(list_with_date_components_currnet_year[[1]][1])
    
    if(i <= nlayers(rasterbrick_with_daily_data)){
    first_raster_of_the_current_year<-rasterbrick_with_daily_data[[i]]
    }
    
    annual_cumm_raster_current_year<-first_raster_of_the_current_year
    
    # This loop is to accumualte the current year
    i=i+1
    year_accumulated_succesfully<-FALSE
    
    while (year_accumulated_succesfully==FALSE && i <= nlayers(rasterbrick_with_daily_data)) {
      
      date_i<-as.Date(i, origin = date_origin)
      date_text_i<-as.character(date_i)
      
      list_with_date_components_i<-strsplit(date_text_i,"-")
      year_i<- as.numeric(list_with_date_components_i[[1]][1])
      
      if(i <= nlayers(rasterbrick_with_daily_data)) {
        raster_dayi<-rasterbrick_with_daily_data[[i]]  
      }
      
      annual_cumm_raster_current_year<-annual_cumm_raster_current_year+raster_dayi
      
      i=i+1
      date_nexti<-as.Date(i, origin = date_origin)
      date_text_nexti<-as.character(date_nexti)
      list_with_date_components_date_nexti<-strsplit(date_text_nexti,"-")
      year_nexti<- as.numeric(list_with_date_components_date_nexti[[1]][1])
    
      # Once you change to the next year you stop the accumulation
      if(year_nexti>current_year){
        rasterbrick_annual_values<-addLayer(rasterbrick_annual_values,
                                            annual_cumm_raster_current_year,
                                            current_year_index)
        current_year_index=current_year_index+1
        year_accumulated_succesfully==TRUE
      }
      
      print(paste("current day:",i, "out of",limit, sep=" "))
      elapsed_time<-time_diff(time_before_executing)
      print(paste("Elapsed time since start [hh:mm:ss]",elapsed_time,sep=" "))
      
    }
  }
  elapsed_time<-time_diff(time_before_executing)
  print(paste("Execution time [hh:mm:ss]",elapsed_time,sep=" "))
  
  return(rasterbrick_annual_values)
}
# clean console
cat("\014")
