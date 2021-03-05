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
  
  print(paste("The index for the requested date is:",index))
  return(index)
  
  rm(name_for_index_search_part1)
  rm(string)
  rm(name_for_index_search_part2)
  rm(string_with_name_to_find_index)
  rm(index)
}
#clean console
cat("\014")
