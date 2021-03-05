# this function converts a string with a date time value in the following format :

# Inputs:
# yyyy-mm-dd

# into a date value accoridnt to the ISO date standart 

# this function returns a 'date' element

day_date_char_to_date_element<- function(input_day_date_string)
{
  
  
  list_with_date_components<- strsplit(input_day_date_string,"-")
  
  year_number<- as.numeric(list_with_date_components[[1]][1])
  month_number<- as.numeric(list_with_date_components[[1]][2])
  day_number<- as.numeric(list_with_date_components[[1]][3])
  
  input_date_time<- ISOdate(year_number,month_number,day_number)
  
  return(input_date_time)
}
# clean console 
cat("\014")

