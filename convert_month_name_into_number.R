# This function converts a string with a month name value in lowercase into a month number [1-12]

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

