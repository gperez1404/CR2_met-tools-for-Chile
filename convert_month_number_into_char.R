# This function converts a string with a month name value in lowercase into a month number [1-12]

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
