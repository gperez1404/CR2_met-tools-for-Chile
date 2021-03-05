# This fucntion generates a list containing the index numbers of a specified year 

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
