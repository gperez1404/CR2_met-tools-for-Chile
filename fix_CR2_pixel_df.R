# This function returns a dataframe element with the time series of corrected values

# input parameters:

#   pixel_df         = the dataframe with the pixel time series


# the NO data values should be NA but R convert those values to 300 or 32000 and I dunno why ?

# Example of how to call the function:
#     fix_CR2_pixel_df() 

fix_CR2_pixel_df <- function(pixel_index,pixel_df) 
{
  print("start of pixel dataframe fixing:")
  
  start_index<-1
  end_index<-dim(pixel_df)[1]
  
  for (i in start_index:end_index) 
  { 
      current_value<-pixel_df[i,"var"]
    
      if (current_value>299) 
      {
        pixel_df[i,"var"]<-NA
      }
      print(paste("fixing dataframe NAs for pixel number",pixel_index,";value",i,"out of",end_index,sep=" "))
  }
  print("Pixel dataframe fixed succesfully !")
  
  return(pixel_df)
  
}
#clean console
cat("\014")

