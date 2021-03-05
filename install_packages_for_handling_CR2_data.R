# This function install all the neccesary packages 
# To execute this function you need internet conection

# this fucntion doesn't have any outputs

# this function does not require any inputs

install_packages_for_handling_CR2_data<- function()
{
  # The following code is used to Download the required packages 
  
  packages_to_install <- c( "ggplot2", "ggmap", "rgdal", "rgeos", "maptools", "dplyr", "tidyr",
                            "tmap","tmaptools","OpenStreetMap","Rcpp","rJava","rlang","osmar",
                            "chron", "ncdf4","sf","tiff","tidyverse","resample",'broom',"magick",
                            "gganimate","devtools","spData","ggExtra","animation")
  
  
  # You just need to do this once
  # warning: Executing the following loop may take several minutes and you need to make sure you have internet connection\
  
  i=1
  while (i <= length(packages_to_install)) {
    
    if (!require(packages_to_install[i])){
      install.packages(packages_to_install[i])
    }
    i=i+1
  }

}
# clean console 
cat("\014")

