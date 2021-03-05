# This function loads all the linraries required for executing CR2 data extraction or data analysis


# exaple of how to call the fucntion
# load_libraries()

load_libraries<- function()
{
  
  # loading basic libraries 
  
  libraries_to_load <- c("stringr","dplyr","ggplot2", "grid","gridExtra","MASS", "devtools", "tmap", "sp",
                         "rgdal","raster","osmar","chron","RColorBrewer","lattice","ncdf4",
                         "tiff","spData","dplyr","sf","resample", "parallel","broom","magick",
                         "lubridate")
  
  lapply(libraries_to_load , library, character.only = TRUE) 
  
  #loading the libraries for plots
  library(ggplot2)
  library(ggExtra)
  library(grid)
  library(MASS)
  library(gridExtra)
  library(raster)
  library(RColorBrewer)
  library(spData)
  library(dplyr)
  library(sf)

  #loading libraries for animations one by one:

  library(gganimate)
  library(magick)
  library(animation)
  
  # Library for daily rpcipitation data analysis:
  
  library(hydroTSM)
  require(lattice)
  
  rm(libraries_to_load)
    
    # clean console 
    cat("\014")
}
# clean console 
cat("\014")

