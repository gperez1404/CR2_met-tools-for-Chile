# This function loads all the linraries required for executing CR2 data extraction or data analysis


# exaple of how to call the fucntion
# load_libraries()

load_libraries<- function()
{
  
  #libraries for GIS and spatial data-handling
  library(ncdf4)
  library(raster)
  library(sf)
  library(sp)
  library(grid)
  library(gridExtra)
  library(spData)
  library(dplyr)
  library(rgdal)
  library(osmar)
  library(tiff)
  library(resample)
  
  #loading the libraries for plots
  library(ggplot2)
  library(ggExtra)
  library(MASS)
  library(RColorBrewer)

  #loading libraries for animations one by one:

  library(gganimate)
  library(magick)
  library(animation)
  
  # Library for daily precipitation data analysis:
  
  library(hydroTSM)
  require(lattice)
  
  # Other libraries 
  libraries_to_load <- c("tmap","chron","lattice", "parallel","broom","magick", "lubridate")
  
  lapply(libraries_to_load , library, character.only = TRUE) 
  
  rm(libraries_to_load)
    
    # clean console 
    cat("\014")
}
# clean console 
cat("\014")

