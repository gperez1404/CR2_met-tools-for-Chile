# CR2_met-tools-for-Chile
This repository contains code to analyse and process CR2_met (climate product) elaborated by CR2 research group in Chile
##############################################################################################################
#                            CR2 data extraction -> processing -> analysis                                   #
##############################################################################################################

# These scripts execute an analysis of CR2_met data at a regional/reiver basin scale. The precipitation 
# statistics will be executed for a group of catchments (river basins) within Chile that have been 
# predefined by the user

# the CR2_met climate data is saved in NetCDF format (network Common Data Form).This is a self-documenting, 
# machine-independent format for creating and distributing arrays of gridded data. It was originally 
# developed for storing and distributing climate data.


# a netCDF file  has dimensions, variables and attributes. In this particular case, CR2 data has 3 dimensions
# (x,y and Time). The R packages 'ncdf', 'ncdf4' and 'raster' provide the  necessary support for reading and 
# writing NetCDF files. the package ncdf is not supported in the new versions of R, so all the code will be  
# developed using ncdf4.

# For more info about the fucntions of the package "ncdf4" visit the following website :

# https://www.rdocumentation.org/packages/ncdf4/versions/1.16


# the origanl ncdf file with CR2_met product was obtained from : http://www.cr2.cl/eng/ 

##############################################################################################################
#                            CR2 data extraction -> processing -> analysis                                   #
##############################################################################################################
