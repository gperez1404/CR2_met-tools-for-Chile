# This function extracts a raster out of an input raster

# This function has the following input parameters:

#  raster_mask
#  input_raster

extract_by_mask_from_a_raster<-function(raster_mask, input_raster)
{
  # How to extract the extent of the mask raster?
  mask_extent <- extent(raster_mask)
  
  # How to extract the extent of the input  raster?
  Input_raster_extent <- extent(input_raster)
  
  # display the extent in console
  #print(mask_extent)
  #print(Input_raster_extent)
  
  # Be careful with the functions extent() and extend() because they are different in R !!!
  
  # the following line re-extend the input raster using the coordinates of the mask raster
  new_input_raster <- extend(input_raster,raster_mask)
  
  
  old_extent_of_input_raster <-extent(input_raster)
  new_extent_of_input_raster <-extent(new_input_raster)
  
  # to apply the function mask() we need to crop the pr raster first to the same extent as the mask raster
  cropped_raster <-crop(new_input_raster,mask_extent)
  
  # How to extract the extent of the cropped raster ?
  cropped_raster_extent <- extent(cropped_raster)
  
  # How to quiclky/visualy assess the two rasters to be used in the mask process ?
  
  #plot(raster_mask)               # mask (catchment)
  #plot(cropped_raster)            # cropped data (rainfall for the catchment)
  
  #print(mask_extent)                      # mask (catchment)
  #print(cropped_raster_extent)            # cropped data (rainfall for the catchment)
  
  # How to check if the extent of the two rasters is the same  ?
  
  # the result at the end of this check should be ->[1] TRUE
  # even a small difference in decimals will make the mask() function to crash
  #print(all.equal(mask_extent, cropped_raster_extent))
  
  # if the extents or the cellsize are not identical we need to resample  the mask to be exactly the same as 
  # the slice raster. There are 4 different methods to resample a raster depending on the type of data it 
  # contains:
  
  #   Nearest Neighbor 
  #   Bilinear
  #   Cubic convolution
  #   Majority resampling
  
  # when dealing with continuous values like precipitation, DEMs or temperature rasters, you must use the 
  # Bilinear method 
  
  # Apparently the resample() function uses another function that is not available for R 3.5 ; resampleFun() 
  
  # An alternative for this issue is to use crop(extend()), since the two rasters are not prefectly aligned. 
  # If you use resample() you need to be aware that the result will have modified data due to the 
  # interpolation carried out by this function. 
  
  # If you use the alternative described here it would be much slower and more resource-consuming but it 
  # will work under any version of R:
  
  #ideally this should works, but in this case it is not working I dunno why :(
  #cropped_pr_raster <- crop(extend(cropped_pr_raster,raster_mask),raster_mask)
  
  # when nothing else works you can change the extent by brute force with the following line:
  extent(cropped_raster)<- extent(raster_mask)
  
  # recalcualte the cropped raster extent after changinig it 
  cropped_raster_extent <-extent(cropped_raster)
  
  
  #print(all.equal(cropped_raster_extent, mask_extent))
  
  # The function mask() extracts only the values that are different than NA
  # if the extent of the two input rasters is not identical the function won't work
  masked_raster <- mask(cropped_raster,raster_mask) 
  
  return(masked_raster)
  
  rm(mask_extent)
  rm(Input_raster_extent)
  rm(new_input_raster)
  rm(cropped_raster_extent)
  rm(old_extent_of_input_raster)
  rm(new_extent_of_input_raster)
  rm(re_extended_input_raster)
  rm(cropped_raster)
  rm(cropped_raster_extent)

  
  #clean the console
  cat("\014")  
}
#clean the console
cat("\014")

