# This function plots Cr2 data in rasterbrick format
# This fucntion only plots it doesn't save any file

# As the only plotting function available to operate over rasterbricks is plot(),
# the aesthetics of the graphics is limited

# Input parameters:
  
#raster_brick_to_plot
#type_of_plot            This is a string/ cahracter 
#                               possible values:
#                                               "pr"
#                                               "tmax"
#                                               "tmin"
#                                               "perce_pr_annual_data"                    

plot_raster_brick_elements<-function(raster_brick_to_plot,type_of_plot)
{
  # As this is an assessment function it will temporary change RStudio configuration to show 
  # plots in a new separete window instead of the tiny little "Plot" section of the GUI:
  
  # If you execute this script on Mac:
  # options(device = "quartz")
  
  # If you execute this Script on Windows:
  options(device = "windows")
  
  if (type_of_plot=="pr"| type_of_plot=="perce_pr_annual_data"| type_of_plot=="perce_pr_daily_data"){
    
    #pr | perce_pr_annual_data | perce_pr_daily_data
    #plot with a customize colour palette (blues scale)
    
    white_to_blue_palette <-colorRampPalette(c("white", "blue"))(255)
    
    plot(raster_brick_to_plot,
         col=white_to_blue_palette)
  
  } else if (type_of_plot=="tmax"){  
    #tmax
    #plot with a customize colour palette (yellow to red  scale)
    yellow_to_red_palette <-colorRampPalette(c("yellow", "red"))(255)
    
    plot(raster_brick_to_plot,
         col=yellow_to_red_palette)
    
  } else if (type_of_plot=="tmin"){    
    #tmin
    #plot with a customize colour palette (yellow to red  scale)
    blue_to_red_palette <-colorRampPalette(c("blue", "red"))(255)
    
    plot(raster_brick_to_plot,
         col=blue_to_red_palette)    
  }
  
  # we set-up the default Rstudio configuration for plots:
  # If you execute this Script on Windows:
  options(device = "RStudioGD")
  
  # Clean the consle
  cat("\014")
}
# Clean the consle
cat("\014")
