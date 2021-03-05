# This function plots  Cr2 data in raster format
# This fucntion only plots it doesn't save any file

# As the only plotting function available to operate over rasters is plot(),
# the aesthetics of the graphics is limited

# Input parameters:

#raster_to_plot
#type_of_plot     String/cher/text with the identifier of he tye of date youa re trying to plot
#                 possible values:
#                                 pr
#                                 tmin
#                                 tmax
#                                 Floods_hazard           perce_pr_daily_data
#                                 Droughts_hazard         perce_pr_annual_data 
#                                 Floods_Vulnerability    perce_annual_series_max_daily_pr 
#                                 Droughts_velnerability  Vul_metrics_floods
#                                 nummin

plot_raster_elements<-function(raster_to_plot,type_of_plot)
{
  # As this is an assessment function it will temporary change RStudio configuration to show 
  # plots in a new separete window instead of the tiny little "Plot" section of the GUI:
  
  # If you execute this script on Mac:
  # options(device = "quartz")
  
  # If you execute this Script on Windows:
  options(device = "windows")
  
  # if you wanna know the anmes of the colours to define a nice colour palette check this out:
  
  # https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf
  
  # examples of good colour pallettes:
  # c("moccasin","darkslategray")
  
  
  # This range will be used to create the label of the plot:
  
  range_to_plot <- c(minValue(raster_to_plot), maxValue(raster_to_plot))
  
  if (type_of_plot=="pr"){
    #pr
    #plot with a customize colour palette (blues scale)
    
    # we need to replace the NA data values in this graph
    
    values(raster_to_plot)[values(raster_to_plot) < 0] = NA
    
    white_to_blue_palette <-colorRampPalette(c("white", "blue"))(255)
    
    plot(raster_to_plot, col=white_to_blue_palette, legend=FALSE,
         main = "Daily precipitation plot",
         xlab = "Longitude [Degrees WGS84]",
         ylab = "Latitude  [Degrees WGS84]")
    
    plot(raster_to_plot,
         legend.only=TRUE,
         col=white_to_blue_palette,
         legend.width=1, 
         legend.shrink=0.75,
         axis.args=list(at=seq(range_to_plot[1], range_to_plot[2], 25), labels=seq(round(range_to_plot[1], digits = 2), round(range_to_plot[2], digits = 2), 25), cex.axis=0.6),
         legend.args=list(text='precipitation [mm]', side=4, font=2, line=2.5, cex=0.8))
    
    rm(white_to_blue_palette)
    
  } else if (type_of_plot=="tmax"){  
    #tmax
    #plot with a customize colour palette (yellow to red  scale)
    yellow_to_red_palette <-colorRampPalette(c("yellow", "red"))(255)
    
    plot(raster_to_plot, col=yellow_to_red_palette, legend=FALSE,
         main = "Maximun daily temperature plot",
         xlab = "Longitude [Degrees WGS84]",
         ylab = "Latitude  [Degrees WGS84]")
    
    plot(raster_to_plot,
         legend.only=TRUE,
         col=yellow_to_red_palette,
         legend.width=1, 
         legend.shrink=0.75,
         axis.args=list(at=seq(range_to_plot[1], range_to_plot[2], 25), labels=seq(round(range_to_plot[1], digits = 2), round(range_to_plot[2], digits = 2), 25), cex.axis=0.6),
         legend.args=list(text='temperature [Celcius]', side=4, font=2, line=2.5, cex=0.8))
    rm(yellow_to_red_palette)
    
  } else if (type_of_plot=="tmin"){    
    #tmin
    #plot with a customize colour palette (yellow to red  scale)
    blue_to_red_palette <-colorRampPalette(c("blue", "red"))(255)
    
    plot(raster_to_plot, col=blue_to_red_palette, legend=FALSE,
         main = "Minimum daily temperature plot",
         xlab = "Longitude [Degrees WGS84]",
         ylab = "Latitude  [Degrees WGS84]")
    
    plot(raster_to_plot,
         legend.only=TRUE,
         col=blue_to_red_palette,
         legend.width=1, 
         legend.shrink=0.75,
         axis.args=list(at=seq(range_to_plot[1], range_to_plot[2], 25), labels=seq(round(range_to_plot[1], digits = 2), round(range_to_plot[2], digits = 2), 25), cex.axis=0.6),
         legend.args=list(text='temperature [Celcius]', side=4, font=2, line=2.5, cex=0.8))
    rm(blue_to_red_palette)
    
  } else if (type_of_plot=="Floods_hazard"){ 
    #percentile daily values
    #plot with a customize colour palette ()
    
    percentile_palette <-colorRampPalette(c("white","blue"))(255)
    
    plot(raster_to_plot, col=percentile_palette, legend=FALSE, 
         main = "Percentile plot",
         xlab = "Longitude [Degrees WGS84]",
         ylab = "Latitude  [Degrees WGS84]")
    
    plot(raster_to_plot,
         legend.only=TRUE,
         col=percentile_palette,
         legend.width=1, 
         legend.shrink=0.75,
         axis.args=list(at=seq(range_to_plot[1], range_to_plot[2], 25), labels=seq(round(range_to_plot[1], digits = 2), round(range_to_plot[2], digits = 2), 25), cex.axis=0.6),
         legend.args=list(text='precipitation [mm]', side=4, font=2, line=2.5, cex=0.8))
    
    rm(percentile_palette)
  
  } else if (type_of_plot=="Droughts_hazard"){ 
    #percentile annual values
    #plot with a customize colour palette ()
    
    percentile_palette <-colorRampPalette(c("khaki","darkslategrey"))(255)
    
    plot(raster_to_plot, col=percentile_palette, legend=FALSE, 
         main = "Percentile plot",
         xlab = "Longitude [Degrees WGS84]",
         ylab = "Latitude  [Degrees WGS84]")
    
    plot(raster_to_plot,
         legend.only=TRUE,
         col=percentile_palette,
         legend.width=1, 
         legend.shrink=0.75,
         axis.args=list(at=seq(range_to_plot[1], range_to_plot[2], 25), labels=seq(round(range_to_plot[1], digits = 2), round(range_to_plot[2], digits = 2), 25), cex.axis=0.6),
         legend.args=list(text='precipitation [mm]', side=4, font=2, line=2.5, cex=0.8))
    
    rm(percentile_palette)  
  
  } else if (type_of_plot=="Floods_Vulnerability"){ 
    #percentile maximun daily values for each year
    #plot with a customize colour palette ()
    
    percentile_palette <-colorRampPalette(c("lightgreen","firebrick4"))(255)
    
    plot(raster_to_plot, col=percentile_palette, legend=FALSE, 
         main = "Percentile plot",
         xlab = "Longitude [Degrees WGS84]",
         ylab = "Latitude  [Degrees WGS84]")
    
    plot(raster_to_plot,
         legend.only=TRUE,
         col=percentile_palette,
         legend.width=1, 
         legend.shrink=0.75,
         axis.args=list(at=seq(range_to_plot[1], range_to_plot[2], 25), labels=seq(round(range_to_plot[1], digits = 2), round(maxValue(raster_to_plot), digits = 2), 25), cex.axis=0.6),
         legend.args=list(text='precipitation [mm]', side=4, font=2, line=2.5, cex=0.8))
    
    rm(percentile_palette)
  
  } else if (type_of_plot=="perce_annual_series_pr"){ 
    #percentile maximun daily values for each year
    #plot with a customize colour palette ()
    
    percentile_palette <-colorRampPalette(c("pink","blue"))(255)
    
    plot(raster_to_plot, col=percentile_palette, legend=FALSE, 
         main = "percentile for annual series of pr values",
         xlab = "Longitude [Degrees WGS84]",
         ylab = "Latitude  [Degrees WGS84]")
    
    plot(raster_to_plot,
         legend.only=TRUE,
         col=percentile_palette,
         legend.width=1, 
         legend.shrink=0.75,
         axis.args=list(at=seq(range_to_plot[1], range_to_plot[2], 25), labels=seq(round(range_to_plot[1], digits = 2), round(maxValue(raster_to_plot), digits = 2), 25), cex.axis=0.6),
         legend.args=list(text='precipitation [mm]', side=4, font=2, line=2.5, cex=0.8))
    
    rm(percentile_palette)    
    
  } else if (type_of_plot=="perce_annual_series_max_daily_pr"){ 
    #percentile maximun daily values for each year
    #plot with a customize colour palette ()
    
    percentile_palette <-colorRampPalette(c("papayawhip","purple4"))(255)
    
    plot(raster_to_plot, col=percentile_palette, legend=FALSE, 
         main = "95p/50p for annual series of max daily pr values",
         xlab = "Longitude [Degrees WGS84]",
         ylab = "Latitude  [Degrees WGS84]")
    
    plot(raster_to_plot,
         legend.only=TRUE,
         col=percentile_palette,
         legend.width=1, 
         legend.shrink=0.75,
         axis.args=list(at=seq(range_to_plot[1], range_to_plot[2], 25), labels=seq(round(range_to_plot[1], digits = 2), round(maxValue(raster_to_plot), digits = 2), 25), cex.axis=0.6),
         legend.args=list(text='precipitation [mm]', side=4, font=2, line=2.5, cex=0.8))
    
    rm(percentile_palette)  
  
  } else if (type_of_plot=="Droughts_vulnerability"){ 
    #percentile maximun daily values for each year
    #plot with a customize colour palette ()
    
    #percentile_palette <-colorRampPalette(c("darkolivegreen1","darkred"))(255)
    percentile_palette <-colorRampPalette(c("lightgreen","firebrick4"))(255)
    
    plot(raster_to_plot, col=percentile_palette, legend=FALSE, 
         main = "(50p-5p)/50p for annual series of pr values",
         xlab = "Longitude [Degrees WGS84]",
         ylab = "Latitude  [Degrees WGS84]")
    
    plot(raster_to_plot,
         legend.only=TRUE,
         col=percentile_palette,
         legend.width=1, 
         legend.shrink=0.75,
         axis.args=list(at=seq(range_to_plot[1], range_to_plot[2], 25), labels=seq(round(range_to_plot[1], digits = 2), round(maxValue(raster_to_plot), digits = 2), 25), cex.axis=0.6),
         legend.args=list(text='precipitation [mm]', side=4, font=2, line=2.5, cex=0.8))
    
    rm(percentile_palette)  
  
  } else if (type_of_plot=="nummin"){ 
    #percentile maximun daily values for each year
    #plot with a customize colour palette ()
    
    #percentile_palette <-colorRampPalette(c("darkolivegreen1","darkred"))(255)
    percentile_palette <-colorRampPalette(c("lightgreen","firebrick4"))(255)
    
    plot(raster_to_plot, col=percentile_palette, legend=FALSE, 
         main = "Number of mines",
         xlab = "Longitude [Degrees WGS84]",
         ylab = "Latitude  [Degrees WGS84]")
    
    plot(raster_to_plot,
         legend.only=TRUE,
         col=percentile_palette,
         legend.width=1, 
         legend.shrink=0.75,
         axis.args=list(at=seq(range_to_plot[1], range_to_plot[2], 25), labels=seq(round(range_to_plot[1], digits = 2), round(maxValue(raster_to_plot), digits = 2), 25), cex.axis=0.6),
         legend.args=list(text='[Number of mine points]', side=4, font=2, line=2.5, cex=0.8))
    
    rm(percentile_palette)  
    
          
  } else {
    print("Plot type not supported by this function. Please change it to any of the following" )
    print("pr")
    print("tmin")
    print("tmax")
    print("Floods_hazard")
    print("Droughts_hazard")
    print("Floods_Vulnerability")
    print("Droughts_velnerability")
    peint("nummin")
  }

  rm(range_to_plot)
  
  # we set-up the default Rstudio configuration for plots:
  # If you execute this Script on Windows:
  options(device = "RStudioGD")
}
# Clean the consle
cat("\014")

