# This function saves a plot with  Cr2 data in a specified location

# Input parameters:

# raster_to_plot           raster element to plot using ggplot2()

# variable_short_name      text/string/char element
#                               possible values:
#                                               pr
#                                               tmin
#                                               tmax
#                                               perce_pr_daily_data
#                                               perce_pr_annual_data
#                                               perce_annual_series_max_daily_pr


# variable_long_name      text/string/char element
# variable_units          text/string/char element
# area_name               text/string/char element

# file_type               text/string/char element
#                             possible values:
#                                             png
#                                             pdf

# date_text               # text/string/char element
# name_of_saving_folder   # text/string/char element
# main_directory          # text/string/char element

save_plot_in_a_specified_location<-function(raster_to_plot,
                                            variable_short_name,
                                            variable_long_name,
                                            variable_units,
                                            area_name,
                                            file_type,
                                            date_text,
                                            name_of_saving_folder,
                                            main_directory)
{
  
  
  #RColorBrewer palette options:
  
  #   "Reds"
  #   "Greens"
  #   "Oranges"
  #   "Blues"
  #   "Greys"
  #   "Spectral" ->  From red to Purple
  #   "YIOrRd"   ->  From Yellow to Red (Risk pallete)
  #   "YIOrBr"   ->  From green to Blue
  #   "BrBG"     ->  From brown to Green 
  
  # More predefined palletes:
    
  #  http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/ 
  
  
  # In the following line you convert the raster to spatial pixels dataframe
  spdf_with_data <- as( raster_to_plot,"SpatialPixelsDataFrame")
  
  names(spdf_with_data)<-"data"
  
  min_value<-min(spdf_with_data$data, na.rm=TRUE)
  max_value<-max(spdf_with_data$data, na.rm=TRUE)
  
  # You need to calcualte the minimun and the maximun value in roder to apply a personalized
  # color palette
  
  plot_title <-paste( "CR2",
                      variable_long_name,
                      "for",
                      area_name,
                      date_text,
                      sep=" ")
  
  if (variable_short_name=="pr")
  {
    # we need to replace the NA data values in this dataset
    values(raster_to_plot)[values(raster_to_plot) < 0] = NA
  }
  
  raster::as.data.frame(raster_to_plot,xy=TRUE) -> df_to_plot

  if (variable_short_name=="pr"){
    
    #pr
    #plot with a customize colour palette (blues scale)
    
    names(df_to_plot) <- c("lon", "lat", "pr")
    
    if(min_value>0 && max_value<10){
      df_to_plot$cuts=cut(df_to_plot$pr,
                          breaks=c(0,0.1,0.5,0.75,1,1.5,2,2.5,5,7.5,10))
    } else if (min_value>0 && max_value<50){
      df_to_plot$cuts=cut(df_to_plot$pr,
                          breaks=c(0,1,2.5,5,10,15,20,25,30,40,50))
    } else if (min_value>0 && max_value<100){
      df_to_plot$cuts=cut(df_to_plot$pr,
                          breaks=c(0,5,10,20,25,50,75,90,95,100))
    } else if (min_value>10 && max_value<1000){
      df_to_plot$cuts=cut(df_to_plot$pr,
                          breaks=c(0,2.5,5.10,25,50,75,100,250,500,750,1000))
    } else if (min_value>10 && max_value<2000){
      df_to_plot$cuts=cut(df_to_plot$pr,
                          breaks=c(0,1,2.5,5,10,15,20,25,30,40,50))
    } else {
      df_to_plot$cuts=cut(df_to_plot$pr,
                          breaks=c(0,5,10,50,100,250,500,1000,1500,2000,3000))  
    }
  

    if (file_type=="png"){
      
      plot_extension<-".png"
      
      plot_filename <- paste(main_directory,
                             "/",
                             name_of_saving_folder,
                             "/",
                             variable_short_name,
                             "-",
                             area_name,
                             "-CR2-",
                             date_text,
                             plot_extension,
                             sep ="")

      png(plot_filename)
      plot_pr<-ggplot(data=df_to_plot) +
        geom_tile(aes(x=df_to_plot$lon,y=df_to_plot$lat, fill=cuts)) + 
        scale_fill_brewer(variable_units,type = "seq", palette = "Blues") +
        xlab("Longitude") + 
        ylab("Latitude") +
        ggtitle(plot_title) 
      print(plot_pr)
      # Remember that when you save plots this way, the plot isn't actually written to 
      # the file until you call:
      dev.off()
      
    } else if (file_type=="pdf"){   
      
      plot_extension<-".pdf"
      
      plot_filename <- paste(main_directory,
                             "/",
                             name_of_saving_folder,
                             "/",
                             variable_short_name,
                             "-",
                             area_name,
                             "-CR2-",
                             date_text,
                             plot_extension,
                             sep ="")
      
      print(plot_filename)
      
      pdf(plot_filename)
      plot_pr<-ggplot(data=df_to_plot) +
        geom_tile(aes(x=df_to_plot$lon,y=df_to_plot$lat, fill=cuts)) + 
        scale_fill_brewer(variable_units,type = "seq", palette = "Blues") +
        xlab("Longitude") + 
        ylab("Latitude") +
        ggtitle(plot_title) 
      print(plot_pr)
      # Remember that when you save plots this way, the plot isn't actually written to 
      # the file until you call:
      dev.off()
    } else {
      print("File type not supported by this function. Please change it to pdf or png" )
    }
    
  } else if (variable_short_name=="tmax"){  
    
    #tmax
    raster_to_plot_range <- c(minValue(raster_to_plot), maxValue(raster_to_plot))
    
    names(df_to_plot) <- c("lon", "lat", "tmax")
    
    df_to_plot$cuts=cut(df_to_plot$tmax,
                        breaks=c(-20,-10,-5,0,5,10,15,20,25,30,35,40,45,50,55))
    
    #plot with a customize colour palette (yellow to red scale)
    yellow_to_red_palette <-colorRampPalette(c("yellow", "red"))(255)
    plot_range <- c(minValue(raster_to_plot), maxValue(raster_to_plot))

    if (file_type=="png"){
      
      plot_extension<-".png"
      
      plot_filename <- paste(main_directory,
                             "/",
                             name_of_saving_folder,
                             "/",
                             variable_short_name,
                             "-",
                             area_name,
                             "-CR2-",
                             date_text,
                             plot_extension,
                             sep ="")
      png(plot_filename)
      plotx<-ggplot(data=df_to_plot) +
        geom_tile(aes(x=df_to_plot$lon,y=df_to_plot$lat, fill=cuts)) + 
        scale_fill_brewer(variable_units,type = "seq", palette = yellow_to_red_palette) +
        xlab("Longitude") + 
        ylab("Latitude") +
        ggtitle(plot_title) 
      print(plotx)
      # Remember that when you save plots this way, the plot isn't actually written to 
      # the file until you call:
      dev.off()
      
    } else if (file_type=="pdf"){   
      
      plot_extension<-".pdf"
      
      plot_filename <- paste(main_directory,
                             "/",
                             name_of_saving_folder,
                             "/",
                             variable_short_name,
                             "-",
                             area_name,
                             "-CR2-",
                             date_text,
                             plot_extension,
                             sep ="")
      pdf(plot_filename)
      plotx<-ggplot(data=df_to_plot) +
        geom_tile(aes(x=df_to_plot$lon,y=df_to_plot$lat, fill=cuts)) + 
        scale_fill_brewer(variable_units,type = "seq", palette = yellow_to_red_palette) +
        xlab("Longitude") + 
        ylab("Latitude") +
        ggtitle(plot_title) 
      print(plotx)
      # Remember that when you save plots this way, the plot isn't actually written to 
      # the file until you call:
      dev.off()
    } else {
      print("File type not supported by this function. Please change it to pdf or png" )
    }
    
    
  } else if (variable_short_name=="tmin"){    
    #tmin

    names(df_to_plot) <- c("lon", "lat", "tmin")
    
    #plot with a customize colour palette (yellow to red  scale)
    blue_to_red_palette <-colorRampPalette(c("blue", "red"))(255)
    
    df_to_plot$cuts=cut(df_to_plot$tmin,
                        breaks=c(-50,-20,-10,-5,0,5,10,15,20,25,30,35,40,45,50))
    

    plot_range <- c(minValue(raster_to_plot), maxValue(raster_to_plot))
    
    if (file_type=="png"){
      
      plot_extension<-".png"
      
      plot_filename <- paste(main_directory,
                             "/",
                             name_of_saving_folder,
                             "/",
                             variable_short_name,
                             "-",
                             area_name,
                             "-CR2-",
                             date_text,
                             plot_extension,
                             sep ="")
      png(plot_filename)
      plotx<-ggplot(data=df_to_plot) +
        geom_tile(aes(x=df_to_plot$lon,y=df_to_plot$lat, fill=cuts)) + 
        scale_fill_brewer(variable_units,type = "seq", palette = blue_to_red_palette) +
        xlab("Longitude") + 
        ylab("Latitude") +
        ggtitle(plot_title) 
      print(plotx)
      # Remember that when you save plots this way, the plot isn't actually written to 
      # the file until you call:
      dev.off()
      
    } else if (file_type=="pdf"){   
      
      plot_extension<-".pdf"
      
      plot_filename <- paste(main_directory,
                             "/",
                             name_of_saving_folder,
                             "/",
                             variable_short_name,
                             "-",
                             area_name,
                             "-CR2-",
                             date_text,
                             plot_extension,
                             sep ="")
      pdf(plot_filename)
      plotx<-ggplot(data=df_to_plot) +
        geom_tile(aes(x=df_to_plot$lon,y=df_to_plot$lat, fill=cuts)) + 
        scale_fill_brewer(variable_units,type = "seq", palette = blue_to_red_palette) +
        xlab("Longitude") + 
        ylab("Latitude") +
        ggtitle(plot_title) 
      print(plotx)
      # Remember that when you save plots this way, the plot isn't actually written to 
      # the file until you call:
      dev.off()
    } else {
      print("File type not supported by this function. Please change it to pdf or png" )
    }
    
  } else if (variable_short_name=="perce_pr_daily_data" | variable_short_name== "Floods_hazard"){
    
    names(df_to_plot) <- c("lon", "lat", "perce")
    
    if(min_value>=0 && max_value<10){
      df_to_plot$cuts=cut(df_to_plot$perce,
                          breaks=c(0,0.1,0.5,0.75,1,1.5,2,2.5,5,7.5,10))
    } else if (min_value>=0 && max_value<50){
      df_to_plot$cuts=cut(df_to_plot$perce,
                          breaks=c(0,1,2.5,5,10,15,20,25,30,40,50))
    } else if (min_value>=25 && max_value<100){
      df_to_plot$cuts=cut(df_to_plot$perce,
                          breaks=c(25,30,40,50,60,70,80,90,100))
    } else if (min_value>=0 && max_value<100){
      df_to_plot$cuts=cut(df_to_plot$perce,
                          breaks=c(0,5,10,20,25,50,75,90,95,100))
    } else if (min_value>=25 && max_value<250){
      df_to_plot$cuts=cut(df_to_plot$perce,
                          breaks=c(25,50,75,80,100,120,150,175,200,250))
    } else if (min_value>=10 && max_value<500){
      df_to_plot$cuts=cut(df_to_plot$perce,
                          breaks=c(10,20,25,50,75,100,150,200,250,500))
    } else if (min_value>=10 && max_value<1000){
      df_to_plot$cuts=cut(df_to_plot$perce,
                          breaks=c(10,25,50,75,100,150,200,250,500,750,1000))
    } else if (min_value>=10 && max_value<2000){
      df_to_plot$cuts=cut(df_to_plot$perce,
                          breaks=c(10,25,50,100,250,500,1000,1250,1500,1750,2000))
    } else {
      df_to_plot$cuts=cut(df_to_plot$perce,
                          breaks=c(0,5,10,50,100,250,500,1000,1500,2000,3000))  
    }
    
    plot_title<-paste( "CR2_met (1978-2016)",
                       variable_long_name,
                       "for",
                       area_name,
                       sep=" ")
    
    if (file_type=="png"){
      
      plot_extension<-".png"
      
      plot_filename <- paste(main_directory,
                             "/",
                             name_of_saving_folder,
                             "/",
                             gsub(" ", "-",variable_long_name),
                             "-",
                             area_name,
                             plot_extension,
                             sep ="")
      
      png(plot_filename)
      plot_perce<-ggplot(data=df_to_plot) +
        geom_tile(aes(x=df_to_plot$lon,y=df_to_plot$lat, fill=cuts)) + 
        scale_fill_brewer(variable_units,type = "seq", palette = "BrBG") +
        xlab("Longitude") + 
        ylab("Latitude") +
        ggtitle(plot_title) 
      print(plot_perce)
      # Remember that when you save plots this way, the plot isn't actually written to 
      # the file until you call:
      dev.off()
      
    } else if (file_type=="pdf"){   
      
      plot_extension<-".pdf"
      
      plot_filename <- paste(main_directory,
                             "/",
                             name_of_saving_folder,
                             "/",
                             gsub(" ", "-",variable_long_name),
                             "-",
                             area_name,
                             plot_extension,
                             sep ="")
      
      pdf(plot_filename)
      plot_perce<-ggplot(data=df_to_plot) +
        geom_tile(aes(x=df_to_plot$lon,y=df_to_plot$lat, fill=cuts)) + 
        scale_fill_brewer(variable_units,type = "seq", palette = "BrBG") +
        xlab("Longitude") + 
        ylab("Latitude") +
        ggtitle(plot_title) 
      print(plot_perce)
      # Remember that when you save plots this way, the plot isn't actually written to 
      # the file until you call:
      dev.off()
    } else {
      print("File type not supported by this function. Please change it to pdf or png" )
    }
  } else if (variable_short_name=="perce_pr_annual_data"| variable_short_name=="Droughts_hazard"){
      
      names(df_to_plot) <- c("lon", "lat", "perce")
      
      if(min_value>=0 && max_value<10){
        df_to_plot$cuts=cut(df_to_plot$perce,
                            breaks=c(-0.1,0,0.1,0.5,0.75,1,1.5,2,2.5,5,7.5,10))
      } else if (min_value>=0 && max_value<50){
        df_to_plot$cuts=cut(df_to_plot$perce,
                            breaks=c(-0.1,0,1,2.5,5,10,15,20,25,30,40,50))
      } else if (min_value>=20 && max_value<70){
        df_to_plot$cuts=cut(df_to_plot$perce,
                            breaks=c(20,25,30,35,40,45,50,55,60,65,70))  
      } else if (min_value>=0 && max_value<100){
        df_to_plot$cuts=cut(df_to_plot$perce,
                            breaks=c(-0.1,0,5,10,20,25,50,75,90,95,100))
      } else if (min_value>25 && max_value<250){
        df_to_plot$cuts=cut(df_to_plot$perce,
                            breaks=c(25,50,75,80,100,120,150,175,200,250))
      } else if (min_value>10 && max_value<500){
        df_to_plot$cuts=cut(df_to_plot$perce,
                            breaks=c(10,20,25,50,75,100,150,200,250,500))
      } else if (min_value>10 && max_value<1000){
        df_to_plot$cuts=cut(df_to_plot$perce,
                            breaks=c(10,25,50,75,100,150,200,250,500,750,1000))
      } else if (min_value>10 && max_value<2000){
        df_to_plot$cuts=cut(df_to_plot$perce,
                            breaks=c(10,25,50,100,250,500,1000,1250,1500,1750,2000))
      } else {
        df_to_plot$cuts=cut(df_to_plot$perce,
                            breaks=c(0,5,10,50,100,250,500,1000,1500,2000,3000))  
      }
      
      plot_title<-paste( "CR2_met (1978-2016)",
                         variable_long_name,
                         "for",
                         area_name,
                         sep=" ")
      
      if (file_type=="png"){
        
        plot_extension<-".png"
        
        plot_filename <- paste(main_directory,
                               "/",
                               name_of_saving_folder,
                               "/",
                               gsub(" ", "-",variable_long_name),
                               "-",
                               area_name,
                               plot_extension,
                               sep ="")
        
        png(plot_filename)
        plot_perce<-ggplot(data=df_to_plot) +
          geom_tile(aes(x=df_to_plot$lon,y=df_to_plot$lat, fill=df_to_plot$cuts)) + 
          scale_fill_brewer(variable_units,type = "seq", palette = "BrBG") +
          xlab("Longitude") + 
          ylab("Latitude") +
          ggtitle(plot_title) 
        print(plot_perce)
        # Remember that when you save plots this way, the plot isn't actually written to 
        # the file until you call:
        dev.off()
        
      } else if (file_type=="pdf"){   
        
        plot_extension<-".pdf"
        
        plot_filename <- paste(main_directory,
                               "/",
                               name_of_saving_folder,
                               "/",
                               gsub(" ", "-",variable_long_name),
                               "-",
                               area_name,
                               plot_extension,
                               sep ="")
        
        pdf(plot_filename)
        plot_perce<-ggplot(data=df_to_plot) +
          geom_tile(aes(x=df_to_plot$lon,y=df_to_plot$lat, fill=df_to_plot$cuts)) + 
          scale_fill_brewer(variable_units,type = "seq", palette = "BrBG") +
          xlab("Longitude") + 
          ylab("Latitude") +
          ggtitle(plot_title) +
        print(plot_perce)
        # Remember that when you save plots this way, the plot isn't actually written to 
        # the file until you call:
        dev.off()      
        
    } else {
      print("File type not supported by this function. Please change it to pdf or png" )
    }
    
    
  } else {
    print("variable not supported by this function. Please change it to any of the following:")
    print("pr")
    print("tmin")
    print("tmax")
    print("perce_pr_daily_data")
    print("perce_pr_annual_data")
    print("variable_short_name")
  }
}
# Clean the consle
cat("\014")

