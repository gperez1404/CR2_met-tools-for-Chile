# This function creates a new plot in a new window


# Input parameters:

#raster_to_plot

#shape                  Shapefile element with the boundary of the interest area

#type_of_variable_to_plot          text/string/char element
#                               possible values:
#                                               pr
#                                               tmin
#                                               tmax
#                                               Floods_hazard           perce_pr_daily_data
#                                               Droughts_hazard         perce_pr_annual_data 
#                                               Floods_Vulnerability    perce_annual_series_max_daily_pr 
#                                               Droughts_velnerability  Vul_metrics_floods
#                                               nummin                  Exposure

#plot_title
#plot_units

plot_raster_elements_with_shp_background<-function(raster_to_plot,
                                                   shape,
                                                   type_of_variable_to_plot,
                                                   plot_title,
                                                   plot_units)
{
  # As this is an assessment function it will temporary change RStudio configuration to show 
  # plots in a new separate window instead of the tiny little "Plot" section of the GUI:
  
  # If you execute this script on Mac:
  # options(device = "quartz")
  
  # If you execute this Script on Windows:
  options(device = "windows")
  
  # In the following line you convert the raster to a spatial pixels dataframe element 
  spdf_with_data <- as( raster_to_plot,"SpatialPixelsDataFrame")
  
  names(spdf_with_data)<-"data"
  
  # In the following lines you calculate the range of values of the data you wanna plot
  min_value<-min(spdf_with_data$data, na.rm=TRUE)
  max_value<-max(spdf_with_data$data, na.rm=TRUE)
  
  # here you convert the shapefile into a dataframe 
  interest_area_border_df <- tidy(shape)
  
  # here you rename the columns of the df to keep consistency with the raster you wanna plot
  colnames(interest_area_border_df) <- c("lon", "lat", "order", "hole", "piece", "group", "id")
  
  # if you want to know more about colour palletes for your plots you can visit this website:
  
  # http://colorbrewer2.org/
  
  # Arguments of ratser plotting function
  
  # scale_fill_brewer(units= , type =  , palette = ) 
  #                                  ^
  #                                "seq"
  #                                "qualitative"
  #                                "diverging"   
  
  #if you want to use  continous sprectrum colours you need to pick a palette:
  # some exmaples :
  
  # palette="green2red"
  # palette="matlablike2"
  # palette="Blues"
  
  #more exmaples of colours to use with fill_brewwer:
  
  # http://colorbrewer2.org/#type=sequential&scheme=BuGn&n=9
  

  if (type_of_variable_to_plot=="pr"){
    #pr
    
    # we need to replace the NA data values in this graph
    values(raster_to_plot)[values(raster_to_plot) < 0] = NA
    
    #you need to covnert the raster into a dfataframe if you want to use ggplot
    raster::as.data.frame(raster_to_plot,xy=TRUE) -> df_to_plot
  
    # Here you change the names of the columns
    names(df_to_plot) <- c("lon", "lat", "pr")
    
    print("you need to complete the code for this type of plot")
    
  } else if (type_of_variable_to_plot=="tmax"){  
    #tmax
    
    #you need to covnert the raster into a dfataframe if you want to use ggplot
    raster::as.data.frame(raster_to_plot,xy=TRUE) -> df_to_plot
    
    # Here you change the names of the columns
    names(df_to_plot) <- c("lon", "lat", "tmax")
    
    print("you need to complete the code for this type of plot")
    
  } else if (type_of_variable_to_plot=="tmin"){    
    #tmin
    #plot with a customize colour palette (yellow to red  scale)
    blue_to_red_palette <-colorRampPalette(c("blue", "red"))(255)
    
    print("you need to complete the code for this type of plot")
    
    
  } else if (type_of_variable_to_plot=="perce_pr_daily_data" | type_of_variable_to_plot== "Floods_hazard"){ 
    
    #you need to covnert the raster into a dfataframe if you want to use ggplot
    raster::as.data.frame(raster_to_plot,xy=TRUE) -> df_to_plot
    
    names(df_to_plot) <- c("lon", "lat", "perce")
    
    if(min_value>=0 && max_value<10){
      df_to_plot$cuts=cut(df_to_plot$perce,
                          breaks=c(-0.1,0,0.1,0.5,0.75,1,1.5,2,2.5,5,7.5,10))
    } else if (min_value>=0 && max_value<50){
      df_to_plot$cuts=cut(df_to_plot$perce,
                          breaks=c(-0.1,0,1,2.5,5,10,15,20,25,30,40,50))
    } else if (min_value>=25 && max_value<100){
      df_to_plot$cuts=cut(df_to_plot$perce,
                          breaks=c(25,30,40,50,60,70,80,90,100))
    } else if (min_value>=0 && max_value<100){
      df_to_plot$cuts=cut(df_to_plot$perce,
                          breaks=c(-0.1,0,5,10,20,25,50,75,90,95,100))
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
    
    p1<-ggplot(data=df_to_plot) +  
      geom_tile(aes(x=df_to_plot$lon,y=df_to_plot$lat, fill=df_to_plot$cuts)) + 
      scale_fill_brewer(plot_units,type = "seq", palette = "BrBG") +
      aes(lon, lat) +
      geom_path(data = interest_area_border_df, aes(group = group)) + 
      xlab("Longitude") + 
      ylab("Latitude") +
      ggtitle(plot_title) +
      theme_bw()
    print(p1)
    
    print("percentile/hazard data plotted successfully !")
  
  } else if (type_of_variable_to_plot=="perce_pr_annual_data" | type_of_variable_to_plot=="Droughts_hazard"){ 
    
    #you need to covnert the raster into a dfataframe if you want to use ggplot
    raster::as.data.frame(raster_to_plot,xy=TRUE) -> df_to_plot
    
    names(df_to_plot) <- c("lon", "lat", "perce")
    
    if(min_value>=0 && max_value<10){
      df_to_plot$cuts=cut(df_to_plot$perce,
                          breaks=c(-0.1,0,0.1,0.25,0.5,0.75,1,1.5,2,2.5,5,7.5,10))
    } else if (min_value>=0 && max_value<20){
      df_to_plot$cuts=cut(df_to_plot$perce,
                          breaks=c(-0.1,0,0.1,0.25,0.5,0.75,1,1.5,2.5,5,10,15,20))
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
    
    p1<-ggplot(data=df_to_plot) +  
      geom_tile(aes(x=df_to_plot$lon,y=df_to_plot$lat, fill=df_to_plot$cuts)) + 
      scale_fill_brewer(plot_units,type = "seq", palette = "BrBG") +
      aes(lon, lat) +
      geom_path(data = interest_area_border_df, aes(group = group)) + 
      xlab("Longitude") + 
      ylab("Latitude") +
      ggtitle(plot_title) +
      theme_bw()
    print(p1)
    
    print("Droughts hazard map plotted successfully !")  
  
  } else if (type_of_variable_to_plot=="Floods_Vulnerability" ){ 
    
    #you need to covnert the raster into a dfataframe if you want to use ggplot
    raster::as.data.frame(raster_to_plot,xy=TRUE) -> df_to_plot
    
    names(df_to_plot) <- c("lon", "lat", "floodsvul")
    
    if(min_value>=0 && max_value<10){
      df_to_plot$cuts=cut(df_to_plot$floodsvul,
                          breaks=c(-0.1,0,0.1,0.25,0.5,0.75,1,1.5,2,2.5,5,7.5,10))
    } else if (min_value>=0 && max_value<20){
      df_to_plot$cuts=cut(df_to_plot$floodsvul,
                          breaks=c(-0.1,0,0.1,0.25,0.5,0.75,1,1.5,2.5,5,10,15,20))
    } else if (min_value>=0 && max_value<50){
      df_to_plot$cuts=cut(df_to_plot$floodsvul,
                          breaks=c(-0.1,0,1,2.5,5,10,15,20,25,30,40,50))
    } else if (min_value>=20 && max_value<70){
      df_to_plot$cuts=cut(df_to_plot$floodsvul,
                          breaks=c(20,25,30,35,40,45,50,55,60,65,70))  
    } else if (min_value>=0 && max_value<100){
      df_to_plot$cuts=cut(df_to_plot$floodsvul,
                          breaks=c(-0.1,0,5,10,20,25,50,75,90,95,100))
    } else if (min_value>25 && max_value<250){
      df_to_plot$cuts=cut(df_to_plot$floodsvul,
                          breaks=c(25,50,75,80,100,120,150,175,200,250))
    } else if (min_value>10 && max_value<500){
      df_to_plot$cuts=cut(df_to_plot$floodsvul,
                          breaks=c(10,20,25,50,75,100,150,200,250,500))
    } else if (min_value>10 && max_value<1000){
      df_to_plot$cuts=cut(df_to_plot$floodsvul,
                          breaks=c(10,25,50,75,100,150,200,250,500,750,1000))
    } else if (min_value>10 && max_value<2000){
      df_to_plot$cuts=cut(df_to_plot$floodsvul,
                          breaks=c(10,25,50,100,250,500,1000,1250,1500,1750,2000))
    } else {
      df_to_plot$cuts=cut(df_to_plot$perce,
                          breaks=c(0,5,10,50,100,250,500,1000,1500,2000,3000))  
    }
    
    p1<-ggplot(data=df_to_plot) +  
      geom_tile(aes(x=df_to_plot$lon,y=df_to_plot$lat, fill=df_to_plot$cuts)) + 
      scale_fill_brewer(plot_units,type = "seq", palette = "BrBG") +
      aes(lon, lat) +
      geom_path(data = interest_area_border_df, aes(group = group)) + 
      xlab("Longitude") + 
      ylab("Latitude") +
      ggtitle(plot_title) +
      theme_bw()
    print(p1)
    
    print("Floods vulnerability map plotted successfully !")  
  
  } else if (type_of_variable_to_plot=="nummin" ){ 
    
    # Plot of number of mines
    # You need to covnert the raster into a dataframe if you want to use ggplot()
    raster::as.data.frame(raster_to_plot,xy=TRUE) -> df_to_plot
    
    names(df_to_plot) <- c("lon", "lat", "nummin")
    
    
    # When using scale_fill_gradient2() you have a lot of argumens you can personalized
    
    # https://www.rdocumentation.org/packages/ggplot2/versions/1.0.0/topics/scale_colour_gradient2
    
    
    #   midpoint
    #   guide
    #   na.value
    #   low
    #   mid
    #   high
    #   space  -> This is a very complex parameter so you need to read more about it:
    #             https://cran.r-project.org/web/packages/colordistance/vignettes/color-spaces.html
    #              
    #              Options:
    #                       "Lab"
    #                        "RGB"
    #                        "HSV"
    
    
    p1<-ggplot(data=df_to_plot) +  
      geom_tile(aes(x=df_to_plot$lon,y=df_to_plot$lat, fill=df_to_plot$nummin)) + 
      scale_fill_gradient2(midpoint =(max_value/2),
                           guide="colourbar",
                           limits=c(0,max_value),
                           na.value="transparent",
                           low="lightgreen",
                           mid="orange",
                           high = "red",
                           space = "Lab") +
      labs(fill = plot_units)+
      aes(lon, lat) +
      geom_path(data = interest_area_border_df, aes(group = group)) + 
      xlab("Longitude") + 
      ylab("Latitude") +
      ggtitle(plot_title) +
      theme_bw()
    print(p1)
    
    print("Raster with number of mines plotted successfully !")    
          
  } else { 
    print("The type of variable you want to plot is not supported by this function")
    print("Please change it to any of the following types:" )
    print("pr")
    print("tmin")
    print("tmax")
    print("perce_pr_daily_data or Floods_hazard")
    print("perce_pr_annual_data or Droughts_hazard")
    print("perce_annual_series_max_daily_pr")
    print("Floods_Vulnerability")
    print("Droughts_vulnerability")
    print("nummin")
  }
  # we set-up the default Rstudio configuration for plots:
  # If you execute this Script on Windows:
  options(device = "RStudioGD")
}
# Clean the consle
cat("\014")

