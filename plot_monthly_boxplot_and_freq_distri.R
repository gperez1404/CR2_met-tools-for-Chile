
# This function plots s single box-plot for multi-annual m monthly pr values 

#input parameters:

# month_number    a numeric value with the minth number [1-12]

plot_monthly_boxplot_and_freq_distri<-function(month_number)
{

  # This display pltos in adifferetn screen
  options(device = "windows")

  if(month_number==1){
    plot_title<-"january"
    variable_to_plot<-df_mean_monthly_values_interest_area_janaury$mean
    
    number_of_breaks_for_hist<-floor(length(variable_to_plot)/2)+1
    
    # Layout to split the screen
    layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
    
    # Draw the boxplot and the histogram 
    par(mar=c(0, 3.1, 1.1, 2.1))
    boxplot(variable_to_plot,
            horizontal=TRUE,
            ylim=c(min(variable_to_plot),(floor(max(variable_to_plot))+1)),
            xaxt="n" ,
            col='deepskyblue',
            frame=F)
    par(mar=c(4, 3.1, 1.1, 2.1))
    hist(variable_to_plot,
         breaks=number_of_breaks_for_hist,
         col=rgb(0.2,0.8,0.5,0.5) ,
         border=F ,
         main=plot_title,
         ylim=c(0,(length(variable_to_plot))),
         xlab="cummulative monthly pr [mm]",
         xlim=c(min(variable_to_plot),(floor(max(variable_to_plot))+1)))
    
  } else if (month_number==2){
    plot_title<-"february"
    variable_to_plot<-df_mean_monthly_values_interest_area_february$mean
    
    number_of_breaks_for_hist<-floor(length(variable_to_plot)/2)+1
    
    # Layout to split the screen
    layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
    
    # Draw the boxplot and the histogram 
    par(mar=c(0, 3.1, 1.1, 2.1))
    boxplot(variable_to_plot,
            horizontal=TRUE,
            ylim=c(min(variable_to_plot),(floor(max(variable_to_plot))+1)),
            xaxt="n" ,
            col='deepskyblue',
            frame=F)
    par(mar=c(4, 3.1, 1.1, 2.1))
    hist(variable_to_plot,
         breaks=number_of_breaks_for_hist,
         col=rgb(0.2,0.8,0.5,0.5) ,
         border=F ,
         main=plot_title,
         ylim=c(0,(length(variable_to_plot))),
         xlab="cummulative monthly pr [mm]",
         xlim=c(min(variable_to_plot),(floor(max(variable_to_plot))+1)))
    
    
  } else if (month_number==3){
    plot_title<-"march"
    variable_to_plot<-df_mean_monthly_values_interest_area_march$mean
    
    number_of_breaks_for_hist<-floor(length(variable_to_plot)/2)+1
    
    # Layout to split the screen
    layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
    
    # Draw the boxplot and the histogram 
    par(mar=c(0, 3.1, 1.1, 2.1))
    boxplot(variable_to_plot,
            horizontal=TRUE,
            ylim=c(min(variable_to_plot),(floor(max(variable_to_plot))+1)),
            xaxt="n" ,
            col='deepskyblue',
            frame=F)
    par(mar=c(4, 3.1, 1.1, 2.1))
    hist(variable_to_plot,
         breaks=number_of_breaks_for_hist,
         col=rgb(0.2,0.8,0.5,0.5) ,
         border=F ,
         main=plot_title,
         ylim=c(0,(length(variable_to_plot))),
         xlab="cummulative monthly pr [mm]",
         xlim=c(min(variable_to_plot),(floor(max(variable_to_plot))+1)))
    
  } else if (month_number==4){
    plot_title<-"april"
    variable_to_plot<-df_mean_monthly_values_interest_area_april$mean
    
    number_of_breaks_for_hist<-floor(length(variable_to_plot)/2)+1
    
    # Layout to split the screen
    layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
    
    # Draw the boxplot and the histogram 
    par(mar=c(0, 3.1, 1.1, 2.1))
    boxplot(variable_to_plot,
            horizontal=TRUE,
            ylim=c(min(variable_to_plot),(floor(max(variable_to_plot))+1)),
            xaxt="n" ,
            col='deepskyblue',
            frame=F)
    par(mar=c(4, 3.1, 1.1, 2.1))
    hist(variable_to_plot,
         breaks=number_of_breaks_for_hist,
         col=rgb(0.2,0.8,0.5,0.5) ,
         border=F ,
         main=plot_title,
         ylim=c(0,(length(variable_to_plot))),
         xlab="cummulative monthly pr [mm]",
         xlim=c(min(variable_to_plot),(floor(max(variable_to_plot))+1)))
    
  } else if (month_number==5){
    plot_title<-"may"
    variable_to_plot<-df_mean_monthly_values_interest_area_may$mean
    
    number_of_breaks_for_hist<-floor(length(variable_to_plot)/2)+1
    
    # Layout to split the screen
    layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
    
    # Draw the boxplot and the histogram 
    par(mar=c(0, 3.1, 1.1, 2.1))
    boxplot(variable_to_plot,
            horizontal=TRUE,
            ylim=c(min(variable_to_plot),(floor(max(variable_to_plot))+1)),
            xaxt="n" ,
            col='deepskyblue',
            frame=F)
    par(mar=c(4, 3.1, 1.1, 2.1))
    hist(variable_to_plot,
         breaks=number_of_breaks_for_hist,
         col=rgb(0.2,0.8,0.5,0.5) ,
         border=F ,
         main=plot_title,
         ylim=c(0,(length(variable_to_plot)-10)),
         xlab="cummulative monthly pr [mm]",
         xlim=c(min(variable_to_plot),(floor(max(variable_to_plot))+1)))
    
  } else if (month_number==6){
    plot_title<-"june"
    variable_to_plot<-df_mean_monthly_values_interest_area_june$mean
    
    number_of_breaks_for_hist<-floor(length(variable_to_plot)/2)+1
    
    # Layout to split the screen
    layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
    
    # Draw the boxplot and the histogram 
    par(mar=c(0, 3.1, 1.1, 2.1))
    boxplot(variable_to_plot,
            horizontal=TRUE,
            ylim=c(min(variable_to_plot),(floor(max(variable_to_plot))+1)),
            xaxt="n" ,
            col='deepskyblue',
            frame=F)
    par(mar=c(4, 3.1, 1.1, 2.1))
    hist(variable_to_plot,
         breaks=number_of_breaks_for_hist,
         col=rgb(0.2,0.8,0.5,0.5) ,
         border=F ,
         main=plot_title,
         ylim=c(0,(length(variable_to_plot))),
         xlab="cummulative monthly pr [mm]",
         xlim=c(min(variable_to_plot),(floor(max(variable_to_plot))+1)))
     
  } else if (month_number==7){
    plot_title<-"july"
    variable_to_plot<-df_mean_monthly_values_interest_area_july$mean
    
    number_of_breaks_for_hist<-floor(length(variable_to_plot)/2)+1
    
    # Layout to split the screen
    layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
    
    # Draw the boxplot and the histogram 
    par(mar=c(0, 3.1, 1.1, 2.1))
    boxplot(variable_to_plot,
            horizontal=TRUE,
            ylim=c(min(variable_to_plot),(floor(max(variable_to_plot))+1)),
            xaxt="n" ,
            col='deepskyblue',
            frame=F)
    par(mar=c(4, 3.1, 1.1, 2.1))
    hist(variable_to_plot,
         breaks=number_of_breaks_for_hist,
         col=rgb(0.2,0.8,0.5,0.5) ,
         border=F ,
         main=plot_title,
         ylim=c(0,(length(variable_to_plot)-10)),
         xlab="cummulative monthly pr [mm]",
         xlim=c(min(variable_to_plot),(floor(max(variable_to_plot))+5)))
    
  } else if (month_number==8){
    plot_title<-"august"
    variable_to_plot<-df_mean_monthly_values_interest_area_august$mean
    
    number_of_breaks_for_hist<-floor(length(variable_to_plot)/2)+1
    
    # Layout to split the screen
    layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
    
    # Draw the boxplot and the histogram 
    par(mar=c(0, 3.1, 1.1, 2.1))
    boxplot(variable_to_plot,
            horizontal=TRUE,
            ylim=c(min(variable_to_plot),(floor(max(variable_to_plot))+1)),
            xaxt="n" ,
            col='deepskyblue',
            frame=F)
    par(mar=c(4, 3.1, 1.1, 2.1))
    hist(variable_to_plot,
         breaks=number_of_breaks_for_hist,
         col=rgb(0.2,0.8,0.5,0.5) ,
         border=F ,
         main=plot_title,
         ylim=c(0,(length(variable_to_plot))),
         xlab="cummulative monthly pr [mm]",
         xlim=c(min(variable_to_plot),(floor(max(variable_to_plot))+1)))
    
  } else if (month_number==9){
    plot_title<-"september"
    variable_to_plot<-df_mean_monthly_values_interest_area_september$mean
    
    number_of_breaks_for_hist<-floor(length(variable_to_plot)/2)+1
    
    # Layout to split the screen
    layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
    
    # Draw the boxplot and the histogram 
    par(mar=c(0, 3.1, 1.1, 2.1))
    boxplot(variable_to_plot,
            horizontal=TRUE,
            ylim=c(min(variable_to_plot),(floor(max(variable_to_plot))+1)),
            xaxt="n" ,
            col='deepskyblue',
            frame=F)
    par(mar=c(4, 3.1, 1.1, 2.1))
    hist(variable_to_plot,
         breaks=number_of_breaks_for_hist,
         col=rgb(0.2,0.8,0.5,0.5) ,
         border=F ,
         main=plot_title,
         ylim=c(0,(length(variable_to_plot)-10)),
         xlab="cummulative monthly pr [mm]",
         xlim=c(min(variable_to_plot),(floor(max(variable_to_plot))+1)))
    
  } else if (month_number==10){
    plot_title<-"october"
    variable_to_plot<-df_mean_monthly_values_interest_area_october$mean
    
    number_of_breaks_for_hist<-floor(length(variable_to_plot)/2)+1
    
    # Layout to split the screen
    layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
    
    # Draw the boxplot and the histogram 
    par(mar=c(0, 3.1, 1.1, 2.1))
    boxplot(variable_to_plot,
            horizontal=TRUE,
            ylim=c(min(variable_to_plot),(floor(max(variable_to_plot))+1)),
            xaxt="n" ,
            col='deepskyblue',
            frame=F)
    par(mar=c(4, 3.1, 1.1, 2.1))
    hist(variable_to_plot,
         breaks=number_of_breaks_for_hist,
         col=rgb(0.2,0.8,0.5,0.5) ,
         border=F ,
         main=plot_title,
         ylim=c(0,(length(variable_to_plot))),
         xlab="cummulative monthly pr [mm]",
         xlim=c(min(variable_to_plot),(floor(max(variable_to_plot))+1)))
    
  } else if (month_number==11){
    plot_title<-"november"
    variable_to_plot<-df_mean_monthly_values_interest_area_november$mean
    
    number_of_breaks_for_hist<-floor(length(variable_to_plot)/2)+1
    
    # Layout to split the screen
    layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
    
    # Draw the boxplot and the histogram 
    par(mar=c(0, 3.1, 1.1, 2.1))
    boxplot(variable_to_plot,
            horizontal=TRUE,
            ylim=c(min(variable_to_plot),(floor(max(variable_to_plot))+1)),
            xaxt="n" ,
            col='deepskyblue',
            frame=F)
    par(mar=c(4, 3.1, 1.1, 2.1))
    hist(variable_to_plot,
         breaks=number_of_breaks_for_hist,
         col=rgb(0.2,0.8,0.5,0.5) ,
         border=F ,
         main=plot_title,
         ylim=c(0,(length(variable_to_plot))),
         xlab="cummulative monthly pr [mm]",
         xlim=c(min(variable_to_plot),(floor(max(variable_to_plot))+1)))
      
  } else if (month_number==12){
    plot_title<-"december"
    variable_to_plot<-df_mean_monthly_values_interest_area_december$mean
    
    number_of_breaks_for_hist<-floor(length(variable_to_plot)/2)+1
    
    # Layout to split the screen
    layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
    
    # Draw the boxplot and the histogram 
    par(mar=c(0, 3.1, 1.1, 2.1))
    boxplot(variable_to_plot,
            horizontal=TRUE,
            ylim=c(min(variable_to_plot),(floor(max(variable_to_plot))+1)),
            xaxt="n" ,
            col='deepskyblue',
            frame=F)
    par(mar=c(4, 3.1, 1.1, 2.1))
    hist(variable_to_plot,
         breaks=number_of_breaks_for_hist,
         col=rgb(0.2,0.8,0.5,0.5) ,
         border=F ,
         main=plot_title,
         ylim=c(0,(length(variable_to_plot)-10)),
         xlab="cummulative monthly pr [mm]",
         xlim=c(min(variable_to_plot),(floor(max(variable_to_plot))+1)))
    
  }  
  # clean console
  cat("\014")
}
# clean console
cat("\014")
