# this script opens the ncdf file with CR2_met product and generates two rasterbricks with:
#                 Total annual  pr for each year
#                 MAximum daily pr for each year


#if your are running in the hard drive of your personal laptop PC:
#setwd("C:/00-C-GPM_INFO/04-C-01-R/04-C-01-03-ARCLIM")
load_libraries()

# if you run this on the ARCLIM project
output_directory_nc_files<-"06-Results-nc"
output_directory_rasters<-"06-Results-rasters"


setwd("C:/00-C-GPM_INFO/04-C-RDM/04-C-01-R/04-C-01-02-CR2-DAP")
setwd("C:/00-C-GPM_INFO/04-C-Bk_R/04-C-01-R/04-C-01-03-ARCLIM")
main_directory<- getwd()
loading_directory<- paste(main_directory,"05-Inputs-nc", sep ="/")

#if you run this for the CR2 Data Analysis:
output_directory_nc_files<-"07-Results-files"
output_directory_rasters<-"08-Result-rasters"

setwd("C:/00-C-GPM_INFO/04-C-Bk_R/04-C-01-R/04-C-01-02-CR2-DAP")
main_directory<- getwd()
loading_directory<- paste(main_directory,"04-Input-files", sep ="/")

# Here you load the input CR2_met product
nc_filename<-"CR2MET_v1.4.2_pr.nc"

#name of the interest area:
area_name<-"chile"

# Here you laod the net cdf file
setwd(loading_directory)
rasterbrick_with_daily_data<-brick(nc_filename)
setwd(main_directory)

# Here you generate a list of years for the annual rasterbrick
number_of_days<-nlayers(rasterbrick_with_daily_data)

first_year_text<-gsub("X","",strsplit(names(rasterbrick_with_daily_data[[1]]),".",fixed=TRUE)[[1]][1])
first_year<-as.numeric(first_year_text)

last_year_text<-gsub("X","",strsplit(names(rasterbrick_with_daily_data[[number_of_days]]),".",fixed=TRUE)[[1]][1])
last_year<-as.numeric(last_year_text)

#list of years
list_of_years<- seq(first_year,last_year, by=1)

# here you habiliatae a independet window for graphics
options(device = "windows")


# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! WARNING
# YOU NEED TO CLOSE ALL THE OTHER APPS AND FREE UP 25GB OF RAM TO RUN THIS    
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! WARNING
time_before_executing<- Sys.time()

# Here you fix all the missing values inside the rasterbrick :

print("correcting NAs values....")

# here you divide the raterbrick in six subsets to correct the NAs values


#p1
index_start_p1<-1
index_end_p1<-as.integer(number_of_days/6)
rasterbrick_subset_1<-subset(rasterbrick_with_daily_data,
                             index_start_p1:index_end_p1,
                             drop=TRUE)
#p2
index_start_p2<-index_end_p1+1
index_end_p2<-as.integer((number_of_days/6)*2)
rasterbrick_subset_2<-subset(rasterbrick_with_daily_data,
                             index_start_p2:index_end_p2,
                             drop=TRUE)
#p3
index_start_p3<-index_end_p2+1
index_end_p3<-as.integer((number_of_days/6)*3)
rasterbrick_subset_3<-subset(rasterbrick_with_daily_data,
                             index_start_p3:index_end_p3,
                             drop=TRUE)

#p4
index_start_p4<-index_end_p3+1
index_end_p4<-as.integer((number_of_days/6)*4)
rasterbrick_subset_4<-subset(rasterbrick_with_daily_data,
                             index_start_p4:index_end_p4,
                             drop=TRUE)

#p5
index_start_p5<-index_end_p4+1
index_end_p5<-as.integer((number_of_days/6)*5)
rasterbrick_subset_5<-subset(rasterbrick_with_daily_data,
                             index_start_p5:index_end_p5,
                             drop=TRUE)

#p6
index_start_p6<-index_end_p5+1
index_end_p6<-number_of_days
rasterbrick_subset_6<-subset(rasterbrick_with_daily_data,
                             index_start_p6:index_end_p6,
                             drop=TRUE)

rm(rasterbrick_with_daily_data)
gc()

rm(index_start_p1)
rm(index_end_p1)

rm(index_start_p2)
rm(index_end_p2)

rm(index_start_p3)
rm(index_end_p3)

rm(index_start_p4)
rm(index_end_p4)

rm(index_start_p5)
rm(index_end_p5)

rm(index_start_p6)
rm(index_end_p6)

values(rasterbrick_subset_1)[values(rasterbrick_subset_1) > 3000] = NA
gc()
print(paste("16.66 % of ",area_name  ," values corrected....",sep=""))

values(rasterbrick_subset_2)[values(rasterbrick_subset_2) > 3000] = NA
gc()
print(paste("33.33 % of ",area_name  ," values corrected....",sep=""))

values(rasterbrick_subset_3)[values(rasterbrick_subset_3) > 3000] = NA
gc()
print(paste("50 % of ",area_name  ," values corrected....",sep=""))

rasterstack1<-stack(rasterbrick_subset_1,
                    rasterbrick_subset_2,
                    rasterbrick_subset_3)

rm(rasterbrick_subset_1)
rm(rasterbrick_subset_2)
rm(rasterbrick_subset_3)
gc()

values(rasterbrick_subset_4)[values(rasterbrick_subset_4) > 3000] = NA
gc()
print(paste("66.66 % of ",area_name  ," values corrected....",sep=""))

values(rasterbrick_subset_5)[values(rasterbrick_subset_5) > 3000] = NA
gc()
print(paste("83.33 % of ",area_name  ," values corrected....",sep=""))

values(rasterbrick_subset_6)[values(rasterbrick_subset_6) > 3000] = NA
gc()
print(paste("100 % of ",area_name  ," values corrected....",sep=""))

rasterstack2<-stack(rasterbrick_subset_4,
                    rasterbrick_subset_5,
                    rasterbrick_subset_6)

# Here you remove all the variables to free up some RAM
rm(rasterbrick_subset_4)
rm(rasterbrick_subset_5)
rm(rasterbrick_subset_6)
gc()

rasterbrick<-brick(stack(rasterstack1,rasterstack2))

rm(rasterstack1,rasterstack2)
gc()

elapsed_time<-time_diff(time_before_executing)
print(paste("Elapsed time for corrections [hh:mm:ss]",elapsed_time,sep=" "))

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! WARNING
# YOU NEED TO CLOSE ALL THE OTHER APPS AND FREE UP 25GB OF RAM TO RUN THIS    
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! WARNING


# @-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@
#                                                  Loop for total annual pr
# @-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@

first_day_date<-names(rasterbrick)[[1]]
last_day_date<-names(rasterbrick)[[number_of_days]]


first_day_date <-sub(".", "", first_day_date)
last_day_date  <-sub(".", "", last_day_date)

first_day_date<-str_replace_all(first_day_date, "[[:punct:]]", "-")
last_day_date<-str_replace_all(last_day_date, "[[:punct:]]", "-")

list_with_date_components<-strsplit(first_day_date,"-")

year_number<- as.numeric(list_with_date_components[[1]][1])
month_number<- as.numeric(list_with_date_components[[1]][2])
day_number<- as.numeric(list_with_date_components[[1]][3])

date_first_day<-ISOdate(year = year_number, month = month_number, day = day_number)
first_year_as_number<-year_number

list_with_date_components<-strsplit(last_day_date,"-")
year_number<- as.numeric(list_with_date_components[[1]][1])
month_number<- as.numeric(list_with_date_components[[1]][2])
day_number<- as.numeric(list_with_date_components[[1]][3])

date_last_day<-ISOdate(year = year_number, month = month_number, day = day_number)
last_year_as_number<-year_number

list_of_years<- seq(first_year_as_number,last_year_as_number, by=1)

# Number of years in the original rasterbrick:
number_of_years<-length(list_of_years)

# Here you initialize the rasterbrcik before filling it with the annual value rasters
rasterbrick_annual_values<-brick()


#  Main loop 
# Thi loop is over all the elements of the input rasterbrick:
print("                            ")
print("----------------------------")
print("                            ")
print("creating rasterbrick with annual pr values")
time_before_executing<- Sys.time()
for (i in 1:number_of_years) {
  
  current_year<- i + 1978
  
  # date of the first day of the interest year [yyyy-mm-dd]:  
  date_text_first_day_of_interest_year<-paste(toString(current_year),"-01-01",sep="")
  
  index_first_day_of_the_interest_year<-return_CR2_index(date_text_first_day_of_interest_year,
                                                         rasterbrick) 
  
  #assuming the year is complete you just need to find the last day of the current year and that is : 
  #                 31st of December
  
  date_text_last_day_of_the_current_year<-paste(toString(current_year),"-12-31",sep="")
  
  index_last_day_of_the_interest_year<-return_CR2_index(date_text_last_day_of_the_current_year,
                                                        rasterbrick)  
  
  # however, for some reason some years may not be complet so you need to find the 
  # closest date which is : 30th of December
  
  try(if(is.na(index_last_day_of_the_interest_year)) 
    index_last_day_of_the_interest_year<-return_CR2_index((paste(toString(year_of_interest),"-12-30",sep="")),condition, rasterbrick))
  
  # here you generate the list of values for the year:
  list_of_indices<-seq(index_first_day_of_the_interest_year,index_last_day_of_the_interest_year, by=1)
  
  rasterbrick_subset<-subset(rasterbrick,list_of_indices, drop=TRUE)
  
  current_year_annual_raster <-sum(rasterbrick_subset) 
  
  print(paste("year:",current_year,"  ",i,"out of",number_of_years, sep=" "))
  print("Annual pr value calculated succesfully !")
  elapsed_time<-time_diff(time_before_executing)
  print(paste("Execution time [hh:mm:ss]",elapsed_time,sep=" "))
  
  rasterbrick_annual_values<-addLayer(rasterbrick_annual_values,
                                      current_year_annual_raster,
                                      i)
  rm(current_year_annual_raster)
  rm(rasterbrick_subset)
  gc()
}

elapsed_time<-time_diff(time_before_executing)
print(paste("Execution time [hh:mm:ss]",elapsed_time,sep=" "))


names(rasterbrick_annual_values)<-list_of_years
print("Annual pr rasterbrick generated succesfully !!")

# @-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@
#      ^       ^       ^        ^         ^        Loop for total annual pr
# @-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@

saving_location<-paste(main_directory,output_directory_nc_files, sep="/")
setwd(saving_location)


nc_filename <- "total_annual_pr_CR2_met.nc"

saving_location<-paste(main_directory,output_directory_nc_files,nc_filename, sep="/")
varunits_to_save<-"[mm/year]"
variable_long_name<-"Annual_precipitation"
time_units<-"year"

writeRaster(rasterbrick_annual_values, 
            saving_location,
            overwrite=TRUE,
            format="CDF",     
            varname="pr", 
            varunit="[mm/day]", 
            longname="precipitation", 
            xname="Longitude",   
            yname="Latitude", 
            zname=time_units)

print("rasterbrick with annual pr values saved successfully !!")


# @-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@
#                                                  Loop for max  annual pr
# @-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@


# Here you initialize the rasterbrcik before filling it with the annual value rasters
rasterbrick_max_annual_pr<-brick()

#  Main loop 
# Thi loop is over all the elements of the input rasterbrick:
print("                            ")
print("----------------------------")
print("                            ")
print("creating rasterbrick with max annual pr values")
time_before_executing<- Sys.time()
for (i in 1:number_of_years) {
  
  current_year<- i + 1978
  
  # date of the first day of the interest year [yyyy-mm-dd]:  
  date_text_first_day_of_interest_year<-paste(toString(current_year),"-01-01",sep="")
  
  index_first_day_of_the_interest_year<-return_CR2_index(date_text_first_day_of_interest_year,
                                                         rasterbrick) 
  
  #assuming the year is complete you just need to find the last day of the current year and that is : 
  #                 31st of December
  
  date_text_last_day_of_the_current_year<-paste(toString(current_year),"-12-31",sep="")
  
  index_last_day_of_the_interest_year<-return_CR2_index(date_text_last_day_of_the_current_year,
                                                        rasterbrick)  
  
  # however, for some reason some years may not be complet so you need to find the 
  # closest date which is : 30th of December
  
  try(if(is.na(index_last_day_of_the_interest_year)) 
    index_last_day_of_the_interest_year<-return_CR2_index((paste(toString(year_of_interest),"-12-30",sep="")),condition, rasterbrick))
  
  # here you generate the list of values for the year:
  list_of_indices<-seq(index_first_day_of_the_interest_year,index_last_day_of_the_interest_year, by=1)
  
  rasterbrick_subset<-subset(rasterbrick,list_of_indices, drop=TRUE)
  
  current_year_max_raster <-max(rasterbrick_subset) 
  
  print(paste("year:",current_year,"  ",i,"out of",number_of_years, sep=" "))
  print("max pr value calculated succesfully !")
  elapsed_time<-time_diff(time_before_executing)
  print(paste("Execution time [hh:mm:ss]",elapsed_time,sep=" "))
  
  rasterbrick_max_annual_pr<-addLayer(rasterbrick_max_annual_pr,
                                      current_year_max_raster,
                                      i)
  rm(current_year_max_raster)
  rm(rasterbrick_subset)
  gc()
}

elapsed_time<-time_diff(time_before_executing)
print(paste("Execution time [hh:mm:ss]",elapsed_time,sep=" "))

names(rasterbrick_max_annual_pr)<-list_of_years
print("MAximum pr rasterbrick generated succesfully !!")

# @-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@
#      ^       ^       ^        ^         ^        Loop for max  annual pr
# @-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@

nc_filename <- "maximum_daily_pr_CR2_met.nc"

saving_location<-paste(main_directory,output_directory_nc_files,nc_filename, sep="/")
varunits_to_save<-"[mm/day]"
variable_long_name<-"maximum_daily_precipitation"
time_units<-"year"

writeRaster(rasterbrick_max_annual_pr, 
            saving_location,
            overwrite=TRUE,
            format="CDF",     
            varname="pr", 
            varunit="[mm/day]", 
            longname="precipitation", 
            xname="Longitude",   
            yname="Latitude", 
            zname=time_units)

print("rasterbrick with max daily pr values saved successfully !!")

# ###################################
# ###################################################
# ##################################################################
# Here yoru create the eman pr rasters for 1980-2010 to compare with ARCLIM
# #########################################################################
# ###################################################################################
# ###########################################################################################

subset_1980_2010_annual_pr<-subset(rasterbrick_annual_values,2:32,drop=TRUE)
subset_1980_2010_max_daily_pr<-subset(rasterbrick_max_annual_pr,2:32,drop=TRUE)

mean_annual_pr_1980_2010<-mean(subset_1980_2010_annual_pr)
max_daily_pr_1980_2010<-mean(subset_1980_2010_max_daily_pr)

# S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S
#                                                                                save results
# S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S

# here you saved the newly created mean rasters:

saving_location<-paste(main_directory,output_directory_rasters, sep="/")
setwd(saving_location)
rasterfile_extension <- ".tif"

file_name_with_extension <- paste("mean_annual_pr_1980_2010_CR2met",rasterfile_extension, sep ="")
writeRaster(mean_annual_pr_1980_2010, filename=file_name_with_extension, overwrite=TRUE)

file_name_with_extension <- paste("max_daily_pr_1980_2010_CR2met",rasterfile_extension, sep ="")
writeRaster(max_daily_pr_1980_2010, filename=file_name_with_extension, overwrite=TRUE)

setwd(main_directory)

rm(saving_location)
rm(rasterfile_extension)
rm(file_name_with_extension)

# S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S
#                                                                                save results
# S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S-S



