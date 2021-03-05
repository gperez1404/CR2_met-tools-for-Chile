
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#                                 Fixing dates for a pre-existing ncdf file
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# when loading the original ncdf file for "pr"  there is a problem with 6 sections of dates 
# This error is unexplicable and if you don't fix it, the rest of the analysis won't work

# if you suspect the rasterbrickfile is corrupted you need to check the vector (list) with dates:

# date labels for each precipitation raster are in the following list:
vector_with_date_names<-names(rasterbrick_with_all_chile)

# This is the list of potentially problematic sections:

#      Time periods              index
# 1994.10.11 - 1994.10.30     5763 - 5782
# 1995.01.02 - 1995.01.10     5846 - 5854
# 1995.02.10 - 1995.02.27     5885 - 5902
# 1995.07.06 - 1995.07.11     6031 - 6036  
# 1995.07.21 - 1995.07.23     6046 - 6048
# 1995.07.26 - 1995.08.12     6051 - 6068

# visual assessment of the loaded data

plot(rasterbrick_with_all_chile[[5765]])
plot(rasterbrick_with_all_chile[[5850]])
plot(rasterbrick_with_all_chile[[5900]])
plot(rasterbrick_with_all_chile[[6035]])
plot(rasterbrick_with_all_chile[[6060]])
plot(rasterbrick_with_all_chile[[13000]])


# Delete all plots 
dev.off(dev.list()["RStudioGD"])

# if you want to inspect the date values externally using excel you need to execute the 
# following lines:

# saving a file with the list of dates for each raster:
csv_file_with_time_names <- "Original_rasterbrick_dates_column_before_corrections.csv"
saving_directory <- paste(execution_directory,"results-files", sep="/") 
csv_file_with_time_names_full_name<- paste(saving_directory,csv_file_with_time_names, sep ="/")

setwd(saving_directory)
write.table(na.omit(vector_with_date_names), csv_file_with_time_names_full_name, row.names = FALSE, sep = ",")
setwd(execution_directory)

#if  you found errors in the dates names you need to execute the following code 
#for fixing the problems:

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! WARNING 
#  WARNING: THIS CAN TAKE SOME TIME TO RUN !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! WARNING

start_index<- 1
end_index <- nlayers(rasterbrick)

# to meassure how much time does it take to execute this part of the code use the following instruction
time_before_executing<-format(Sys.time(), "%a %b %d %X %Y %Z")
rasterbrick<-fix_CR2_date_values_for_a_rasterbrick(start_index,end_index,rasterbrick)
time_after_executing<-format(Sys.time(), "%a %b %d %X %Y %Z")

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! WARNING 
#  WARNING: THIS CAN TAKE SOME TIME TO RUN !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! WARNING


# Clean the consOle
cat("\014")

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#                                   End of Fixing dates for a pre-existing ncdf file
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
