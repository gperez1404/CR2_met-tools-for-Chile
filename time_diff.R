# This function return a string with the text containign a time difference  

# input parameters

# start_time

time_diff <- function(start_time) {
  diff = as.numeric(difftime(Sys.time(), start_time, units="mins"))
  hr <- diff%/%60
  min <- floor(diff - hr * 60)
  sec <- round(diff%%1 * 60,digits=2)
  
  return(paste(hr,min,sec,sep=':'))
}
#clean console
cat("\014")

