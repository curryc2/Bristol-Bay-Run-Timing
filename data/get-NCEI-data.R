
####Airport data
####GSODR no longer works, so I had to find a new way to access the airport data.  Also, this is the hourly data
#so now we have more information, including wind direction!  So I will update the data structure accordingly.  
#This file only dowloads the csv files for get-airport-data to then pull in.

library(here)

setwd(here())


station <- "USW00025503"   # King Salmon Airport
years <- 1963:2025

for(y in years){
  url <- paste0(
    "https://www.ncei.noaa.gov/oa/local-climatological-data/v2/access/",
    y,
    "/LCD_",
    station,
    "_",
    y,
    ".csv"
  )
  
  dest <- paste0("C:/Users/Student/Desktop/Bristol-Bay-Run-Timing/Data/Airport data/", "King_Salmon_Airport", "_", y, ".csv")
  
  download.file(url, destfile = dest, mode = "wb")
}


