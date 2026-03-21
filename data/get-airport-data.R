
####Airport data read 

library(here)
library(dplyr)

setwd(here())


years <- 1963:2025  #data years

for (yr in years) {
  file <- file.path(here("data/Airport data"),paste0("King_Salmon_Airport_", yr, ".csv"))#set to file location
  if (yr==1963){
    king_salmon_data <- read.csv(file)
  }
  else {
    data <- read.csv(file)
    king_salmon_data <- rbind(king_salmon_data,data)
  }
}#combine all files into one data frame

library(lubridate)
king_salmon_data$datetime <- ymd_hms(king_salmon_data$DATE)
king_salmon_data$DATE <- as.Date(king_salmon_data$datetime)
king_salmon_data$TIME <- format(king_salmon_data$datetime, "%H:%M:%S")
king_salmon_data$YEAR <- year(king_salmon_data$DATE)
king_salmon_data$MONTH <- month(king_salmon_data$DATE)
king_salmon_data$DAY <- day(king_salmon_data$DATE)

Airport_dat <- dplyr::select(king_salmon_data,DATE,TIME,YEAR,MONTH,DAY,HourlyWindDirection,HourlyWindSpeed,
                             HourlySeaLevelPressure,HourlyDryBulbTemperature) ##select and compile data 


Airport_dat$HourlyWindDirection <- as.numeric(Airport_dat$HourlyWindDirection)
Airport_dat$HourlyWindSpeed <- as.numeric(Airport_dat$HourlyWindSpeed)
options(scipen = 999)


Airport_dat <- Airport_dat %>%  #Calculate east and north wind components
  mutate(
    theta = HourlyWindDirection * pi / 180,      
    Wind_East = - HourlyWindSpeed * sin(theta),          # east-west 
    Wind_North = - HourlyWindSpeed * cos(theta)           # north-south 
  ) %>% dplyr::select(-theta)  


