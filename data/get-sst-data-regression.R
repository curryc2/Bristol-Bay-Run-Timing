library(terra)
library(ncdf4)
library(raster)
library(here)
library(ncdf4)
library(lubridate)
library(dplyr)



years <- 1982:2025
julian <- 1:365


#repeat for Ugashik

#Create data frame to store data 
SST_Ugashik <- as.data.frame(
  matrix(0,
         nrow = length(julian),
         ncol = length(years))
)
colnames(SST_Ugashik) <- years
# Add Julian day
SST_Ugashik$Julian_Day <- julian
SST_Ugashik <- SST_Ugashik[, c("Julian_Day", as.character(years))]



#loop through files to create daily average data frame 

for (yr in years) {
  file <- file.path(here("data/SST data/SST_Ugashik"),paste0("sstUgashik_", yr, ".nc"))
  r <- rast(file) #pull in data layer
  
  daily_mean <- global(r, mean, na.rm=TRUE) #calculate mean for each day
  
  column <- as.character(yr)#index to save to data frame for each year
  SST_Ugashik[,column] <- daily_mean
}

#repeat for Egegik

#Create data frame to store data 
SST_Egegik <- as.data.frame(
  matrix(0,
         nrow = length(julian),
         ncol = length(years))
)
colnames(SST_Egegik) <- years
# Add Julian day
SST_Egegik$Julian_Day <- julian
SST_Egegik <- SST_Egegik[, c("Julian_Day", as.character(years))]



#loop through files to create daily average data frame 

for (yr in years) {
  file <- file.path(here("data/SST data/SST_Egegik"),paste0("sstEgegik_", yr, ".nc"))
  r <- rast(file) #pull in data layer
  
  daily_mean <- global(r, mean, na.rm=TRUE) #calculate mean for each day
  
  column <- as.character(yr)#index to save to data frame for each year
  SST_Egegik[,column] <- daily_mean
}



#repeat for Kvichak

#Create data frame to store data 
SST_Kvichak <- as.data.frame(
  matrix(0,
         nrow = length(julian),
         ncol = length(years))
)
colnames(SST_Kvichak) <- years
# Add Julian day
SST_Kvichak$Julian_Day <- julian
SST_Kvichak <- SST_Kvichak[, c("Julian_Day", as.character(years))]



#loop through files to create daily average data frame 

for (yr in years) {
  file <- file.path(here("data/SST data/SST_Kvichak"),paste0("sstKvichak_", yr, ".nc"))
  r <- rast(file) #pull in data layer
  
  daily_mean <- global(r, mean, na.rm=TRUE) #calculate mean for each day
  
  column <- as.character(yr)#index to save to data frame for each year
  SST_Kvichak[,column] <- daily_mean
}



#repeat for Nushagak

#Create data frame to store data 
SST_Nushagak <- as.data.frame(
  matrix(0,
         nrow = length(julian),
         ncol = length(years))
)
colnames(SST_Nushagak) <- years
# Add Julian day
SST_Nushagak$Julian_Day <- julian
SST_Nushagak <- SST_Nushagak[, c("Julian_Day", as.character(years))]



#loop through files to create daily average data frame 

for (yr in years) {
  file <- file.path(here("data/SST data/SST_Nushagak"),paste0("sstNushagak_", yr, ".nc"))
  r <- rast(file) #pull in data layer
  
  daily_mean <- global(r, mean, na.rm=TRUE) #calculate mean for each day
  
  column <- as.character(yr)#index to save to data frame for each year
  SST_Nushagak[,column] <- daily_mean
}



#repeat for Togiak
#Create data frame to store data 
SST_Togiak <- as.data.frame(
  matrix(0,
         nrow = length(julian),
         ncol = length(years))
)
colnames(SST_Togiak) <- years
# Add Julian day
SST_Togiak$Julian_Day <- julian
SST_Togiak <- SST_Togiak[, c("Julian_Day", as.character(years))]



#loop through files to create daily average data frame 

for (yr in years) {
  file <- file.path(here("data/SST data/SST_Togiak"),paste0("sstTogiak_", yr, ".nc"))
  r <- rast(file) #pull in data layer
  
  daily_mean <- global(r, mean, na.rm=TRUE) #calculate mean for each day
  
  column <- as.character(yr)#index to save to data frame for each year
  SST_Togiak[,column] <- daily_mean
}


#######################################

#now for testing, lets summarize these for a given time period


Ugashik <- filter(SST_Ugashik, Julian_Day >160 & Julian_Day< 211) #range of CE days
year_means <- colMeans(Ugashik[, -1], na.rm = TRUE)

# Make it a nice data frame
Ugashik_mean <- data.frame(
  YEAR = as.integer(colnames(Ugashik)[-1]),
  Ugashik_SST = year_means
)

Egegik <- filter(SST_Egegik, Julian_Day >160 & Julian_Day< 211) 
year_means <- colMeans(Egegik[, -1], na.rm = TRUE)

# Make it a nice data frame
Egegik_mean <- data.frame(
  YEAR = as.integer(colnames(Egegik)[-1]),
  Egegik_SST = year_means
)

Kvichak <- filter(SST_Kvichak, Julian_Day >160 & Julian_Day< 211) 
year_means <- colMeans(Kvichak[, -1], na.rm = TRUE)

# Make it a nice data frame
Kvichak_mean <- data.frame(
  YEAR = as.integer(colnames(Kvichak)[-1]),
  Kvichak_SST = year_means
)

Nushagak <- filter(SST_Nushagak, Julian_Day >160 & Julian_Day< 211) 
year_means <- colMeans(Nushagak[, -1], na.rm = TRUE)

# Make it a nice data frame
Nushagak_mean <- data.frame(
  YEAR = as.integer(colnames(Nushagak)[-1]),
  Nushagak_SST = year_means
)

Togiak <- filter(SST_Togiak, Julian_Day >160 & Julian_Day< 211) 
year_means <- colMeans(Togiak[, -1], na.rm = TRUE)

# Make it a nice data frame
Togiak_mean <- data.frame(
  YEAR = as.integer(colnames(Togiak)[-1]),
  Togiak_SST = year_means
)



SST_means <- full_join(Ugashik_mean, Egegik_mean, by= "YEAR")
SST_means <- full_join(SST_means, Kvichak_mean, by= "YEAR")
SST_means <- full_join(SST_means, Nushagak_mean, by= "YEAR")
SST_means <- full_join(SST_means, Togiak_mean, by= "YEAR")




