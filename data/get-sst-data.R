library(terra)
library(ncdf4)
library(raster)
library(here)
library(ncdf4)
library(lubridate)
library(dplyr)



years <- 1982:2025
julian <- 1:365

#Create data frame to store data for GOA
SST_GOA <- as.data.frame(
  matrix(0,
         nrow = length(julian),
         ncol = length(years))
)
colnames(SST_GOA) <- years
# Add Julian day
SST_GOA$Julian_Day <- julian
SST_GOA <- SST_GOA[, c("Julian_Day", as.character(years))]



#loop through files to create daily average data frame 

for (yr in years) {
  file <- file.path(here("data/SST_GOA"),paste0("sstGOA_", yr, ".nc"))
  r <- rast(file) #pull in data layer
  
  daily_mean <- global(r, mean, na.rm=TRUE) #calculate mean for each day
  
  column <- as.character(yr)#index to save to data frame for each year
  SST_GOA[,column] <- daily_mean
}

#Warning is due to leap years, I think this can be ignnored as february isnt important, 
#but the whole year is here for completeness


#repeat for Bristol Bay data 

#Create data frame to store data for GOA
SST_BB <- as.data.frame(
  matrix(0,
         nrow = length(julian),
         ncol = length(years))
)
colnames(SST_BB) <- years
# Add Julian day
SST_BB$Julian_Day <- julian
SST_BB <- SST_BB[, c("Julian_Day", as.character(years))]



#loop through files to create daily average data frame 

for (yr in years) {
  file <- file.path(here("data/SST_BristolBay"),paste0("sstBB_", yr, ".nc"))
  r <- rast(file) #pull in data layer
  
  daily_mean <- global(r, mean, na.rm=TRUE) #calculate mean for each day
  
  column <- as.character(yr)#index to save to data frame for each year
  SST_BB[,column] <- daily_mean
}

#Warning is due to leap years, I think this can be ignnored as february isnt important, 
#but the whole year is here for completeness



#######################################

#now for testing, lets summarize these for a given time period

SpringGOA <- filter(SST_GOA, Julian_Day >90 & Julian_Day< 152) # April and May
year_means <- colMeans(SpringGOA[, -1], na.rm = TRUE)

# Make it a nice data frame
SpringGOA_mean <- data.frame(
  YEAR = as.integer(colnames(SpringGOA)[-1]),
  GOA_SpringSST = year_means
)

JuneBB <- filter(SST_BB, Julian_Day >151 & Julian_Day< 182) # June
year_means <- colMeans(JuneBB[, -1], na.rm = TRUE)

# Make it a nice data frame
JuneBB_mean <- data.frame(
  YEAR = as.integer(colnames(JuneBB)[-1]),
  BB_JuneSST = year_means
)




