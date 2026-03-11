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
  file <- file.path(here("data/SST data/SST_GOA"),paste0("sstGOA_", yr, ".nc"))
  r <- rast(file) #pull in data layer
  
  daily_mean <- global(r, mean, na.rm=TRUE) #calculate mean for each day
  
  column <- as.character(yr)#index to save to data frame for each year
  SST_GOA[,column] <- daily_mean
}

#Warning is due to leap years, I think this can be ignnored as february isnt important, 
#but the whole year is here for completeness


#repeat for Bristol Bay data 

#Create data frame to store data 
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
  file <- file.path(here("data/SST data/SST_BristolBay"),paste0("sstBB_", yr, ".nc"))
  r <- rast(file) #pull in data layer
  
  daily_mean <- global(r, mean, na.rm=TRUE) #calculate mean for each day
  
  column <- as.character(yr)#index to save to data frame for each year
  SST_BB[,column] <- daily_mean
}


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


JuneUgashik <- filter(SST_Ugashik, Julian_Day >151 & Julian_Day< 182) # June
year_means <- colMeans(JuneUgashik[, -1], na.rm = TRUE)

# Make it a nice data frame
JuneUgashik_mean <- data.frame(
  YEAR = as.integer(colnames(JuneUgashik)[-1]),
  Ugashik_JuneSST = year_means
)

JuneEgegik <- filter(SST_Egegik, Julian_Day >151 & Julian_Day< 182) # June
year_means <- colMeans(JuneEgegik[, -1], na.rm = TRUE)

# Make it a nice data frame
JuneEgegik_mean <- data.frame(
  YEAR = as.integer(colnames(JuneEgegik)[-1]),
  Egegik_JuneSST = year_means
)

JuneKvichak <- filter(SST_Kvichak, Julian_Day >151 & Julian_Day< 182) # June
year_means <- colMeans(JuneKvichak[, -1], na.rm = TRUE)

# Make it a nice data frame
JuneKvichak_mean <- data.frame(
  YEAR = as.integer(colnames(JuneKvichak)[-1]),
  Kvichak_JuneSST = year_means
)

JuneNushagak <- filter(SST_Nushagak, Julian_Day >151 & Julian_Day< 182) # June
year_means <- colMeans(JuneNushagak[, -1], na.rm = TRUE)

# Make it a nice data frame
JuneNushagak_mean <- data.frame(
  YEAR = as.integer(colnames(JuneNushagak)[-1]),
  Nushagak_JuneSST = year_means
)

JuneTogiak <- filter(SST_Togiak, Julian_Day >151 & Julian_Day< 182) # June
year_means <- colMeans(JuneTogiak[, -1], na.rm = TRUE)

# Make it a nice data frame
JuneTogiak_mean <- data.frame(
  YEAR = as.integer(colnames(JuneTogiak)[-1]),
  Togiak_JuneSST = year_means
)

SST_means <- full_join(SpringGOA_mean, JuneBB_mean, by= "YEAR")
SST_means <- full_join(SST_means, JuneUgashik_mean, by= "YEAR")
SST_means <- full_join(SST_means, JuneEgegik_mean, by= "YEAR")
SST_means <- full_join(SST_means, JuneKvichak_mean, by= "YEAR")
SST_means <- full_join(SST_means, JuneNushagak_mean, by= "YEAR")
SST_means <- full_join(SST_means, JuneTogiak_mean, by= "YEAR")




