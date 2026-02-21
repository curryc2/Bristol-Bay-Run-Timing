
####Airport data
library(GSODR)
library(here)

setwd(here())


years_KS <- 1963:2025  #data years

#Dillingham stations
#lat <- 59.04549
#lon <- -158.51054
#stations <- nearest_stations(LAT=lat, LON=lon, distance=50)
#Missing data from 1997 to 2006.  Best to stay with the King Salmon Airport data


station_id_king_salmon <- "703260-25503"  # King Salmon Airport station ID


country <- NULL  # You can specify a country, e.g., "USA" if needed
max_missing <- NULL  
agroclimatology <- FALSE

# Fetch air temperature data for King Salmon
king_salmon_data <- get_GSOD(
  years = years_KS,
  station = station_id_king_salmon, 
  country = country,
  max_missing = max_missing,
  agroclimatology = agroclimatology
)


