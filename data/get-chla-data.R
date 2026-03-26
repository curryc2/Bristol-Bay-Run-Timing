
library(here)
library(dplyr)

ChlA <- read.csv("data/Chl a data.csv")



#GOA bounds, same as SST.  However, ChlA data only goes to 53 degrees of latitude, so lets cut this down to 54.
# lon_min <- -150
# lon_max <- -140
# lat_min <- 50
# lat_max <- 58

ChlA_GOA  <- ChlA %>% filter(between(lon, -150, -140), between(lat, 54, 58))

ChlA_timing_GOA <- ChlA_GOA %>% filter(type=="tm1") %>% rename(YEAR = year)
ChlA_magnitude_GOA <- ChlA_GOA %>% filter(type=="chlm1") %>% rename(YEAR = year)


ChlA_timing_GOA <- as.data.frame(ChlA_timing_GOA) %>% group_by(YEAR) %>%
  summarise(ChlA_GOAtiming = median(value, na.rm = TRUE))
ChlA_magnitude_GOA <- as.data.frame(ChlA_magnitude_GOA) %>% group_by(YEAR) %>%
  summarise(ChlA_GOAmagnitude = median(value, na.rm = TRUE))
