library (here)
library (lubridate)
library(GSODR)
library (dplyr)

here()#Bristol-Bay-Run-Timing base working directory 


#Start pulling in data, all stored in data subfolder. 

#######Environmental data
source(here("data/get-pdo-enso.r"))

Index_dat <- get_pdo_npgo(years=1963:2025, months=c(1,2,3,4,5), lags=c(0,0,0,0,0)) 
PDO <- as.data.frame(Index_dat$pdo)##Break into data frames to work with
ENSO <-as.data.frame(Index_dat$enso)


source(here("data/get-airport-data.r"))

Airport_dat <- select(king_salmon_data,YEAR,MONTH,DAY,WDSP,SLP,TEMP) ##select and compile data 


ICE_ext <- read.csv(here("data/Ice Extent.csv")) #csv file from NOAA
ICE_ext <- ICE_ext %>%
  rename(YEAR = year)


#####Salmon data
Median_Timing <- read.csv(here("data/Median Table.csv"))

#RDS files, historical and current.  Combine as necessary
CPUE_Dist <- readRDS(here("data/CPUE data/Historical CPUE Dist.rds"))
Dist_Current <- readRDS(here("data/CPUE data/Current CPUE Dist.rds"))
CPUE_Dist <- abind::abind(CPUE_Dist, Dist_Current, along = 3)


CPUE_Interp <- as.data.frame(read.csv(here("data/CPUE data/Historical CPUE Interp.csv"),check.names = FALSE))
Interp_Current <- as.data.frame(read.csv(here("data/CPUE data/Current CPUE Interp.csv"),check.names = FALSE))
CPUE_Interp$`2025` <- Interp_Current$`2025`


#Break into bay and districts from the interpolated data
Bay_CPUE <- CPUE_Interp
Ugashik_CPUE <- as.data.frame(CPUE_Dist[,1,])
Egegik_CPUE <- as.data.frame(CPUE_Dist[,2,])
Kvichak_CPUE <- as.data.frame(CPUE_Dist[,3,])
Nushagak_CPUE <- as.data.frame(CPUE_Dist[,4,])
Togiak_CPUE <- as.data.frame(CPUE_Dist[,5,])



#Summarize airport data as we see fit, here I have June and the first half of July
June_dat <- filter(Airport_dat, MONTH==6)
June_datmeans <- June_dat %>% group_by(YEAR) %>%
  summarise(
    mean_temp_June = mean(TEMP, na.rm = TRUE),
    mean_wind_June = mean(WDSP, na.rm = TRUE),
    mean_pressure_June = mean(SLP, na.rm = TRUE),
  )

July_dat <- filter(Airport_dat, MONTH==7 & DAY<=15)
July_datmeans <- July_dat %>% group_by(YEAR) %>%
  summarise(
    mean_temp_July = mean(TEMP, na.rm = TRUE),
    mean_wind_July = mean(WDSP, na.rm = TRUE),
    mean_pressure_July = mean(SLP, na.rm = TRUE),
  )


######Clean up data and combine into a data frame to generate models from
Model_dat <- cbind(PDO, ENSO, June_datmeans, July_datmeans[,2:4])
Model_dat <- full_join(Model_dat, ICE_ext, by= "YEAR")
Model_dat <- Model_dat[-63,]  ##adjusting for no 2025 median timing data yet
Model_dat <- cbind(Median_Timing, Model_dat)
Model_dat <- Model_dat %>%
  rename(PDO_Jan = `pdo_1_0`) %>%rename(PDO_Feb = `pdo_2_0`) %>%rename(PDO_Mar = `pdo_3_0`) %>%
  rename(PDO_Apr = `pdo_4_0`) %>% rename(PDO_May = `pdo_5_0`) %>% rename(ENSO_Jan = `enso_1_0`) %>% 
  rename(ENSO_Feb = `enso_2_0`) %>% rename(ENSO_Mar = `enso_3_0`) %>% rename(ENSO_Apr = `enso_4_0`) %>%
  rename(ENSO_May = `enso_5_0`)



