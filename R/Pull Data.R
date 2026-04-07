library (here)
library (lubridate)
library (dplyr)


here()#Bristol-Bay-Run-Timing base working directory 


#Start pulling in data, all stored in data subfolder. 

#######Environmental data
source(here("data/get-pdo-enso.r"))

Index_dat <- get_pdo_npgo(years=1963:2025, months=c(1,2,3,4,5), lags=c(0,0,0,0,0)) 
PDO <- as.data.frame(Index_dat$pdo)##Break into data frames to work with
ENSO <-as.data.frame(Index_dat$enso)

source(here("data/get-sst-data.r"))

source(here("data/get-chla-data.r"))

source(here("data/get-airport-data.r"))

Pink_abundance <- read.csv(here("data/Pink Salmon Abundance.csv"))

ICE_ext <- read.csv(here("data/Ice Extent.csv")) #csv file from NOAA
ICE_ext <- ICE_ext %>%
  rename(YEAR = year)
ICE_ext <- ICE_ext %>% dplyr::select(YEAR, extent)

source(here("data/get-streamflow-data.R"))

source(here("data/get-riverproportions-data.R"))
#Gives proportions from late returning rivers within a district (Kvichak and Igushik)


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
    mean_temp_June = mean(HourlyDryBulbTemperature, na.rm = TRUE),
    mean_eastwind_June = mean(Wind_East, na.rm = TRUE),
    cumulative_eastwind_June = sum(Wind_East, na.rm = TRUE),
    mean_northwind_June = mean(Wind_North, na.rm = TRUE),
    cumulative_northwind_June = sum(Wind_North, na.rm = TRUE),
    mean_pressure_June = mean(HourlySeaLevelPressure, na.rm = TRUE),
  )

#Spring temperatures for proxy to river temps, differential between nearshore and river
Spring_dat <- filter(Airport_dat, MONTH==c(4,5))
Spring_datmeans <- Spring_dat %>% group_by(YEAR) %>%
  summarise(
    cumulative_temp_spring = sum(HourlyDryBulbTemperature, na.rm = TRUE))


######Clean up data and combine into a data frame to generate models from
Model_dat <- cbind(PDO, ENSO, June_datmeans)
Model_dat <- full_join(Model_dat, Spring_datmeans, by= "YEAR")
Model_dat <- full_join(Model_dat, ICE_ext, by= "YEAR")
Model_dat <- full_join(Model_dat, ChlA_timing_GOA, by= "YEAR")
Model_dat <- full_join(Model_dat, ChlA_magnitude_GOA, by= "YEAR")
Model_dat <- full_join(Model_dat, Pink_abundance, by= "YEAR")
Model_dat <- full_join(Model_dat, SST_means, by= "YEAR")
Model_dat <- full_join(Model_dat, Streamflow_June_means, by= "YEAR")
Model_dat <- full_join(Model_dat, River_Proportions, by= "YEAR")
Model_dat <- full_join (Median_Timing, Model_dat, by="YEAR")
Model_dat <- Model_dat[-63,]  ##adjusting for no 2025 median timing data yet
Model_dat <- Model_dat %>%
  rename(PDO_Jan = `pdo_1_0`) %>%rename(PDO_Feb = `pdo_2_0`) %>%rename(PDO_Mar = `pdo_3_0`) %>%
  rename(PDO_Apr = `pdo_4_0`) %>% rename(PDO_May = `pdo_5_0`) %>% rename(ENSO_Jan = `enso_1_0`) %>% 
  rename(ENSO_Feb = `enso_2_0`) %>% rename(ENSO_Mar = `enso_3_0`) %>% rename(ENSO_Apr = `enso_4_0`) %>%
  rename(ENSO_May = `enso_5_0`) 


#Calculate anomaly (from mean of available time series) for SST, Air temp, wind (cumulative anomaly?)

Model_dat$GOA_SpringSST_anomaly <- Model_dat$GOA_SpringSST-mean(Model_dat$GOA_SpringSST, na.rm=TRUE)
Model_dat$BB_JuneSST_anomaly <- Model_dat$BB_JuneSST-mean(Model_dat$BB_JuneSST, na.rm=TRUE)
Model_dat$Uga_JuneSST_anomaly <- Model_dat$Ugashik_JuneSST-mean(Model_dat$Ugashik_JuneSST, na.rm=TRUE)
Model_dat$Ege_JuneSST_anomaly <- Model_dat$Egegik_JuneSST-mean(Model_dat$Egegik_JuneSST, na.rm=TRUE)
Model_dat$Kvi_JuneSST_anomaly <- Model_dat$Kvichak_JuneSST-mean(Model_dat$Kvichak_JuneSST, na.rm=TRUE)
Model_dat$Nush_JuneSST_anomaly <- Model_dat$Nushagak_JuneSST-mean(Model_dat$Nushagak_JuneSST, na.rm=TRUE)
Model_dat$Tog_JuneSST_anomaly <- Model_dat$Togiak_JuneSST-mean(Model_dat$Togiak_JuneSST, na.rm=TRUE)


# #Z score and find the sum of spring air temp and SST to proxy the difference in temperatures at the fresh/saltwater interface
# Model_dat <- Model_dat %>% mutate(z_springtemp = as.numeric(scale(cumulative_temp_spring)))   
# Model_dat <- Model_dat %>% mutate(z_UgaSST = as.numeric(scale(Model_dat$Ugashik_JuneSST)))
# Model_dat <- Model_dat %>% mutate(z_EgeSST = as.numeric(scale(Egegik_JuneSST)))
# Model_dat <- Model_dat %>% mutate(z_KviSST = as.numeric(scale(Kvichak_JuneSST)))
# Model_dat <- Model_dat %>% mutate(z_NushSST = as.numeric(scale(Nushagak_JuneSST)))
# Model_dat <- Model_dat %>% mutate(z_TogSST = as.numeric(scale(Togiak_JuneSST)))
# 
# Model_dat$Uga_tempdiff <- Model_dat$z_UgaSST + Model_dat$z_springtemp
# Model_dat$Ege_tempdiff <- Model_dat$z_EgeSST + Model_dat$z_springtemp
# Model_dat$Kvi_tempdiff <- Model_dat$z_KviSST + Model_dat$z_springtemp
# Model_dat$Nush_tempdiff <- Model_dat$z_NushSST + Model_dat$z_springtemp
# Model_dat$Tog_tempdiff <- Model_dat$z_TogSST + Model_dat$z_springtemp

#Airport data anomalies 
Model_dat$June_temp_anomaly <- Model_dat$mean_temp_June-mean(Model_dat$mean_temp_June, na.rm=TRUE)
Model_dat$June_pressure_anomaly <- Model_dat$mean_pressure_June-mean(Model_dat$mean_pressure_June, na.rm=TRUE)
Model_dat$June_cumeastwind_anomaly <- Model_dat$cumulative_eastwind_June-mean(Model_dat$cumulative_eastwind_June, na.rm=TRUE)
Model_dat$June_cumnorthwind_anomaly <- Model_dat$cumulative_northwind_June-mean(Model_dat$cumulative_northwind_June, na.rm=TRUE)

 
 ####Break up model_dat for each of the systems so there arent so many covariates in the data frame.  Right now these use anomalies
#I have the tempdiff data removed due to heavy correlation with other parameters.  Its also the 
#hardest to understand and interpret


 Model_dat_Uga <- Model_dat %>% dplyr::select(YEAR,Uga, Uga_lag1, Ugashik_meanflow_June, June_temp_anomaly, 
                                              June_cumeastwind_anomaly,June_cumnorthwind_anomaly,PDO_Jan, ENSO_Jan, PDO_May,
                                              ENSO_May, ChlA_GOAmagnitude,ChlA_GOAtiming, Abundance,
                                              June_pressure_anomaly,extent,
                                              GOA_SpringSST_anomaly, Uga_JuneSST_anomaly) #Uga_tempdiff
 
 Model_dat_Ege <- Model_dat %>% dplyr::select(YEAR,Ege, Ege_lag1, Egegik_meanflow_June,June_temp_anomaly, 
                                              June_cumeastwind_anomaly,June_cumnorthwind_anomaly,PDO_Jan, ENSO_Jan, PDO_May,
                                              ChlA_GOAmagnitude,ChlA_GOAtiming,ENSO_May, Abundance,
                                              June_pressure_anomaly,extent,
                                              GOA_SpringSST_anomaly, Ege_JuneSST_anomaly)  #Ege_tempdiff
 
 Model_dat_Kvi <- Model_dat %>% dplyr::select(YEAR,Kvi, Kvi_lag1,KvichakDistrict_meanflow_June, June_temp_anomaly, 
                                              June_cumeastwind_anomaly,June_cumnorthwind_anomaly,PDO_Jan, ENSO_Jan, PDO_May,
                                              ENSO_May, ChlA_GOAmagnitude,ChlA_GOAtiming, Abundance,
                                              June_pressure_anomaly,extent,
                                              GOA_SpringSST_anomaly, Kvichakproportion,Kvi_JuneSST_anomaly) #Kvi_tempdiff 
 
 Model_dat_Nush <- Model_dat %>% dplyr::select(YEAR,Nush, Nush_lag1, NushagakDistrict_meanflow_June, June_temp_anomaly, 
                                               June_cumeastwind_anomaly,June_cumnorthwind_anomaly,PDO_Jan, ENSO_Jan, PDO_May,
                                               ChlA_GOAmagnitude,ChlA_GOAtiming,ENSO_May, Abundance,
                                               June_pressure_anomaly,extent,
                                               GOA_SpringSST_anomaly,Igushikproportion, Nush_JuneSST_anomaly)  #Nush_tempdiff
 
 Model_dat_Tog <- Model_dat %>% dplyr::select(YEAR,Tog, Tog_lag1, Togiak_meanflow_June, June_temp_anomaly, 
                                              June_cumeastwind_anomaly,June_cumnorthwind_anomaly,PDO_Jan, ENSO_Jan, PDO_May,
                                              ENSO_May, ChlA_GOAmagnitude,ChlA_GOAtiming, Abundance,
                                              June_pressure_anomaly,extent,
                                              GOA_SpringSST_anomaly, Tog_JuneSST_anomaly)  #Tog_tempdiff
