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


#Summarize airport data as we see fit, here I have June 
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

#Break into long and short time series

Model_dat_ChlA <- na.omit(Model_dat)

Model_dat_noChlA <- Model_dat %>% dplyr::select(-c(ChlA_GOAmagnitude, ChlA_GOAtiming)) %>% na.omit()

#Calculate z scores 

#Z score and find the sum of spring air temp and SST to proxy the difference in temperatures at the fresh/saltwater interface
Model_dat_ChlA <- Model_dat_ChlA %>% mutate(S_GOA_SpringSST = as.numeric(scale(GOA_SpringSST)))
Model_dat_ChlA <- Model_dat_ChlA %>% mutate(S_Uga_JuneSST = as.numeric(scale(Ugashik_JuneSST)))
Model_dat_ChlA <- Model_dat_ChlA %>% mutate(S_Ege_JuneSST = as.numeric(scale(Egegik_JuneSST)))
Model_dat_ChlA <- Model_dat_ChlA %>% mutate(S_Kvi_JuneSST = as.numeric(scale(Kvichak_JuneSST)))
Model_dat_ChlA <- Model_dat_ChlA %>% mutate(S_Nush_JuneSST = as.numeric(scale(Nushagak_JuneSST)))
Model_dat_ChlA <- Model_dat_ChlA %>% mutate(S_Tog_JuneSST = as.numeric(scale(Togiak_JuneSST)))

Model_dat_ChlA <- Model_dat_ChlA %>% mutate(S_Ugashik_meanflow_June = as.numeric(scale(Ugashik_meanflow_June)))
Model_dat_ChlA <- Model_dat_ChlA %>% mutate(S_Egegik_meanflow_June = as.numeric(scale(Egegik_meanflow_June)))
Model_dat_ChlA <- Model_dat_ChlA %>% mutate(S_KvichakDistrict_meanflow_June = as.numeric(scale(KvichakDistrict_meanflow_June)))
Model_dat_ChlA <- Model_dat_ChlA %>% mutate(S_NushagakDistrict_meanflow_June = as.numeric(scale(NushagakDistrict_meanflow_June)))
Model_dat_ChlA <- Model_dat_ChlA %>% mutate(S_Togiak_meanflow_June = as.numeric(scale(Togiak_meanflow_June)))

Model_dat_ChlA <- Model_dat_ChlA %>% mutate(S_Uga_lag1 = as.numeric(scale(Uga_lag1)))
Model_dat_ChlA <- Model_dat_ChlA %>% mutate(S_Ege_lag1 = as.numeric(scale(Ege_lag1)))
Model_dat_ChlA <- Model_dat_ChlA %>% mutate(S_Kvi_lag1 = as.numeric(scale(Kvi_lag1)))
Model_dat_ChlA <- Model_dat_ChlA %>% mutate(S_Nush_lag1 = as.numeric(scale(Nush_lag1)))
Model_dat_ChlA <- Model_dat_ChlA %>% mutate(S_Tog_lag1 = as.numeric(scale(Tog_lag1)))

Model_dat_ChlA <- Model_dat_ChlA %>% mutate(S_ChlA_GOAmagnitude = as.numeric(scale(ChlA_GOAmagnitude)))
Model_dat_ChlA <- Model_dat_ChlA %>% mutate(S_ChlA_GOAtiming = as.numeric(scale(ChlA_GOAtiming)))
Model_dat_ChlA <- Model_dat_ChlA %>% mutate(S_Abundance = as.numeric(scale(Abundance)))
Model_dat_ChlA <- Model_dat_ChlA %>% mutate(S_extent = as.numeric(scale(extent)))
Model_dat_ChlA <- Model_dat_ChlA %>% mutate(S_Igushikproportion = as.numeric(scale(Igushikproportion)))
Model_dat_ChlA <- Model_dat_ChlA %>% mutate(S_Kvichakproportion = as.numeric(scale(Kvichakproportion)))

Model_dat_ChlA <- Model_dat_ChlA %>% mutate(S_June_cumeastwind = as.numeric(scale(cumulative_eastwind_June)))
Model_dat_ChlA <- Model_dat_ChlA %>% mutate(S_June_cumnorthwind = as.numeric(scale(cumulative_northwind_June)))
Model_dat_ChlA <- Model_dat_ChlA %>% mutate(S_June_temp = as.numeric(scale(mean_temp_June)))
Model_dat_ChlA <- Model_dat_ChlA %>% mutate(S_June_pressure = as.numeric(scale(mean_pressure_June)))
Model_dat_ChlA <- Model_dat_ChlA %>% mutate(S_PDO_May = as.numeric(scale(PDO_May)))
Model_dat_ChlA <- Model_dat_ChlA %>% mutate(S_ENSO_May = as.numeric(scale(ENSO_May)))

Model_dat_noChlA <- Model_dat_noChlA %>% mutate(S_GOA_SpringSST = as.numeric(scale(GOA_SpringSST)))
Model_dat_noChlA <- Model_dat_noChlA %>% mutate(S_Uga_JuneSST = as.numeric(scale(Ugashik_JuneSST)))
Model_dat_noChlA <- Model_dat_noChlA %>% mutate(S_Ege_JuneSST = as.numeric(scale(Egegik_JuneSST)))
Model_dat_noChlA <- Model_dat_noChlA %>% mutate(S_Kvi_JuneSST = as.numeric(scale(Kvichak_JuneSST)))
Model_dat_noChlA <- Model_dat_noChlA %>% mutate(S_Nush_JuneSST = as.numeric(scale(Nushagak_JuneSST)))
Model_dat_noChlA <- Model_dat_noChlA %>% mutate(S_Tog_JuneSST = as.numeric(scale(Togiak_JuneSST)))

Model_dat_noChlA <- Model_dat_noChlA %>% mutate(S_Ugashik_meanflow_June = as.numeric(scale(Ugashik_meanflow_June)))
Model_dat_noChlA <- Model_dat_noChlA %>% mutate(S_Egegik_meanflow_June = as.numeric(scale(Egegik_meanflow_June)))
Model_dat_noChlA <- Model_dat_noChlA %>% mutate(S_KvichakDistrict_meanflow_June = as.numeric(scale(KvichakDistrict_meanflow_June)))
Model_dat_noChlA <- Model_dat_noChlA %>% mutate(S_NushagakDistrict_meanflow_June = as.numeric(scale(NushagakDistrict_meanflow_June)))
Model_dat_noChlA <- Model_dat_noChlA %>% mutate(S_Togiak_meanflow_June = as.numeric(scale(Togiak_meanflow_June)))

Model_dat_noChlA <- Model_dat_noChlA %>% mutate(S_Uga_lag1 = as.numeric(scale(Uga_lag1)))
Model_dat_noChlA <- Model_dat_noChlA %>% mutate(S_Ege_lag1 = as.numeric(scale(Ege_lag1)))
Model_dat_noChlA <- Model_dat_noChlA %>% mutate(S_Kvi_lag1 = as.numeric(scale(Kvi_lag1)))
Model_dat_noChlA <- Model_dat_noChlA %>% mutate(S_Nush_lag1 = as.numeric(scale(Nush_lag1)))
Model_dat_noChlA <- Model_dat_noChlA %>% mutate(S_Tog_lag1 = as.numeric(scale(Tog_lag1)))

Model_dat_noChlA <- Model_dat_noChlA %>% mutate(S_Abundance = as.numeric(scale(Abundance)))
Model_dat_noChlA <- Model_dat_noChlA %>% mutate(S_extent = as.numeric(scale(extent)))
Model_dat_noChlA <- Model_dat_noChlA %>% mutate(S_Igushikproportion = as.numeric(scale(Igushikproportion)))
Model_dat_noChlA <- Model_dat_noChlA %>% mutate(S_Kvichakproportion = as.numeric(scale(Kvichakproportion)))

Model_dat_noChlA <- Model_dat_noChlA %>% mutate(S_June_cumeastwind = as.numeric(scale(cumulative_eastwind_June)))
Model_dat_noChlA <- Model_dat_noChlA %>% mutate(S_June_cumnorthwind = as.numeric(scale(cumulative_northwind_June)))
Model_dat_noChlA <- Model_dat_noChlA %>% mutate(S_June_temp = as.numeric(scale(mean_temp_June)))
Model_dat_noChlA <- Model_dat_noChlA %>% mutate(S_June_pressure = as.numeric(scale(mean_pressure_June)))
Model_dat_noChlA <- Model_dat_noChlA %>% mutate(S_PDO_May = as.numeric(scale(PDO_May)))
Model_dat_noChlA <- Model_dat_noChlA %>% mutate(S_ENSO_May = as.numeric(scale(ENSO_May)))

####Break up model_dat for each of the systems so there arent so many covariates in the data frame. 

S_Model_dat_Uga_ChlA <- Model_dat_ChlA %>% dplyr::select(YEAR,Uga, S_Uga_lag1, S_Ugashik_meanflow_June, S_June_temp, 
                                             S_June_cumeastwind,S_June_cumnorthwind,S_PDO_May,
                                             S_ENSO_May, S_ChlA_GOAmagnitude,S_ChlA_GOAtiming, S_Abundance,
                                             S_June_pressure,S_extent,S_GOA_SpringSST, S_Uga_JuneSST)

S_Model_dat_Ege_ChlA <- Model_dat_ChlA  %>% dplyr::select(YEAR,Ege, S_Ege_lag1, S_Egegik_meanflow_June, S_June_temp, 
                                             S_June_cumeastwind,S_June_cumnorthwind,S_PDO_May,
                                             S_ENSO_May, S_ChlA_GOAmagnitude,S_ChlA_GOAtiming, S_Abundance,
                                             S_June_pressure,S_extent,S_GOA_SpringSST, S_Ege_JuneSST)

S_Model_dat_Kvi_ChlA<- Model_dat_ChlA  %>% dplyr::select(YEAR,Kvi, S_Kvi_lag1, S_KvichakDistrict_meanflow_June, S_June_temp, 
                                             S_June_cumeastwind,S_June_cumnorthwind,S_PDO_May,
                                             S_ENSO_May, S_ChlA_GOAmagnitude,S_ChlA_GOAtiming, S_Abundance,
                                             S_June_pressure,S_extent,S_GOA_SpringSST, S_Kvi_JuneSST, S_Kvichakproportion)

S_Model_dat_Nush_ChlA <- Model_dat_ChlA  %>% dplyr::select(YEAR,Nush, S_Nush_lag1, S_NushagakDistrict_meanflow_June, S_June_temp, 
                                              S_June_cumeastwind,S_June_cumnorthwind,S_PDO_May,
                                              S_ENSO_May, S_ChlA_GOAmagnitude,S_ChlA_GOAtiming, S_Abundance,
                                              S_June_pressure,S_extent,S_GOA_SpringSST, S_Nush_JuneSST, S_Igushikproportion)

S_Model_dat_Tog_ChlA <- Model_dat_ChlA %>% dplyr::select(YEAR,Tog, S_Tog_lag1, S_Togiak_meanflow_June, S_June_temp, 
                                             S_June_cumeastwind,S_June_cumnorthwind,S_PDO_May,
                                             S_ENSO_May, S_ChlA_GOAmagnitude,S_ChlA_GOAtiming, S_Abundance,
                                             S_June_pressure,S_extent,S_GOA_SpringSST, S_Tog_JuneSST)
#Now without ChlA

S_Model_dat_Uga_noChlA <- Model_dat_noChlA %>% dplyr::select(YEAR,Uga, S_Uga_lag1, S_Ugashik_meanflow_June, S_June_temp, 
                                                  S_June_cumeastwind,S_June_cumnorthwind,S_PDO_May,
                                                  S_ENSO_May, S_Abundance,
                                                  S_June_pressure,S_extent,S_GOA_SpringSST, S_Uga_JuneSST)

S_Model_dat_Ege_noChlA <- Model_dat_noChlA  %>% dplyr::select(YEAR,Ege, S_Ege_lag1, S_Egegik_meanflow_June, S_June_temp, 
                                                   S_June_cumeastwind,S_June_cumnorthwind,S_PDO_May,
                                                   S_ENSO_May, S_Abundance,
                                                   S_June_pressure,S_extent,S_GOA_SpringSST, S_Ege_JuneSST)

S_Model_dat_Kvi_noChlA <- Model_dat_noChlA  %>% dplyr::select(YEAR,Kvi, S_Kvi_lag1, S_KvichakDistrict_meanflow_June, S_June_temp, 
                                                   S_June_cumeastwind,S_June_cumnorthwind,S_PDO_May,
                                                   S_ENSO_May, S_Abundance,S_Kvichakproportion,
                                                   S_June_pressure,S_extent,S_GOA_SpringSST, S_Kvi_JuneSST)

S_Model_dat_Nush_noChlA <- Model_dat_noChlA  %>% dplyr::select(YEAR,Nush, S_Nush_lag1, S_NushagakDistrict_meanflow_June, S_June_temp, 
                                                    S_June_cumeastwind,S_June_cumnorthwind,S_PDO_May,
                                                    S_ENSO_May, S_Abundance,S_Igushikproportion,
                                                    S_June_pressure,S_extent,S_GOA_SpringSST, S_Nush_JuneSST)

S_Model_dat_Tog_noChlA <- Model_dat_noChlA %>% dplyr::select(YEAR,Tog, S_Tog_lag1, S_Togiak_meanflow_June, S_June_temp, 
                                                  S_June_cumeastwind,S_June_cumnorthwind,S_PDO_May,
                                                  S_ENSO_May, S_Abundance,
                                                  S_June_pressure,S_extent,S_GOA_SpringSST, S_Tog_JuneSST)

