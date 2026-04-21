require(here)
require(tidyverse)
require(dplyr)
require(mgcv)
require(corrplot)
#install.packages("visreg")
library(visreg)

source(here("R/Pull Data.R"))


# Explore Data ======================
Model_dat


# Evaluate correlations of predictors from the best fit LM models

##Ugashik
dev.new()
Model_dat_Uga %>% dplyr::select(Uga_lag1, June_temp_anomaly, June_cumeastwind_anomaly, June_cumnorthwind_anomaly, PDO_Jan, PDO_May, ENSO_Jan, ENSO_May,
                                ChlA_GOAmagnitude, ChlA_GOAtiming, June_pressure_anomaly, Uga_tempdiff, extent, 
                                Abundance, GOA_SpringSST_anomaly, Ugashik_meanflow_June, Uga_JuneSST_anomaly) %>% na.omit() %>% 
  cor() %>% corrplot::corrplot.mixed()

##Remove correlated parameters and replot
Model_dat_Uga %>% dplyr::select(Uga_lag1, June_temp_anomaly, June_cumeastwind_anomaly, June_cumnorthwind_anomaly, PDO_May, ENSO_Jan, ENSO_May,
                                ChlA_GOAmagnitude, ChlA_GOAtiming, June_pressure_anomaly, extent, 
                                Abundance, GOA_SpringSST_anomaly, Ugashik_meanflow_June, Uga_JuneSST_anomaly) %>% na.omit() %>% 
  cor() %>% corrplot::corrplot.mixed()

##Egegik
dev.new()
Model_dat_Ege %>% dplyr::select(Ege_lag1, June_temp_anomaly, June_cumeastwind_anomaly, June_cumnorthwind_anomaly, PDO_May, ENSO_May,
                                ChlA_GOAmagnitude, ChlA_GOAtiming, June_pressure_anomaly, extent, 
                                Abundance, GOA_SpringSST_anomaly, Egegik_meanflow_June, Ege_JuneSST_anomaly) %>% na.omit() %>% 
  cor() %>% corrplot::corrplot.mixed()

##Kvichak
dev.new()
Model_dat_Kvi %>% dplyr::select(Kvi_lag1, June_temp_anomaly, June_cumeastwind_anomaly, June_cumnorthwind_anomaly, PDO_Jan, PDO_May, ENSO_Jan, ENSO_May,
                                ChlA_GOAmagnitude, ChlA_GOAtiming, June_pressure_anomaly, Kvi_tempdiff, extent, 
                                  Abundance, GOA_SpringSST_anomaly, KvichakDistrict_meanflow_June, Kvichakproportion, Kvi_JuneSST_anomaly) %>% na.omit() %>% 
  cor() %>% corrplot::corrplot.mixed()

#####Remove correlated parameters and replot
Model_dat_Kvi %>% dplyr::select(Kvi_lag1, June_temp_anomaly, June_cumeastwind_anomaly, June_cumnorthwind_anomaly, PDO_May, ENSO_May,
                                ChlA_GOAmagnitude, ChlA_GOAtiming, June_pressure_anomaly, extent, 
                                Abundance, GOA_SpringSST_anomaly, KvichakDistrict_meanflow_June, Kvichakproportion, Kvi_JuneSST_anomaly) %>% na.omit() %>% 
  cor() %>% corrplot::corrplot.mixed()

##Nushagak
dev.new()
Model_dat_Nush %>% dplyr::select( June_temp_anomaly,June_cumeastwind_anomaly,PDO_Jan,ENSO_Jan, ChlA_GOAtiming, 
                                    ChlA_GOAmagnitude,June_pressure_anomaly,Nush_tempdiff, extent,Abundance) %>% na.omit() %>% 
  cor() %>% corrplot::corrplot.mixed()

##Togiak
dev.new()
Model_dat_Tog %>% dplyr::select(June_temp_anomaly,PDO_May,ChlA_GOAmagnitude,June_pressure_anomaly,extent,
                                   GOA_SpringSST_anomaly,Togiak_meanflow_June) %>% na.omit() %>% 
  cor() %>% corrplot::corrplot.mixed()

###Well in general, PDO and ENSO are correlated fairly heavily, particularly with themselves if we were to use January and
#May values.  Additionally, there are strong correlations between the different SST metrics, as well as with sea
#ice extent (As I would expect, more sea ice should equal colder SSTs).  Beyond this, if we only select one metric 
#for things like wind and air temperature, the correlations are largely pretty small.  


