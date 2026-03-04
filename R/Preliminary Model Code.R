require(here)
require(tidyverse)
require(GGally)
require(dplyr)
require(mgcv)
require(corrplot)


source(here("R/Pull Data.R"))


# Explore Data ======================
Model_dat

# Calculate 
??ggpairs

# Model_dat %>% dplyr::select(PDO_Jan:GOA_SpringSST_anomaly) %>% GGally::ggpairs()

preds.df <- Model_dat %>% 
              dplyr::select(PDO_Jan:GOA_SpringSST_anomaly) %>% 
              dplyr::select(-c(region, source_dataset, mo))

cor.preds <- preds.df %>% na.omit() %>% cor() %>% corrplot::corrplot.mixed()

# Example predictor correlations for GAMs below
Model_dat %>% dplyr::select(June_temp_anomaly, 
                           June_cumwind_anomaly,
                           PDO_May,
                           ENSO_May,
                           mean_pressure_June,
                           extent,
                           GOA_SpringSST_anomaly) %>% na.omit() %>% 
         cor() %>% corrplot::corrplot.mixed()

s(June_temp_anomaly) + June_cumwind_anomaly + s(PDO_May) + s(ENSO_May)+ mean_pressure_June+extent+s(GOA_SpringSST_anomaly)

###Linear model

model_1 <- lm(bay ~ mean_temp_June + June_temp_anomaly + mean_wind_June +June_wind_anomaly+cumulative_wind_June+ June_cumwind_anomaly+ PDO_May +ENSO_May+ mean_pressure_June+extent+GOA_SpringSST+BB_JuneSST, 
              data = Model_dat, na.action = na.omit)
summary(model_1)

model_1u <- lm(Uga ~ mean_temp_June + mean_wind_June + PDO_May +ENSO_May+ mean_pressure_June+extent+GOA_SpringSST+BB_JuneSST, 
               data = Model_dat, na.action = na.omit)
summary(model_1u)

model_1e <- lm(Ege ~ mean_temp_June + mean_wind_June + PDO_May +ENSO_May+ mean_pressure_June+extent+GOA_SpringSST+BB_JuneSST, 
               data = Model_dat, na.action = na.omit)
summary(model_1e)

model_1k <- lm(Nak.Kvi~ mean_temp_June + mean_wind_June + PDO_May +ENSO_May+ mean_pressure_June+extent+GOA_SpringSST+BB_JuneSST, 
               data = Model_dat, na.action = na.omit)
summary(model_1k)

model_1n <- lm(Nush ~ mean_temp_June + mean_wind_June + PDO_May +ENSO_May+ mean_pressure_June+extent+GOA_SpringSST+BB_JuneSST, 
               data = Model_dat, na.action = na.omit)
summary(model_1n)

model_1t <- lm(Tog ~ mean_temp_June + mean_wind_June + PDO_May +ENSO_May+ mean_pressure_June+extent+GOA_SpringSST+BB_JuneSST, 
               data = Model_dat, na.action = na.omit)
summary(model_1t)

#####using anomalys instead of raw data.  Little change, LMs do not perform very well.  

model_1 <- lm(bay ~ June_temp_anomaly + June_cumwind_anomaly+ PDO_May +ENSO_May+ mean_pressure_June+extent+GOA_SpringSST_anomaly + BB_JuneSST_anomaly, 
              data = Model_dat, na.action = na.omit)
summary(model_1)
plot(residuals(model_1))

model_1u <- lm(Uga ~ June_temp_anomaly + June_cumwind_anomaly+ PDO_May +ENSO_May+ mean_pressure_June+extent+GOA_SpringSST_anomaly + BB_JuneSST_anomaly, 
               data = Model_dat, na.action = na.omit)
summary(model_1u)

model_1e <- lm(Ege ~ June_temp_anomaly + June_cumwind_anomaly+ PDO_May +ENSO_May+ mean_pressure_June+extent+GOA_SpringSST_anomaly + BB_JuneSST_anomaly, 
               data = Model_dat, na.action = na.omit)
summary(model_1e)

model_1k <- lm(Nak.Kvi~ June_temp_anomaly + June_cumwind_anomaly+ PDO_May +ENSO_May+ mean_pressure_June+extent+GOA_SpringSST_anomaly + BB_JuneSST_anomaly, 
               data = Model_dat, na.action = na.omit)
summary(model_1k)

model_1n <- lm(Nush ~ June_temp_anomaly + June_cumwind_anomaly+ PDO_May +ENSO_May+ mean_pressure_June+extent+GOA_SpringSST_anomaly + BB_JuneSST_anomaly, 
               data = Model_dat, na.action = na.omit)
summary(model_1n)

model_1t <- lm(Tog ~ June_temp_anomaly + June_cumwind_anomaly+ PDO_May +ENSO_May+ mean_pressure_June+extent+GOA_SpringSST_anomaly + BB_JuneSST_anomaly, 
               data = Model_dat, na.action = na.omit)
summary(model_1t)




###GAMS
library(mgcv)

model_2 <- gam(bay ~ s(June_temp_anomaly) + June_cumwind_anomaly + s(PDO_May) + s(ENSO_May)+ mean_pressure_June+extent+s(GOA_SpringSST_anomaly), 
               data = Model_dat, na.action = na.omit)

# Curry's tweak
model_2 <- gam(bay ~ s(June_temp_anomaly) + June_cumwind_anomaly + s(PDO_May) + s(ENSO_May)+ mean_pressure_June+extent+s(GOA_SpringSST_anomaly) + s(YEAR), 
               data = Model_dat, na.action = na.omit)

summary(model_2)
plot(residuals(model_2))
par(mfrow=c(2,2))
gam.check(model_2)
visreg(model_2)

model_2u <- gam(Uga ~ s(June_temp_anomaly) + June_cumwind_anomaly + s(PDO_May) + s(ENSO_May)+ mean_pressure_June+extent+s(GOA_SpringSST_anomaly), 
                data = Model_dat, na.action = na.omit)
summary(model_2u)
gam.check(model_2u)

model_2e <- gam(Ege  ~ s(June_temp_anomaly) + June_cumwind_anomaly + s(PDO_May) + s(ENSO_May)+ mean_pressure_June+extent+s(GOA_SpringSST_anomaly), 
                data = Model_dat, na.action = na.omit)
summary(model_2e)

model_2k <- gam(Nak.Kvi  ~ s(June_temp_anomaly) + June_cumwind_anomaly + s(PDO_May) + s(ENSO_May)+ mean_pressure_June+extent+s(GOA_SpringSST_anomaly), 
                data = Model_dat, na.action = na.omit)
summary(model_2k)

model_2n <- gam(Nush  ~ s(June_temp_anomaly) + June_cumwind_anomaly + s(PDO_May) + s(ENSO_May)+ mean_pressure_June+extent+s(GOA_SpringSST_anomaly), 
                data = Model_dat, na.action = na.omit)
summary(model_2n)

model_2t <- gam(Tog ~ s(June_temp_anomaly) + June_cumwind_anomaly + s(PDO_May) + s(ENSO_May)+ mean_pressure_June+extent+s(GOA_SpringSST_anomaly), 
                data = Model_dat, na.action = na.omit)
summary(model_2t)

#Well, here deviance explained is largely between 40 and 60 percent.  Only the nushugak had a worse performance, with Togiak having the best.  This may be logical, given that 
#this is the latest run, and therefore may be impacted the most by conditions within the bay.  We need to discuss model parameters and structure to proceed with.  

#####

