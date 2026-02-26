
library

source(here("R/Pull Data.R"))


###Linear model

model_1 <- lm(bay ~ mean_temp_June + mean_wind_June + PDO_May +ENSO_May+ mean_pressure_June+extent+GOA_SpringSST+BB_JuneSST, 
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

###GAMS
library(mgcv)

model_2 <- gam(bay ~ s(mean_temp_June) + mean_wind_June + s(PDO_May) + s(ENSO_May)+ mean_pressure_June+extent+s(GOA_SpringSST), 
               data = Model_dat, na.action = na.omit)
summary(model_2)

model_2u <- gam(Uga ~ s(mean_temp_June) + mean_wind_June + s(PDO_May) + s(ENSO_May)+ mean_pressure_June+extent+s(GOA_SpringSST)+s(BB_JuneSST), 
                data = Model_dat, na.action = na.omit)
summary(model_2u)

model_2e <- gam(Ege  ~ s(mean_temp_June) + mean_wind_June + s(PDO_May) + s(ENSO_May)+ mean_pressure_June++extent+s(GOA_SpringSST)+s(BB_JuneSST), 
                data = Model_dat, na.action = na.omit)
summary(model_2e)

model_2k <- gam(Nak.Kvi  ~ s(mean_temp_June) + mean_wind_June + s(PDO_May) + s(ENSO_May)+ mean_pressure_June++extent+s(GOA_SpringSST)+s(BB_JuneSST), 
                data = Model_dat, na.action = na.omit)
summary(model_1k)

model_1n <- gam(Nush  ~ s(mean_temp_June) + mean_wind_June + s(PDO_May) + s(ENSO_May)+ mean_pressure_June+extent+s(GOA_SpringSST)+s(BB_JuneSST), 
                data = Model_dat, na.action = na.omit)
summary(model_2n)

model_2t <- gam(Tog  ~ s(mean_temp_June) + mean_wind_June + s(PDO_May) + s(ENSO_May)+ mean_pressure_June++extent+s(GOA_SpringSST)+s(BB_JuneSST), 
                data = Model_dat, na.action = na.omit)
summary(model_2t)


#####

