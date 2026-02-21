
source(here("R/Pull_Data.R"))




###Linear model

model_1 <- lm(bay ~ mean_temp + mean_wind + PDO_May +ENSO_May+ mean_pressure+extent, 
              data = Model_dat, na.action = na.omit)
summary(model_1)

model_1u <- lm(Uga ~ mean_temp + mean_wind + PDO_May +ENSO_May+ mean_pressure+extent, 
               data = Model_dat, na.action = na.omit)
summary(model_1u)

model_1e <- lm(Ege ~ mean_temp + mean_wind + PDO_May +ENSO_May+ mean_pressure+extent, 
               data = Model_dat, na.action = na.omit)
summary(model_1e)

model_1k <- lm(Nak.Kvi ~ mean_temp + mean_wind + PDO_May +ENSO_May+ mean_pressure+extent, 
               data = Model_dat, na.action = na.omit)
summary(model_1k)

model_1n <- lm(Nush ~ mean_temp + mean_wind + PDO_May +ENSO_May+ mean_pressure+extent, 
               data = Model_dat, na.action = na.omit)
summary(model_1n)

model_1t <- lm(Tog ~ mean_temp + mean_wind + PDO_May +ENSO_May+ mean_pressure+extent, 
               data = Model_dat, na.action = na.omit)
summary(model_1t)

###GAMS
library(mgcv)

model_1 <- gam(bay ~ s(mean_temp) + mean_wind + s(PDO_May) + s(ENSO_May)+ mean_pressure, 
               data = Model_dat, na.action = na.omit)
summary(model_1)

model_1u <- gam(Uga ~  s(mean_temp) + mean_wind + s(PDO_May) + s(ENSO_May)+ mean_pressure, 
                data = Model_dat, na.action = na.omit)
summary(model_1u)

model_1e <- gam(Ege  ~ s(mean_temp) + mean_wind + s(PDO_May) + s(ENSO_May)+ mean_pressure, 
                data = Model_dat, na.action = na.omit)
summary(model_1e)

model_1k <- gam(Nak.Kvi  ~ s(mean_temp) + mean_wind + s(PDO_May) + s(ENSO_May)+ mean_pressure,
                data = Model_dat, na.action = na.omit)
summary(model_1k)

model_1n <- gam(Nush  ~ s(mean_temp) + mean_wind + s(PDO_May) + s(ENSO_May)+ mean_pressure,
                data = Model_dat, na.action = na.omit)
summary(model_1n)

model_1t <- gam(Tog  ~ s(mean_temp) + mean_wind + s(PDO_May) + s(ENSO_May)+ mean_pressure, 
                data = Model_dat, na.action = na.omit)
summary(model_1t)


#####

