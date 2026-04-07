
###GAMS
library(mgcv)

source(here("R/Pull Data.R"))
#Using the best equation from the LM

model_2u <- gam(Uga ~ Uga_lag1 + June_temp_anomaly + PDO_May + ChlA_GOAtiming +Uga_tempdiff + extent,
                data = Model_dat_Uga, na.action = na.omit)
summary(model_2u)
plot(residuals(model_2u))
par(mfrow=c(2,2))
gam.check(model_2u)
visreg(model_2u)

model_2e <- gam(Ege ~ June_cumeastwind_anomaly + PDO_Jan + PDO_May + ENSO_Jan +ChlA_GOAtiming + 
                  ChlA_GOAmagnitude + extent + Abundance +Egegik_meanflow_June,
                data = Model_dat_Ege, na.action = na.omit)
summary(model_2e)
plot(residuals(model_2e))
par(mfrow=c(2,2))
gam.check(model_2e)
visreg(model_2e)

model_2k <- gam(Nak.Kvi ~ PDO_May + ChlA_GOAtiming + ChlA_GOAmagnitude + extent + Kvichakproportion,  
                data = Model_dat_Kvi, na.action = na.omit)
summary(model_2k)
plot(residuals(model_2k))
par(mfrow=c(2,2))
gam.check(model_2k)
visreg(model_2k)


model_2n <- gam(Nush ~ June_temp_anomaly + June_cumeastwind_anomaly + PDO_Jan +ENSO_Jan + ChlA_GOAtiming + 
                  ChlA_GOAmagnitude + June_pressure_anomaly + s(extent) + Abundance, 
                data = Model_dat_Nush, na.action = na.omit)
summary(model_2n)
plot(residuals(model_2n))
par(mfrow=c(2,2))
gam.check(model_2n)
visreg(model_2n)


model_2t <- gam(Tog ~ June_temp_anomaly + PDO_May + ChlA_GOAmagnitude + June_pressure_anomaly +extent + 
                  GOA_SpringSST_anomaly + Togiak_meanflow_June, 
                data = Model_dat_Tog, na.action = na.omit)
summary(model_2t)
plot(residuals(model_2t))
par(mfrow=c(2,2))
gam.check(model_2t)
visreg(model_2t)

#####

#The Gams dont seem to be necessary, where a smoother did not appear to be necessary.  
#This of course assumes that the best LM model is also the best GAM.  
#We need to address some correlation issues.

#####

