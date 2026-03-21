require(here)
require(tidyverse)
#install.packages("GGally")
require(GGally)
require(dplyr)
require(mgcv)
require(corrplot)
#install.packages("visreg")
library(visreg)

source(here("R/Pull Data.R"))


# Explore Data ======================
Model_dat
dev.off()
par(mar = c(0,0,1,0))
par(mfrow=c(1,1))
plot(Model_dat$bay, Model_dat$PDO_Jan)
plot(Model_dat$bay, Model_dat$ENSO_Jan)
plot(Model_dat$bay, Model_dat$PDO_Jan)
plot(Model_dat$bay, Model_dat$PDO_Jan)
plot(Model_dat$bay, Model_dat$PDO_Jan)
plot(Model_dat$bay, Model_dat$PDO_Jan)
plot(Model_dat$bay, Model_dat$PDO_Jan)
plot(Model_dat$bay, Model_dat$PDO_Jan)
plot(Model_dat$bay, Model_dat$PDO_Jan)
plot(Model_dat$bay, Model_dat$PDO_Jan)
plot(Model_dat$bay, Model_dat$PDO_Jan)

# Calculate 

dev.new()
# Example predictor correlations for GAMs below (Nushagak)
Model_dat %>% dplyr::select(June_temp_anomaly, 
                           June_cumwind_anomaly,
                           PDO_Jan, PDO_May,
                           ENSO_May,
                           mean_pressure_June,
                           extent, NushagakDistrict_meanflow_June,
                           GOA_SpringSST_anomaly, Nushagak_JuneSST) %>% na.omit() %>% 
         cor() %>% corrplot::corrplot.mixed()

###Well in general, PDO and ENSO are correlated fairly heavily, particularly with themselves if we were to use January 
#may values.  Additionally, there are strong correlations between the different SST metrics, as well as with sea
#ice extent (As I would expect, more sea ice should equal colder SSTs).  Beyond this, if we only select one metric 
#for things like wind and air temperature, the correlations are largely pretty small.  

#When plotting a number of these predictors agaisnt return timing, there are very few with clear relationships.  
#Most look like a shotgun blast, and do not seem to be super helpful in modeling return timing.  

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

model_1 <- lm(bay ~ June_temp_anomaly + June_cumwind_anomaly + 
                PDO_Jan + PDO_May + ENSO_Jan + mean_pressure_June+extent+GOA_SpringSST_anomaly, 
              data = Model_dat, na.action = na.omit)
summary(model_1)
plot(residuals(model_1))

model_1u <- lm(Uga ~ June_temp_anomaly + Ugashik_meanflow_June + Ugashik_JuneSST + June_cumwind_anomaly + 
                 PDO_Jan + PDO_May + ENSO_Jan + ENSO_May+ mean_pressure_June+extent+GOA_SpringSST_anomaly, 
               data = Model_dat, na.action = na.omit)
summary(model_1u)

model_1e <- lm(Ege ~ June_temp_anomaly + Egegik_meanflow_June + Egegik_JuneSST + June_cumwind_anomaly + 
                 PDO_Jan + PDO_May + ENSO_Jan + mean_pressure_June+extent + GOA_SpringSST_anomaly, 
               data = Model_dat, na.action = na.omit)
summary(model_1e)
visreg(model_1e)

model_1k <- lm(Nak.Kvi~ June_temp_anomaly + KvichakDistrict_meanflow_June + Kvichak_JuneSST + June_cumwind_anomaly + 
                 PDO_Jan + PDO_May + ENSO_Jan + mean_pressure_June+extent + GOA_SpringSST_anomaly, 
               data = Model_dat, na.action = na.omit)
summary(model_1k)
visreg(model_1k)


model_1n <- lm(Nush ~ June_temp_anomaly + NushagakDistrict_meanflow_June + Nushagak_JuneSST + June_cumwind_anomaly + 
                 PDO_Jan + PDO_May + ENSO_Jan + mean_pressure_June+extent + GOA_SpringSST_anomaly, 
               data = Model_dat, na.action = na.omit)
summary(model_1n)
visreg(model_1n)


model_1t <- lm(Tog ~ June_temp_anomaly + Togiak_meanflow_June + Togiak_JuneSST + June_cumwind_anomaly + 
                 PDO_Jan + PDO_May + ENSO_Jan + mean_pressure_June+extent + GOA_SpringSST_anomaly, 
               data = Model_dat, na.action = na.omit)
summary(model_1t)
visreg(model_1t)


###The linear models do a really poor job of fitting to the data.  This seems to follow from the previous shotgun 
#observation.  Im afraid these data are missing the factors that are actually driving the observed variability.


###GAMS
library(mgcv)

model_2 <- gam(bay ~ s(June_temp_anomaly) + June_cumwind_anomaly + PDO_Jan + PDO_May + ENSO_Jan+ mean_pressure_June+extent+s(GOA_SpringSST_anomaly) + s(YEAR), 
               data = Model_dat, na.action = na.omit)

summary(model_2)
plot(residuals(model_2))
par(mfrow=c(2,2))
gam.check(model_2)
visreg(model_2)

model_2u <- gam(Uga ~ June_temp_anomaly + s(Ugashik_meanflow_June) + Ugashik_JuneSST + June_cumwind_anomaly + 
                  PDO_Jan + PDO_May + ENSO_Jan + ENSO_May + mean_pressure_June+extent + s(GOA_SpringSST_anomaly) + s(YEAR), 
                data = Model_dat, na.action = na.omit)
summary(model_2u)
plot(residuals(model_2u))
par(mfrow=c(2,2))
gam.check(model_2u)
visreg(model_2u)

model_2e <- gam(Ege  ~ June_temp_anomaly + s(Egegik_meanflow_June) + Egegik_JuneSST + June_cumwind_anomaly + 
                  PDO_Jan + PDO_May + ENSO_Jan + mean_pressure_June+extent + s(GOA_SpringSST_anomaly) + s(YEAR), 
                data = Model_dat, na.action = na.omit)
summary(model_2e)
plot(residuals(model_2e))
par(mfrow=c(2,2))
gam.check(model_2e)
visreg(model_2e)

model_2k <- gam(Nak.Kvi  ~ June_temp_anomaly + s(KvichakDistrict_meanflow_June) + Kvichak_JuneSST + June_cumwind_anomaly + 
                  PDO_Jan + PDO_May + ENSO_Jan + mean_pressure_June+extent + s(GOA_SpringSST_anomaly) + Kvichakproportion+ s(YEAR), 
                data = Model_dat, na.action = na.omit)
summary(model_2k)
plot(residuals(model_2k))
par(mfrow=c(2,2))
gam.check(model_2k)
visreg(model_2k)


model_2n <- gam(Nush  ~ June_temp_anomaly + s(NushagakDistrict_meanflow_June) + Nushagak_JuneSST + June_cumwind_anomaly + 
                  PDO_Jan + PDO_May + ENSO_Jan + mean_pressure_June+extent + Igushikproportion + s(GOA_SpringSST_anomaly) + s(YEAR), 
                 data = Model_dat, na.action = na.omit)
summary(model_2n)0
plot(residuals(model_2n))
par(mfrow=c(2,2))
gam.check(model_2n)
visreg(model_2n)


model_2t <- gam(Tog ~ June_temp_anomaly + s(Togiak_meanflow_June) + Togiak_JuneSST + June_cumwind_anomaly + 
                  PDO_Jan + PDO_May + ENSO_Jan + mean_pressure_June+extent + s(GOA_SpringSST_anomaly) +  s(YEAR), 
                data = Model_dat, na.action = na.omit)
summary(model_2t)
plot(residuals(model_2t))
par(mfrow=c(2,2))
gam.check(model_2t)
visreg(model_2t)

#####

#The gams do a much better job of fitting, PARTICULARLY with the Year effect included.  In looking at the year effect, there appears to 
#be autocorrelation in the data, with some form of cyclical relationship that are current predictors cannot explain.  
#Im wondering if we put in an autocorrelation term and see how that contributes, as that can be used in prediction
#whereas year effect cannot.  Overall, the most important variables seem to vary from system to system, but
#PDO and extent are the most consistent in explaining variability from year to year.  
