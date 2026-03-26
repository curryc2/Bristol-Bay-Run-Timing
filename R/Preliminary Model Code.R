require(here)
require(tidyverse)
#install.packages("GGally")
require(GGally)
require(dplyr)
require(mgcv)
require(corrplot)
#install.packages("visreg")
library(visreg)

# All models are fit with the anomaly covariates, but this could be changed if needed.  Overall, the fits are not 
#exceptional using the full set of covariates, but I have seen worse.  The Gams fit much better, even without smoothed terms (which, were not merited based 
#on the model outputs when they were included for variables in which a smoothed relationship was logical)

#In terms of the ChlA data, 


###Linear models
#####using anomaly data instead of raw data


####Ugashik 

model_1u <- lm(Uga ~ Uga_lag1 + June_temp_anomaly + June_cumeastwind_anomaly + PDO_Jan + PDO_May +ENSO_Jan +# ChlA_GOAtiming + ChlA_GOAmagnitude +
                 June_pressure_anomaly + Uga_tempdiff + extent + Abundance + 
                 GOA_SpringSST_anomaly + Ugashik_meanflow_June, 
               data = Model_dat_Uga, na.action = na.omit)
summary(model_1u)
visreg(model_1u)

#I brute force created the full model set using the above covariates, then selected the model with the lowest AIC.  That is provided 
#below.

#Uga~ Uga_lag1 + June_temp_anomaly + PDO_May + ChlA_GOAtiming +Uga_tempdiff + extent

model_bestu <- lm(Uga ~ Uga_lag1 + June_temp_anomaly + PDO_May + ChlA_GOAtiming +Uga_tempdiff + extent,
               data = Model_dat_Uga, na.action = na.omit)
summary(model_bestu)
plot(residuals(model_bestu))
visreg(model_bestu)

#This looks WAYYYYY better.  Maybe this is the model structure we employ in our bayesian approach?   I am going to try this for
#each of the districts, lets see how consistent this is from district to district.  


####Egegik
model_1e <- lm(Ege  ~ Ege_lag1 + June_temp_anomaly + June_cumeastwind_anomaly + PDO_Jan + PDO_May +ENSO_Jan + #ChlA_GOAtiming + ChlA_GOAmagnitude +
                 June_pressure_anomaly + Ege_tempdiff + extent + Abundance + 
                 GOA_SpringSST_anomaly + Egegik_meanflow_June, 
               data = Model_dat_Ege, na.action = na.omit)
summary(model_1e)
plot(residuals(model_bestu))
visreg(model_1e)
#Best is Ege ~ June_cumeastwind_anomaly + PDO_Jan + PDO_May + ENSO_Jan +ChlA_GOAtiming + ChlA_GOAmagnitude + extent + Abundance +Egegik_meanflow_June

model_beste <- lm(Ege ~ June_cumeastwind_anomaly + PDO_Jan + PDO_May + ENSO_Jan +ChlA_GOAtiming + 
                 ChlA_GOAmagnitude + extent + Abundance +Egegik_meanflow_June,
               data = Model_dat_Ege, na.action = na.omit)
summary(model_beste)
plot(residuals(model_beste))
visreg(model_beste)

#Again, really good.  However, lets remember this is only using the past 20 years where there is ChlA data....



####Kvichak

model_1k <- lm(Nak.Kvi  ~ Nak.Kvi_lag1 + June_temp_anomaly + June_cumeastwind_anomaly + PDO_Jan + PDO_May +ENSO_Jan +# ChlA_GOAtiming + ChlA_GOAmagnitude +
                 June_pressure_anomaly + Kvi_tempdiff + extent + Abundance + 
                 GOA_SpringSST_anomaly + KvichakDistrict_meanflow_June + Kvichakproportion, 
               data = Model_dat_Kvi, na.action = na.omit)
summary(model_1k)
plot(residuals(model_beste))
visreg(model_1k)

#Best is Nak.Kvi ~ PDO_May + ChlA_GOAtiming + ChlA_GOAmagnitude + extent + Kvichakproportion

model_bestk <- lm(Nak.Kvi ~ PDO_May + ChlA_GOAtiming + ChlA_GOAmagnitude + extent + Kvichakproportion, 
               data = Model_dat_Kvi, na.action = na.omit)
summary(model_bestk)
plot(residuals(model_bestk))
visreg(model_bestk)



####Nushagak

model_1n <- lm(Nush ~ Nush_lag1 + June_temp_anomaly + June_cumeastwind_anomaly + PDO_Jan + PDO_May +ENSO_Jan + #ChlA_GOAtiming + ChlA_GOAmagnitude +
                 June_pressure_anomaly + Nush_tempdiff + extent + Abundance + 
                 GOA_SpringSST_anomaly + NushagakDistrict_meanflow_June + Igushikproportion, 
               data = Model_dat_Nush, na.action = na.omit)
summary(model_1n)
plot(residuals(model_bestk))
visreg(model_1n)

#Best is Nush ~ June_temp_anomaly + June_cumeastwind_anomaly + PDO_Jan +ENSO_Jan + ChlA_GOAtiming + ChlA_GOAmagnitude + June_pressure_anomaly +Nush_tempdiff + extent + Abundance


model_bestn <- lm(Nush ~ June_temp_anomaly + June_cumeastwind_anomaly + PDO_Jan +ENSO_Jan + ChlA_GOAtiming + 
                 ChlA_GOAmagnitude + June_pressure_anomaly +Nush_tempdiff + extent + Abundance, 
               data = Model_dat_Nush, na.action = na.omit)
summary(model_bestn)
plot(residuals(model_bestn))
visreg(model_bestn)



####Togiak

model_1t <- lm(Tog ~ Tog_lag1 + June_temp_anomaly + June_cumeastwind_anomaly + PDO_Jan + PDO_May +ENSO_Jan + ENSO_May +# ChlA_GOAtiming + ChlA_GOAmagnitude +
                 June_pressure_anomaly + Tog_tempdiff + extent + Abundance + 
                 GOA_SpringSST_anomaly + Togiak_meanflow_June, 
               data = Model_dat_Tog, na.action = na.omit)
summary(model_1t)
visreg(model_1t)

#Best is Tog ~ June_temp_anomaly + PDO_May + ChlA_GOAmagnitude + June_pressure_anomaly +extent + GOA_SpringSST_anomaly + Togiak_meanflow_June

model_bestt <- lm(Tog ~ June_temp_anomaly + PDO_May + ChlA_GOAmagnitude + June_pressure_anomaly +extent + 
                    GOA_SpringSST_anomaly + Togiak_meanflow_June, 
               data = Model_dat_Tog, na.action = na.omit)
summary(model_bestt)
plot(residuals(model_bestt))
visreg(model_bestt)





###The linear models do a really poor job of fitting to the data.  There are almost no predictors that are statistically 
#significant.  Part of this may be due to the ChlA data being so short.  Im going to remove these and refit.  

#Without these data the fits are still pretty terrible, maybe even more so using the full time series.  For most covariates, 
#the relationships look like a shotgun blast. Im afraid these data are missing the factors that are actually driving the 
#observed variability.  Lets look at Gams.  




#Code for testing all combinations of predictors and selecting the best model via AIC

vars <- c(   #Currently the Nushagak predictors are included
  "Nush_lag1",
  "June_temp_anomaly",
  "June_cumeastwind_anomaly",
  "PDO_Jan",
  "PDO_May",
  "ENSO_Jan",
  "ENSO_May",
  "ChlA_GOAmagnitude",
  "ChlA_GOAtiming",
  "June_pressure_anomaly",
  "Nush_tempdiff",
  "extent",
  "Abundance",
  "GOA_SpringSST_anomaly",
  "NushagakDistrict_meanflow_June"
)

results <- data.frame(
  model = character(),
  AIC = numeric(),
  R2 = numeric(),
  stringsAsFactors = FALSE
)
models <- list()

k <- 1

for (i in 1:length(vars)) {
  combos <- combn(vars, i, simplify = FALSE)
  
  for (combo in combos) {
    
    formula <- as.formula(
      paste("Nush ~", paste(combo, collapse = " + "))
    )
    
    fit <- lm(formula, data = Model_dat_Nush, na.action = na.omit)
    
    results <- rbind(results, data.frame(
      model = deparse(formula),
      AIC = AIC(fit),
      R2 = summary(fit)$r.squared
    ))
    
    models[[k]] <- fit
    k <- k + 1
  }
}


results <- results[order(results$AIC), ]

# Show top models
head(results)





###GAMS
library(mgcv)

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
                  ChlA_GOAmagnitude + June_pressure_anomaly +Nush_tempdiff + extent + Abundance, 
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

#The Gams dont seem to be necessary, where a smoother did not appear to be necessary.  We need to address some correlation issues.

#####

