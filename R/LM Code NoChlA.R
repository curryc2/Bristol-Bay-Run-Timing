require(here)
require(tidyverse)
require(dplyr)
require(corrplot)

library(visreg)

source(here("R/Pull Data.R"))

###Linear models
# All models are fit with the anomaly covariates.  ChlA data is excluded here.  


####Ugashik 

model_1u_noChlA <- lm(Uga ~ Uga_lag1 + June_temp_anomaly + June_cumeastwind_anomaly + PDO_Jan + PDO_May +ENSO_Jan +
                 June_pressure_anomaly + Uga_tempdiff + extent + Abundance + 
                 GOA_SpringSST_anomaly + Ugashik_meanflow_June, 
               data = Model_dat_Uga, na.action = na.omit)
summary(model_1u_noChlA)
#visreg(model_1u_noChlA)

#I brute force created the full model set using the above covariates, then selected the model with the lowest AIC.  That is provided 
#below.

#Best is Uga ~ Uga_lag1 + June_temp_anomaly + GOA_SpringSST_anomaly + Ugashik_meanflow_June

model_bestu_noChlA <- lm(Uga ~ Uga_lag1 + June_temp_anomaly + GOA_SpringSST_anomaly + Ugashik_meanflow_June,
                  data = Model_dat_Uga, na.action = na.omit)
summary(model_bestu_noChlA)
plot(residuals(model_bestu_noChlA))
#visreg(model_bestu_noChlA)

#adj r2 .136

####Egegik
model_1e_noChlA <- lm(Ege  ~ Ege_lag1 + June_temp_anomaly + June_cumeastwind_anomaly + PDO_Jan + PDO_May +ENSO_Jan +
                 June_pressure_anomaly + Ege_tempdiff + extent + Abundance + 
                 GOA_SpringSST_anomaly + Egegik_meanflow_June, 
               data = Model_dat_Ege, na.action = na.omit)
summary(model_1e_noChlA)
plot(residuals(model_bestu_noChlA))
#visreg(model_1e_noChlA)
#Best is Ege ~ Ege_lag1 + June_temp_anomaly + PDO_Jan + PDO_May + extent +Abundance + GOA_SpringSST_anomaly

model_beste_noChlA <- lm(Ege ~ Ege_lag1 + June_temp_anomaly + PDO_Jan + PDO_May + extent +Abundance + GOA_SpringSST_anomaly,
                  data = Model_dat_Ege, na.action = na.omit)
summary(model_beste_noChlA)
plot(residuals(model_beste_noChlA))
#visreg(model_beste_noChlA)

#adj r2 .3353



####Kvichak

model_1k_noChlA <- lm(Nak.Kvi  ~ Nak.Kvi_lag1 + June_temp_anomaly + June_cumeastwind_anomaly + PDO_Jan + PDO_May +ENSO_Jan +
                 June_pressure_anomaly + Kvi_tempdiff + extent + Abundance + 
                 GOA_SpringSST_anomaly + KvichakDistrict_meanflow_June + Kvichakproportion, 
               data = Model_dat_Kvi, na.action = na.omit)
summary(model_1k_noChlA)
plot(residuals(model_bestk_noChlA))
#visreg(model_1k_noChlA)

#Best is Nak.Kvi ~ Nak.Kvi_lag1 + June_temp_anomaly + PDO_Jan + PDO_May + extent + Kvichakproportion + GOA_SpringSST_anomaly

model_bestk_noChlA <- lm(Nak.Kvi ~ Nak.Kvi_lag1 + June_temp_anomaly + PDO_Jan + PDO_May + extent + Kvichakproportion + GOA_SpringSST_anomaly, 
                  data = Model_dat_Kvi, na.action = na.omit)
summary(model_bestk_noChlA)
plot(residuals(model_bestk_noChlA))
#visreg(model_bestk_noChlA)

#adj r2 .436



####Nushagak

model_1n_noChlA <- lm(Nush ~ Nush_lag1 + June_temp_anomaly + June_cumeastwind_anomaly + PDO_Jan + PDO_May +ENSO_Jan + 
                 June_pressure_anomaly + Nush_tempdiff + extent + Abundance + 
                 GOA_SpringSST_anomaly + NushagakDistrict_meanflow_June + Igushikproportion, 
               data = Model_dat_Nush, na.action = na.omit)
summary(model_1n_noChlA)
plot(residuals(model_bestn_noChlA))
#visreg(model_1n_noChlA)

#Best is Nush ~ June_cumeastwind_anomaly + PDO_Jan + PDO_May + ENSO_May +June_pressure_anomaly + Nush_tempdiff + extent + Abundance +NushagakDistrict_meanflow_June


model_bestn_noChlA <- lm(Nush ~ June_cumeastwind_anomaly + PDO_Jan + PDO_May + ENSO_May +June_pressure_anomaly + 
                    Nush_tempdiff + extent + Abundance +NushagakDistrict_meanflow_June, 
                  data = Model_dat_Nush, na.action = na.omit)
summary(model_bestn_noChlA)
plot(residuals(model_bestn_noChlA))
#visreg(model_bestn_noChlA)

#adj r2 is .35



####Togiak

model_1t_noChlA <- lm(Tog ~ Tog_lag1 + June_temp_anomaly + June_cumeastwind_anomaly + PDO_Jan + PDO_May +ENSO_Jan + ENSO_May +
                 June_pressure_anomaly + Tog_tempdiff + extent + Abundance + 
                 GOA_SpringSST_anomaly + Togiak_meanflow_June, 
               data = Model_dat_Tog, na.action = na.omit)
summary(model_1t_noChlA)
#visreg(model_1t_noChlA)

#Best is Tog ~ PDO_May + June_pressure_anomaly + extent + GOA_SpringSST_anomaly

model_bestt_noChlA <- lm(Tog ~ PDO_May + June_pressure_anomaly + extent + GOA_SpringSST_anomaly, 
                  data = Model_dat_Tog, na.action = na.omit)
summary(model_bestt_noChlA)
plot(residuals(model_bestt_noChlA))
#visreg(model_bestt_noChlA)

# adj r2 .3811





# 
# #Code for testing all combinations of predictors and selecting the best model via AIC
# 
# vars <- c(   #Replace for each district as necessary, marked (proportions need to be added for Kvi and Nushagak)
#   "Uga_lag1", ####
#   "June_temp_anomaly",
#   "June_cumeastwind_anomaly",
#   "PDO_Jan",
#   "PDO_May",
#   "ENSO_Jan",
#   "ENSO_May",
#   "June_pressure_anomaly",
#   "Uga_tempdiff", ####
#   "extent",
#   "Abundance",
#   "GOA_SpringSST_anomaly",
#   "Ugashik_meanflow_June" ####
# )
# 
# results <- data.frame(
#   model = character(),
#   AIC = numeric(),
#   R2 = numeric(),
#   stringsAsFactors = FALSE
# )
# models <- list()
# 
# k <- 1
# 
# for (i in 1:length(vars)) {
#   combos <- combn(vars, i, simplify = FALSE)
#   
#   for (combo in combos) {
#     
#     formula <- as.formula(
#       paste("Uga ~", paste(combo, collapse = " + ")) #adjust model structure
#     )
#     
#     fit <- lm(formula, data = Model_dat_Uga, na.action = na.omit) #adjust data 
#     
#     results <- rbind(results, data.frame(
#       model = deparse(formula),
#       AIC = AIC(fit),
#       R2 = summary(fit)$r.squared
#     ))
#     
#     models[[k]] <- fit
#     k <- k + 1
#   }
# }
# 
# 
# results <- results[order(results$AIC), ]
# 
# # Show top models
# head(results)
# 
# 
