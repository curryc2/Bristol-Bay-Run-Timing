require(here)
require(tidyverse)
require(dplyr)
require(corrplot)
library(visreg)


source(here("R/Pull Data.R"))


# All models are fit with the anomaly covariates, but this could be changed if needed.  Overall, the fits are not 
#exceptional using the full set of covariates, but I have seen worse.  The Gams fit much better, even without smoothed terms (which, were not merited based 
#on the model outputs when they were included for variables in which a smoothed relationship was logical)

#In terms of the ChlA data, 


###Linear models
#####using anomaly data instead of raw data


####Ugashik 

#I brute force created the full model set using the above covariates, then selected the model with the lowest AIC.  That is provided 
#below.

#Uga~ PDO_May + ChlA_GOAmagnitude + June_pressure_anomaly + extent +GOA_SpringSST_anomaly + Ugashik_meanflow_June + Uga_JuneSST_anomaly

model_bestu <- lm(Uga ~ PDO_May + ChlA_GOAmagnitude + June_pressure_anomaly + extent +
                    GOA_SpringSST_anomaly + Ugashik_meanflow_June + Uga_JuneSST_anomaly,
               data = Model_dat_Uga, na.action = na.omit)
summary(model_bestu)
plot(residuals(model_bestu))

#adj r2 .7922

#visreg(model_bestu)


####Egegik

#Best is Ege ~ June_cumeastwind_anomaly + June_cumnorthwind_anomaly +  PDO_May + ChlA_GOAmagnitude + June_pressure_anomaly + extent + Abundance + GOA_SpringSST_anomaly + Egegik_meanflow_June +  Ege_JuneSST_anomaly

model_beste <- lm(Ege ~ June_cumeastwind_anomaly + June_cumnorthwind_anomaly +  PDO_May + ChlA_GOAmagnitude + June_pressure_anomaly + 
                    extent + Abundance + GOA_SpringSST_anomaly + Egegik_meanflow_June +  Ege_JuneSST_anomaly,
               data = Model_dat_Ege, na.action = na.omit)
summary(model_beste)
plot(residuals(model_beste))
#visreg(model_beste)

#adj r2 .864



####Kvichak

#Best is Kvi ~ Kvi_lag1 + PDO_May + ENSO_May + ChlA_GOAmagnitude + ChlA_GOAtiming +extent + GOA_SpringSST_anomaly + KvichakDistrict_meanflow_June + Kvichakproportion + Kvi_JuneSST_anomaly

model_bestk <- lm(Kvi ~ Kvi_lag1 + PDO_May + ENSO_May + ChlA_GOAmagnitude + ChlA_GOAtiming +
                    extent + GOA_SpringSST_anomaly + KvichakDistrict_meanflow_June + Kvichakproportion + Kvi_JuneSST_anomaly, 
               data = Model_dat_Kvi, na.action = na.omit)
summary(model_bestk)
plot(residuals(model_bestk))
#visreg(model_bestk)

#adj r2 .8434


####Nushagak

#Best is Nush ~ Nush_lag1 + June_temp_anomaly + June_cumeastwind_anomaly +June_cumnorthwind_anomaly + PDO_May + ENSO_May + ChlA_GOAmagnitude +ChlA_GOAtiming + June_pressure_anomaly + extent + GOA_SpringSST_anomaly +NushagakDistrict_meanflow_June + Igushikproportion + Nush_JuneSST_anomaly


model_bestn <- lm(Nush ~ Nush_lag1 + June_temp_anomaly + June_cumeastwind_anomaly +
                    June_cumnorthwind_anomaly + PDO_May + ENSO_May + ChlA_GOAmagnitude +
                    ChlA_GOAtiming + June_pressure_anomaly + extent + GOA_SpringSST_anomaly +
                    NushagakDistrict_meanflow_June + Igushikproportion + Nush_JuneSST_anomaly, 
               data = Model_dat_Nush, na.action = na.omit)
summary(model_bestn)
plot(residuals(model_bestn))
#visreg(model_bestn)

#adj r2 .907


####Togiak

#Best is Tog ~ Tog_lag1 + June_temp_anomaly + June_cumeastwind_anomaly + June_cumnorthwind_anomaly + PDO_May + ENSO_May + ChlA_GOAmagnitude +June_pressure_anomaly + extent + Abundance + GOA_SpringSST_anomaly +Togiak_meanflow_June

model_bestt <- lm(Tog ~ Tog_lag1 + June_temp_anomaly + June_cumeastwind_anomaly + June_cumnorthwind_anomaly + 
                    PDO_May + ENSO_May + ChlA_GOAmagnitude +June_pressure_anomaly + 
                    extent + Abundance + GOA_SpringSST_anomaly +Togiak_meanflow_June, 
               data = Model_dat_Tog, na.action = na.omit)
summary(model_bestt)
plot(residuals(model_bestt))
#visreg(model_bestt)

#adj r2 .8237




# # Ugashik Code for testing all combinations of predictors and selecting the best model via AIC
# 
# vars <- c(  
#   "Uga_lag1",
#   "June_temp_anomaly",
#   "June_cumeastwind_anomaly",
#   "June_cumnorthwind_anomaly",
#   "PDO_May",
#   "ENSO_May",
#   "ChlA_GOAmagnitude",
#   "ChlA_GOAtiming",
#   "June_pressure_anomaly",
#   "extent",
#   "Abundance",
#   "GOA_SpringSST_anomaly",
#   "Ugashik_meanflow_June",
#   "Uga_JuneSST_anomaly"
# )
# 
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
#       paste("Uga ~", paste(combo, collapse = " + "))####
#     )
# 
#     fit <- lm(formula, data = Model_dat_Uga, na.action = na.omit)
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
# # Egegik Code for testing all combinations of predictors and selecting the best model via AIC
# 
# vars <- c(   #Currently the Nushagak predictors are included
#   "Ege_lag1",####
#   "June_temp_anomaly",
#   "June_cumeastwind_anomaly",
#   "June_cumnorthwind_anomaly",
#   "PDO_May",
#   "ENSO_May",
#   "ChlA_GOAmagnitude",
#   "ChlA_GOAtiming",
#   "June_pressure_anomaly",
#   "extent",
#   "Abundance",
#   "GOA_SpringSST_anomaly",
#   "Egegik_meanflow_June",
#   "Ege_JuneSST_anomaly"###
# )
# 
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
#       paste("Ege ~", paste(combo, collapse = " + "))####
#     )
#     
#     fit <- lm(formula, data = Model_dat_Ege, na.action = na.omit)
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
# #Kvichak Code for testing all combinations of predictors and selecting the best model via AIC
# 
# vars <- c(  
#   "Kvi_lag1",####
#   "June_temp_anomaly",
#   "June_cumeastwind_anomaly",
#   "June_cumnorthwind_anomaly",
#   "PDO_May",
#   "ENSO_May",
#   "ChlA_GOAmagnitude",
#   "ChlA_GOAtiming",
#   "June_pressure_anomaly",
#   "extent",
#   "Abundance",
#   "GOA_SpringSST_anomaly",
#   "KvichakDistrict_meanflow_June",###
#   "Kvichakproportion",####
#   "Kvi_JuneSST_anomaly"###
# )
# 
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
#       paste("Kvi ~", paste(combo, collapse = " + "))####
#     )
#     
#     fit <- lm(formula, data = Model_dat_Kvi, na.action = na.omit)
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


# # Nushagak Code for testing all combinations of predictors and selecting the best model via AIC

vars <- c(
  "Nush_lag1",####
  "June_temp_anomaly",
  "June_cumeastwind_anomaly",
  "June_cumnorthwind_anomaly",
  "PDO_May",
  "ENSO_May",
  "ChlA_GOAmagnitude",
  "ChlA_GOAtiming",
  "June_pressure_anomaly",
  "extent",
  "Abundance",
  "GOA_SpringSST_anomaly",
  "NushagakDistrict_meanflow_June",
  "Igushikproportion",
  "Nush_JuneSST_anomaly"###
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
      paste("Nush ~", paste(combo, collapse = " + "))####
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



# Togiak Code for testing all combinations of predictors and selecting the best model via AIC

vars <- c(  
  "Tog_lag1",####
  "June_temp_anomaly",
  "June_cumeastwind_anomaly",
  "June_cumnorthwind_anomaly",
  "PDO_May",
  "ENSO_May",
  "ChlA_GOAmagnitude",
  "ChlA_GOAtiming",
  "June_pressure_anomaly",
  "extent",
  "Abundance",
  "GOA_SpringSST_anomaly",
  "Togiak_meanflow_June",
  "Tog_JuneSST_anomaly"###
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
      paste("Tog ~", paste(combo, collapse = " + "))####
    )
    
    fit <- lm(formula, data = Model_dat_Tog, na.action = na.omit)
    
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


