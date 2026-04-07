require(here)
require(tidyverse)
require(dplyr)
require(corrplot)

library(visreg)

source(here("R/Pull Data.R"))

###Linear models
# All models are fit with the anomaly covariates.  ChlA data is excluded here.  


####Ugashik 

#I brute force created the full model set using the above covariates, then selected the model with the lowest AIC.  That is provided 
#below.

#Best is Uga ~ Uga_lag1 + June_temp_anomaly + GOA_SpringSST_anomaly +  Ugashik_meanflow_June

model_bestu_noChlA <- lm(Uga ~ Uga_lag1 + June_pressure_anomaly + GOA_SpringSST_anomaly +  Ugashik_meanflow_June,
                  data = Model_dat_Uga, na.action = na.omit)
summary(model_bestu_noChlA)
plot(residuals(model_bestu_noChlA))
#visreg(model_bestu_noChlA)

#adj r2 .1356

####Egegik

#Best is Ege ~ Ege_lag1 + June_cumnorthwind_anomaly + June_pressure_anomaly + extent + Abundance + GOA_SpringSST_anomaly + Egegik_meanflow_June

model_beste_noChlA <- lm(Ege ~ Ege_lag1 + June_cumnorthwind_anomaly + June_pressure_anomaly + 
                           extent + Abundance + GOA_SpringSST_anomaly + Egegik_meanflow_June,
                  data = Model_dat_Ege, na.action = na.omit)
summary(model_beste_noChlA)
plot(residuals(model_beste_noChlA))
#visreg(model_beste_noChlA)

#adj r2 .329



####Kvichak

#Best is Kvi ~ Kvi_lag1 + June_pressure_anomaly + extent + GOA_SpringSST_anomaly +KvichakDistrict_meanflow_June

model_bestk_noChlA <- lm(Kvi ~ Kvi_lag1 + June_pressure_anomaly + extent + GOA_SpringSST_anomaly +KvichakDistrict_meanflow_June, 
                  data = Model_dat_Kvi, na.action = na.omit)
summary(model_bestk_noChlA)
plot(residuals(model_bestk_noChlA))
#visreg(model_bestk_noChlA)

#adj r2 .3116



####Nushagak

#Best is Nush ~ June_cumeastwind_anomaly + ENSO_May + June_pressure_anomaly +  extent + Abundance + Nush_JuneSST_anomaly


model_bestn_noChlA <- lm(Nush ~ June_cumeastwind_anomaly + ENSO_May + June_pressure_anomaly +  extent + Abundance + Nush_JuneSST_anomaly, 
                  data = Model_dat_Nush, na.action = na.omit)
summary(model_bestn_noChlA)
plot(residuals(model_bestn_noChlA))
#visreg(model_bestn_noChlA)

#adj r2 is .2274



####Togiak

#Best is Tog ~ PDO_May + June_pressure_anomaly + extent + Togiak_meanflow_June +Tog_JuneSST_anomaly

model_bestt_noChlA <- lm(Tog ~ PDO_May + June_pressure_anomaly + extent + Togiak_meanflow_June +Tog_JuneSST_anomaly, 
                  data = Model_dat_Tog, na.action = na.omit)
summary(model_bestt_noChlA)
plot(residuals(model_bestt_noChlA))
#visreg(model_bestt_noChlA)

# adj r2 .4116






# Ugashik Code for testing all combinations of predictors and selecting the best model via AIC

vars <- c(
  "Uga_lag1",
  "June_temp_anomaly",
  "June_cumeastwind_anomaly",
  "June_cumnorthwind_anomaly",
  "PDO_May",
  "ENSO_May",
  "June_pressure_anomaly",
  "extent",
  "Abundance",
  "GOA_SpringSST_anomaly",
  "Ugashik_meanflow_June",
  "Uga_JuneSST_anomaly"
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
      paste("Uga ~", paste(combo, collapse = " + "))####
    )

    fit <- lm(formula, data = Model_dat_Uga, na.action = na.omit)

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

# vars <- c(
#   "Nush_lag1",####
#   "June_temp_anomaly",
#   "June_cumeastwind_anomaly",
#   "June_cumnorthwind_anomaly",
#   "PDO_May",
#   "ENSO_May",
#   "June_pressure_anomaly",
#   "extent",
#   "Abundance",
#   "GOA_SpringSST_anomaly",
#   "NushagakDistrict_meanflow_June",
#   "Igushikproportion",
#   "Nush_JuneSST_anomaly"###
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
#       paste("Nush ~", paste(combo, collapse = " + "))####
#     )
# 
#     fit <- lm(formula, data = Model_dat_Nush, na.action = na.omit)
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



# # Togiak Code for testing all combinations of predictors and selecting the best model via AIC
# 
# vars <- c(  
#   "Tog_lag1",####
#   "June_temp_anomaly",
#   "June_cumeastwind_anomaly",
#   "June_cumnorthwind_anomaly",
#   "PDO_May",
#   "ENSO_May",
#   "June_pressure_anomaly",
#   "extent",
#   "Abundance",
#   "GOA_SpringSST_anomaly",
#   "Togiak_meanflow_June",
#   "Tog_JuneSST_anomaly"###
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
#       paste("Tog ~", paste(combo, collapse = " + "))####
#     )
#     
#     fit <- lm(formula, data = Model_dat_Tog, na.action = na.omit)
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


