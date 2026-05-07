require(here)
require(tidyverse)
require(dplyr)
require(corrplot)
library(visreg)


source(here("R/Pull Data Scaled.R"))


###Linear models
#####using scaled data 


####Ugashik 

#I brute force created the full model set using the above covariates, then selected the model with the lowest AIC.  That is provided 
#in the LM Code.R script.

model_bestu <- lm(Uga ~ Uga ~ S_ChlA_GOAmagnitude + S_PDO_May + S_ENSO_May + S_extent,
                  data = S_Model_dat_Uga_ChlA, na.action = na.omit)
summary(model_bestu)
plot(residuals(model_bestu))

#adj r2 .7922

#visreg(model_bestu)


####Egegik


model_beste <- lm(Ege ~ S_June_cumnorthwind + S_ChlA_GOAmagnitude + S_extent,
                  data = S_Model_dat_Ege_ChlA, na.action = na.omit)
summary(model_beste)
plot(residuals(model_beste))
#visreg(model_beste)

#adj r2 .864



####Kvichak


model_bestk <- lm(Kvi ~Kvi ~ S_ChlA_GOAmagnitude + S_ChlA_GOAtiming + S_PDO_May + S_ENSO_May + S_extent, 
                  data = S_Model_dat_Kvi_ChlA, na.action = na.omit)
summary(model_bestk)
plot(residuals(model_bestk))
#visreg(model_bestk)

#adj r2 .8434


####Nushagak


model_bestn <- lm(Nush ~ S_Nush_lag1 + S_June_temp + S_June_cumeastwind +
                    S_June_cumnorthwind + S_PDO_May + S_ENSO_May + S_ChlA_GOAmagnitude +
                    S_ChlA_GOAtiming + S_June_pressure + S_extent + S_GOA_SpringSST +
                    S_NushagakDistrict_meanflow_June + S_Igushikproportion + S_Nush_JuneSST, 
                  data = S_Model_dat_Nush_ChlA, na.action = na.omit)
summary(model_bestn)
plot(residuals(model_bestn))
#visreg(model_bestn)

#adj r2 .907


####Togiak

model_bestt <- lm(Tog ~ S_Tog_lag1 + S_June_temp + S_June_cumeastwind + S_June_cumnorthwind + 
                    S_PDO_May + S_ENSO_May + S_ChlA_GOAmagnitude +S_June_pressure + 
                    S_extent + S_Abundance + S_GOA_SpringSST +S_Togiak_meanflow_June, 
                  data = S_Model_dat_Tog_ChlA, na.action = na.omit)
summary(model_bestt)
plot(residuals(model_bestt))
#visreg(model_bestt)

#adj r2 .8237


##########################################################################################

#Now, lets do the whole time series LM fits with scaled data


####Ugashik 


model_bestu_noChlA <- lm(Uga ~ S_Uga_lag1 + S_June_pressure + S_GOA_SpringSST +  S_Ugashik_meanflow_June,
                         data = S_Model_dat_Uga_noChlA, na.action = na.omit)
summary(model_bestu_noChlA)
plot(residuals(model_bestu_noChlA))
#visreg(model_bestu_noChlA)

#adj r2 .1356


####Egegik


model_beste_noChlA <- lm(Ege ~ S_Ege_lag1 + S_June_cumnorthwind + S_June_pressure + 
                           S_extent + S_Abundance + S_GOA_SpringSST + S_Egegik_meanflow_June,
                         data = S_Model_dat_Ege_noChlA, na.action = na.omit)
summary(model_beste_noChlA)
plot(residuals(model_beste_noChlA))
#visreg(model_beste_noChlA)

#adj r2 .329


####Kvichak


model_bestk_noChlA <- lm(Kvi ~ S_Kvi_lag1 + S_June_pressure + S_extent + S_GOA_SpringSST +S_KvichakDistrict_meanflow_June, 
                         data = S_Model_dat_Kvi_noChlA, na.action = na.omit)
summary(model_bestk_noChlA)
plot(residuals(model_bestk_noChlA))
#visreg(model_bestk_noChlA)

#adj r2 .3116


####Nushagak


model_bestn_noChlA <- lm(Nush ~ S_June_cumeastwind + S_ENSO_May + S_June_pressure +  S_extent + 
                           S_Abundance + S_Nush_JuneSST, 
                         data = S_Model_dat_Nush_noChlA, na.action = na.omit)
summary(model_bestn_noChlA)
plot(residuals(model_bestn_noChlA))
#visreg(model_bestn_noChlA)

#adj r2 is .2274


####Togiak

model_bestt_noChlA <- lm(Tog ~ S_PDO_May + S_June_pressure + S_extent + S_Togiak_meanflow_June +S_Tog_JuneSST, 
                         data = S_Model_dat_Tog_noChlA, na.action = na.omit)
summary(model_bestt_noChlA)
plot(residuals(model_bestt_noChlA))
#visreg(model_bestt_noChlA)

# adj r2 .4116

##########################################################################################


#What if we fit the long time series data for just 2000 forward?


S_Model_dat_Uga_noChlA2000 <- dplyr::filter(S_Model_dat_Uga_noChlA, YEAR>=2000)
S_Model_dat_Ege_noChlA2000 <- dplyr::filter(S_Model_dat_Ege_noChlA, YEAR>=2000)
S_Model_dat_Kvi_noChlA2000 <- dplyr::filter(S_Model_dat_Kvi_noChlA, YEAR>=2000)
S_Model_dat_Nush_noChlA2000 <- dplyr::filter(S_Model_dat_Nush_noChlA, YEAR>=2000)
S_Model_dat_Tog_noChlA2000 <- dplyr::filter(S_Model_dat_Tog_noChlA, YEAR>=2000)

##redo brute force AIC model selection, code at bottom



#Ugashik
Uga ~ S_June_cumeastwind + S_June_cumnorthwind + S_PDO_May +S_extent

#Egegik
Ege ~ S_PDO_May + S_ENSO_May + S_extent

#Kvichak
Kvi ~ S_PDO_May + S_ENSO_May + S_extent

#Nushagak
Nush ~ S_June_cumeastwind + S_June_cumnorthwind + S_PDO_May + S_ENSO_May + S_extent + S_Abundance + S_GOA_SpringSST + S_NushagakDistrict_meanflow_June +S_Nush_JuneSST

#Togiak
Tog ~ S_June_temp + S_June_cumnorthwind + S_PDO_May + S_June_pressure + S_extent + S_Tog_JuneSST 




####Ugashik 


model_bestu_2000 <- lm(Uga ~ S_June_cumeastwind + S_June_cumnorthwind + S_PDO_May +S_extent,
                  data = S_Model_dat_Uga_noChlA2000, na.action = na.omit)
summary(model_bestu_2000)
plot(residuals(model_bestu_2000))

#adj r2 .4074  (Full time series is .1356)

#visreg(model_bestu)


####Egegik


model_beste_2000 <- lm( Ege ~ S_June_cumnorthwind + S_ChlA_GOAmagnitude + S_extent,
                  data = S_Model_dat_Ege_noChlA2000, na.action = na.omit)
summary(model_beste_2000)
plot(residuals(model_beste_2000))
#visreg(model_beste)

#adj r2 .649  (Full time series is .329)



####Kvichak

model_bestk_2000 <- lm(Kvi ~ S_ChlA_GOAmagnitude + S_ChlA_GOAtiming + S_PDO_May + S_ENSO_May + S_extent, 
                  data = S_Model_dat_Kvi_noChlA2000, na.action = na.omit)
summary(model_bestk_2000)
plot(residuals(model_bestk_2000))
#visreg(model_bestk)

#adj r2 .579  (Full time series is .3116)


####Nushagak


model_bestn_2000 <- lm(Nush ~ S_June_cumeastwind + S_June_cumnorthwind + S_PDO_May + S_ENSO_May + S_extent + 
                    S_Abundance + S_GOA_SpringSST + S_NushagakDistrict_meanflow_June +S_Nush_JuneSST, 
                  data = S_Model_dat_Nush_noChlA2000, na.action = na.omit)
summary(model_bestn_2000)
plot(residuals(model_bestn_2000))
#visreg(model_bestn)

#adj r2 .5184  (Full time series is .2274)


####Togiak

model_bestt_2000 <- lm(Tog ~ S_ChlA_GOAmagnitude + S_PDO_May + S_extent + S_Abundance, 
                  data = S_Model_dat_Tog_noChlA2000, na.action = na.omit)
summary(model_bestt_2000)
plot(residuals(model_bestt_2000))
#visreg(model_bestt)

#adj r2 .4449  (Full time series is .4116)


##Overall, it seems that the ChlA data is in fact important to our model fitting in recent decades.  While 
#7 years were added in fitting to 2000, the R2 values were greatly reduced or even halved.  I think we can 
#scrap further investigation into this.  I think it also shows that we are on the right track with focusing
#on recent years where we have all data available.  
# 
# AICc_lm <- function(model) {
#   n <- nobs(model)
#   k <- length(coef(model)) + 1  # coefficients + residual variance
#   
#   if ((n - k - 1) <= 0) return(NA)
#   
#   AIC(model) + (2 * k * (k + 1)) / (n - k - 1)
# }
# 
#
# # Ugashik Code for testing all combinations of predictors and selecting the best model via AIC

vars <- c(
  "S_Uga_lag1",
  "S_June_temp",
  "S_June_cumeastwind",
  "S_June_cumnorthwind",
  "S_ChlA_GOAmagnitude",
  "S_ChlA_GOAtiming",
  "S_PDO_May",
  "S_ENSO_May",
  "S_June_pressure",
  "S_extent",
  "S_Abundance",
  "S_GOA_SpringSST",
  "S_Ugashik_meanflow_June",
  "S_Uga_JuneSST"
)


results <- data.frame(
  model = character(),
  AICc = numeric(),
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

    fit <- lm(formula, data = S_Model_dat_Uga_ChlA, na.action = na.omit)

    results <- rbind(results, data.frame(
      model = deparse(formula),
      AICc = AICc_lm(fit),
      R2 = summary(fit)$r.squared
    ))

    models[[k]] <- fit
    k <- k + 1
  }
}


results <- results[order(results$AICc), ]

# Show top models
head(results)

# # Egegik Code for testing all combinations of predictors and selecting the best model via AIC

vars <- c(
  "S_Ege_lag1",
  "S_June_temp",
  "S_June_cumeastwind",
  "S_June_cumnorthwind",
  "S_ChlA_GOAmagnitude",
  "S_ChlA_GOAtiming",
  "S_PDO_May",
  "S_ENSO_May",
  "S_June_pressure",
  "S_extent",
  "S_Abundance",
  "S_GOA_SpringSST",
  "S_Egegik_meanflow_June",
  "S_Ege_JuneSST"
)


results <- data.frame(
  model = character(),
  AICc = numeric(),
  R2 = numeric(),
  stringsAsFactors = FALSE
)
models <- list()

k <- 1

for (i in 1:length(vars)) {
  combos <- combn(vars, i, simplify = FALSE)

  for (combo in combos) {

    formula <- as.formula(
      paste("Ege ~", paste(combo, collapse = " + "))####
    )

    fit <- lm(formula, data = S_Model_dat_Ege_ChlA, na.action = na.omit)

    results <- rbind(results, data.frame(
      model = deparse(formula),
      AICc = AICc_lm(fit),
      R2 = summary(fit)$r.squared
    ))

    models[[k]] <- fit
    k <- k + 1
  }
}


results <- results[order(results$AICc), ]

# Show top models
head(results)

# # Kvichak Code for testing all combinations of predictors and selecting the best model via AIC

vars <- c(
  "S_Kvi_lag1",
  "S_June_temp",
  "S_June_cumeastwind",
  "S_June_cumnorthwind",
  "S_ChlA_GOAmagnitude",
  "S_ChlA_GOAtiming",
  "S_PDO_May",
  "S_ENSO_May",
  "S_June_pressure",
  "S_extent",
  "S_Abundance",
  "S_GOA_SpringSST",
  "S_KvichakDistrict_meanflow_June",
  "S_Kvi_JuneSST"
)


results <- data.frame(
  model = character(),
  AICc = numeric(),
  R2 = numeric(),
  stringsAsFactors = FALSE
)
models <- list()

k <- 1

for (i in 1:length(vars)) {
  combos <- combn(vars, i, simplify = FALSE)

  for (combo in combos) {

    formula <- as.formula(
      paste("Kvi ~", paste(combo, collapse = " + "))####
    )

    fit <- lm(formula, data = S_Model_dat_Kvi_ChlA, na.action = na.omit)

    results <- rbind(results, data.frame(
      model = deparse(formula),
      AICc = AICc_lm(fit),
      R2 = summary(fit)$r.squared
    ))

    models[[k]] <- fit
    k <- k + 1
  }
}


results <- results[order(results$AICc), ]

# Show top models
head(results)


# Nushagak Code for testing all combinations of predictors and selecting the best model via AIC

vars <- c(
  "S_Nush_lag1",
  "S_June_temp",
  "S_June_cumeastwind",
  "S_June_cumnorthwind",
  "S_ChlA_GOAmagnitude",
  "S_ChlA_GOAtiming",
  "S_PDO_May",
  "S_ENSO_May",
  "S_June_pressure",
  "S_extent",
  "S_Abundance",
  "S_GOA_SpringSST",
  "S_NushagakDistrict_meanflow_June",
  "S_Nush_JuneSST"
)


results <- data.frame(
  model = character(),
  AICc = numeric(),
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

    fit <- lm(formula, data = S_Model_dat_Nush_ChlA, na.action = na.omit)

    results <- rbind(results, data.frame(
      model = deparse(formula),
      AICc = AICc_lm(fit),
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
  "S_Tog_lag1",
  "S_June_temp",
  "S_June_cumeastwind",
  "S_June_cumnorthwind",
  "S_ChlA_GOAmagnitude",
  "S_ChlA_GOAtiming",
  "S_PDO_May",
  "S_ENSO_May",
  "S_June_pressure",
  "S_extent",
  "S_Abundance",
  "S_GOA_SpringSST",
  "S_Togiak_meanflow_June",
  "S_Tog_JuneSST"
)


results <- data.frame(
  model = character(),
  AICc = numeric(),
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

    fit <- lm(formula, data = S_Model_dat_Tog_ChlA, na.action = na.omit)

    results <- rbind(results, data.frame(
      model = deparse(formula),
      AICc = AICc_lm(fit),
      R2 = summary(fit)$r.squared
    ))

    models[[k]] <- fit
    k <- k + 1
  }
}


results <- results[order(results$AICc), ]

# Show top models
head(results)


