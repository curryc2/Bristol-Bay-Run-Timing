library(here)
library(BAS)
library(dplyr)
library(Metrics)

source(here("R/Pull Data Scaled.R"))

#I am going to do two seperate loops given the varying number of years and the complexity of cutting out variables in all aspects.  
#This will just be easier and yield the same results

#Datasets for each district 

districts<- list(
  Ege  = na.omit(S_Model_dat_Ege_ChlA %>% dplyr::select(-YEAR)),
  Kvi  = na.omit(S_Model_dat_Kvi_ChlA %>% dplyr::select(-YEAR)),
  Nush = na.omit(S_Model_dat_Nush_ChlA %>% dplyr::select(-YEAR)),
  Tog  = na.omit(S_Model_dat_Tog_ChlA %>% dplyr::select(-YEAR)),
  Uga  = na.omit(S_Model_dat_Uga_ChlA %>% dplyr::select(-YEAR))
)

Years <- na.omit(S_Model_dat_Uga_ChlA) %>% dplyr::select(YEAR)

# Predictors for each district and both model types

lm_variables<- list(
  Ege  = c("S_June_cumeastwind","S_June_cumnorthwind","S_PDO_May","S_ChlA_GOAmagnitude","S_June_pressure","S_extent","S_Abundance","S_GOA_SpringSST","S_Egegik_meanflow_June","S_Ege_JuneSST"),
  Kvi  = c("S_Kvi_lag1","S_PDO_May","S_ENSO_May","S_ChlA_GOAmagnitude","S_ChlA_GOAtiming","S_extent","S_GOA_SpringSST","S_KvichakDistrict_meanflow_June","S_Kvichakproportion","S_Kvi_JuneSST"),
  Nush = c("S_Nush_lag1","S_June_temp","S_June_cumeastwind","S_June_cumnorthwind","S_PDO_May","S_ENSO_May","S_ChlA_GOAmagnitude","S_ChlA_GOAtiming","S_June_pressure","S_extent","S_GOA_SpringSST","S_NushagakDistrict_meanflow_June","S_Igushikproportion","S_Nush_JuneSST"),
  Tog  = c("S_Tog_lag1","S_June_temp","S_June_cumeastwind","S_June_cumnorthwind","S_PDO_May","S_ENSO_May","S_ChlA_GOAmagnitude","S_June_pressure","S_extent","S_Abundance","S_GOA_SpringSST","S_Togiak_meanflow_June"),
  Uga  = c("S_PDO_May","S_ChlA_GOAmagnitude","S_June_pressure","S_extent","S_GOA_SpringSST","S_Ugashik_meanflow_June","S_Uga_JuneSST")
) 

bas_variables <- list(
  Ege  = c("S_Ege_lag1","S_June_temp","S_June_cumeastwind","S_June_cumnorthwind","S_PDO_May","S_ENSO_May","S_ChlA_GOAmagnitude","S_ChlA_GOAtiming","S_June_pressure","S_extent","S_Abundance","S_GOA_SpringSST","S_Egegik_meanflow_June","S_Ege_JuneSST"),
  Kvi  = c("S_Kvi_lag1","S_June_temp","S_June_cumeastwind","S_June_cumnorthwind","S_PDO_May","S_ENSO_May","S_ChlA_GOAmagnitude","S_ChlA_GOAtiming","S_June_pressure","S_extent","S_Abundance","S_GOA_SpringSST","S_KvichakDistrict_meanflow_June","S_Kvi_JuneSST", "S_Kvichakproportion"),
  Nush = c("S_Nush_lag1","S_June_temp","S_June_cumeastwind","S_June_cumnorthwind","S_PDO_May","S_ENSO_May","S_ChlA_GOAmagnitude","S_ChlA_GOAtiming","S_June_pressure","S_extent","S_Abundance","S_GOA_SpringSST","S_NushagakDistrict_meanflow_June","S_Nush_JuneSST","S_Igushikproportion"),
  Tog  = c("S_Tog_lag1","S_June_temp","S_June_cumeastwind","S_June_cumnorthwind","S_PDO_May","S_ENSO_May","S_ChlA_GOAmagnitude","S_ChlA_GOAtiming","S_June_pressure","S_extent","S_Abundance","S_GOA_SpringSST","S_Togiak_meanflow_June","S_Tog_JuneSST"),
  Uga  = c("S_Uga_lag1","S_June_temp","S_June_cumeastwind","S_June_cumnorthwind","S_PDO_May","S_ENSO_May","S_ChlA_GOAmagnitude","S_ChlA_GOAtiming","S_June_pressure","S_extent","S_Abundance","S_GOA_SpringSST","S_Ugashik_meanflow_June","S_Uga_JuneSST")
)

#Store results
performance <- list()
predictions <- list()

#For each district
for(district in names(districts)){
  
  district_data <- districts[[district]]
  lm_vars <- lm_variables[[district]]
  bas_vars <- bas_variables[[district]]
  n <- nrow(district_data)
  
  # Storage for predictions
  pred_bas <- numeric(n)
  pred_lm  <- numeric(n)
  
  # For each year
  for(i in 1:n){
    
    train <- district_data[-i, ]
    test  <- district_data[i, ]
    
    formula_lm <- as.formula(
      paste(district, "~", paste(lm_vars, collapse = " + "))
    ) 
    
    formula_bas <- as.formula(
      paste(district, "~", paste(bas_vars, collapse = " + "))
    )
    
    
    
    # ---- LM ----
    mod_lm <- lm(formula_lm, data = train)
    pred_lm[i] <- predict(mod_lm, newdata = test)
    
    
    # ---- BAS ----
    mod_bas <- bas.lm(formula_bas,
                      data = train,
                      prior = "ZS-null",
                      modelprior = uniform(),
                      force.heredity = FALSE)
    pred_bas[i] <- as.numeric(predict(mod_bas,
                                      newdata = test[, bas_vars, drop = FALSE],
                                      estimator = "BMA")$fit)
  }
  
  # ---- Compute metrics ----
  response <- district_data[[district]] 
  rmse_bas <- sqrt(mean((response - pred_bas)^2))
  rmse_lm  <- sqrt(mean((response - pred_lm)^2))
  
  mape_bas <- mape(actual = response, predicted = pred_bas)
  mape_lm <- mape(actual = response, predicted = pred_lm)
  
  # BAS
  r2_bas <- 1 - sum((response - pred_bas)^2, na.rm = TRUE) /
    sum((response - mean(response, na.rm = TRUE))^2)
  
  
  
  # LM
  r2_lm <- 1 - sum((response - pred_lm)^2, na.rm = TRUE) /
    sum((response - mean(response, na.rm = TRUE))^2)
  
  # ---- Save metrics ----
  performance[[district]] <- data.frame(
    District = district,
    Model    = c("BAS","LM"),
    RMSE     = c(rmse_bas, rmse_lm),
    MAPE     = c(mape_bas, mape_lm),
    R2       = c(r2_bas, r2_lm)
  )
  
  # ---- Save predictions ----
  predictions[[district]] <- bind_rows(
    data.frame(District = district, Model = "BAS", Year = Years, Observed = response, Predicted = pred_bas, Residual = response - pred_bas),
    data.frame(District = district, Model = "LM", Year = Years, Observed = response, Predicted = pred_lm, Residual = response - pred_lm)
  )
}

# --- 5. Combine results ---
S_performance_summary_ChlA <- do.call(rbind, performance)
S_predictions_summary_ChlA <- do.call(rbind, predictions)




##############################################################################
#Now with no ChlA data


#Datasets for each district 

districts_noChlA <- list(
  Ege  = na.omit(S_Model_dat_Ege_noChlA %>% dplyr::select(-YEAR)),
  Kvi  = na.omit(S_Model_dat_Kvi_noChlA %>% dplyr::select(-YEAR)),
  Nush = na.omit(S_Model_dat_Nush_noChlA %>% dplyr::select(-YEAR)),
  Tog  = na.omit(S_Model_dat_Tog_noChlA %>% dplyr::select(-YEAR)),
  Uga  = na.omit(S_Model_dat_Uga_noChlA %>% dplyr::select(-YEAR))
)

Years_noChlA <- na.omit(S_Model_dat_Uga_noChlA) %>% dplyr::select(YEAR)
# Predictors for each district and both model types

lm_variables_noChlA<- list(
  Ege  = c("S_June_cumeastwind","S_June_cumnorthwind","S_PDO_May","S_June_pressure","S_extent","S_Abundance","S_GOA_SpringSST","S_Egegik_meanflow_June","S_Ege_JuneSST"),
  Kvi  = c("S_Kvi_lag1","S_PDO_May","S_ENSO_May","S_extent","S_GOA_SpringSST","S_KvichakDistrict_meanflow_June","S_Kvichakproportion","S_Kvi_JuneSST"),
  Nush = c("S_Nush_lag1","S_June_temp","S_June_cumeastwind","S_June_cumnorthwind","S_PDO_May","S_ENSO_May","S_June_pressure","S_extent","S_GOA_SpringSST","S_NushagakDistrict_meanflow_June","S_Igushikproportion","S_Nush_JuneSST"),
  Tog  = c("S_Tog_lag1","S_June_temp","S_June_cumeastwind","S_June_cumnorthwind","S_PDO_May","S_ENSO_May","S_June_pressure","S_extent","S_Abundance","S_GOA_SpringSST","S_Togiak_meanflow_June"),
  Uga  = c("S_PDO_May","S_June_pressure","S_extent","S_GOA_SpringSST","S_Ugashik_meanflow_June","S_Uga_JuneSST")
) 

bas_variables_noChlA <- list(
  Ege  = c("S_Ege_lag1","S_June_temp","S_June_cumeastwind","S_June_cumnorthwind","S_PDO_May","S_ENSO_May","S_June_pressure","S_extent","S_Abundance","S_GOA_SpringSST","S_Egegik_meanflow_June","S_Ege_JuneSST"),
  Kvi  = c("S_Kvi_lag1","S_June_temp","S_June_cumeastwind","S_June_cumnorthwind","S_PDO_May","S_ENSO_May","S_June_pressure","S_extent","S_Abundance","S_GOA_SpringSST","S_KvichakDistrict_meanflow_June","S_Kvi_JuneSST", "S_Kvichakproportion"),
  Nush = c("S_Nush_lag1","S_June_temp","S_June_cumeastwind","S_June_cumnorthwind","S_PDO_May","S_ENSO_May","S_June_pressure","S_extent","S_Abundance","S_GOA_SpringSST","S_NushagakDistrict_meanflow_June","S_Nush_JuneSST","S_Igushikproportion"),
  Tog  = c("S_Tog_lag1","S_June_temp","S_June_cumeastwind","S_June_cumnorthwind","S_PDO_May","S_ENSO_May","S_June_pressure","S_extent","S_Abundance","S_GOA_SpringSST","S_Togiak_meanflow_June","S_Tog_JuneSST"),
  Uga  = c("S_Uga_lag1","S_June_temp","S_June_cumeastwind","S_June_cumnorthwind","S_PDO_May","S_ENSO_May","S_June_pressure","S_extent","S_Abundance","S_GOA_SpringSST","S_Ugashik_meanflow_June","S_Uga_JuneSST")
)


#Store results
performance_noChlA <- list()
predictions_noChlA <- list()

#For each district
for(district in names(districts)){
  
  district_data <- districts_noChlA[[district]]
  lm_vars <- lm_variables_noChlA[[district]]
  bas_vars <- bas_variables_noChlA[[district]]
  n <- nrow(district_data)
  
  # Storage for predictions
  pred_bas <- numeric(n)
  pred_lm  <- numeric(n)
  
  # For each year
  for(i in 1:n){
    
    train <- district_data[-i, ]
    test  <- district_data[i, ]
    
    formula_lm <- as.formula(
      paste(district, "~", paste(lm_vars, collapse = " + "))
    ) 
    
    formula_bas <- as.formula(
      paste(district, "~", paste(bas_vars, collapse = " + "))
    )
    
    
    
    # ---- LM ----
    mod_lm <- lm(formula_lm, data = train)
    pred_lm[i] <- predict(mod_lm, newdata = test)
    
    
    # ---- BAS ----
    mod_bas <- bas.lm(formula_bas,
                      data = train,
                      prior = "ZS-null",
                      modelprior = uniform(),
                      force.heredity = FALSE)
    pred_bas[i] <- as.numeric(predict(mod_bas,
                                      newdata = test[, bas_vars, drop = FALSE],
                                      estimator = "BMA")$fit)
  }
  
  # ---- Compute metrics ----
  response <- district_data[[district]] 
  rmse_bas <- sqrt(mean((response - pred_bas)^2))
  rmse_lm  <- sqrt(mean((response - pred_lm)^2))
  
  
  mape_bas <- mape(actual = response, predicted = pred_bas)
  mape_lm <- mape(actual = response, predicted = pred_lm)
  
  # BAS
  r2_bas <- 1 - sum((response - pred_bas)^2, na.rm = TRUE) /
    sum((response - mean(response, na.rm = TRUE))^2)
  
  # LM
  r2_lm <- 1 - sum((response - pred_lm)^2, na.rm = TRUE) /
    sum((response - mean(response, na.rm = TRUE))^2)
  
  # ---- Save metrics ----
  performance_noChlA[[district]] <- data.frame(
    District = district,
    Model    = c("BAS","LM"),
    RMSE     = c(rmse_bas, rmse_lm),
    MAPE     = c(mape_bas, mape_lm),
    R2       = c(r2_bas, r2_lm)
  )
  
  # ---- Save predictions ----
  predictions_noChlA[[district]] <- bind_rows(
    data.frame(District = district, Model = "BAS", Year = Years_noChlA, Observed = response, Predicted = pred_bas, Residual = response - pred_bas),
    data.frame(District = district, Model = "LM", Year = Years_noChlA, Observed = response, Predicted = pred_lm, Residual = response - pred_lm)
  )
}

# --- 5. Combine results ---
S_performance_summary_noChlA <- do.call(rbind, performance_noChlA)
S_predictions_summary_noChlA <- do.call(rbind, predictions_noChlA)


#######################################################################

#combine the results from the two different loops

S_performance_summary_ChlA$ChlA <- "Yes"
S_predictions_summary_ChlA$ChlA <- "Yes"

S_performance_summary_noChlA$ChlA <- "No"
S_predictions_summary_noChlA$ChlA <- "No"
S_performance_all <- rbind(S_performance_summary_ChlA, S_performance_summary_noChlA)
S_predictions_all <- rbind(S_predictions_summary_ChlA, S_predictions_summary_noChlA)

