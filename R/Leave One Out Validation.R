library(here)
library(BAS)
library(dplyr)


source(here("R/Pull Data.R"))

#I am going to do two seperate loops given the varying number of years and the complexity of cutting out variables in all aspects.  
#This will just be easier and yield the same results

#Datasets for each district 

districts<- list(
  Ege  = na.omit(Model_dat_Ege %>% dplyr::select(-c(YEAR, ENSO_Jan, PDO_Jan))),
  Kvi  = na.omit(Model_dat_Kvi %>% dplyr::select(-c(YEAR, ENSO_Jan, PDO_Jan))),
  Nush = na.omit(Model_dat_Nush %>% dplyr::select(-c(YEAR, ENSO_Jan, PDO_Jan))),
  Tog  = na.omit(Model_dat_Tog %>% dplyr::select(-c(YEAR, ENSO_Jan, PDO_Jan))),
  Uga  = na.omit(Model_dat_Uga %>% dplyr::select(-c(YEAR, ENSO_Jan, PDO_Jan)))
)

Years <- na.omit(Model_dat_Uga) %>% dplyr::select(YEAR)

# Predictors for each district and both model types

lm_variables<- list(
  Ege  = c("June_cumeastwind_anomaly","June_cumnorthwind_anomaly","PDO_May","ChlA_GOAmagnitude","June_pressure_anomaly","extent","Abundance","GOA_SpringSST_anomaly","Egegik_meanflow_June","Ege_JuneSST_anomaly"),
  Kvi  = c("Kvi_lag1","PDO_May","ENSO_May","ChlA_GOAmagnitude","ChlA_GOAtiming","extent","GOA_SpringSST_anomaly","KvichakDistrict_meanflow_June","Kvichakproportion","Kvi_JuneSST_anomaly"),
  Nush = c("Nush_lag1","June_temp_anomaly","June_cumeastwind_anomaly","June_cumnorthwind_anomaly","PDO_May","ENSO_May","ChlA_GOAmagnitude","ChlA_GOAtiming","June_pressure_anomaly","extent","GOA_SpringSST_anomaly","NushagakDistrict_meanflow_June","Igushikproportion","Nush_JuneSST_anomaly"),
  Tog  = c("Tog_lag1","June_temp_anomaly","June_cumeastwind_anomaly","June_cumnorthwind_anomaly","PDO_May","ENSO_May","ChlA_GOAmagnitude","June_pressure_anomaly","extent","Abundance","GOA_SpringSST_anomaly","Togiak_meanflow_June"),
  Uga  = c("PDO_May","ChlA_GOAmagnitude","June_pressure_anomaly","extent","GOA_SpringSST_anomaly","Ugashik_meanflow_June","Uga_JuneSST_anomaly")
) 

bas_variables <- list(
  Ege  = c("Ege_lag1","June_temp_anomaly","June_cumeastwind_anomaly","June_cumnorthwind_anomaly","PDO_May","ENSO_May","ChlA_GOAmagnitude","ChlA_GOAtiming","June_pressure_anomaly","extent","Abundance","GOA_SpringSST_anomaly","Egegik_meanflow_June","Ege_JuneSST_anomaly"),
  Kvi  = c("Kvi_lag1","June_temp_anomaly","June_cumeastwind_anomaly","June_cumnorthwind_anomaly","PDO_May","ENSO_May","ChlA_GOAmagnitude","ChlA_GOAtiming","June_pressure_anomaly","extent","Abundance","GOA_SpringSST_anomaly","KvichakDistrict_meanflow_June","Kvi_JuneSST_anomaly","Kvichakproportion"),
  Nush = c("Nush_lag1","June_temp_anomaly","June_cumeastwind_anomaly","June_cumnorthwind_anomaly","PDO_May","ENSO_May","ChlA_GOAmagnitude","ChlA_GOAtiming","June_pressure_anomaly","extent","Abundance","GOA_SpringSST_anomaly","NushagakDistrict_meanflow_June","Nush_JuneSST_anomaly","Igushikproportion"),
  Tog  = c("Tog_lag1","June_temp_anomaly","June_cumeastwind_anomaly","June_cumnorthwind_anomaly","PDO_May","ENSO_May","ChlA_GOAmagnitude","ChlA_GOAtiming","June_pressure_anomaly","extent","Abundance","GOA_SpringSST_anomaly","Togiak_meanflow_June","Tog_JuneSST_anomaly"),
  Uga  = c("Uga_lag1","June_temp_anomaly","June_cumeastwind_anomaly","June_cumnorthwind_anomaly","PDO_May","ENSO_May","ChlA_GOAmagnitude","ChlA_GOAtiming","June_pressure_anomaly","extent","Abundance","GOA_SpringSST_anomaly","Ugashik_meanflow_June","Uga_JuneSST_anomaly")
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
    R2       = c(r2_bas, r2_lm)
  )
  
  # ---- Save predictions ----
  predictions[[district]] <- bind_rows(
    data.frame(District = district, Model = "BAS", Year = Years, Observed = response, Predicted = pred_bas, Residual = response - pred_bas),
    data.frame(District = district, Model = "LM", Year = Years, Observed = response, Predicted = pred_lm, Residual = response - pred_lm)
  )
}

  # --- 5. Combine results ---
  performance_summary <- do.call(rbind, performance)
  predictions_summary <- do.call(rbind, predictions)

  
  
  
##############################################################################
#Now with no ChlA data
  
  
  #Datasets for each district 
  
  districts_noChlA<- list(
    Ege  = na.omit(Model_dat_Ege %>% dplyr::select(-c(YEAR, ENSO_Jan, PDO_Jan, ChlA_GOAtiming, ChlA_GOAmagnitude))),
    Kvi  = na.omit(Model_dat_Kvi %>% dplyr::select(-c(YEAR, ENSO_Jan, PDO_Jan, ChlA_GOAtiming, ChlA_GOAmagnitude))),
    Nush = na.omit(Model_dat_Nush %>% dplyr::select(-c(YEAR, ENSO_Jan, PDO_Jan, ChlA_GOAtiming, ChlA_GOAmagnitude))),
    Tog  = na.omit(Model_dat_Tog %>% dplyr::select(-c(YEAR, ENSO_Jan, PDO_Jan, ChlA_GOAtiming, ChlA_GOAmagnitude))),
    Uga  = na.omit(Model_dat_Uga %>% dplyr::select(-c(YEAR, ENSO_Jan, PDO_Jan, ChlA_GOAtiming, ChlA_GOAmagnitude)))
  )
  
  Years_noChlA <- na.omit(Model_dat_Uga %>% dplyr::select(-c( ChlA_GOAtiming, ChlA_GOAmagnitude))) %>% dplyr::select(YEAR)
  
  # Predictors for each district and both model types
  
  lm_variables_noChlA<- list(
    Ege  = c("June_cumeastwind_anomaly","June_cumnorthwind_anomaly","PDO_May","June_pressure_anomaly","extent","Abundance","GOA_SpringSST_anomaly","Egegik_meanflow_June","Ege_JuneSST_anomaly"),
    Kvi  = c("Kvi_lag1","PDO_May","ENSO_May","extent","GOA_SpringSST_anomaly","KvichakDistrict_meanflow_June","Kvichakproportion","Kvi_JuneSST_anomaly"),
    Nush = c("Nush_lag1","June_temp_anomaly","June_cumeastwind_anomaly","June_cumnorthwind_anomaly","PDO_May","ENSO_May","June_pressure_anomaly","extent","GOA_SpringSST_anomaly","NushagakDistrict_meanflow_June","Igushikproportion","Nush_JuneSST_anomaly"),
    Tog  = c("Tog_lag1","June_temp_anomaly","June_cumeastwind_anomaly","June_cumnorthwind_anomaly","PDO_May","ENSO_May","June_pressure_anomaly","extent","Abundance","GOA_SpringSST_anomaly","Togiak_meanflow_June"),
    Uga  = c("PDO_May","June_pressure_anomaly","extent","GOA_SpringSST_anomaly","Ugashik_meanflow_June","Uga_JuneSST_anomaly")
  ) 
  
  bas_variables_noChlA <- list(
    Ege  = c("Ege_lag1","June_temp_anomaly","June_cumeastwind_anomaly","June_cumnorthwind_anomaly","PDO_May","ENSO_May","June_pressure_anomaly","extent","Abundance","GOA_SpringSST_anomaly","Egegik_meanflow_June","Ege_JuneSST_anomaly"),
    Kvi  = c("Kvi_lag1","June_temp_anomaly","June_cumeastwind_anomaly","June_cumnorthwind_anomaly","PDO_May","ENSO_May","June_pressure_anomaly","extent","Abundance","GOA_SpringSST_anomaly","KvichakDistrict_meanflow_June","Kvi_JuneSST_anomaly","Kvichakproportion"),
    Nush = c("Nush_lag1","June_temp_anomaly","June_cumeastwind_anomaly","June_cumnorthwind_anomaly","PDO_May","ENSO_May","June_pressure_anomaly","extent","Abundance","GOA_SpringSST_anomaly","NushagakDistrict_meanflow_June","Nush_JuneSST_anomaly","Igushikproportion"),
    Tog  = c("Tog_lag1","June_temp_anomaly","June_cumeastwind_anomaly","June_cumnorthwind_anomaly","PDO_May","ENSO_May","June_pressure_anomaly","extent","Abundance","GOA_SpringSST_anomaly","Togiak_meanflow_June","Tog_JuneSST_anomaly"),
    Uga  = c("Uga_lag1","June_temp_anomaly","June_cumeastwind_anomaly","June_cumnorthwind_anomaly","PDO_May","ENSO_May","June_pressure_anomaly","extent","Abundance","GOA_SpringSST_anomaly","Ugashik_meanflow_June","Uga_JuneSST_anomaly")
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
      R2       = c(r2_bas, r2_lm)
    )
    
    # ---- Save predictions ----
    predictions_noChlA[[district]] <- bind_rows(
      data.frame(District = district, Model = "BAS", Year = Years_noChlA, Observed = response, Predicted = pred_bas, Residual = response - pred_bas),
      data.frame(District = district, Model = "LM", Year = Years_noChlA, Observed = response, Predicted = pred_lm, Residual = response - pred_lm)
    )
  }
  
  # --- 5. Combine results ---
  performance_summary_noChlA <- do.call(rbind, performance_noChlA)
  predictions_summary_noChlA <- do.call(rbind, predictions_noChlA)
  
  
#######################################################################
  
  #combine the results from the two different loops
  
  performance_summary$ChlA <- "Yes"
  predictions_summary$ChlA <- "Yes"
  
  performance_summary_noChlA$ChlA <- "No"
  predictions_summary_noChlA$ChlA <- "No"
  performance_all <- rbind(performance_summary, performance_summary_noChlA)
  predictions_all <- rbind(predictions_summary, predictions_summary_noChlA)
  
  