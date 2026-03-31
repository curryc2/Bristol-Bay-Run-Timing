library(ggplot2)
library(ggplot2)
library(viridis)
library(here)
library(visreg)
library(dplyr)


source(here("R/Pull Data.R"))
source(here("R/LM Code.R"))
source(here("R/LM Code NoChlA.R"))

#This code generates plots for the partial effects of all covariates for each of the models
#This includes for all districts with and without the ChlA data.  Plots are placed into separate
#folders within the Figs folder.  


#######Ugashik
#With ChlA
predictors <- c("Uga_lag1", "June_temp_anomaly", "PDO_May","ChlA_GOAtiming", "Uga_tempdiff","extent")


for (pred in predictors) {
  
  # Create visreg plot
  p <- visreg(model_bestu,
              pred,
              gg = TRUE,
              partial = TRUE)
  
  # Get the model frame and add YEAR
  Uga_data <- model.frame(model_bestu)
  Uga_data$YEAR <- Model_dat_Uga$YEAR[as.numeric(rownames(Uga_data))]
  p$data$YEAR <- Uga_data$YEAR
  
  # Add points colored by YEAR
  p_final <- p +
    geom_point(data = p$data,
               aes(x = x,
                   y = y,
                   color = YEAR),
               size = 4,
               inherit.aes = FALSE) +
    scale_color_viridis_c()
  
  # Save the plot
  ggsave(filename = paste0("figs/Ugashik Partial Effects/Uga_", pred, ".png"),
         plot = p_final,
         width = 6,
         height = 4,
         dpi = 300)
}

#No ChlA
predictors <- c("Uga_lag1", "June_temp_anomaly", "GOA_SpringSST_anomaly","Ugashik_meanflow_June")
Uga_lag1 + June_temp_anomaly + GOA_SpringSST_anomaly + Ugashik_meanflow_June

for (pred in predictors) {
  
  # Create visreg plot
  p <- visreg(model_bestu_noChlA,
              pred,
              gg = TRUE,
              partial = TRUE)
  
  # Get the model frame and add YEAR
  Uga_data <- model.frame(model_bestu_noChlA)
  Uga_data$YEAR <- Model_dat_Uga$YEAR[as.numeric(rownames(Uga_data))]
  p$data$YEAR <- Uga_data$YEAR
  
  # Add points colored by YEAR
  p_final <- p +
    geom_point(data = p$data,
               aes(x = x,
                   y = y,
                   color = YEAR),
               size = 4,
               inherit.aes = FALSE) +
    scale_color_viridis_c()
  
  # Save the plot
  ggsave(filename = paste0("figs/Ugashik Partial Effects/Uga_", pred, "_NoChlA.png"),
         plot = p_final,
         width = 6,
         height = 4,
         dpi = 300)
}


####Egegik
#With ChlA
predictors <- c("June_cumeastwind_anomaly", "PDO_Jan", "ENSO_Jan", "PDO_May","ChlA_GOAtiming", "ChlA_GOAmagnitude","extent", "Abundance", "Egegik_meanflow_June")

for (pred in predictors) {
  
  # Create visreg plot
  p <- visreg(model_beste,
              pred,
              gg = TRUE,
              partial = TRUE)
  
  # Get the model frame and add YEAR
  Ege_data <- model.frame(model_beste)
  Ege_data$YEAR <- Model_dat_Ege$YEAR[as.numeric(rownames(Ege_data))]
  p$data$YEAR <- Ege_data$YEAR
  
  # Add points colored by YEAR
  p_final <- p +
    geom_point(data = p$data,
               aes(x = x,
                   y = y,
                   color = YEAR),
               size = 4,
               inherit.aes = FALSE) +
    scale_color_viridis_c()
  
  # Save the plot
  ggsave(filename = paste0("figs/Egegik Partial Effects/Ege_", pred, ".png"),
         plot = p_final,
         width = 6,
         height = 4,
         dpi = 300)
}

#No ChlA
predictors <- c("Ege_lag1", "June_temp_anomaly","PDO_Jan", "PDO_May", "GOA_SpringSST_anomaly","extent","Abundance")

for (pred in predictors) {
  
  # Create visreg plot
  p <- visreg(model_beste_noChlA,
              pred,
              gg = TRUE,
              partial = TRUE)
  
  # Get the model frame and add YEAR
  Ege_data <- model.frame(model_beste_noChlA)
  Ege_data$YEAR <- Model_dat_Ege$YEAR[as.numeric(rownames(Ege_data))]
  p$data$YEAR <- Ege_data$YEAR
  
  # Add points colored by YEAR
  p_final <- p +
    geom_point(data = p$data,
               aes(x = x,
                   y = y,
                   color = YEAR),
               size = 4,
               inherit.aes = FALSE) +
    scale_color_viridis_c()
  
  # Save the plot
  ggsave(filename = paste0("figs/Egegik Partial Effects/Ege_", pred, "_NoChlA.png"),
         plot = p_final,
         width = 6,
         height = 4,
         dpi = 300)
}

####Kvichak

#With ChlA
predictors <- c("Kvichakproportion", "PDO_May","ChlA_GOAtiming", "ChlA_GOAmagnitude","extent")

for (pred in predictors) {
  
  # Create visreg plot
  p <- visreg(model_bestk,
              pred,
              gg = TRUE,
              partial = TRUE)
  
  # Get the model frame and add YEAR
  Kvi_data <- model.frame(model_bestk)
  Kvi_data$YEAR <- Model_dat_Kvi$YEAR[as.numeric(rownames(Kvi_data))]
  p$data$YEAR <- Kvi_data$YEAR
  
  # Add points colored by YEAR
  p_final <- p +
    geom_point(data = p$data,
               aes(x = x,
                   y = y,
                   color = YEAR),
               size = 4,
               inherit.aes = FALSE) +
    scale_color_viridis_c()
  
  # Save the plot
  ggsave(filename = paste0("figs/Kvichak Partial Effects/Kvi_", pred, ".png"),
         plot = p_final,
         width = 6,
         height = 4,
         dpi = 300)
}

#No ChlA
predictors <- c("Nak.Kvi_lag1", "June_temp_anomaly","PDO_Jan", "PDO_May", "GOA_SpringSST_anomaly","extent","Kvichakproportion")

for (pred in predictors) {
  
  # Create visreg plot
  p <- visreg(model_bestk_noChlA,
              pred,
              gg = TRUE,
              partial = TRUE)
  
  # Get the model frame and add YEAR
  Kvi_data <- model.frame(model_bestk_noChlA)
  Kvi_data$YEAR <- Model_dat_Kvi$YEAR[as.numeric(rownames(Kvi_data))]
  p$data$YEAR <- Kvi_data$YEAR
  
  # Add points colored by YEAR
  p_final <- p +
    geom_point(data = p$data,
               aes(x = x,
                   y = y,
                   color = YEAR),
               size = 4,
               inherit.aes = FALSE) +
    scale_color_viridis_c()
  
  # Save the plot
  ggsave(filename = paste0("figs/Kvichak Partial Effects/Kvi_", pred, "_NoChlA.png"),
         plot = p_final,
         width = 6,
         height = 4,
         dpi = 300)
}

####Nushagak

#With ChlA
predictors <- c("June_cumeastwind_anomaly", "June_pressure_anomaly","PDO_Jan","extent","Abundance", "June_temp_anomaly","ENSO_Jan", "Nush_tempdiff","ChlA_GOAtiming", "ChlA_GOAmagnitude")


for (pred in predictors) {
  
  # Create visreg plot
  p <- visreg(model_bestn,
              pred,
              gg = TRUE,
              partial = TRUE)
  
  # Get the model frame and add YEAR
  Nush_data <- model.frame(model_bestn)
  Nush_data$YEAR <- Model_dat_Nush$YEAR[as.numeric(rownames(Nush_data))]
  p$data$YEAR <- Nush_data$YEAR
  
  # Add points colored by YEAR
  p_final <- p +
    geom_point(data = p$data,
               aes(x = x,
                   y = y,
                   color = YEAR),
               size = 4,
               inherit.aes = FALSE) +
    scale_color_viridis_c()
  
  # Save the plot
  ggsave(filename = paste0("figs/Nushagak Partial Effects/Nush_", pred, ".png"),
         plot = p_final,
         width = 6,
         height = 4,
         dpi = 300)
}

#No ChlA
predictors <- c("June_cumeastwind_anomaly", "June_pressure_anomaly","PDO_Jan","extent","Abundance", "PDO_May","ENSO_May", "Nush_tempdiff","NushagakDistrict_meanflow_June")

for (pred in predictors) {
  
  # Create visreg plot
  p <- visreg(model_bestn_noChlA,
              pred,
              gg = TRUE,
              partial = TRUE)
  
  # Get the model frame and add YEAR
  Nush_data <- model.frame(model_bestn_noChlA)
  Nush_data$YEAR <- Model_dat_Nush$YEAR[as.numeric(rownames(Nush_data))]
  p$data$YEAR <- Nush_data$YEAR
  
  # Add points colored by YEAR
  p_final <- p +
    geom_point(data = p$data,
               aes(x = x,
                   y = y,
                   color = YEAR),
               size = 4,
               inherit.aes = FALSE) +
    scale_color_viridis_c()
  
  # Save the plot
  ggsave(filename = paste0("figs/Nushagak Partial Effects/Nush_", pred, "_NoChlA.png"),
         plot = p_final,
         width = 6,
         height = 4,
         dpi = 300)
}


####Togiak

#With ChlA
predictors <- c("June_temp_anomaly", "June_pressure_anomaly", "GOA_SpringSST_anomaly","extent","PDO_May","ChlA_GOAmagnitude", "Togiak_meanflow_June")


for (pred in predictors) {
  
  # Create visreg plot
  p <- visreg(model_bestt,
              pred,
              gg = TRUE,
              partial = TRUE)
  
  # Get the model frame and add YEAR
  Tog_data <- model.frame(model_bestt)
  Tog_data$YEAR <- Model_dat_Tog$YEAR[as.numeric(rownames(Tog_data))]
  p$data$YEAR <- Tog_data$YEAR
  
  # Add points colored by YEAR
  p_final <- p +
    geom_point(data = p$data,
               aes(x = x,
                   y = y,
                   color = YEAR),
               size = 4,
               inherit.aes = FALSE) +
    scale_color_viridis_c()
  
  # Save the plot
  ggsave(filename = paste0("figs/Togiak Partial Effects/Tog_", pred, ".png"),
         plot = p_final,
         width = 6,
         height = 4,
         dpi = 300)
}

#No ChlA
predictors <- c("PDO_May", "June_pressure_anomaly", "GOA_SpringSST_anomaly","extent")

for (pred in predictors) {
  
  # Create visreg plot
  p <- visreg(model_bestt_noChlA,
              pred,
              gg = TRUE,
              partial = TRUE)
  
  # Get the model frame and add YEAR
  Tog_data <- model.frame(model_bestt_noChlA)
  Tog_data$YEAR <- Model_dat_Tog$YEAR[as.numeric(rownames(Tog_data))]
  p$data$YEAR <- Tog_data$YEAR
  
  # Add points colored by YEAR
  p_final <- p +
    geom_point(data = p$data,
               aes(x = x,
                   y = y,
                   color = YEAR),
               size = 4,
               inherit.aes = FALSE) +
    scale_color_viridis_c()
  
  # Save the plot
  ggsave(filename = paste0("figs/Togiak Partial Effects/Tog_", pred, "_NoChlA.png"),
         plot = p_final,
         width = 6,
         height = 4,
         dpi = 300)
}



