library(ggplot2)
library(ggplot2)
library(viridis)
library(here)
library(visreg)
library(dplyr)



source(here("R/LM Code Scaled AICc.R"))

#This code generates plots for the partial effects of all covariates for each of the models
#This includes for all districts with and without the ChlA data.  Plots are placed into separate
#folders within the Figs folder.  

Model_dat_Uga_BAS <- Model_dat_Uga_BAS %>% rename(
  "Cumulative East Wind (June)" = "S_June_cumeastwind" ,
  "Cumulative North Wind (June)" = "S_June_cumnorthwind",
  "PDO (May)" = "S_PDO_May",
  "Gulf of Alaska ChlA Spring Bloom Magnitude" = "S_ChlA_GOAmagnitude",
  "Gulf of Alaska ChlA Spring Bloom Timing" = "S_ChlA_GOAtiming",
  "Mean Air Pressure (June)" = "S_June_pressure",
  "Mean Air Temperature (June)" = "S_June_temp",
  "Sea Ice Extent (May)" = "S_extent",
  "Pink Salmon Abundance" = "S_Abundance",
  "Mean Gulf of Alaska Sea Surface Temperature (April/May)" = "S_GOA_SpringSST",
  "Total District River Meanflow (June)" = "S_Ugashik_meanflow_June",
  "Mean District Sea Surfact Temperature (June)" = "S_Uga_JuneSST",
  "Previous Year Median Return Date" = "S_Uga_lag1",
  "ENSO (May)" = "S_ENSO_May")

#######Ugashik
#With ChlA
predictors <- c("S_PDO_May","S_ChlA_GOAmagnitude","S_extent","S_ENSO_May")

x_labels <- c(
  S_PDO_May = "PDO (May)",
  S_ChlA_GOAmagnitude = "Gulf of Alaska ChlA Spring Bloom Magnitude",
  S_extent = "Sea Ice Extent (May)",
  S_ENSO_May = "ENSO (May)"
)

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
    scale_color_viridis_c() +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 16),
      
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 16)
    ) +
    ylab("Julian Day") +
    xlab(x_labels[pred])
  
  # Save the plot
  ggsave(filename = paste0("figs/Ugashik Partial Effects/Uga_", x_labels[pred], ".png"),
         plot = p_final,
         width = 6,
         height = 4,
         dpi = 300)
}

####Egegik
#With ChlA
predictors <- c("S_June_cumnorthwind","S_ChlA_GOAmagnitude","S_extent")

x_labels <- c(
  S_June_cumnorthwind = "Cumulative North Wind (June)",
  S_ChlA_GOAmagnitude = "Gulf of Alaska ChlA Spring Bloom Magnitude",
  S_extent = "Sea Ice Extent (May)"
)

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
    scale_color_viridis_c() +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 16),
      
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 16)
    ) +
    ylab("Julian Day") +
    xlab(x_labels[pred])
  
  # Save the plot
  ggsave(filename = paste0("figs/Egegik Partial Effects/Ege_", x_labels[pred], ".png"),
         plot = p_final,
         width = 6,
         height = 4,
         dpi = 300)
}


####Kvichak

#With ChlA
predictors <- c("S_PDO_May","S_ENSO_May","S_ChlA_GOAmagnitude","S_ChlA_GOAtiming","S_extent")
x_labels <- c(
  S_PDO_May = "PDO (May)",
  S_ChlA_GOAmagnitude = "Gulf of Alaska ChlA Spring Bloom Magnitude",
  S_ChlA_GOAtiming = "Gulf of Alaska ChlA Spring Bloom Timing",
  S_extent = "Sea Ice Extent (May)",
  S_ENSO_May = "ENSO (May)"
)

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
    scale_color_viridis_c() +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 16),
      
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 16)
    ) +
    ylab("Julian Day") +
    xlab(x_labels[pred])
  
  # Save the plot
  ggsave(filename = paste0("figs/Kvichak Partial Effects/Kvi_", x_labels[pred], ".png"),
         plot = p_final,
         width = 6,
         height = 4,
         dpi = 300)
}

####Nushagak

#With ChlA
predictors <- c("S_extent","S_June_temp","S_Abundance")

x_labels <- c(
  S_June_temp = "Mean Air Temperature (June)",
  S_extent = "Sea Ice Extent (May)",
  S_Abundance = "Pink Salmon Abundance"
)

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
    scale_color_viridis_c() +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 16),
      
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 16)
    ) +
    ylab("Julian Day") +
    xlab(x_labels[pred])
  
  # Save the plot
  ggsave(filename = paste0("figs/Nushagak Partial Effects/Nush_", x_labels[pred], ".png"),
         plot = p_final,
         width = 6,
         height = 4,
         dpi = 300)
}


####Togiak

#With ChlA
predictors <-  c("S_June_cumeastwind","S_PDO_May","S_ChlA_GOAmagnitude","S_extent","S_Togiak_meanflow_June")

x_labels <- c(
  S_PDO_May = "PDO (May)",
  S_ChlA_GOAmagnitude = "Gulf of Alaska ChlA Spring Bloom Magnitude",
  S_extent = "Sea Ice Extent (May)",
  S_June_cumeastwind = "Cumulative East Wind (June)",
  S_Togiak_meanflow_June = "Total District River Meanflow (June)"
)

for (pred in predictors) {
  
  # Create visreg plot
  p_final <- p +
    geom_point(data = p$data,
               aes(x = x,
                   y = y,
                   color = YEAR),
               size = 4,
               inherit.aes = FALSE) +
    scale_color_viridis_c() +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 16),
      
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 16)
    ) +
    ylab("Julian Day") +
    xlab(x_labels[pred])
  
  # Save the plot
  ggsave(filename = paste0("figs/Togiak Partial Effects/Tog_", x_labels[pred], ".png"),
         plot = p_final,
         width = 6,
         height = 4,
         dpi = 300)
}


