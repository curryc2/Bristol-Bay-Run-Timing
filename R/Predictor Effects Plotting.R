require(here)
require(tidyverse)
require(dplyr)
require(corrplot)
library(visreg)


#source(here("R/LM Code Scaled.R"))
source(here("R/LM Code Scaled AICc.R"))

#Lets extract all coefficients and use these to make the plot to visualize effect size of with standardized predictors

models <- list(
  Ugashik  = model_bestu,
  Egegik  = model_beste,
  Kvichak  = model_bestk,
  Nushagak = model_bestn,
  Togiak  = model_bestt
)

#Make a data frame of the coefficients
Plot_data <- bind_rows(
  lapply(names(models), function(district){
    
    mod <- models[[district]]
    
    tidy(mod) %>%
      filter(term != "(Intercept)") %>%
      mutate(
        District = district,
        Effect = estimate,
        AbsEffect = abs(estimate)
      )
  })
)


#Order districts geographically

Plot_data$District <- factor(Plot_data$District,
                             levels = c("Ugashik", "Egegik", "Kvichak", "Nushagak", "Togiak"))

#Provide common names for predictors to ensure they plot on the same row, we can adapt these to anything 
Plot_data <- Plot_data %>% mutate(term = recode(term,
                       "S_June_cumeastwind" = "Cumulative East wind (June)",
                       "S_June_cumnorthwind" = "Cumulative North wind (June)",
                       "S_PDO_May" = "PDO (May)",
                       "S_ChlA_GOAmagnitude" = "Gulf of Alaska ChlA Spring Bloom Magnitude",
                       "S_ChlA_GOAtiming" = "Gulf of Alaska ChlA Spring Bloom Timing",
                       "S_June_pressure" = "Mean Air Pressure (June)",
                       "S_June_temp" = "Mean Air Temperature (June)",
                       "S_extent" = "Sea Ice Extent (May)",
                       "S_Abundance" = "Pink Salmon Abundance",
                       "S_GOA_SpringSST" = "Mean Gulf of Alaska Sea Surface Temperature (April/May)",
                       "S_Egegik_meanflow_June" = "Total District River Meanflow (June)",
                       "S_KvichakDistrict_meanflow_June" = "Total District River Meanflow (June)",
                       "S_NushagakDistrict_meanflow_June" = "Total District River Meanflow (June)",
                       "S_Togiak_meanflow_June" = "Total District River Meanflow (June)",
                       "S_Ugashik_meanflow_June" = "Total District River Meanflow (June)",
                       "S_Ege_JuneSST" = "Mean District Sea Surfact Temperature (June)",
                       "S_Kvi_JuneSST" = "Mean District Sea Surfact Temperature (June)",
                       "S_Nush_JuneSST" = "Mean District Sea Surfact Temperature (June)",
                       "S_Tog_JuneSST" = "Mean District Sea Surfact Temperature (June)",
                       "S_Uga_JuneSST" = "Mean District Sea Surfact Temperature (June)",
                       "S_Ege_lag1" = "Previous Year Median Return Date",
                       "S_Kvi_lag1" = "Previous Year Median Return Date",
                       "S_Nush_lag1" = "Previous Year Median Return Date",
                       "S_Tog_lag1" = "Previous Year Median Return Date",
                       "S_Uga_lag1" = "Previous Year Median Return Date",
                       "S_ENSO_May" = "ENSO (May)",
                       "S_June_cumnorthwind" = "Cumulative North wind (June)",
                       "S_June_cumnorthwind" = "Cumulative North wind (June)",
                       "S_June_cumnorthwind" = "Cumulative North wind (June)",
                       "S_June_cumnorthwind" = "Cumulative North wind (June)",
                       "S_June_cumnorthwind" = "Cumulative North wind (June)",
                       "S_Kvichakproportion" = "Proportion of Late District Run",
                       "S_Igushikproportion" = "Proportion of Late District Run",
  ))  


#Ensure all are included, with effect value of zero added for all predictors not in final models
all_terms <- unique(Plot_data$term)

Plot_data <- Plot_data %>%
  complete(District, term = all_terms)

Plot_data <- Plot_data %>%
  mutate(
    Effect = ifelse(is.na(Effect), 0, Effect),
    AbsEffect  = abs(Effect)
  )


#Order rows so largest effect plots on the bottom
Plot_data <- Plot_data %>% group_by(term) %>% mutate(mean_effect = mean(AbsEffect, na.rm = TRUE)) %>% ungroup()
Plot_data$term <- reorder(Plot_data$term, -Plot_data$mean_effect)



#Plot data 
Predictor_plot <- ggplot(Plot_data, aes(x = District, y = term)) +
  geom_point(aes(size = AbsEffect, color = Effect)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  scale_size(range = c(2, 16)) +
  guides(size = "none", color = guide_colorbar(barwidth = 2, barheight = 8))+
  scale_x_discrete(
    breaks = c("Ugashik","Egegik", "Kvichak", "Nushagak", "Togiak"),
    labels = c(
      "Ugashik"  = expression(atop("Ugashik", atop((R^2 == 0.73), (RMSE == 3.09)))),
      "Egegik"  = expression(atop("Egegik", atop((R^2 == 0.61), (RMSE == 2.85)))),
      "Kvichak"  = expression(atop("Kvichak", atop((R^2 == 0.73), (RMSE == 2.11)))),
      "Nushagak" = expression(atop("Nushagak", atop((R^2 == 0.39), (RMSE == 1.95)))),
      "Togiak"  = expression(atop("Togiak", atop((R^2 == 0.75), (RMSE == 2.08))))
    )
  )+
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 20)
  ) +
  labs(
    x = "",
    y = "Predictor",
    color = "Effect"
  )

#Save the plot

ggsave(filename = paste0("figs/Predictor Effects AICc.png"),
       plot = Predictor_plot,
       width = 16,
       height = 18,
       dpi = 300)


