require(here)
require(tidyverse)
require(dplyr)
require(corrplot)
library(visreg)
library(ggplot2)
library(tidyr)

here()

source(here("R/Leave One Out Validation Scaled AICc.R"))


####Lets plot the predictions by district against actual observations

Prediction_plotdata <- S_predictions_summary_ChlA

Prediction_plotdata$District <- factor(Prediction_plotdata$District,
                                  levels = c("Uga", "Ege", "Kvi", "Nush", "Tog"))
label_map <- c(
  Uga  = "Ugashik",
  Ege  = "Egegik",
  Kvi  = "Kvichak",
  Nush = "Nushagak",
  Tog  = "Togiak"
)


Predictions_plot <-ggplot(Prediction_plotdata) +
  geom_line(aes(x=YEAR, y=Observed), size=1.2)+
  geom_point(aes(x=YEAR, y=Predicted, color=Model), size=3)+
  scale_x_continuous(breaks = seq(1965,
                                  2025,
                                  by = 5)) +
  scale_y_continuous(
    breaks = seq(178,
                 210,
                 by = 4))+
  facet_wrap(~ District, labeller = labeller(District = label_map), nrow = 1)+
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.5, "lines"),
    
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 16, face = "bold"),
    
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 20)) +
  labs(x = "Year", y = "Julian Day")

ggsave(filename = paste0("figs/Predictions.png"),
       plot = Predictions_plot,
       width = 20,
       height = 10,
       dpi = 300)



