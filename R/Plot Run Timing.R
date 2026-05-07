require(here)
require(tidyverse)
require(dplyr)
require(corrplot)
library(visreg)
library(ggplot2)
library(tidyr)

#Lets plot the run timing by district

here()

Median_Timing <- read.csv(here("data/Median Table.csv"))

Median_Timing_long <- Median_Timing %>%
  pivot_longer(
    cols = c("Uga","Ege","Kvi","Nush","Tog"),
    names_to = "District",
    values_to = "Timing"
  )

#Plot data 
Runtiming_plot <- ggplot(Median_Timing_long, aes(x = YEAR, y = Timing, color=District)) +
  geom_rect(
    aes(xmin = 2002, xmax = 2024, ymin = 178, ymax = 210),
    fill = "grey",
    alpha = 0.1,
    inherit.aes = FALSE
  )+

  geom_line(size=1.5)+
  scale_size(range = c(2, 16)) +
  scale_x_continuous(breaks = seq(1965,
                                  2025,
                                  by = 5)) +
  scale_y_continuous(breaks = seq(178,
                                  210,
                                  by = 4))+
  scale_color_discrete(labels = c(
    Uga  = "Ugashik",
    Ege  = "Egegik",
    Kvi  = "Kvichak",
    Nush = "Nushagak",
    Tog  = "Togiak"
  ))+
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title = element_text(size = 20, face = "bold"),
    
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 20)
  ) +
  labs(
    x = "Year",
    y = "Julian Day",
    color = "District"
  )

#Save the plot

ggsave(filename = paste0("figs/Run Timing.png"),
       plot = Runtiming_plot,
       width = 18,
       height = 12,
       dpi = 300)
