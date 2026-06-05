library(here)
library(tidyr)
library(dplyr)
library(ggplot2)

source(here("R/Pull Data.r"))

Available_data <- Model_dat %>% dplyr::select(YEAR, bay, Abundance, Igushikproportion,extent,
                                              mean_temp_June, PDO_Jan, ENSO_May, 
                                              ChlA_GOAmagnitude, 
                                              NushagakDistrict_meanflow_June,GOA_SpringSST) %>%
                              rename('PDO' = PDO_Jan ) %>% rename('ENSO' = ENSO_May) %>% 
                              rename('Airport Data'= mean_temp_June)%>% 
                              rename('Median Run Timing' = bay) %>% rename('Pink Salmon Abundance' = Abundance)%>% 
                              rename('Proportion of Late District Run' = Igushikproportion )%>% 
                              rename('Gulf of Alaska ChlA Data' = ChlA_GOAmagnitude )%>% 
                              rename('River Meanflow' = NushagakDistrict_meanflow_June)%>% 
                              rename('Sea Surface Temperature' = GOA_SpringSST) %>% rename('Ice Extent' = extent)

Available_data_long <- Available_data %>%
  pivot_longer(-YEAR, names_to = "Source", values_to = "Value")%>%
  mutate(Present = ifelse(is.na(Value), 0, 1))

Available_data_long <- Available_data_long %>%
  filter(!is.na(Value))

Available_data_long$Source <- factor(
  Available_data_long$Source,
  levels = c("Median Run Timing", "Proportion of Late District Run", "Pink Salmon Abundance", "PDO", "Airport Data", "Ice Extent","ENSO", "River Meanflow", "Sea Surface Temperature", "Gulf of Alaska ChlA Data")
)

plot1 <- ggplot(Available_data_long, aes(x = YEAR, y = Source, fill = factor(Present))) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("0" = "white", "1" = "black")) +
  labs(fill = "Data\nAvailable") +
  scale_x_continuous(
    breaks = seq(1960, 2024, by = 10))+
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.5, "lines"),
    
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    strip.text = element_text(size = 16),
    
    legend.text = element_blank(),
    legend.key.size = unit(1, "cm"),
    legend.title = element_text(size = 20),
    legend.position = "none"
  ) +
  labs(x = "Year", y = "Data Type")

ggsave("figs/data_coverage_plot.png", plot = plot1, width = 12,
       height = 8,
       dpi = 300)

