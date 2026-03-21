library(here)
library(tidyr)
library(dplyr)
library(ggplot2)

source(here("data/Pull Data.r"))

Available_data <- Model_dat %>% dplyr::select(YEAR, bay, Abundance, Igushikproportion,extent,
                                              mean_temp_June, PDO_Jan, ENSO_May, 
                                              ChlA_GOAmeanmagnitude, 
                                              NushagakDistrict_meanflow_June,GOA_SpringSST) %>%
                              rename('PDO data' = PDO_Jan ) %>% rename('ENSO data' = ENSO_May) %>% 
                              rename('Airport data (Temp, Wind, Pressure)'= mean_temp_June)%>% 
                              rename('Run Timing' = bay) %>% rename('Pink Abundance' = Abundance)%>% 
                              rename('River run proportions' = Igushikproportion )%>% 
                              rename('ChlA data' = ChlA_GOAmeanmagnitude )%>% 
                              rename('River Flow' = NushagakDistrict_meanflow_June)%>% 
                              rename('SST data' = GOA_SpringSST) %>% rename('Ice extent' = extent)

Available_data_long <- Available_data %>%
  pivot_longer(-YEAR, names_to = "Source", values_to = "Value")%>%
  mutate(Present = ifelse(is.na(Value), 0, 1))

Available_data_long <- Available_data_long %>%
  filter(!is.na(Value))

plot1 <- ggplot(Available_data_long, aes(x = YEAR, y = Source, fill = factor(Present))) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("0" = "white", "1" = "black")) +
  labs(fill = "Data\nAvailable") +
  theme_minimal()

ggsave("figs/data_coverage_plot.png", plot = plot1, width = 12,
       height = 8,
       dpi = 300)

