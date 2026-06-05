library(here)
library(tidyr)
library(dplyr)
library(ggplot2)

here()


CatchEscapement <- read.csv(here("data/Catch and Escapement.csv"))
DailyDataEscapement <- CatchEscapement %>%filter(list.type=="escapement")%>%filter(year>1989)%>%
  group_by(dist_name, year, jdate) %>%
  summarise(
    DistrictEscapement = sum(number, na.rm = TRUE),
    .groups = "drop"
  )
DailyDataEscapement[is.na(DailyData)] <- 0

DailyDataCatch <- CatchEscapement %>%filter(list.type=="catch")%>%filter(year>1989)%>%
  group_by(dist_name, year, jdate) %>%
  summarise(
    Catch = sum(number, na.rm = TRUE),
    .groups = "drop"
  )
DailyDataCatch[is.na(DailyDataCatch)] <- 0



CumulativeProprotionEscapement <- DailyDataEscapement %>%
  group_by(dist_name, year) %>%
  arrange(dist_name, year, jdate) %>%
  mutate(
    
    Cum_Escapement = cumsum(DistrictEscapement),
    Cum_EscapementProp = Cum_Escapement / sum(DistrictEscapement),
  ) %>%
  ungroup()

CumulativeProprotionCatch <- DailyDataCatch %>%
  group_by(dist_name, year) %>%
  arrange(dist_name, year, jdate) %>%
  mutate(
    Cum_Catch = cumsum(Catch),
    Cum_CatchProp = Cum_Catch / sum(Catch)
  ) %>%
  ungroup()



#Plot for each district

ggplot(CumulativeProprotionEscapement,
       aes(x = jdate, y = Cum_EscapementProp, color = factor(year))) +
  geom_line(linewidth = 1) +
  facet_wrap(~ dist_name) +
  labs(
    x = "Day (jdate)",
    y = "Cumulative Catch Proportion",
    color = "Year"
  ) +
  scale_x_continuous(limits = c(170, 220))+
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title = element_text(size = 20, face = "bold"),
    
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 20)
  ) 


ggplot(CumulativeProprotionCatch,
       aes(x = jdate, y = Cum_CatchProp, color = factor(year))) +
  geom_line(linewidth = 1) +
  facet_wrap(~ dist_name) +
  labs(
    x = "Day (jdate)",
    y = "Cumulative Catch Proportion",
    color = "Year"
  ) +
  scale_x_continuous(limits = c(170, 220))+
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title = element_text(size = 20, face = "bold"),
    
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 20)
  ) 

