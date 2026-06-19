install.packages("tidyr")
install.packages("ggplot2")
library(here)
library(tidyr)
library(dplyr)
library(ggplot2)
library(purrr)

here()


#CPUE from RDS files, historical and current.  Combine as necessary
CPUE_Dist <- readRDS(here("data/CPUE data/Historical CPUE Dist.rds"))
Dist_Current <- readRDS(here("data/CPUE data/Current CPUE Dist.rds"))
CPUE_Dist <- abind::abind(CPUE_Dist, Dist_Current, along = 3)
dimnames(CPUE_Dist)[[3]][dimnames(CPUE_Dist)[[3]] == ""] <- "2025"


DailyDataCPUE <- as.data.frame.table(CPUE_Dist, responseName = "CPUE")
names(DailyDataCPUE) <- c("jdate", "dist_name", "year", "CPUE")
DailyDataCPUE$year <- as.numeric(as.character(DailyDataCPUE$year))
DailyDataCPUE$day <- as.numeric(as.character(DailyDataCPUE$day))
DailyDataCPUE$dist_name <- recode(
  DailyDataCPUE$dist_name,
  "321" = "Ugashik",
  "322" = "Egegik",
  "324" = "Naknek-Kvichak",
  "325" = "Nushagak",
  "326" = "Togiak"
)
head(DailyDataCPUE)


CatchEscapement <- read.csv(here("data/Catch and Escapement.csv"))
DailyDataEscapement <- CatchEscapement %>%filter(list.type=="escapement")%>%filter(year>1989)%>%
  group_by(dist_name, year, jdate) %>%
  summarise(
    DistrictEscapement = sum(number, na.rm = TRUE),
    .groups = "drop"
  )
DailyDataEscapement[is.na(DailyDataEscapement)] <- 0

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


CumulativeProprotionCPUE <- DailyDataCPUE %>%
  group_by(dist_name, year) %>%
  arrange(dist_name, year, jdate) %>%
  mutate(
    Cum_CPUE = cumsum(CPUE),
    Cum_CPUEProp = Cum_CPUE / sum(CPUE)
  ) %>%
  ungroup()

CumulativeProprotionCPUE$jdate <- as.integer(as.character(CumulativeProprotionCPUE$jdate))
CumulativeProprotionCPUE$dist_name <- as.character(CumulativeProprotionCPUE$dist_name)
CumulativeProprotionCPUE$year <- as.integer(CumulativeProprotionCPUE$year)

Plot_Data <- list(
  CumulativeProprotionEscapement,
  CumulativeProprotionCPUE,
  CumulativeProprotionCatch) %>%
  reduce(full_join, by = c("year", "dist_name", "jdate"))

Plot_Data <- Plot_Data %>%
  arrange(dist_name, year, jdate)

Plot_Uga <- Plot_Data%>% filter(dist_name=="Ugashik")%>% filter(year>=2005)
Plot_Ege<- Plot_Data%>% filter(dist_name=="Egegik")%>% filter(year>=2005)
Plot_Kvi <- Plot_Data%>% filter(dist_name=="Naknek-Kvichak")%>% filter(year>=2005)
Plot_Nush <- Plot_Data%>% filter(dist_name=="Nushagak")%>% filter(year>=2005)
Plot_Tog <- Plot_Data%>% filter(dist_name=="Togiak")%>% filter(year>=2005)





#Plot for each district


Uga_plot <- ggplot(Plot_Uga, aes(x = jdate)) +
  geom_line(aes(y = Cum_CPUEProp, color = "CPUE"), linewidth = 1) +
  geom_line(aes(y = Cum_CatchProp, color = "Catch"), linewidth = 1) +
  geom_line(aes(y = Cum_EscapementProp, color = "Escapement"), linewidth = 1) +
  facet_wrap(~year) +
  
  scale_color_manual(
    values = c(
      "Escapement" = "black",
      "Catch" = "blue",
      "CPUE" = "red"
    ),
    breaks = c("CPUE","Catch","Escapement")  # <-- THIS controls legend order
  ) +
  
  labs(
    x = "Day (jdate)",
    y = "Cumulative Proportion",
    color = "Metric"
  ) +
  
  scale_x_continuous(limits = c(170, 220)) +
  
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
ggsave(filename = paste0("figs/Ugashik_Proportions.png"),
       plot = Uga_plot,
       width = 24,
       height = 12,
       dpi = 300)




Ege_plot <- ggplot(Plot_Ege, aes(x = jdate)) +
  geom_line(aes(y = Cum_CPUEProp, color = "CPUE"), linewidth = 1) +
  geom_line(aes(y = Cum_CatchProp, color = "Catch"), linewidth = 1) +
  geom_line(aes(y = Cum_EscapementProp, color = "Escapement"), linewidth = 1) +
  facet_wrap(~year) +
  
  scale_color_manual(
    values = c(
      "Escapement" = "black",
      "Catch" = "blue",
      "CPUE" = "red"
    ),
    breaks = c("CPUE","Catch","Escapement")  # <-- THIS controls legend order
  ) +
  
  labs(
    x = "Day (jdate)",
    y = "Cumulative Proportion",
    color = "Metric"
  ) +
  
  scale_x_continuous(limits = c(170, 220)) +
  
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
ggsave(filename = paste0("figs/Egegik_Proportions.png"),
       plot = Ege_plot,
       width = 24,
       height = 12,
       dpi = 300)



Kvi_plot <- ggplot(Plot_Kvi, aes(x = jdate)) +
  geom_line(aes(y = Cum_CPUEProp, color = "CPUE"), linewidth = 1) +
  geom_line(aes(y = Cum_CatchProp, color = "Catch"), linewidth = 1) +
  geom_line(aes(y = Cum_EscapementProp, color = "Escapement"), linewidth = 1) +
  facet_wrap(~year) +
  
  scale_color_manual(
    values = c(
      "Escapement" = "black",
      "Catch" = "blue",
      "CPUE" = "red"
    ),
    breaks = c("CPUE","Catch","Escapement")  # <-- THIS controls legend order
  ) +
  
  labs(
    x = "Day (jdate)",
    y = "Cumulative Proportion",
    color = "Metric"
  ) +
  
  scale_x_continuous(limits = c(170, 220)) +
  
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
ggsave(filename = paste0("figs/Kvichak_Proportions.png"),
       plot = Kvi_plot,
       width = 24,
       height = 12,
       dpi = 300)



Nush_plot <- ggplot(Plot_Nush, aes(x = jdate)) +
  geom_line(aes(y = Cum_CPUEProp, color = "CPUE"), linewidth = 1) +
  geom_line(aes(y = Cum_CatchProp, color = "Catch"), linewidth = 1) +
  geom_line(aes(y = Cum_EscapementProp, color = "Escapement"), linewidth = 1) +
  facet_wrap(~year) +
  
  scale_color_manual(
    values = c(
      "Escapement" = "black",
      "Catch" = "blue",
      "CPUE" = "red"
    ),
    breaks = c("CPUE","Catch","Escapement")  # <-- THIS controls legend order
  ) +
  
  labs(
    x = "Day (jdate)",
    y = "Cumulative Proportion",
    color = "Metric"
  ) +
  
  scale_x_continuous(limits = c(170, 220)) +
  
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
ggsave(filename = paste0("figs/Nushagak_Proportions.png"),
       plot = Nush_plot,
       width = 24,
       height = 12,
       dpi = 300)



Tog_plot <- ggplot(Plot_Tog, aes(x = jdate)) +
  geom_line(aes(y = Cum_CPUEProp, color = "CPUE"), linewidth = 1) +
  geom_line(aes(y = Cum_CatchProp, color = "Catch"), linewidth = 1) +
  geom_line(aes(y = Cum_EscapementProp, color = "Escapement"), linewidth = 1) +
  facet_wrap(~year) +
  
  scale_color_manual(
    values = c(
      "Escapement" = "black",
      "Catch" = "blue",
      "CPUE" = "red"
    ),
    breaks = c("CPUE","Catch","Escapement")  # <-- THIS controls legend order
  ) +
  
  labs(
    x = "Day (jdate)",
    y = "Cumulative Proportion",
    color = "Metric"
  ) +
  
  scale_x_continuous(limits = c(170, 220)) +
  
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
ggsave(filename = paste0("figs/Togiak_Proportions.png"),
       plot = Tog_plot,
       width = 24,
       height = 12,
       dpi = 300)
