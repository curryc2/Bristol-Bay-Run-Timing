library(here)
library(tidyr)
library(dplyr)
library(ggplot2)
library(purrr)

here()

################CPUE from RDS files, historical and current

CPUE <- read.csv(here("data/CPUE data/Historical CPUE Interp.csv"), header=TRUE, check.names=FALSE, row.names = 1)
CPUE_Current <- read.csv(here("data/CPUE data/Current CPUE Interp.csv"), header=TRUE,check.names=FALSE, row.names = 1)
CPUE_data <- as.data.frame(t(cbind(CPUE, CPUE_Current)))

#################Bring in the catch and escapement data
CatchEscapement <- read.csv(here("data/Catch and Escapement.csv"))

DailyDataEscapement <- CatchEscapement %>%filter(list.type=="escapement")%>%filter(year>1989)%>%
  group_by(dist_name, year, jdate) %>%  #only have escapement from 2005 so select these years
  summarise(
    DistrictEscapement = sum(number, na.rm = TRUE),
    .groups = "drop"
  ) %>% filter(jdate>=161 & jdate <=210) %>% arrange(year,jdate) #filter to appropriate date range
DailyDataEscapement[is.na(DailyDataEscapement)] <- 0


DailyDataCatch <- CatchEscapement %>%filter(list.type=="catch")%>%filter(year>1989)%>%
  group_by(dist_name, year, jdate) %>%
  summarise(
    Catch = sum(number, na.rm = TRUE),
    .groups = "drop"
  ) %>% filter(jdate>=161 & jdate <=210) %>% arrange(year,jdate) #filter to appropriate date range
DailyDataCatch[is.na(DailyDataCatch)] <- 0


#Function for combining data with incorporated lag
lagsum_CatchandEscapment <- function(DailyDataCatch,
                                     DailyDataEscapement,
                                     lags){
  
  library(dplyr)
  library(tidyr)
  library(purrr)
  
  # lag vector should be in this order:
  # (Ugashik, Egegik, Kvichak, Nushagak, Togiak)
  
  total_catch <- DailyDataCatch %>%
    group_by(year, jdate) %>%
    summarise(
      CE_data = sum(Catch, na.rm = TRUE),
      .groups = "drop"
    ) %>% filter(year>1989)
  

  districts <- c("Ugashik", "Egegik", "Kvichak", "Nushagak", "Togiak")
  
  CE_data <- total_catch #This will be the final dataframe that I output, 
  #but initializing with total_catch

###lag the escapement data by district
  for(i in seq_along(districts)){
    
    esc <- DailyDataEscapement %>%
      filter(dist_name == districts[i]) %>%
      group_by(year, jdate) %>%
      summarise(
        esc = sum(DistrictEscapement, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(jdate = jdate - lags[i])
    
    CE_data <- CE_data %>%
      full_join(esc, by = c("year", "jdate")) %>%
      mutate(
        CE_data = CE_data + replace_na(esc, 0)
      ) %>%
      select(-esc)
  }
  

  CE_data <- CE_data %>%
    mutate(across(everything(), ~replace_na(., 0))) #
  
  # 6. FINAL PIVOT (year columns, jdate rows)
  CE_data <- CE_data %>%
    pivot_wider(
      names_from = year,
      values_from = CE_data,
      values_fill = 0,
      names_sort = TRUE
    ) %>%
    arrange(jdate)
  
  days <- CE_data$jdate
  CE_data <- CE_data %>%
    as.data.frame()
  rownames(CE_data) <- days 
  CE_data <- CE_data %>% select (-jdate)
  CE_data <- as.data.frame(t(CE_data))
  CE_data <- as.data.frame(CE_data %>% select (-c("155","156","157","158","159","160")))
  return(CE_data)
}

#use funtion, given lag and the already cleaned daily catch and escapement

lags <- c(4,2,2,2,6)

CE_data <- lagsum_CatchandEscapment(
  DailyDataCatch,
  DailyDataEscapement,
  lags
)

