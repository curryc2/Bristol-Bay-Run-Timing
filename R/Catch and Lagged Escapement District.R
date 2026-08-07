library(here)
library(tidyr)
library(dplyr)
library(ggplot2)
library(purrr)
library(abind)

here()

################CPUE from RDS files, historical and current

CPUE <- readRDS(here("data/CPUE data/Historical CPUE Dist.rds"))
CPUE_Current <- readRDS(here("data/CPUE data/Current CPUE Dist.rds"))
CPUE_Current <- array(CPUE_Current,
                      dim = c(dim(CPUE_Current), 1))

# Combine along the year dimension
CPUE_data <- abind::abind(CPUE, CPUE_Current, along = 3)
CPUE_data<- aperm(CPUE_data, c(2, 3, 1))


#Lets give names to each dimension
daysPM <- 161:198

districts <- c("Ugashik", "Egegik", "Kvichak", "Nushagak", "Togiak")

years <- 2005:2025

dimnames(CPUE_data) <- list(
  District = districts,
  Year = years,
  Day = daysPM
)

#################Bring in the catch and escapement data
CatchEscapement <- read.csv(here("data/Catch and Escapement.csv"))

DailyDataEscapement <- CatchEscapement %>%filter(list.type=="escapement")%>%filter(year>2004)%>%
  group_by(dist_name, year, jdate) %>%  #only have escapement from 2005 so select these years
  summarise(
    DistrictEscapement = sum(number, na.rm = TRUE),
    .groups = "drop"
  ) %>% filter(jdate>=161 & jdate <=210) %>% arrange(year,jdate) #filter to appropriate date range
DailyDataEscapement[is.na(DailyDataEscapement)] <- 0


DailyDataCatch <- CatchEscapement %>%filter(list.type=="catch")%>%filter(year>2004)%>%
  group_by(dist_name, year, jdate) %>%
  summarise(
    Catch = sum(number, na.rm = TRUE),
    .groups = "drop"
  ) %>% filter(jdate>=161 & jdate <=210) %>% arrange(year,jdate) #filter to appropriate date range
DailyDataCatch[is.na(DailyDataCatch)] <- 0


daysCE <- 161:210

#Function for combining data with incorporated lag
lagsum_CatchandEscapment_district <- function(DailyDataCatch,
                                     DailyDataEscapement,
                                     lags){
  
  library(dplyr)
  library(tidyr)
  library(purrr)
  
  # lag vector should be in this order:
  # (Ugashik, Egegik, Kvichak, Nushagak, Togiak)
  
  districts <- c("Ugashik", "Egegik", "Kvichak", "Nushagak", "Togiak")
  
  CE_data <- array(
    0,
    dim = c(length(districts), length(years), length(daysCE)),
    dimnames = list(
      District = districts,
      Year = years,
      Day = daysCE
    )
  )

###lag the escapement data by district
  for(i in seq_along(districts)){
    
    # Create complete year-day grid
    grid <- expand.grid(
      dist_name = districts[i],
      year = years,
      jdate = daysCE
    )
    
    # Lag escapement dates
    esc <- DailyDataEscapement %>%
      filter(dist_name == districts[i]) %>%
      mutate(jdate = jdate - lags[i]) %>%
      select(dist_name, year, jdate, DistrictEscapement) %>%
      right_join(grid, by = c("dist_name", "year", "jdate")) %>%
      mutate(
        DistrictEscapement = tidyr::replace_na(DistrictEscapement, 0)
      )
    
    # Catch
    catch <- DailyDataCatch %>%
      filter(dist_name == districts[i]) %>%
      select(dist_name, year, jdate, Catch) %>%
      right_join(grid, by = c("dist_name", "year", "jdate")) %>%
      mutate(
        Catch = tidyr::replace_na(Catch, 0)
      )
    
    ce <- full_join(
      catch,
      esc,
      by = c("dist_name", "year", "jdate")
    ) %>%
      mutate(
        CE = Catch + DistrictEscapement
      ) %>%
      arrange(year, jdate)
    
    # Fill array
    CE_data[i,,] <- matrix(
      ce$CE,
      nrow = length(years),
      ncol = length(daysCE),
      byrow = TRUE
    )
   
  }
  
  return(CE_data)
}

#use funtion, given lag and the already cleaned daily catch and escapement

lags <- c(4,2,2,2,6)

CE_data <- lagsum_CatchandEscapment_district(
  DailyDataCatch,
  DailyDataEscapement,
  lags
)

