# install.packages("lubridate")
# install.packages("icecdr")
# install.packages("dplyr")
# install.packages("terra")
# install.packages("here")
# install.packages("abind")
# install.packages("tibble")

library(icecdr)
library(lubridate)
library(dplyr)
library(terra)
library(here)
library(abind)
library(tibble)

#setwd("C:/Users/Student/Desktop/Bristol-Bay-Run-Timing")
workingdir <- "C:/Users/Student/Desktop/Bristol-Bay-Run-Timing"

#Set the time period over which data is being pulled
Pred_year <- 2026 #User input for all data up to current year

#Pull in the data

#' Extract PDO and ENSO Data
#'
#' @param years vector of year of interest
#' @param months vector of months of interest
#' @param lags vector of lags for months of interest
#'
#' @return list containing area and extent arrays(year,month), and description
#' @export
#'
#' @examples

get_pdo_npgo <- function(years=1963:Pred_year, months=5, lags=0) {
  
  
  require(rsoi)
  
  # Tests
  if(length(months)!=length(lags)) { stop("Lengths of months and lags differ!") }
  
  # Metadata
  n.years <- length(years)
  n.months <- length(months)
  
  # Output object
  pdo <- array(dim=c(n.years, n.months), dimnames=list(years,paste("pdo",months,lags,sep="_")))
  enso <- array(dim=c(n.years, n.months), dimnames=list(years,paste("enso",months,lags,sep="_")))
  
  # Extract data
  pdo.dat <- download_pdo()  # Index derived as the leading PC of monthly SST anomalies in the North Pacific
  enso.dat <- download_mei()  # EOF of SLP, SST, zonal winds, surface winds, outgoing longwave radiation.
  
  # Fill in specific values
  y <- 1
  for(y in 1:n.years) {
    m <- 1
    for(m in 1:n.months) {
      
      # TESTING
      # (years[y]+lags[m])
      # 
      # months[m]
      # 
      # pdo.dat[pdo.dat$Year==(years[y]+lags[m]) &
      #           month(pdo.dat$Date)==months[m],]
      # npgo[y,m] <- npgo.dat$NPGO[npgo.dat$Year==(years[y]+lags[m]) &
      # npgo.dat$Month==month.abb[months[m]]]
      
      # PDO
      if(nrow(pdo.dat[pdo.dat$Year==(years[y]+lags[m]) &
                      month(pdo.dat$Date)==months[m],]) != 0) {
        pdo[y,m] <- pdo.dat$PDO[pdo.dat$Year==(years[y]+lags[m]) &
                                  month(pdo.dat$Date)==months[m]]
      }else {
        pdo[y,m] <- NA
      }
      
      
      # ENSO
      if(nrow(enso.dat[enso.dat$Year==(years[y]+lags[m]) &
                       month(enso.dat$Date)==months[m],]) != 0) {
        enso[y,m] <- enso.dat$MEI[enso.dat$Year==(years[y]+lags[m]) &
                                    month(enso.dat$Date)==months[m]]
      }else {
        enso[y,m] <- NA
      }
      
    } #next m
  } # next y
  
  # Return section
  out <- NULL
  out$pdo <- pdo
  out$enso <- enso
  out$description <- "PDO (Index derived as the leading PC of monthly SST anomalies in the North Pacific) and ENSO (EOF of SLP, SST, zonal winds, surface winds, outgoing longwave radiation.) summarized by month and year."
  return(out)
}


Fit_data <- get_pdo_npgo(years=1963:Pred_year, months=5, lags=0) 

PDO <- as.data.frame(Fit_data$pdo)
PDO <- rownames_to_column(PDO, var = "name")%>%  rename(PDO_May = `pdo_5_0`) %>%  rename(YEAR = `name`)
PDO$YEAR <- as.numeric(PDO$YEAR)

ENSO <-as.data.frame(Fit_data$enso)
ENSO <- rownames_to_column(ENSO, var = "name")%>%  rename(ENSO_May = `enso_5_0`) %>%  rename(YEAR = `name`)
ENSO$YEAR <- as.numeric(ENSO$YEAR)


###ICE data 

ice <- sea_ice_index(
  hemisphere = "north", # or "south"
  resolution = "monthly", # or "monthly"
  use_cache = FALSE
)

file.copy(
  "C:/Users/Student/AppData/Local/Temp/RtmpE95n5P/sea-ice-index_north_monthly.csv", #Copy from wherever this was 
  #downloaded to into the working directory
  file.path(getwd(), "sea-ice-index_north_monthly.csv"),
  overwrite = TRUE
)

ice_data <- read.csv(here("sea-ice-index_north_monthly.csv"))

# Convert to millions of sq km
ice_data$extent <- ice_data$extent/1000000000000

ice_data <- ice_data %>%
  mutate(
    time = as.Date(time),
    YEAR = year(time),
    month = month(time)
  ) %>% filter(month == 5) %>%
  dplyr::select(YEAR, extent)


#Salmon Data 

Median_Timing <- read.csv(here("data/Median Table.csv"))  #needs updating 


######################


#merge data and scale, break up by region
Model_dat <- full_join(PDO, ENSO, by="YEAR")
Model_dat <- full_join(Model_dat, ice_data, by= "YEAR")
Model_dat <- full_join (Median_Timing, Model_dat, by="YEAR")

Model_dat<- Model_dat %>% mutate(S_extent = as.numeric(scale(extent)))
Model_dat<- Model_dat %>% mutate(S_PDO_May = as.numeric(scale(PDO_May)))
Model_dat <- Model_dat %>% mutate(S_ENSO_May = as.numeric(scale(ENSO_May)))

Pred_data <- Model_dat[nrow(Model_dat),c(2,18:20)]
Model_dat <- Model_dat[-nrow(Model_dat), ]


Model_dat_Uga <- Model_dat %>% dplyr::select(YEAR,Uga, S_PDO_May, S_ENSO_May, S_extent)
Model_dat_Ege <- Model_dat %>% dplyr::select(YEAR,Ege, S_PDO_May, S_ENSO_May, S_extent)
Model_dat_Kvi<- Model_dat %>% dplyr::select(YEAR,Kvi, S_PDO_May, S_ENSO_May, S_extent)
Model_dat_Nush<- Model_dat %>% dplyr::select(YEAR,Nush, S_PDO_May, S_ENSO_May, S_extent)
Model_dat_Tog <- Model_dat %>% dplyr::select(YEAR,Tog, S_PDO_May, S_ENSO_May, S_extent)


############


#refit best model with the most recent data


####Ugashik 


model_bestu <- lm(Uga ~ S_extent + S_ENSO_May + S_PDO_May,
                         data = Model_dat_Uga, na.action = na.omit)

####Egegik


model_beste <- lm(Ege ~ S_extent,
                         data = Model_dat_Ege, na.action = na.omit)

####Kvichak


model_bestk <- lm(Kvi ~ S_extent + S_ENSO_May + S_PDO_May, 
                         data = Model_dat_Kvi, na.action = na.omit)

####Nushagak

model_bestn <- lm(Nush ~ S_extent,
                         data = Model_dat_Nush, na.action = na.omit)

####Togiak

model_bestt <- lm(Tog ~ S_PDO_May +  S_extent, 
                         data = Model_dat_Tog, na.action = na.omit)


#####################


#Predict based on updated models

pred_uga <- predict(model_bestu, newdata = Pred_data)
pred_ege <- predict(model_beste, newdata = Pred_data)
pred_kvi <- predict(model_bestk, newdata = Pred_data)
pred_nush <- predict(model_bestn, newdata = Pred_data)
pred_tog <- predict(model_bestt, newdata = Pred_data)

pred_uga
pred_ege
pred_kvi
pred_nush
pred_tog




#################################
#For if/when needed

#RDS files, historical and current.  Combine as necessary
CPUE_Dist <- readRDS(here("data/CPUE data/Historical CPUE Dist.rds"))
Dist_Current <- readRDS(here("data/CPUE data/Current CPUE Dist.rds"))
CPUE_Dist <- abind::abind(CPUE_Dist, Dist_Current, along = 3)


CPUE_Interp <- as.data.frame(read.csv(here("data/CPUE data/Historical CPUE Interp.csv"),check.names = FALSE))
Interp_Current <- as.data.frame(read.csv(here("data/CPUE data/Current CPUE Interp.csv"),check.names = FALSE))
CPUE_Interp$`2025` <- Interp_Current$`2025`


#Break into bay and districts from the interpolated data
Bay_CPUE <- CPUE_Interp
Ugashik_CPUE <- as.data.frame(CPUE_Dist[,1,])
Egegik_CPUE <- as.data.frame(CPUE_Dist[,2,])
Kvichak_CPUE <- as.data.frame(CPUE_Dist[,3,])
Nushagak_CPUE <- as.data.frame(CPUE_Dist[,4,])
Togiak_CPUE <- as.data.frame(CPUE_Dist[,5,])

