library (here)
library (dplyr)
library (tidyverse)

here()#Bristol-Bay-Run-Timing base working directory 


Alagnak <- read.csv(here("data/Streamflow data/At the mouth/Alagnak.csv")) #GRFR file output
Egegik <- read.csv(here("data/Streamflow data/At the mouth/Egegik.csv"))
Igushik <- read.csv(here("data/Streamflow data/At the mouth/Igushik.csv"))
Kvichak <- read.csv(here("data/Streamflow data/At the mouth/Kvichak.csv"))
Naknek <- read.csv(here("data/Streamflow data/At the mouth/Naknek.csv"))
Nushagak <- read.csv(here("data/Streamflow data/At the mouth/Nushagak.csv"))
Togiak <- read.csv(here("data/Streamflow data/At the mouth/Togiak.csv"))
Ugashik <- read.csv(here("data/Streamflow data/At the mouth/Ugashik.csv"))
Wood <- read.csv(here("data/Streamflow data/At the mouth/Wood.csv"))

#Combine into a data frame
Streamflow <- full_join(Alagnak, Egegik, by="Date")
Streamflow <- full_join(Streamflow, Igushik, by="Date")
Streamflow <- full_join(Streamflow, Kvichak, by="Date")
Streamflow <- full_join(Streamflow, Naknek, by="Date")
Streamflow <- full_join(Streamflow, Nushagak, by="Date")
Streamflow <- full_join(Streamflow, Togiak, by="Date")
Streamflow <- full_join(Streamflow, Ugashik, by="Date")
Streamflow <- full_join(Streamflow, Wood, by="Date")

#Calculate cumulative discharge for districts with multiple rivers
Streamflow$NushagakDistrict <- Streamflow$Igushik + Streamflow$Nushagak + Streamflow$Wood
Streamflow$KvichakDistrict <- Streamflow$Kvichak+Streamflow$Naknek #can add alagnak if we decide to use the tower values
Streamflow$Date <- as.Date(Streamflow$Date, format = "%m/%d/%Y")
Streamflow$Month <- month(as.Date(Streamflow$Date))
Streamflow$YEAR <- year(as.Date(Streamflow$Date))

#Calculate means by year for each system during June
Streamflow_June <- filter(Streamflow, Month==6)
Streamflow_June_means <- Streamflow_June %>% group_by(YEAR) %>%
  summarise(
    Togiak_meanflow_June = mean(Togiak, na.rm = TRUE),
    Ugashik_meanflow_June = mean(Ugashik, na.rm = TRUE),
    Egegik_meanflow_June = mean(Egegik, na.rm = TRUE),
    NushagakDistrict_meanflow_June = mean(NushagakDistrict, na.rm = TRUE),
    KvichakDistrict_meanflow_June = mean(KvichakDistrict, na.rm = TRUE)
  )
