library (here)
library (lubridate)
library (dplyr)
library(ncdf4)

here()#Bristol-Bay-Run-Timing base working directory 


#Start pulling in data, all stored in data subfolder. 

#######Environmental data

source(here("data/get-sst-data-regression.r"))

source(here("data/get-airport-data.r"))

source(here("data/get-streamflow-data.R"))

#SST data 

SST_means <- SST_means %>%
  mutate(meanSST = rowMeans(
    dplyr::select(., Ugashik_SST, Egegik_SST, Kvichak_SST, Nushagak_SST, Togiak_SST),
    na.rm = TRUE
  )) %>%
  dplyr::select(YEAR, meanSST)

#Summarize airport data (wind specifically)
Airport_dat_CE <- Airport_dat %>%
  filter((MONTH == 6 & DAY >= 10) |(MONTH == 7 & DAY <= 29))

Wind_dat <- Airport_dat_CE %>% group_by(YEAR) %>%
  summarise(
    mean_eastwind = mean(Wind_East, na.rm = TRUE),
    mean_northwind = mean(Wind_North, na.rm = TRUE)
  )

#Streamflow data for same time period

Streamflow <- Streamflow %>%
  mutate(jday = yday(Date)) %>% filter(jday >= 161, jday <= 210) %>% group_by(YEAR) %>%
  summarise(
    meanflow = mean(Alagnak+Egegik+Igushik+Kvichak+Naknek+Nushagak+Togiak+Ugashik+Wood, na.rm = TRUE)
  )


######Clean up data and combine into a data frame to generate models from
Regression_dat <- full_join(SST_means, Wind_dat, by= "YEAR")
Regression_dat <- full_join(Regression_dat, Streamflow, by= "YEAR")%>%arrange(YEAR) %>% filter(YEAR>1989)
Regression_dat <- as.data.frame(Regression_dat)

#Calculate z scores 

#Z score 
Regression_dat <- Regression_dat %>% mutate(meanSST = as.numeric(scale(meanSST)))
Regression_dat <- Regression_dat %>% mutate(mean_eastwind = as.numeric(scale(mean_eastwind)))
Regression_dat <- Regression_dat %>% mutate(mean_northwind = as.numeric(scale(mean_northwind)))
Regression_dat <- Regression_dat %>% mutate(meanflow = as.numeric(scale(meanflow)))

Regression_dat[is.na(Regression_dat)] <- 0


#Transpose

Regression_dat <- t(Regression_dat)
colnames(Regression_dat) <- Regression_dat["YEAR", ]

Regression_dat <- Regression_dat[
  rownames(Regression_dat) != "YEAR",
  ,drop = FALSE]
