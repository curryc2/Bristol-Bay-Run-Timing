install.packages("readnoaa")
install.packages("janitor")
library(readnoaa)
library(janitor)


KingSalmon_Data <- noaa_get(
  dataset = "daily-summaries", 
  station = "USW00025503",
  start_date = "2000-01-01",
  end_date = "2026-01-01")

KingSalmon_Data <- remove_empty(KingSalmon_Data, which= "cols")

#can also use noaa_monthly or noaa_daily to get summaries for a given station.  