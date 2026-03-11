library (here)
library (dplyr)
library (tidyverse)
library(tidyr)

here()#Bristol-Bay-Run-Timing base working directory 

Numbers <- read.csv(here("data/Bristol Bay Brood Table.csv"))

#Summarize brood table by year and system
NumbersbyRiver <- Numbers %>% filter (YEAR>1962) %>% group_by(YEAR, System) %>% 
  summarise(
    Total_Numbers = sum(return, na.rm = TRUE),.groups = "drop"
  )
NumbersbyRiver <- pivot_wider(NumbersbyRiver,
                                         names_from = System,
                                         values_from = Total_Numbers)

#Calculate the proportions for the later running fish (Kvichak in Kvichak/naknek district and igushik in nushagak district)
NumbersbyRiver$Kvichakproportion <- NumbersbyRiver$Kvichak / (NumbersbyRiver$Alagnak + 
                                                                NumbersbyRiver$Naknek + NumbersbyRiver$Kvichak)
NumbersbyRiver$Igushikproportion <- NumbersbyRiver$Igushik / (NumbersbyRiver$Wood + 
                                                                NumbersbyRiver$Nushagak + NumbersbyRiver$Igushik)
NumbersbyRiver <- NumbersbyRiver %>%
  arrange(YEAR)


River_Proportions <- NumbersbyRiver %>% dplyr::select(YEAR,Kvichakproportion, Igushikproportion)
