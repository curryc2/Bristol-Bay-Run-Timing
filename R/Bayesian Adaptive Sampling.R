library(here)
#install.packages("BAS")
library(BAS)


source(here("R/Pull Data.R"))

# Ugashik 

Ugashik.ZS <- bas.lm(Uga ~ .,
                   data = Model_dat_Uga,
                   prior = "ZS-null",
                   modelprior = uniform(), initprobs = "eplogp",
                   force.heredity = FALSE, pivot = TRUE
)

par(mfrow=c(2,2))
plot(Ugashik.ZS, ask = F)


# Egegik 

Egegik.ZS <- bas.lm(Ege ~ .,
                     data = Model_dat_Ege,
                     prior = "ZS-null",
                     modelprior = uniform(), initprobs = "eplogp",
                     force.heredity = FALSE, pivot = TRUE
)

par(mfrow=c(2,2))
plot(Egegik.ZS, ask = F)

#Kvichak

Kvichak.ZS <- bas.lm(Nak.Kvi ~ .,
                    data = Model_dat_Kvi[,-13], #remove abundance, not running with all varaibles and this has never been significant
                    prior = "ZS-null",
                    modelprior = uniform(), initprobs = "eplogp",
                    force.heredity = FALSE, pivot = TRUE
)

par(mfrow=c(2,2))
plot(Kvichak.ZS, ask = F)


# Nushagak

Nushagak.ZS <- bas.lm(Nush ~ .,
                     data = Model_dat_Nush[,-13],#remove abundance, same as above
                     prior = "ZS-null",
                     modelprior = uniform(), initprobs = "eplogp",
                     force.heredity = FALSE, pivot = TRUE
)

par(mfrow=c(2,2))
plot(Nushagak.ZS, ask = F)


#Togiak

Togiak.ZS <- bas.lm(Tog ~ .,
                      data = Model_dat_Tog,
                      prior = "ZS-null",
                      modelprior = uniform(), initprobs = "eplogp",
                      force.heredity = FALSE, pivot = TRUE
)

par(mfrow=c(2,2))
plot(Togiak.ZS, ask = F)


#Takeaway, most covariates do not seem to be super informative.  The few that are vary from region to region.  I could be wrong, 
#but overall the plots dont look great.  The ChlA data timeseries is too short, although it is often informative when included.  
#I think we should axe this data though, the LMs wont even fit using it at the moment due to the limitation of available years.
#Without it, the importance of the other predictor variables also changes greatly.  Also, lots of correlation with these variables.
