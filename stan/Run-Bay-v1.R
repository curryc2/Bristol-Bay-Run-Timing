# Script to Run Stan Move Bay-v1
library(here)
example(stan_model, package = "rstan", run.dontrun = TRUE)

library(rstan)
source(here("R/Catch and Lagged Escapement.R"))

Nyear <- as.integer(nrow(CE_data))
NdayPM <- as.integer(ncol(CPUE_data))
NdayCE <- as.integer(ncol(CE_data))

# MCMC Parameters
n.chains <- 4
n.iter <- 5e3 #1e4
n.thin <- 2 #4
# Determine number of Stan Samples
(n.iter/n.thin)*0.5*n.chains
version <- "v1"

# Create Stan data
stan.data <- list("CPUE"=CPUE_data, "CE"=CE_data, "Nyear"=Nyear,
                  "NdayPM"=NdayPM, "NdayCE"=NdayCE
)

# Run Stan Model

stan.fit <- stan(file=file.path(here("stan", paste0("Bay-", version, ".stan"))),
                 model_name=paste0("Bay-", version),
                 data=stan.data,
                 chains=n.chains, iter=n.iter, thin=n.thin,
                 #cores=n.chains, 
                 verbose=FALSE,
                 seed=101,
                 control = list(adapt_delta = 0.99)) 

install.packages("Rtools")
