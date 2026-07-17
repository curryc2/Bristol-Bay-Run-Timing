# Script to Run Stan Move Bay-v1
library(here)
library(rstan)


options(mc.cores = parallel::detectCores())

source(here("R/Catch and Lagged Escapement.R"))

Nyear <- as.integer(nrow(CE_data))
NdayPM <- as.integer(ncol(CPUE_data))
NdayCE <- as.integer(ncol(CE_data))

# MCMC Parameters
n.chains <- 1
n.iter <- 1e4 #1e4
n.thin <- 2 #4
# Determine number of Stan Samples
(n.iter/n.thin)*0.5*n.chains
version <- "v2"

# Create Stan data
stan.data <- list("CPUE"=CPUE_data, "CE"=CE_data, "Nyear"=Nyear,
                  "NdayPM"=NdayPM, "NdayCE"=NdayCE
)

# With random variation
init_fun <- function(chain_id=1) {
  list(
    RPI = runif(n=Nyear, 3000, 5000),
    TT = runif(n=Nyear, 5, 8),
    sigma_CE = runif(n=Nyear,0.2,0.8)
  )
}

# Run Stan Model

stan.fit <- stan(file=file.path(here("stan", paste0("Bay-", version, ".stan"))),
                 model_name=paste0("Bay-", version),
                 data=stan.data, init = init_fun,
                 chains=n.chains, iter=n.iter, thin=n.thin,
                 cores=1,
                 verbose=FALSE,
                 seed=101,
                 control = list(adapt_delta = 0.99)) 





