# Script to Run Stan Move Bay-v1
library(here)
# install.packages("rstan")
library(rstan)

options(mc.cores = parallel::detectCores())

source(here("R/Catch and Lagged Escapement.R"))

# Control Section =======================
# Specify which years to fit
years <- as.integer(rownames(CE_data))
years

# Define subset of years to fit
fit.years <- 1990:2025 # full range is 1990-2025
n.fit.years <- length(fit.years)

# Determine location of years to fi
loc.fit.years <- which(years %in% fit.years)

# Truncate data objects
CE_data <- CE_data[loc.fit.years,]
CPUE_data <- CPUE_data[loc.fit.years,]

# Define dimensions ==================
Nyear <- as.integer(nrow(CE_data))
NdayPM <- as.integer(ncol(CPUE_data))
NdayCE <- as.integer(ncol(CE_data))
Lags <- seq(0,20,1)
Nlags <- length(Lags)

# MCMC Parameters
n.chains <- 3
n.iter <- 5e2 #1e4
n.thin <- 2 #4
# Determine number of Stan Samples
(n.iter/n.thin)*0.5*n.chains
version <- "v5"

# Create Stan data
stan.data <- list("CPUE"=CPUE_data, "CE"=CE_data, "Nyear"=Nyear,
                  "NdayPM"=NdayPM, "NdayCE"=NdayCE, "Nlags"=Nlags, "Lags"=Lags
)

# With random variation
init_fun <- function(chain_id=1) {
  list(
    ln_RPI = log(runif(n=Nyear, 1, 5)),
    TT = runif(n=Nyear, 5, 8),
    # sigma_CE = runif(n=Nyear,0.2,0.8)
#     Normal likelihood
    sigma_CE = runif(n=Nyear,50,100)
  )
}

# Run Stan Model

stan.fit <- stan(file=file.path(here("stan", paste0("Bay-", version, ".stan"))),
                 model_name=paste0("Bay-", version),
                 data=stan.data, init = init_fun,
                 chains=n.chains, iter=n.iter, thin=n.thin,
                 cores=n.chains,
                 verbose=FALSE,
                 seed=101)#,
                 # control = list(adapt_delta = 0.99)) 


saveRDS(stan.fit, here("output", paste0("stan_fit_", version, ".rds")))


