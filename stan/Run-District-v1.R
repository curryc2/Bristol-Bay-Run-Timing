# Script to Run Stan Move Bay-v1
library(here)
install.packages("rstan")
library(rstan)

options(mc.cores = parallel::detectCores())

source(here("R/Catch and Lagged Escapement District.R"))

# Control Section =======================
# Specify which years to fit
years <- as.integer(dimnames(CE_data)[[2]])
years

# Define subset of years to fit
fit.years <- 2005:2025 # full range is 2005-2025
n.fit.years <- length(fit.years)

# Determine location of years to fi
loc.fit.years <- which(years %in% fit.years)

# Truncate data objects
CE_data <- CE_data[,loc.fit.years,]
CPUE_data <- CPUE_data[,loc.fit.years,]/1000

# Define dimensions ==================
Nyear <- as.integer(length(dimnames(CE_data)[[2]]))
NdayPM <- as.integer(length(dimnames(CPUE_data)[[3]]))
NdayCE <- as.integer(length(dimnames(CE_data)[[3]]))
Ndistrict <- as.integer(length(dimnames(CE_data)[[1]]))
Lags <- seq(0,20,1)
Nlags <- length(Lags)

# MCMC Parameters
n.chains <- 3
n.iter <- 2e3 #1e4
n.thin <- 2 #4
# Determine number of Stan Samples
(n.iter/n.thin)*0.5*n.chains
version <- "v1"

# Create Stan data
stan.data <- list("CPUE"=CPUE_data, "CE"=CE_data, "Nyear"=Nyear,
                  "NdayPM"=NdayPM, "NdayCE"=NdayCE, "Nlags"=Nlags, "Lags"=Lags, "Ndistrict"=Ndistrict
)

# With random variation
init_fun <- function(chain_id = 1) {
  list(
    ln_RPI = matrix(log(runif(Ndistrict * Nyear, 7, 9)),
                    nrow = Ndistrict,
                    ncol = Nyear),
    
    TT = matrix(runif(Ndistrict * Nyear, 5, 8),
                nrow = Ndistrict,
                ncol = Nyear),
    
    sigma_CE = matrix(runif(Ndistrict * Nyear, 50, 100),
                      nrow = Ndistrict,
                      ncol = Nyear)
  )
}

# Run Stan Model

stan.fit <- stan(file=file.path(here("stan", paste0("District-", version, ".stan"))),
                 model_name=paste0("Bay-", version),
                 data=stan.data, init = init_fun,
                 chains=n.chains, iter=n.iter, thin=n.thin,
                 cores=1,
                 verbose=FALSE,
                 seed=101)#,
                 # control = list(adapt_delta = 0.99)) 


saveRDS(stan.fit, here("output", paste0("stan_fit_", version, ".rds")))


