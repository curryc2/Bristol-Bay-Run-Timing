# Script to Run Stan Move Bay-v1
library(here)
library(rstan)

source(here("R/Catch and Lagged Escapement.R"))

Nyear <- as.integer(nrow(CE_data))
NdayPM <- as.integer(ncol(CPUE_data))
NdayCE <- as.integer(ncol(CE_data))

# MCMC Parameters
n.chains <- 1
n.iter <- 1e3 #1e4
n.thin <- 2 #4
# Determine number of Stan Samples
(n.iter/n.thin)*0.5*n.chains
version <- "v1"

# Create Stan data
stan.data <- list("CPUE"=CPUE_data, "CE"=CE_data, "Nyear"=Nyear,
                  "NdayPM"=NdayPM, "NdayCE"=NdayCE
)
init_fun <- function() {
  list(
    RPI = rep(1000, Nyear),
    TT = rep(7, Nyear),
    sigma_CE = 5
  )
}

# Run Stan Model

stan.fit <- stan(file=file.path(here("stan", paste0("Bay-", version, ".stan"))),
                 model_name=paste0("Bay-", version),
                 data=stan.data, init = init_fun,
                 chains=n.chains, iter=n.iter, thin=n.thin,
                 #cores=n.chains, 
                 verbose=FALSE,
                 seed=101,
                 control = list(adapt_delta = 0.99)) 







##########################Testing code from stan file, looks like it works properly!

TT <- runif(Nyear, min = 6, max = 10)
RPI <- runif(Nyear, min = 700, max = 3000)
# Initialize
pred_CE   <- matrix(0, Nyear, NdayCE)
totalCPUE <- matrix(0, Nyear, NdayCE)

propCPUE <- array(0, dim = c(Nyear, NdayPM, NdayCE))
dailyCPUE <- array(0, dim = c(Nyear, NdayPM, NdayCE))

# Distribute CPUE from each Port Moller day
for(i in 1:Nyear){
  
  for(j in 1:NdayPM){
    
    arrival <- j + TT[i]
    
    for(k in 1:NdayCE){
      
      if(abs(arrival - k) < 1){
        propCPUE[i,j,k] <- 1 - abs(arrival - k)
        dailyCPUE[i,j,k] <- propCPUE[i,j,k] * CPUE_data[i,j]
      }
      
    }
    
  }
  
}

# Sum CPUE for each CE day
for(i in 1:Nyear){
  
  for(j in 1:NdayCE){
    
    totalCPUE[i,j] <- sum(dailyCPUE[i,,j])
    
  }
  
}

# Calculate predicted CE
for(i in 1:Nyear){
  
  pred_CE[i,] <- RPI[i] * totalCPUE[i,]
  
}

