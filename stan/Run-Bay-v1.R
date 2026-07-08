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
    sigma_CE = 100
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




library(bayesplot)

#--------------------------------------------------
# Basic summary
#--------------------------------------------------
print(stan.fit)

# Summary statistics
summary(stan.fit)

# Summary table
summary(stan.fit)$summary

#--------------------------------------------------
# Posterior samples
#--------------------------------------------------
post <- rstan::extract(stan.fit)

# View parameter names
names(post)

# Example parameters
head(post$RPI)
head(post$TT)
post$sigma_CE

#--------------------------------------------------
# Convergence diagnostics
#--------------------------------------------------
summary(stan.fit)$summary[, "Rhat"]
summary(stan.fit)$summary[, "n_eff"]

#--------------------------------------------------
# Trace plots
#--------------------------------------------------
traceplot(stan.fit, pars = c("sigma_CE"))

# First year's parameters
traceplot(stan.fit, pars = c("TT[1]", "RPI[1]"))

#--------------------------------------------------
# Posterior densities
#--------------------------------------------------
stan_dens(stan.fit, pars = c("sigma_CE"))

#--------------------------------------------------
# Pair plot
#--------------------------------------------------
pairs(stan.fit, pars = c("sigma_CE", "TT[1]", "RPI[1]"))

#--------------------------------------------------
# Transformed parameters
#--------------------------------------------------
dim(post$pred_CE)
dim(post$totalCPUE)

# Example posterior distributions
plot(post$TT[,1], type = "l", main = "TT[1]")
hist(post$TT[,1], main = "Posterior TT[1]", xlab = "TT")

plot(post$RPI[,1], type = "l", main = "RPI[1]")
hist(post$RPI[,1], main = "Posterior RPI[1]", xlab = "RPI")

plot(post$pred_CE[,1,20], type = "l",
     main = "pred_CE: Year 1, Day 20")
hist(post$pred_CE[,1,20],
     main = "Posterior pred_CE: Year 1, Day 20",
     xlab = "pred_CE")







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

