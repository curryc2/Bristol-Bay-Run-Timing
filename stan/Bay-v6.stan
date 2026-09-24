// Version 6 - Sept 8, 2026

// Feature Additions:
//   Sepecifies TT and RPI as regressions with covariates

data {
  // Dimension Quantities   
  int<lower=0> Nyear; // Number of Years
  int<lower=0> NdayPM; // Number of Port Moller Days 161:198
  int<lower=0> NdayCE; // Number of catch and escapement days 161:225
  int<lower=0> Nlags; // Length of trial.lags
//   Consider adding in a vector [Nyear] with last day PM operated
  
  // Data objects
  array[Nlags] int Lags; // Lags for distributing CPUE
  array[Nyear, NdayPM] real CPUE;
  array[Nyear, NdayCE] real CE;
  
  // Covariates to include
  int<lower=0> NcovarRPI; // Number of covariates for RPI
  int<lower=0> NcovarTT; // Numbger of covariates for travel time
  
  // Covariate timeseries, should be z-scored within each covariate timeseries  
  array[NcovarRPI, Nyear] real covarRPI; // Covariate timeseries for RPI
  array[NcovarTT, Nyear] real covarTT; // Covariate timeseries for travel time
  
}

parameters {
  // array[Nyear] real ln_RPI; // Run-per-index: May need to estimate in log space
  // array[Nyear] real TT; // Travel Time
  
  real RPI_int; // Intercept for RPI
  array[NcovarRPI] real RPI_slp; // Covariate effects on RPI
  
  real TT_int; // Intercept for RPI
  array[NcovarTT] real TT_slp; // Covariate effects on RPI
  
  
  array[Nyear] real<lower=0> sigma_CE; // Could be expanded to be year-specific
}

// This is where the magic happens!!!!!
transformed parameters {
  // Derived parameters   
  array[Nyear, NdayCE] real pred_CE; // Predicted inshore arrivals
  
  array[Nyear, NdayPM, NdayCE] real propCPUE; // Proportions
  
  array[Nyear, NdayPM, NdayCE] real dailyCPUE; // CPUE allocations
  
  array[Nyear, NdayCE] real totalCPUE; //Sum by CE day of CPUE for each PM day
  
  array[Nyear] real<lower=0> RPI;
  array[Nyear] real<lower=0> TT;
  
  real sigmatravel;
  sigmatravel=1;
  
  // Define RPI and TT based on regression relationships
  for(y in 1:Nyear) {
    RPI[y] = RPI_int + dot_product(RPI_slp, covarRPI[,y]);
    TT[y] = TT_int + dot_product(TT_slp, covarTT[,y]);
  } //next y
  
  
  //Initialize with zeros

  for (i in 1:Nyear) {
    for (j in 1:NdayCE) {
      totalCPUE[i,j] = 0.0;
      pred_CE[i,j] = 0.0;
      for (k in 1:NdayPM) {
        propCPUE[i,k,j] = 0.0;
        dailyCPUE[i,k,j] = 0.0;
      }
    }
  }
  //Vectorize
  // propCPUE[1:Nyear,1:NdayPM,1:NdayCE] = 0.0;
  // dailyCPUE = 0.0;
  // Calculations   

  // Distribute CPUE from each Port Moller day
  for (i in 1:Nyear) {

    for (j in 1:NdayPM) {

      for (k in 1:min(Nlags,NdayCE-j+1)){
        propCPUE[i,j,k+j-1] = normal_cdf(Lags[k], TT[i]-0.5, sigmatravel)-normal_cdf(Lags[k], TT[i]+0.5, sigmatravel);
        
      }
    }
  }
  
  
  //standardize and caluclate daily CPUE
  for (i in 1:Nyear) {

    for (j in 1:NdayPM) {
        real rowsum;
        rowsum = sum(propCPUE[i,j,]);
      for (k in 1:NdayCE){
        if (propCPUE[i,j,k]>0){
          propCPUE[i,j,k] = propCPUE[i,j,k]/rowsum;
          dailyCPUE[i,j,k] = propCPUE[i,j,k] * CPUE[i,j];
          }
        }
      }
    }


// Sum CPUE for each CE day

  for (i in 1:Nyear) {
    for (j in 1:NdayCE) { 
        totalCPUE[i,j] = sum(dailyCPUE[i,,j]);
        pred_CE[i,j] = (RPI[i] * totalCPUE[i,j]);
    }
  }  

}


model {
  // PRIORS
  // RPI ~ uniform(0,2e4);
  // ln_RPI ~ normal(8.5,1);
  // TT ~ normal(7,2);
  
  RPI_int ~ uniform(0,2e4);
  RPI_slp ~ normal(0,100);
  
  
  TT_int ~ normal(7,2);
  TT_slp ~ normal(0,1);
  
  // sigma_CE ~ uniform(0,1e3);
  // sigma_CE ~ normal(0,1); // Reminder: As parameter definition has LB 0, this is half-normal
//   Normal likelihood
  sigma_CE ~ normal(0,100); // Reminder: As parameter definition has LB 0, this is half-normal
  // LIKELIHOODS   
  for (i in 1:Nyear) {
    for (j in 1:NdayCE) {
      if(CE[i,j]>0) {
        // log(CE[i,j]+1) ~ normal(log(pred_CE[i,j]+1), sigma_CE);
        // log(CE[i,j]+1e-3) ~ normal(log(pred_CE[i,j]+1e-3), sigma_CE[i]);
//         Normal likelihood
        CE[i,j] ~ normal(pred_CE[i,j], sigma_CE[i]);
      }
    }
  }

}
