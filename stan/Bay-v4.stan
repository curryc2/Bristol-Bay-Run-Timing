// Version 4 - July 23, 2026

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
}

parameters {
  array[Nyear] real ln_RPI; // Run-per-index: May need to estimate in log space
  array[Nyear] real TT; // Travel Time
  // array[Nyear]
  array[Nyear] real<lower=0> sigma_CE; // Could be expanded to be year-specific
}

// This is where the magic happens!!!!!
transformed parameters {
  // Derived parameters   
  array[Nyear, NdayCE] real pred_CE; // Predicted inshore arrivals
  
  array[Nyear, NdayPM, NdayCE] real propCPUE; // Proportions
  
  array[Nyear, NdayPM, NdayCE] real dailyCPUE; // CPUE allocations
  
  array[Nyear, NdayCE] real totalCPUE; //Sum by CE day of CPUE for each PM day
  
  array[Nyear] real RPI;
  
  real sigmatravel;
  
  RPI = exp(ln_RPI);
  sigmatravel=1;
  
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
          propCPUE[i,j,k] = propCPUE[i,j,k]/rowsum;  //this was to remove the array for storing standardized values.  I believe this works as its written
          dailyCPUE[i,j,k] = propCPUE[i,j,k] * CPUE[i,j];
          }
        }
      }
    }


// Sum CPUE for each CE day

  for (i in 1:Nyear) {
    for (j in 1:NdayCE) { 
        totalCPUE[i,j] = sum(dailyCPUE[i,,j]);
         pred_CE[i,j] = RPI[i] * totalCPUE[i,j];
    }
  }  

}


model {
  // PRIORS
  // RPI ~ uniform(0,2e4);
  ln_RPI ~ normal(0,5);
  TT ~ normal(7,2);
  // sigma_CE ~ uniform(0,1e3);
  sigma_CE ~ normal(0,1); // Reminder: As parameter definition has LB 0, this is half-normal
  // LIKELIHOODS   
  for (i in 1:Nyear) {
    for (j in 1:NdayCE) {
      if(CE[i,j]>0) {
        // log(CE[i,j]+1) ~ normal(log(pred_CE[i,j]+1), sigma_CE);
        log(CE[i,j]+1e-3) ~ normal(log(pred_CE[i,j]+1e-3), sigma_CE[i]);
      }
    }
  }

}

