// Version 4 - July 23, 2026

data {
  // Dimension Quantities   
  int<lower=0> Nyear; // Number of Years
  int<lower=0> NdayPM; // Number of Port Moller Days 161:198
  int<lower=0> NdayCE; // Number of catch and escapement days 161:225
  int<lower=0> Ndistrict; // Number of districts
  int<lower=0> Nlags; // Length of trial.lags
//   Consider adding in a vector [Nyear] with last day PM operated
  
  // Data objects
  array[Nlags] int Lags; // Lags for distributing CPUE
  array[Ndistrict, Nyear, NdayPM] real CPUE;
  array[Ndistrict, Nyear, NdayCE] real CE;
}

parameters {
  array[Ndistrict, Nyear] real ln_RPI; // Run-per-index: May need to estimate in log space
  array[Ndistrict, Nyear] real TT; // Travel Time
  // array[Nyear]
  array[Ndistrict, Nyear] real<lower=0> sigma_CE; // Could be expanded to be year-specific
}

// This is where the magic happens!!!!!
transformed parameters {
  // Derived parameters   
  array[Ndistrict, Nyear, NdayCE] real pred_CE; // Predicted inshore arrivals
  
  array[Ndistrict, Nyear, NdayPM, NdayCE] real propCPUE; // Proportions
  
  array[Ndistrict, Nyear, NdayPM, NdayCE] real dailyCPUE; // CPUE allocations
  
  array[Ndistrict, Nyear, NdayCE] real totalCPUE; //Sum by CE day of CPUE for each PM day
  
  array[Ndistrict, Nyear] real RPI;
  
  real sigmatravel;
  
  RPI = exp(ln_RPI);
  sigmatravel=1;
  
  //Initialize with zeros
for (d in 1:Ndistrict){
  for (i in 1:Nyear) {
    for (j in 1:NdayCE) {
      totalCPUE[d,i,j] = 0.0;
      pred_CE[d,i,j] = 0.0;
      for (k in 1:NdayPM) {
        propCPUE[d,i,k,j] = 0.0;
        dailyCPUE[d,i,k,j] = 0.0;
      }
    }
  }
}
  //Vectorize
  // propCPUE[1:Nyear,1:NdayPM,1:NdayCE] = 0.0;
  // dailyCPUE = 0.0;
  // Calculations   

  // Distribute CPUE from each Port Moller day
 for (d in 1:Ndistrict) {
  
  for (i in 1:Nyear) {

    for (j in 1:NdayPM) {

      for (k in 1:min(Nlags,NdayCE-j+1)){
        propCPUE[d,i,j,k+j-1] = normal_cdf(Lags[k], TT[d,i]-0.5, sigmatravel)-normal_cdf(Lags[k], TT[d,i]+0.5, sigmatravel);
        
        }
      }
    }
 }
  
  
  //standardize and caluclate daily CPUE
for (d in 1:Ndistrict){
  
  for (i in 1:Nyear) {

    for (j in 1:NdayPM) {
        real rowsum;
        rowsum = sum(propCPUE[d,i,j,]);
      for (k in 1:NdayCE){
        if (propCPUE[d,i,j,k]>0){
          propCPUE[d,i,j,k] = propCPUE[d,i,j,k]/rowsum;  //this was to remove the array for storing standardized values.  I believe this works as its written
          dailyCPUE[d,i,j,k] = propCPUE[d,i,j,k] * CPUE[d,i,j];
          }
        }
      }
    }
}

// Sum CPUE for each CE day
for (d in 1:Ndistrict){
  for (i in 1:Nyear) {
    for (j in 1:NdayCE) { 
        totalCPUE[d,i,j] = sum(dailyCPUE[d,i,,j]);
         pred_CE[d,i,j] = RPI[d,i] * totalCPUE[d,i,j];
      }
    }  
  }

}

model {
  // PRIORS
for (d in 1:Ndistrict) {
  for (i in 1:Nyear) {
    ln_RPI[d,i] ~ normal(0, 5);
    TT[d,i] ~ normal(7, 2);        
    sigma_CE[d,i] ~ normal(0, 1);  
  }
}
  // LIKELIHOODS 
for (d in 1:Ndistrict){
  for (i in 1:Nyear) {
    for (j in 1:NdayCE) {
      if(CE[d,i,j]>0) {
        // log(CE[i,j]+1) ~ normal(log(pred_CE[i,j]+1), sigma_CE);
        log(CE[d,i,j]+1e-3) ~ normal(log(pred_CE[d,i,j]+1e-3), sigma_CE[d,i]);
      }
    }
  }
}

}
