// Version 1 - June 24, 2026

data {
  // Dimension Quantities   
  int<lower=0> Nyear; // Number of Years
  int<lower=0> NdayPM; // Number of Port Moller Days 161:198
  int<lower=0> NdayCE; // Number of catch and escapement days 161:225
  
//   Consider adding in a vector [Nyear] with last day PM operated
  
  // Data objects
  array[Nyear, NdayPM] real CPUE;
  array[Nyear, NdayCE] real CE;
}

parameters {
  array[Nyear] real ln_RPI; // Run-per-index: May need to estimate in log space
  array[Nyear] real<lower=3, upper=14> TT; // Travel Time
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
  RPI = exp(ln_RPI);
  
  
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
      
      real arrival = j + TT[i];
      // int llimit = j + 3;
      int ulimit = j + 14;
      
      for (k in 1:NdayCE) {
      // for (k in llimit:min(NdayCE,ulimit)) { // NOTE:  This will only work if start day for dayPM is the same as dayCE

        if (fabs(arrival - k) < 1) {
          propCPUE[i,j,k] = 1 - fabs(arrival - k);
          dailyCPUE[i,j,k] = propCPUE[i,j,k] * CPUE[i,j];
        } else {
          propCPUE[i,j,k] = 0.0;
          dailyCPUE[i,j,k] = 0.0;
        }

      }
    }
  }

// Sum CPUE for each CE day
 // for (i in 1:Nyear) {
 //    for (j in 1:NdayCE) { 
 //      for (k in 1:NdayPM) {
 //        totalCPUE[i,j] += dailyCPUE[i,k,j];
 //      } 
 //    }
 //  } 
  // Vectorized Form
  for (i in 1:Nyear) {
    for (j in 1:NdayCE) { 
      // for (k in 1:NdayPM) {
        totalCPUE[i,j] = sum(dailyCPUE[i,,j]);
      // } 
    }
  }  


  //calculate pred_CE
  for (i in 1:Nyear) {
    for (j in 1:NdayCE) {
      pred_CE[i,j] = RPI[i] * totalCPUE[i,j];
    }
  }
  // Vectorized Version
  // for (i in 1:Nyear) {
  //   // for (j in 1:NdayCE) {
  //     pred_CE[i,] = RPI .* totalCPUE[i,];
  //   // }
  // }

}


model {
  // PRIORS
  // RPI ~ uniform(0,2e4);
  ln_RPI ~ normal(0,3);
  TT ~ normal(7,2);
  // sigma_CE ~ uniform(0,1e3);
  sigma_CE ~ normal(0,1); // Reminder: As parameter definition has LB 0, this is half-normal
  // LIKELIHOODS   
  for (i in 1:Nyear) {
    for (j in 1:NdayCE) {
      // if(CE[i,j]>0 && pred_CE[i,j]>0) {
        // log(CE[i,j]+1) ~ normal(log(pred_CE[i,j]+1), sigma_CE);
        log(CE[i,j]+1e-3) ~ normal(log(pred_CE[i,j]+1e-3), sigma_CE[i]);
      // }
    }
  }

}

