// Version 1 - June 24, 2026

data {
  // Dimension Quantities   
  int<lower=0> Nyear; // Number of Years
  int<lower=0> NdayPM; // Number of Port Moller Days 161:198
  int<lower=0> NdayCE; // Number of catch and escapement days 161:225
  
//   Consider adding in a vector [Nyear] with last day PM operated
  
  // Data objects
  array[Nyear, NdayPM] real cpue;
  array[Nyear, NdayCE] real ce;
}

parameters {
  array[Nyear] real RPI; // Run-per-index: May need to estimate in log space
  array[Nyear] real TT; // Travel Time
  // array[Nyear]
  real sigma_ce; // Could be expanded to be year-specific
}

transformed parameters {
  array[Nyear] real TT_round;
  
  TT_round = round(TT);
}

// This is where the magic happens!!!!!
transformed parameters {
  // Derived parameters   
  array[Nyear, NdayCE] real pred_ce; // Predicted inshore arrivals
  
  array[Nyear, NdayPM, NdayCE] real propCPUE; // Propor
  
  // Calculations   
  // PseudoCode Steps
  
  
  
  //   
  
}

model {
  // PRIORS
  RPI ~ uniform(0,2e4);
  TT ~ normal(7,2);
  
  // LIKELIHOODS   
  for(y in 1:Nyear) {
    log(ce[y,]) ~ normal(log(pred_ce[y,]), sigma_ce);
  }// next y
}

