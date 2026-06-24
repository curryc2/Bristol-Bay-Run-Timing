# Script to Run Stan Move Bay-v1

# MCMC Parameters
n.chains <- 4
n.iter <- 5e3 #1e4
n.thin <- 2 #4
# Determine number of Stan Samples
(n.iter/n.thin)*0.5*n.chains

# Create Stan data
stan.data <- list("sigma_pf"=sigma.pf, "mu_pf"=mu.pf, "curr_pf"=curr.pf,
                  "curr_ce"=curr.ce, "hist_ce"=hist.ce,
                  
                  "prior_ce_alpha"=prior.ce.alpha, "prior_ce_beta"=prior.ce.beta,
                  
                  "n_curr_pm_days"=n.curr.pm.days,  "curr_pm_days"=curr.pm.days,
                  "curr_cpue_daily_obs"=curr.cpue.daily.obs, "hist_cpue_daily_obs"=hist.cpue.daily.obs,
                  
                  
                  "yearPF"=yearPF, "nYearPF"=nYearPF,
                  "yearCE"=yearCE, "nYearCE"=nYearCE,
                  "yearPM"=yearPM, "nYearPM"=nYearPM,
                  
                  "Robs_ce"=Robs.ce, "Robs_pm"=Robs.pm, 
                  
                  "Robs_pm_age"=Robs.pm.age,
                  "n_ages"=n.ages,
                  "curr_age_cpue_obs"=curr.age.cpue.obs, 
                  "hist_age_cpue_obs"=hist.age.cpue.obs
                  
)

# Run Stan Model

stan.fit <- stan(file=file.path(dir.stan, paste0("Bay-", version, ".stan")),
                 model_name=paste0("Bay-", version),
                 data=stan.data,
                 chains=n.chains, iter=n.iter, thin=n.thin,
                 # chains=3, iter=5e3, thin=5,
                 cores=n.chains, verbose=FALSE,
                 seed=101,
                 control = list(adapt_delta = 0.99)) 

