
#--------------------------------------------------
# Basic summary
#--------------------------------------------------
print(stan.fit)

# Summary statistics
summary(stan.fit)

# Summary table
summary(stan.fit)$summary


pars <- rstan::extract(stan.fit)

#--------------------------------------------------
# Convergence diagnostics
#--------------------------------------------------
summary(stan.fit)$summary[, "Rhat"]
summary(stan.fit)$summary[, "n_eff"]

#--------------------------------------------------
# Trace plots
#--------------------------------------------------
traceplot(stan.fit, pars="ln_RPI")
traceplot(stan.fit, pars="RPI")
traceplot(stan.fit, pars="TT")
traceplot(stan.fit, pars="sigma_CE")


#--------------------------------------------------
# Pair plot
#--------------------------------------------------
pairs(stan.fit, pars = c("sigma_CE", "TT[1]", "RPI[1]"))




#install.packages("shinystan")
library(shinystan)

shinystan::launch_shinystan(fit)




############Plot results 
library(ggplot2)
library(dplyr)
library(tidyr)

pred_CE_median <- apply(pars$pred_CE, c(2, 3), median)

rownames(pred_CE_median) <- rownames(CE_data)
colnames(pred_CE_median) <- colnames(CE_data)


# Add year column from row names
CE_long <- CE_data %>%
  tibble::rownames_to_column("year") %>%
  pivot_longer(
    -year,
    names_to = "jdate",
    values_to = "CE"
  )

Pred_long <- as.data.frame(pred_CE_median) %>%
  tibble::rownames_to_column("year") %>%
  pivot_longer(
    -year,
    names_to = "jdate",
    values_to = "Pred_CE"
  )

plot_data <- left_join(CE_long, Pred_long,
                       by = c("year", "jdate"))

fit <- ggplot(plot_data, aes(x = as.numeric(jdate))) +
  geom_col(aes(y = CE),
           fill = "grey70",
           width = 1) +
  geom_line(aes(y = Pred_CE),
              colour = "red",
              linewidth = 1) +
  facet_wrap(~year, scales = "free_y") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title = element_text(size = 20, face = "bold"),
    
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 20)
  )+
  labs(
    x = "Julian day",
    y = "Catch/Escapement"
  )

ggsave(filename = paste0("figs/fit_V3.png"),
       plot = fit,
       width = 24,
       height = 24,
       dpi = 300)
