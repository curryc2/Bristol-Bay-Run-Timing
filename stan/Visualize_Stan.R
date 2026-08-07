require(here)
require(tidyverse)
require(dplyr)
require(rstan)

# Control Section ==================================
version <- "v5"

dir.figs <- here("figs",version)
dir.create(dir.figs, recursive=TRUE)

# Load Fitted Model =============
stan.fit <- readRDS(here("output", paste0("stan_fit_", version, ".rds")))

pars <- rstan::extract(stan.fit)


# Create output director

#--------------------------------------------------
# Basic summary
#--------------------------------------------------
print(stan.fit)

# Summary table
summary(stan.fit)$summary


#--------------------------------------------------
# Convergence diagnostics
#--------------------------------------------------
summary(stan.fit)$summary[, "Rhat"]
summary(stan.fit)$summary[, "n_eff"]





#install.packages("shinystan")
#install.packages("shinystan")
#library(shinystan)

#launch_shinystan(stan.fit)



#--------------------------------------------------
# Traceplots
#--------------------------------------------------

png(here(dir.figs,"RPI_trace.png"), width = 4000, height = 2600, res = 300)
traceplot(stan.fit, pars = "RPI")
dev.off()

png(here(dir.figs,"Sigma_trace.png"), width = 4000, height = 2600, res = 300)
traceplot(stan.fit, pars = "sigma_CE")
dev.off()

png(here(dir.figs,"TT_trace.png"), width = 4000, height = 2600, res = 300)
traceplot(stan.fit, pars = "TT")
dev.off()

#--------------------------------------------------
# CE vs Pred CE by year
#--------------------------------------------------
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

CPUE_long <- CPUE_data %>%
  tibble::rownames_to_column("year") %>%
  pivot_longer(
    -year,
    names_to = "jdate",
    values_to = "CPUE"
  )%>%
  mutate(
    year = as.integer(year),
    jdate = as.integer(jdate)
  ) %>%
  complete(
    year,
    jdate = 161:225,
    fill = list(CPUE = 0)
  ) %>%
  arrange(year, jdate)%>% 
  mutate(
    year = as.character(year),
    jdate = as.character(jdate))


Pred_long <- as.data.frame(pred_CE_median) %>%
  tibble::rownames_to_column("year") %>%
  pivot_longer(
    -year,
    names_to = "jdate",
    values_to = "Pred_CE"
  )

plot_data <- CE_long %>%
  left_join(Pred_long, by = c("year", "jdate")) %>%
  left_join(CPUE_long, by = c("year", "jdate"))

fit <- ggplot(plot_data, aes(x = as.numeric(jdate))) +
  geom_col(aes(y = CE),
           fill = "grey70",
           width = 1) +
  geom_line(aes(y = Pred_CE, colour = "Pred CE"),
            linewidth = 1) +
  geom_line(aes(y = CPUE * 6, colour = "Scaled CPUE"),
            linewidth = 1) +
  scale_color_manual(
    name = "",
    values = c(
      "Pred CE" = "red",
      "Scaled CPUE" = "blue"
    )
  ) +
  facet_wrap(~year, scales = "free_y") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 20)
  ) +
  labs(
    x = "Julian day",
    y = "Catch/Escapement"
  )

ggsave(filename = paste0(here(dir.figs,"fit_",version,".png")),
       plot = fit,
       width = 30,
       height = 24,
       dpi = 300)
