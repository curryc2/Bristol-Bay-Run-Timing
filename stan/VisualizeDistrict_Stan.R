require(here)
require(tidyverse)
require(dplyr)
require(rstan)

# Control Section ==================================
version <- "v1"

dir.figs <- here("figs",paste0("district",version))
dir.create(dir.figs, recursive=TRUE)

# Load Fitted Model =============
stan.fit <- readRDS(here("output", paste0("stan_fit_district_", version, ".rds")))

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
districts <- c("Ugashik", "Egegik", "Kvichak", "Nushagak", "Togiak")
fit.years <- 2005:2025

#This now plots a seperate one for each district with years as panels, repeats for all parameters

#Sigma
for(i in seq_along(districts)) {
  Sigma_i <- pars$sigma_CE[, i, , drop = FALSE]
  Sigma_i <- matrix(Sigma_i,nrow = dim(pars$sigma_CE)[1],ncol = 21)
  png(here(dir.figs, paste0("Sigma_trace_", districts[i], ".png")),
    width = 4000,
    height = 2600,
    res = 300)
  par(mfrow = c(5, 5),mar = c(3, 3, 2, 1))
  
  for(j in 1:21) {
    plot(
      Sigma_i[, j],
      type = "l",
      xlab = "Iteration",
      ylab = "Sigma",
      main = fit.years[j]
    )
  }
  dev.off()
}

#RPI
for(i in seq_along(districts)) {
  RPI_i <- pars$RPI[, i, , drop = FALSE]
  RPI_i <- matrix(RPI_i,nrow = dim(pars$RPI)[1],ncol = 21)
  png(here(dir.figs, paste0("RPI_trace_", districts[i], ".png")),
    width = 4000,
    height = 2600,
    res = 300)
  par(
    mfrow = c(5, 5),
    mar = c(3, 3, 2, 1))
  for(j in 1:21) {
    plot(
      RPI_i[, j],
      type = "l",
      xlab = "Iteration",
      ylab = "RPI",
      main = fit.years[j]
    )
  }
  dev.off()
}

#TT
for(i in seq_along(districts)) {
  TT_i <- pars$TT[, i, , drop = FALSE]
  TT_i <- matrix(TT_i,nrow = dim(pars$TT)[1],ncol = 21)
  png(here(dir.figs, paste0("TT_trace_", districts[i], ".png")),
    width = 4000,
    height = 2600,
    res = 300)
  par(
    mfrow = c(5, 5),
    mar = c(3, 3, 2, 1))
  for(j in 1:21) {
    
    plot(
      TT_i[, j],
      type = "l",
      xlab = "Iteration",
      ylab = "TT",
      main = fit.years[j]
    )
  }
  dev.off()
}



#--------------------------------------------------
# CE vs Pred CE by year for each district
#--------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)

pred_CE_median <- apply(pars$pred_CE, c(2, 3, 4), median)

rownames(pred_CE_median) <- rownames(CE_data)
colnames(pred_CE_median) <- colnames(CE_data)


#CE data
CE_long <- as.data.frame.table(
  CE_data,
  responseName = "CE"
) %>%
  rename(
    district = District,
    year = Year,
    jdate = Day
  ) %>%
  mutate(
    district = as.character(district),
    year = as.integer(as.character(year)),
    jdate = as.integer(as.character(jdate))
  )

#CPUE data
CPUE_long <- as.data.frame.table(
  CPUE_data,
  responseName = "CPUE"
) %>%
  rename(
    district = District,
    year = Year,
    jdate = Day
  ) %>%
  mutate(
    district = as.character(district),
    year = as.integer(as.character(year)),
    jdate = as.integer(as.character(jdate))
  ) %>%
  arrange(district, year, jdate)

#Pred CE data 
jdates <- dimnames(CE_data)[["Day"]]
jdates <- as.character(jdates)


Pred_long <- lapply(seq_along(districts), function(i) {
  
  tmp <- as.data.frame(pred_CE_median[i, , ])
  colnames(tmp) <- jdates
  tmp %>%
    tibble::rownames_to_column("year") %>%
    pivot_longer(
      -year,
      names_to = "jdate",
      values_to = "Pred_CE") %>%
    mutate(district = districts[i])}) %>% bind_rows() %>%
  mutate(
    district = as.character(district),
    year = as.character(year),
    jdate = as.character(jdate)) %>% 
  select(district, year, jdate, Pred_CE)

Pred_long$jdate <- as.integer(Pred_long$jdate)
Pred_long$year <- as.integer(Pred_long$year)

#Combine into one data frame for ggplot
plot_data <- CE_long %>%
  left_join(Pred_long, by = c("district", "year", "jdate")) %>%
  left_join(CPUE_long, by = c("district", "year", "jdate"))


####################################
#Plot the data for each district
#Im going to commment out the CPUE for seperate plots, it makes the plots harder to digest

for(i in seq_along(districts)) {
  
  district_data <- plot_data %>%
    filter(district == districts[i])
  
  fit <- ggplot(district_data,aes(x = as.numeric(jdate))) +
    geom_col(aes(y = CE),fill = "grey70",width = 1) +
    geom_line(aes(y = Pred_CE, colour = "Pred CE"),linewidth = 1) +
    geom_line(aes(y = CPUE * 5, colour = "Scaled CPUE"),linewidth = 1) +
    scale_color_manual(name = "",
      values = c(
        "Pred CE" = "red",
        "Scaled CPUE" = "blue")) +
    facet_wrap( ~year,scales = "free_y") +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(size = 16),
      axis.text.y = element_text(size = 16),
      axis.title = element_text(size = 20, face = "bold"),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 20),
      strip.text = element_text(size = 16, face = "bold")
    ) +
    labs(
      x = "Julian day",
      y = "Catch/Escapement",
      title = districts[i]
    )
  
  ggsave(
    filename = here(
      dir.figs,
      paste0("fit_CPUE_", districts[i], "_", version, ".png")
    ),
    plot = fit,
    width = 30,
    height = 24,
    dpi = 300
  )
}


#--------------------------------------------------
# Caterpillar plots by district
#--------------------------------------------------

RPI_summary <- data.frame(
  district = rep(districts, times = length(years)),
  year = rep(years, each = length(districts)),
  median = as.vector(
    apply(pars$RPI, c(2, 3), median)),
  lower = as.vector(
    apply(pars$RPI, c(2, 3), quantile, probs = 0.025)),
  upper = as.vector(
    apply(pars$RPI, c(2, 3), quantile, probs = 0.975)))
  

RPI_plot <- ggplot(RPI_summary, aes(x = district, y = median, colour = district)) +
  geom_errorbar(aes(ymin = lower,ymax = upper),width = 0.15,linewidth = 0.8) +
  geom_point( size = 3) +
  scale_colour_manual(
    values = c(
      "Ugashik" = "gold",
      "Egegik" = "red",
      "Kvichak" = "forestgreen",
      "Nushagak" = "royalblue3",
      "Togiak" = "darkorchid"
    ),name = "District") +
  facet_wrap(~year, ncol = 7,scales = "free_y") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 16, colour = "black", angle = 90,hjust = 1),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title = element_text(size = 20, colour = "black"),
    legend.text = element_text(size = 16, colour = "black"),
    legend.title = element_text(size = 18,colour = "black")) +
  labs(
    x = "",
    y = "RPI"
  )

ggsave(
  filename = here(
    dir.figs,
    "RPI_caterpillar_by_year.png"
  ),
  plot = RPI_plot,
  width = 16,
  height = 12,
  dpi = 300
)

RPI_plot2 <- ggplot(RPI_summary, aes(x = year, y = median, colour = district)) +
  geom_errorbar(aes(ymin = lower,ymax = upper),width = 0.15,linewidth = 0.8) +
  geom_point( size = 3) +
  scale_colour_manual(
    values = c(
      "Ugashik" = "gold",
      "Egegik" = "red",
      "Kvichak" = "forestgreen",
      "Nushagak" = "royalblue3",
      "Togiak" = "darkorchid"
    ),name = "District") +
  facet_wrap(~district, ncol = 5) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 30,colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle = 90,hjust = 1),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title = element_text(size = 20, colour = "black"),
    legend.text = element_text(size = 16, colour = "black"),
    legend.title = element_text(size = 18,colour = "black")) +
  labs(
    x = "",
    y = "RPI"
  )

ggsave(
  filename = here(
    dir.figs,
    "RPI_caterpillar_by_district.png"
  ),
  plot = RPI_plot2,
  width = 20,
  height = 12,
  dpi = 300
)


RPI_plot3<- ggplot(RPI_summary, aes(x = year, y = median, colour = district, fill = district)) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.25
  ) +
  geom_line(
    linewidth = 1.2
  ) +
  geom_point( size = 3) +
  scale_colour_manual(
    values = c(
      "Ugashik" = "gold",
      "Egegik" = "red",
      "Kvichak" = "forestgreen",
      "Nushagak" = "royalblue3",
      "Togiak" = "darkorchid"
    ),name = "District") +
  scale_fill_manual(
    values = c(
      "Ugashik" = "gold",
      "Egegik" = "red",
      "Kvichak" = "forestgreen",
      "Nushagak" = "royalblue3",
      "Togiak" = "darkorchid"
    )
  ) +
  guides(fill = "none")+
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 30,colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle = 90,hjust = 1),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title = element_text(size = 20, colour = "black"),
    legend.text = element_text(size = 16, colour = "black"),
    legend.title = element_text(size = 18,colour = "black")) +
  labs(
    x = "",
    y = "RPI"
  )

ggsave(
  filename = here(
    dir.figs,
    "RPI_caterpillar.png"
  ),
  plot = RPI_plot3,
  width = 20,
  height = 12,
  dpi = 300
)


TT_summary <- data.frame(
  district = rep(districts, times = length(years)),
  year = rep(years, each = length(districts)),
  median = as.vector(
    apply(pars$TT, c(2, 3), median)),
  lower = as.vector(
    apply(pars$TT, c(2, 3), quantile, probs = 0.025)),
  upper = as.vector(
    apply(pars$TT, c(2, 3), quantile, probs = 0.975)))

TT_plot <- ggplot(TT_summary, aes(x = district, y = median, colour = district)) +
  geom_errorbar(aes(ymin = lower,ymax = upper),width = 0.15,linewidth = 0.8) +
  geom_point( size = 3) +
  scale_colour_manual(
    values = c(
      "Ugashik" = "gold",
      "Egegik" = "red",
      "Kvichak" = "forestgreen",
      "Nushagak" = "royalblue3",
      "Togiak" = "darkorchid"
    ),name = "District") +
  facet_wrap(~year, ncol = 7) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 16, colour = "black", angle = 90,hjust = 1),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title = element_text(size = 20, colour = "black"),
    legend.text = element_text(size = 16, colour = "black"),
    legend.title = element_text(size = 18,colour = "black")) +
  labs(
    x = "",
    y = "TT"
  )

ggsave(
  filename = here(dir.figs, "TT_caterpillar_by_year.png"),
  plot = TT_plot,
  width = 16,
  height =12,
  dpi = 300
)

TT_plot2 <- ggplot(TT_summary, aes(x = year, y = median, colour = district)) +
  geom_errorbar(aes(ymin = lower,ymax = upper),width = 0.15,linewidth = 0.8) +
  geom_point( size = 3) +
  scale_colour_manual(
    values = c(
      "Ugashik" = "gold",
      "Egegik" = "red",
      "Kvichak" = "forestgreen",
      "Nushagak" = "royalblue3",
      "Togiak" = "darkorchid"
    ),name = "District") +
  facet_wrap(~district, ncol = 5) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 30,colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle = 90,hjust = 1),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title = element_text(size = 20, colour = "black"),
    legend.text = element_text(size = 16, colour = "black"),
    legend.title = element_text(size = 18,colour = "black")) +
  labs(
    x = "",
    y = "TT"
  )

ggsave(
  filename = here(
    dir.figs,
    "TT_caterpillar_by_district.png"
  ),
  plot = TT_plot2,
  width = 20,
  height = 12,
  dpi = 300
)


TT_plot3<- ggplot(TT_summary, aes(x = year, y = median, colour = district, fill = district)) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.25
  ) +
  geom_line(
    linewidth = 1.2
  ) +
  geom_point( size = 3) +
  scale_colour_manual(
    values = c(
      "Ugashik" = "gold",
      "Egegik" = "red",
      "Kvichak" = "forestgreen",
      "Nushagak" = "royalblue3",
      "Togiak" = "darkorchid"
    ),name = "District") +
  scale_fill_manual(
    values = c(
      "Ugashik" = "gold",
      "Egegik" = "red",
      "Kvichak" = "forestgreen",
      "Nushagak" = "royalblue3",
      "Togiak" = "darkorchid"
    )
  ) +
  guides(fill = "none")+
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 30,colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle = 90,hjust = 1),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title = element_text(size = 20, colour = "black"),
    legend.text = element_text(size = 16, colour = "black"),
    legend.title = element_text(size = 18,colour = "black")) +
  labs(
    x = "",
    y = "TT"
  )

ggsave(
  filename = here(
    dir.figs,
    "TT_caterpillar.png"
  ),
  plot = TT_plot3,
  width = 20,
  height = 12,
  dpi = 300
)

Sigma_summary <- data.frame(
  district = rep(districts, times = length(years)),
  year = rep(years, each = length(districts)),
  median = as.vector(
    apply(pars$sigma_CE, c(2, 3), median)),
  lower = as.vector(
    apply(pars$sigma_CE, c(2, 3), quantile, probs = 0.025)),
  upper = as.vector(
    apply(pars$sigma_CE, c(2, 3), quantile, probs = 0.975)))


Sigma_plot <- ggplot(Sigma_summary, aes(x = district, y = median, colour = district)) +
  geom_errorbar(aes(ymin = lower,ymax = upper),width = 0.15,linewidth = 0.8) +
  geom_point( size = 3) +
  scale_colour_manual(
    values = c(
      "Ugashik" = "gold",
      "Egegik" = "red",
      "Kvichak" = "forestgreen",
      "Nushagak" = "royalblue3",
      "Togiak" = "darkorchid"
    ),name = "District") +
  facet_wrap(~year, ncol = 7) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 16, colour = "black", angle = 90,hjust = 1),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title = element_text(size = 20, colour = "black"),
    legend.text = element_text(size = 16, colour = "black"),
    legend.title = element_text(size = 18,colour = "black")) +
  labs(
    x = "",
    y = "Sigma"
  )

ggsave(
  filename = here(dir.figs, "Sigma_caterpillar_by_year.png"),
  plot = Sigma_plot,
  width = 16,
  height =12,
  dpi = 300
)

Sigma_plot2 <- ggplot(Sigma_summary, aes(x = year, y = median, colour = district)) +
  geom_errorbar(aes(ymin = lower,ymax = upper),width = 0.15,linewidth = 0.8) +
  geom_point( size = 3) +
  scale_colour_manual(
    values = c(
      "Ugashik" = "gold",
      "Egegik" = "red",
      "Kvichak" = "forestgreen",
      "Nushagak" = "royalblue3",
      "Togiak" = "darkorchid"
    ),name = "District") +
  facet_wrap(~district, ncol = 5) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 30,colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle = 90,hjust = 1),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title = element_text(size = 20, colour = "black"),
    legend.text = element_text(size = 16, colour = "black"),
    legend.title = element_text(size = 18,colour = "black")) +
  labs(
    x = "",
    y = "Sigma"
  )

ggsave(
  filename = here(
    dir.figs,
    "Sigma_caterpillar_by_district.png"
  ),
  plot = Sigma_plot2,
  width = 20,
  height = 12,
  dpi = 300
)

Sigma_plot3<- ggplot(Sigma_summary, aes(x = year, y = median, colour = district, fill = district)) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.25
  ) +
  geom_line(
    linewidth = 1.2
  ) +
  geom_point( size = 3) +
  scale_colour_manual(
    values = c(
      "Ugashik" = "gold",
      "Egegik" = "red",
      "Kvichak" = "forestgreen",
      "Nushagak" = "royalblue3",
      "Togiak" = "darkorchid"
    ),name = "District") +
  scale_fill_manual(
    values = c(
      "Ugashik" = "gold",
      "Egegik" = "red",
      "Kvichak" = "forestgreen",
      "Nushagak" = "royalblue3",
      "Togiak" = "darkorchid"
    )
  ) +
  guides(fill = "none")+
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 30,colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle = 90,hjust = 1),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title = element_text(size = 20, colour = "black"),
    legend.text = element_text(size = 16, colour = "black"),
    legend.title = element_text(size = 18,colour = "black")) +
  labs(
    x = "",
    y = "Sigma"
  )

ggsave(
  filename = here(
    dir.figs,
    "Sigma_caterpillar.png"
  ),
  plot = Sigma_plot3,
  width = 20,
  height = 12,
  dpi = 300
)

