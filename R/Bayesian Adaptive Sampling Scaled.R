library(here)
#install.packages("BAS")
library(BAS)
library(dplyr)


source(here("R/Pull Data Scaled.R"))

####
#rename for the plots

Model_dat_Uga_BAS <- S_Model_dat_Uga_ChlA %>% dplyr::select(-YEAR)

Model_dat_Uga_BAS <- Model_dat_Uga_BAS %>% rename(
                                                "Cumulative East Wind (June)" = "S_June_cumeastwind" ,
                                                "Cumulative North Wind (June)" = "S_June_cumnorthwind",
                                                "PDO (May)" = "S_PDO_May",
                                                "Gulf of Alaska ChlA Spring Bloom Magnitude" = "S_ChlA_GOAmagnitude",
                                                "Gulf of Alaska ChlA Spring Bloom Timing" = "S_ChlA_GOAtiming",
                                                "Mean Air Pressure (June)" = "S_June_pressure",
                                                "Mean Air Temperature (June)" = "S_June_temp",
                                                "Sea Ice Extent (May)" = "S_extent",
                                                "Pink Salmon Abundance" = "S_Abundance",
                                                "Mean Gulf of Alaska Sea Surface Temperature (April/May)" = "S_GOA_SpringSST",
                                                "Total District River Meanflow (June)" = "S_Ugashik_meanflow_June",
                                                "Mean District Sea Surface Temperature (June)" = "S_Uga_JuneSST",
                                                "Previous Year Median Return Date" = "S_Uga_lag1",
                                                "ENSO (May)" = "S_ENSO_May")

Model_dat_Ege_BAS <- S_Model_dat_Ege_ChlA %>% dplyr::select(-YEAR)

Model_dat_Ege_BAS <- Model_dat_Ege_BAS %>% rename(
                                                "Cumulative East Wind (June)" = "S_June_cumeastwind" ,
                                                "Cumulative North Wind (June)" = "S_June_cumnorthwind",
                                                "PDO (May)" = "S_PDO_May",
                                                "Gulf of Alaska ChlA Spring Bloom Magnitude" = "S_ChlA_GOAmagnitude",
                                                "Gulf of Alaska ChlA Spring Bloom Timing" = "S_ChlA_GOAtiming",
                                                "Mean Air Pressure (June)" = "S_June_pressure",
                                                "Mean Air Temperature (June)" = "S_June_temp",
                                                "Sea Ice Extent (May)" = "S_extent",
                                                "Pink Salmon Abundance" = "S_Abundance",
                                                "Mean Gulf of Alaska Sea Surface Temperature (April/May)" = "S_GOA_SpringSST",
                                                "Total District River Meanflow (June)" = "S_Egegik_meanflow_June",
                                                "Mean District Sea Surfact Temperature (June)" = "S_Ege_JuneSST",
                                                "Previous Year Median Return Date" = "S_Ege_lag1",
                                                "ENSO (May)" = "S_ENSO_May")

Model_dat_Kvi_BAS <- S_Model_dat_Kvi_ChlA %>% dplyr::select(-YEAR)

Model_dat_Kvi_BAS <- Model_dat_Kvi_BAS %>% rename(
                                                "Cumulative East Wind (June)" = "S_June_cumeastwind" ,
                                                "Cumulative North Wind (June)" = "S_June_cumnorthwind",
                                                "PDO (May)" = "S_PDO_May",
                                                "Gulf of Alaska ChlA Spring Bloom Magnitude" = "S_ChlA_GOAmagnitude",
                                                "Gulf of Alaska ChlA Spring Bloom Timing" = "S_ChlA_GOAtiming",
                                                "Mean Air Pressure (June)" = "S_June_pressure",
                                                "Mean Air Temperature (June)" = "S_June_temp",
                                                "Sea Ice Extent (May)" = "S_extent",
                                                "Pink Salmon Abundance" = "S_Abundance",
                                                "Mean Gulf of Alaska Sea Surface Temperature (April/May)" = "S_GOA_SpringSST",
                                                "Total District River Meanflow (June)" = "S_KvichakDistrict_meanflow_June",
                                                "Mean District Sea Surfact Temperature (June)" = "S_Kvi_JuneSST",
                                                "Previous Year Median Return Date" = "S_Kvi_lag1",
                                                "ENSO (May)" = "S_ENSO_May",
                                                "Proportion of Late District Run" = "S_Kvichakproportion")

Model_dat_Nush_BAS <- S_Model_dat_Nush_ChlA %>% dplyr::select(-YEAR)

Model_dat_Nush_BAS <- Model_dat_Nush_BAS %>% rename(
                                                "Cumulative East Wind (June)" = "S_June_cumeastwind" ,
                                                "Cumulative North Wind (June)" = "S_June_cumnorthwind",
                                                "PDO (May)" = "S_PDO_May",
                                                "Gulf of Alaska ChlA Spring Bloom Magnitude" = "S_ChlA_GOAmagnitude",
                                                "Gulf of Alaska ChlA Spring Bloom Timing" = "S_ChlA_GOAtiming",
                                                "Mean Air Pressure (June)" = "S_June_pressure",
                                                "Mean Air Temperature (June)" = "S_June_temp",
                                                "Sea Ice Extent (May)" = "S_extent",
                                                "Pink Salmon Abundance" = "S_Abundance",
                                                "Mean Gulf of Alaska Sea Surface Temperature (April/May)" = "S_GOA_SpringSST",
                                                "Total District River Meanflow (June)" = "S_NushagakDistrict_meanflow_June",
                                                "Mean District Sea Surfact Temperature (June)" = "S_Nush_JuneSST",
                                                "Previous Year Median Return Date" = "S_Nush_lag1",
                                                "ENSO (May)" = "S_ENSO_May",
                                                "Proportion of Late District Run" = "S_Igushikproportion")

Model_dat_Tog_BAS <- S_Model_dat_Tog_ChlA %>% dplyr::select(-YEAR)

Model_dat_Tog_BAS <- Model_dat_Tog_BAS %>% rename(
                                                "Cumulative East Wind (June)" = "S_June_cumeastwind" ,
                                                "Cumulative North Wind (June)" = "S_June_cumnorthwind",
                                                "PDO (May)" = "S_PDO_May",
                                                "Gulf of Alaska ChlA Spring Bloom Magnitude" = "S_ChlA_GOAmagnitude",
                                                "Gulf of Alaska ChlA Spring Bloom Timing" = "S_ChlA_GOAtiming",
                                                "Mean Air Pressure (June)" = "S_June_pressure",
                                                "Mean Air Temperature (June)" = "S_June_temp",
                                                "Sea Ice Extent (May)" = "S_extent",
                                                "Pink Salmon Abundance" = "S_Abundance",
                                                "Mean Gulf of Alaska Sea Surface Temperature (April/May)" = "S_GOA_SpringSST",
                                                "Total District River Meanflow (June)" = "S_Togiak_meanflow_June",
                                                "Mean District Sea Surfact Temperature (June)" = "S_Tog_JuneSST",
                                                "Previous Year Median Return Date" = "S_Tog_lag1",
                                                "ENSO (May)" = "S_ENSO_May")

# Ugashik 


Ugashik.ZS <- bas.lm(Uga ~ .,
                     data = Model_dat_Uga_BAS,
                     prior = "ZS-null",
                     modelprior = uniform(), initprobs = "eplogp",
                     force.heredity = FALSE, pivot = TRUE
)

par(mfrow=c(2,2))
plot(Ugashik.ZS, ask = F)

Uga_inclusionprobs <- data.frame(
  Variable = Ugashik.ZS$namesx,
  Inclusion_Prob = Ugashik.ZS$probne0,
  District = "Ugashik"
)
Uga_inclusionprobs$Variable <- gsub("[`'\"]", "", Uga_inclusionprobs$Variable)
Uga_inclusionprobs <- Uga_inclusionprobs %>% filter(Variable != "Intercept")

library(ggplot2)

##### Plot

Inc_plot <-ggplot(Uga_inclusionprobs, aes(x = reorder(Variable, Inclusion_Prob),
                                          y = Inclusion_Prob)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = round(Inclusion_Prob, 2)),
            hjust = -0.1) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 20)
  ) +
  labs(x = "Variable", y = "Inclusion Probability")

ggsave(filename = paste0("figs/Ugashik Partial Effects/Uga_InclusionProbability.png"),
       plot = Inc_plot,
       width =13,
       height = 6,
       dpi = 300)

R2_avg_uga_ChlA <- sum(Ugashik.ZS$R2 * Ugashik.ZS$postprobs)


##################### Egegik 

Egegik.ZS <- bas.lm(Ege ~ .,
                    data = Model_dat_Ege_BAS,
                    prior = "ZS-null",
                    modelprior = uniform(), initprobs = "eplogp",
                    force.heredity = FALSE, pivot = TRUE
)

par(mfrow=c(2,2))
plot(Egegik.ZS, ask = F)

Ege_inclusionprobs <- data.frame(
  Variable = Egegik.ZS$namesx,
  Inclusion_Prob = Egegik.ZS$probne0,
  District = "Egegik"
)
Ege_inclusionprobs$Variable <- gsub("[`'\"]", "", Ege_inclusionprobs$Variable)
Ege_inclusionprobs <- Ege_inclusionprobs %>% filter(Variable != "Intercept")

library(ggplot2)
##Plot 

Inc_plot <-ggplot(Ege_inclusionprobs, aes(x = reorder(Variable, Inclusion_Prob),
                                          y = Inclusion_Prob)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = round(Inclusion_Prob, 2)),
            hjust = -0.1) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 20)
  ) +
  labs(x = "Variable", y = "Inclusion Probability")

ggsave(filename = paste0("figs/Egegik Partial Effects/Ege_InclusionProbability.png"),
       plot = Inc_plot,
       width = 13,
       height = 6,
       dpi = 300)

R2_avg_ege_ChlA <- sum(Egegik.ZS$R2 * Egegik.ZS$postprobs)

####################Kvichak


Kvichak.ZS <- bas.lm(Kvi ~ .,
                     data = Model_dat_Kvi_BAS,
                     prior = "ZS-null",
                     modelprior = uniform(), initprobs = "eplogp",
                     force.heredity = FALSE, pivot = TRUE
)

par(mfrow=c(2,2))
plot(Kvichak.ZS, ask = F)

Kvi_inclusionprobs <- data.frame(
  Variable = Kvichak.ZS$namesx,
  Inclusion_Prob = Kvichak.ZS$probne0,
  District = "Kvichak"
)
Kvi_inclusionprobs$Variable <- gsub("[`'\"]", "", Kvi_inclusionprobs$Variable)
Kvi_inclusionprobs <- Kvi_inclusionprobs %>% filter(Variable != "Intercept")

library(ggplot2)

###Plot

Inc_plot <-ggplot(Kvi_inclusionprobs, aes(x = reorder(Variable, Inclusion_Prob),
                                          y = Inclusion_Prob)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = round(Inclusion_Prob, 2)),
            hjust = -0.1) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 20)
  ) +
  labs(x = "Variable", y = "Inclusion Probability")

ggsave(filename = paste0("figs/Kvichak Partial Effects/Kvi_InclusionProbability.png"),
       plot = Inc_plot,
       width = 13,
       height = 6,
       dpi = 300)

R2_avg_kvi_ChlA <- sum(Kvichak.ZS$R2 * Kvichak.ZS$postprobs)

############ Nushagak

Nushagak.ZS <- bas.lm(Nush ~ .,
                      data = Model_dat_Nush_BAS,
                      prior = "ZS-null",
                      modelprior = uniform(), initprobs = "eplogp",
                      force.heredity = FALSE, pivot = TRUE
)

par(mfrow=c(2,2))
plot(Nushagak.ZS, ask = F)

Nush_inclusionprobs <- data.frame(
  Variable = Nushagak.ZS$namesx,
  Inclusion_Prob = Nushagak.ZS$probne0,
  District = "Nushagak"
)
Nush_inclusionprobs$Variable <- gsub("[`'\"]", "", Nush_inclusionprobs$Variable)
Nush_inclusionprobs <- Nush_inclusionprobs %>% filter(Variable != "Intercept")

library(ggplot2)

###Plot

Inc_plot <-ggplot(Nush_inclusionprobs, aes(x = reorder(Variable, Inclusion_Prob),
                                           y = Inclusion_Prob)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = round(Inclusion_Prob, 2)),
            hjust = -0.1) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 20)
  ) +
  labs(x = "Variable", y = "Inclusion Probability")

ggsave(filename = paste0("figs/Nushagak Partial Effects/Nush_InclusionProbability.png"),
       plot = Inc_plot,
       width = 13,
       height = 6,
       dpi = 300)

R2_avg_nush_ChlA <- sum(Nushagak.ZS$R2 * Nushagak.ZS$postprobs)

##################Togiak

Togiak.ZS <- bas.lm(Tog ~ .,
                    data = Model_dat_Tog_BAS,
                    prior = "ZS-null",
                    modelprior = uniform(), initprobs = "eplogp",
                    force.heredity = FALSE, pivot = TRUE
)

par(mfrow=c(2,2))
plot(Togiak.ZS, ask = F)

Tog_inclusionprobs <- data.frame(
  Variable = Togiak.ZS$namesx,
  Inclusion_Prob = Togiak.ZS$probne0,
  District = "Togiak"
)

Tog_inclusionprobs$Variable <- gsub("[`'\"]", "", Tog_inclusionprobs$Variable)
Tog_inclusionprobs <- Tog_inclusionprobs %>% filter(Variable != "Intercept")

library(ggplot2)

###Plot
Inc_plot <-ggplot(Tog_inclusionprobs, aes(x = reorder(Variable, Inclusion_Prob),
                                          y = Inclusion_Prob)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = round(Inclusion_Prob, 2)),
            hjust = -0.1) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 20)
  ) +
  labs(x = "Variable", y = "Inclusion Probability")

ggsave(filename = paste0("figs/Togiak Partial Effects/Tog_InclusionProbability.png"),
       plot = Inc_plot,
       width = 13,
       height = 6,
       dpi = 300)

R2_avg_tog_ChlA <- sum(Togiak.ZS$R2 * Togiak.ZS$postprobs)

###################Combinded BAS plot

Inclusion_data <- rbind(Uga_inclusionprobs, Ege_inclusionprobs, Kvi_inclusionprobs, Nush_inclusionprobs, Tog_inclusionprobs)
Inclusion_data$District <- factor(Inclusion_data$District,
                             levels = c("Ugashik", "Egegik", "Kvichak", "Nushagak", "Togiak"))

library(ggplot2)


###Plot
Inc_plot <-ggplot(Inclusion_data, aes(x = reorder(Variable, Inclusion_Prob),
                                          y = Inclusion_Prob)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = round(Inclusion_Prob, 2)),
            hjust = -0.1) +
  facet_wrap(~District, nrow = 1)+
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.5, "lines"),
    
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    strip.text = element_text(size = 16),
    
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 20)
  ) +
  labs(x = "Predictor", y = "Inclusion Probability")

ggsave(filename = paste0("figs/InclusionProbability.png"),
       plot = Inc_plot,
       width = 20,
       height = 10,
       dpi = 300)
