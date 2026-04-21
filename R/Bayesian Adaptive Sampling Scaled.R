library(here)
#install.packages("BAS")
library(BAS)
library(dplyr)


source(here("R/Pull Data Scaled.R"))



# Ugashik 


Model_dat_Uga_BAS <- S_Model_dat_Uga_noChlA2000 %>% dplyr::select(-YEAR)
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
  Inclusion_Prob = Ugashik.ZS$probne0
)

library(ggplot2)

Inc_plot <-ggplot(Uga_inclusionprobs, aes(x = reorder(Variable, Inclusion_Prob),
                                          y = Inclusion_Prob)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = round(Inclusion_Prob, 2)),
            hjust = -0.1) +
  theme_minimal() +
  labs(x = "Variable", y = "Inclusion Probability")

ggsave(filename = paste0("figs/Ugashik Partial Effects/Uga_InclusionProbability.png"),
       plot = Inc_plot,
       width = 6,
       height = 4,
       dpi = 300)

R2_avg_uga_ChlA <- sum(Ugashik.ZS$R2 * Ugashik.ZS$postprobs)


# Egegik 


Model_dat_Ege_BAS <- S_Model_dat_Ege_noChlA2000 %>% dplyr::select(-YEAR)
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
  Inclusion_Prob = Egegik.ZS$probne0
)

library(ggplot2)

Inc_plot <-ggplot(Ege_inclusionprobs, aes(x = reorder(Variable, Inclusion_Prob),
                                          y = Inclusion_Prob)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = round(Inclusion_Prob, 2)),
            hjust = -0.1) +
  theme_minimal() +
  labs(x = "Variable", y = "Inclusion Probability")

ggsave(filename = paste0("figs/Egegik Partial Effects/Ege_InclusionProbability.png"),
       plot = Inc_plot,
       width = 6,
       height = 4,
       dpi = 300)

R2_avg_ege_ChlA <- sum(Egegik.ZS$R2 * Egegik.ZS$postprobs)

#Kvichak


Model_dat_Kvi_BAS <- S_Model_dat_Kvi_noChlA2000 %>% dplyr::select(-YEAR)
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
  Inclusion_Prob = Kvichak.ZS$probne0
)

library(ggplot2)

Inc_plot <-ggplot(Kvi_inclusionprobs, aes(x = reorder(Variable, Inclusion_Prob),
                                          y = Inclusion_Prob)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = round(Inclusion_Prob, 2)),
            hjust = -0.1) +
  theme_minimal() +
  labs(x = "Variable", y = "Inclusion Probability")

ggsave(filename = paste0("figs/Kvichak Partial Effects/Kvi_InclusionProbability.png"),
       plot = Inc_plot,
       width = 6,
       height = 4,
       dpi = 300)

R2_avg_kvi_ChlA <- sum(Kvichak.ZS$R2 * Kvichak.ZS$postprobs)

# Nushagak


Model_dat_Nush_BAS <- S_Model_dat_Nush_noChlA2000 %>% dplyr::select(-YEAR)
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
  Inclusion_Prob = Nushagak.ZS$probne0
)

library(ggplot2)

Inc_plot <-ggplot(Nush_inclusionprobs, aes(x = reorder(Variable, Inclusion_Prob),
                                           y = Inclusion_Prob)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = round(Inclusion_Prob, 2)),
            hjust = -0.1) +
  theme_minimal() +
  labs(x = "Variable", y = "Inclusion Probability")

ggsave(filename = paste0("figs/Nushagak Partial Effects/Nush_InclusionProbability.png"),
       plot = Inc_plot,
       width = 6,
       height = 4,
       dpi = 300)

R2_avg_nush_ChlA <- sum(Nushagak.ZS$R2 * Nushagak.ZS$postprobs)

#Togiak


Model_dat_Tog_BAS <- S_Model_dat_Tog_noChlA2000 %>% dplyr::select(-YEAR)
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
  Inclusion_Prob = Togiak.ZS$probne0
)

library(ggplot2)

Inc_plot <-ggplot(Tog_inclusionprobs, aes(x = reorder(Variable, Inclusion_Prob),
                                          y = Inclusion_Prob)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = round(Inclusion_Prob, 2)),
            hjust = -0.1) +
  theme_minimal() +
  labs(x = "Variable", y = "Inclusion Probability")

ggsave(filename = paste0("figs/Togiak Partial Effects/Tog_InclusionProbability.png"),
       plot = Inc_plot,
       width = 6,
       height = 4,
       dpi = 300)

R2_avg_tog_ChlA <- sum(Togiak.ZS$R2 * Togiak.ZS$postprobs)


