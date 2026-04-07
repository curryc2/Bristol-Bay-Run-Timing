library(here)
#install.packages("BAS")
library(BAS)
library(dplyr)


source(here("R/Pull Data.R"))

#after alot of investigation, the January PDO and ENSO are often very low on the inclusion probability
#and are highly correlated with the May PDO and ENSO.  As such, I am removing them from all models.  

# Ugashik 


Model_dat_Uga_BAS <- Model_dat_Uga %>% dplyr::select(-c(YEAR, ENSO_Jan, PDO_Jan))
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


Model_dat_Ege_BAS <- Model_dat_Ege %>% dplyr::select(-c(YEAR, ENSO_Jan, PDO_Jan))
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


Model_dat_Kvi_BAS <- Model_dat_Kvi %>% dplyr::select(-c(YEAR, ENSO_Jan, PDO_Jan))
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


Model_dat_Nush_BAS <- Model_dat_Nush %>% dplyr::select(-c(YEAR, ENSO_Jan, PDO_Jan))
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


Model_dat_Tog_BAS <- Model_dat_Tog %>% dplyr::select(-c(YEAR, ENSO_Jan, PDO_Jan))
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



#########################################################################################
#Repeat the same code as above, but remove the ChlA data and change name of saved plot


source(here("R/Pull Data.R"))

# Ugashik 


Model_dat_Uga_BAS_noChlA <- Model_dat_Uga %>% dplyr::select(-c(YEAR,PDO_Jan, ENSO_Jan,ChlA_GOAtiming, ChlA_GOAmagnitude))
Ugashik.ZS <- bas.lm(Uga ~ .,
                     data = Model_dat_Uga_BAS_noChlA,
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

ggsave(filename = paste0("figs/Ugashik Partial Effects/Uga_InclusionProbability_NoChlA.png"),
       plot = Inc_plot,
       width = 6,
       height = 4,
       dpi = 300)

R2_avg_uga <- sum(Ugashik.ZS$R2 * Ugashik.ZS$postprobs)

# Egegik 


Model_dat_Ege_BAS_noChlA <- Model_dat_Ege %>% dplyr::select(-c(YEAR,PDO_Jan, ENSO_Jan,ChlA_GOAtiming, ChlA_GOAmagnitude))
Egegik.ZS <- bas.lm(Ege ~ .,
                    data = Model_dat_Ege_BAS_noChlA,
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

ggsave(filename = paste0("figs/Egegik Partial Effects/Ege_InclusionProbability_NoChlA.png"),
       plot = Inc_plot,
       width = 6,
       height = 4,
       dpi = 300)

R2_avg_ege <- sum(Egegik.ZS$R2 * Egegik.ZS$postprobs)

#Kvichak


Model_dat_Kvi_BAS_noChlA <- Model_dat_Kvi %>% dplyr::select(-c(YEAR,PDO_Jan, ENSO_Jan,ChlA_GOAtiming, ChlA_GOAmagnitude))
Kvichak.ZS <- bas.lm(Kvi ~ .,
                     data = Model_dat_Kvi_BAS_noChlA,
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

ggsave(filename = paste0("figs/Kvichak Partial Effects/Kvi_InclusionProbability_NoChlA.png"),
       plot = Inc_plot,
       width = 6,
       height = 4,
       dpi = 300)

R2_avg_kvi <- sum(Kvichak.ZS$R2 * Kvichak.ZS$postprobs)

# Nushagak


Model_dat_Nush_BAS_noChlA <- Model_dat_Nush %>% dplyr::select(-c(YEAR,PDO_Jan, ENSO_Jan,ChlA_GOAtiming, ChlA_GOAmagnitude))
Nushagak.ZS <- bas.lm(Nush ~ .,
                      data = Model_dat_Nush_BAS_noChlA,
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

ggsave(filename = paste0("figs/Nushagak Partial Effects/Nush_InclusionProbability_NoChlA.png"),
       plot = Inc_plot,
       width = 6,
       height = 4,
       dpi = 300)

R2_avg_nush <- sum(Nushagak.ZS$R2 * Nushagak.ZS$postprobs)

#Togiak


Model_dat_Tog_BAS_noChlA <- Model_dat_Tog %>% dplyr::select(-c(YEAR,PDO_Jan, ENSO_Jan,ChlA_GOAtiming, ChlA_GOAmagnitude))
Togiak.ZS <- bas.lm(Tog ~ .,
                    data = Model_dat_Tog_BAS_noChlA,
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

Inc_plot <- ggplot(Tog_inclusionprobs, aes(x = reorder(Variable, Inclusion_Prob),
                               y = Inclusion_Prob)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = round(Inclusion_Prob, 2)),
            hjust = -0.1) +
  theme_minimal() +
  labs(x = "Variable", y = "Inclusion Probability")

ggsave(filename = paste0("figs/Togiak Partial Effects/Tog_InclusionProbability_NoChlA.png"),
       plot = Inc_plot,
       width = 6,
       height = 4,
       dpi = 300)

R2_avg_tog <- sum(Togiak.ZS$R2 * Togiak.ZS$postprobs)
