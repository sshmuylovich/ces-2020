#### Preamble ####
# Purpose: Models Identity-Baed variables, Socio-Economic Variables, and Regional Variables for voter data obtained from Harvard's 2020 Cooperative Election Study (CES).
# Author: Sima Shmuylovich
# Date: 16 March 2024
# Contact: sima.shmuylovich@mail.utoronto.ca
# License: MIT
# Pre-requisites: Install tidyverse, rstanarm, and dataverse packages. 
# Pre-requisites: Run 01-download_data.R and 02-data_cleaning.R 

#### Workspace setup ####
library(tidyverse)
library(rstanarm)
library(modelsummary)

#### Read data ####
model_data <- read_csv("data/analysis_data/analysis_data.csv")

model_data_reduced <- 
  model_data |> 
  slice_sample(n = 10000)

### Model data ####
set.seed(853)

identity_based_model_reduced <-
  stan_glm(
    formula = voted_for_bernoulli ~ gender + age_bucket + race,
    family = binomial(),
    data = model_data_reduced,
    prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_intercept = normal(location = 0, scale = 2.5, autoscale = TRUE),
    seed = 853
  )

socio_economic_model_reduced <-
  stan_glm(
    formula = voted_for_bernoulli ~ education + employment_status + income,
    family = binomial(),
    data = model_data_reduced,
    prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_intercept = normal(location = 0, scale = 2.5, autoscale = TRUE),
    seed = 853
  )

regional_model_reduced <-
  stan_glm(
    formula = voted_for_bernoulli ~ region + urban_status + state,
    family = binomial(),
    data = model_data_reduced,
    prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_intercept = normal(location = 0, scale = 2.5, autoscale = TRUE),
    seed = 853
  )

#### Save model ####
saveRDS(
  identity_based_model_reduced,
  file = "models/identity_based_model_reduced.rds"
)

saveRDS(
  socio_economic_model_reduced,
  file = "models/socio_economic_model_reduced.rds"
)

saveRDS(
  regional_model_reduced,
  file = "models/regional_model_reduced.rds"
)
