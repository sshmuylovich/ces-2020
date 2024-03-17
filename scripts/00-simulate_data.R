#### Preamble ####
# Purpose: Simulates a dataset of predictor variables and the party. Based on sketches
# Author: Sima Shmuylovich
# Date: 16 March 2024
# Contact: sima.shmuylovich@mail.utoronto.ca
# License: MIT
# Pre-requisites: Install tidyverse package. 

#### Workspace setup ####
library(tidyverse)

#### Simulate data ####
set.seed(853)

num_obs <- 1000

voted_for_options = c("Biden", "Trump")
gender_options = c("Female", "Male")
age_buckets_options = c("18-29", "30-44", "45-64", "65+")
race_options = c("White", "Black", "Hispanic", "Asian", "Other")
education_options = c(
  "High school or less",
  "Some college or assoc. degree",
  "College graduate",
  "Postgraduate study"
)
employment_status_options = c(
  "Employed",
  "Unemployed",
  "Not in the Workforce",
  "Other"
)
income_options = c(
  "Less than 30,000",
  "30,000 - 49,999",
  "50,000 - 99,999",
  "100,000 - 199,999",
  "200,000 or more"
)
region_options = c(
  "Northeast",
  "Midwest",
  "South",
  "West"
)
urban_status_options = c(
  "City",
  "Suburb",
  "Town",
  "Rural Area"
)
state_options = c(
  "Alabama",
  "Alaska",
  "Arizona",
  "Arkansas",
  "California",
  "Colorado",
  "Connecticut",
  "Delaware",
  "District of Columbia",
  "Florida",
  "Georgia",
  "Hawaii",
  "Idaho",
  "Illinois",
  "Indiana",
  "Iowa",
  "Kansas",
  "Kentucky",
  "Louisiana",
  "Maine",
  "Maryland",
  "Massachusetts",
  "Michigan",
  "Minnesota",
  "Mississippi",
  "Missouri",
  "Montana",
  "Nebraska",
  "Nevada",
  "New Hampshire",
  "New Jersey",
  "New Mexico",
  "New York",
  "North Carolina",
  "North Dakota",
  "Ohio",
  "Oklahoma",
  "Oregon",
  "Pennsylvania",
  "Rhode Island",
  "South Carolina",
  "South Dakota",
  "Tennessee",
  "Texas",
  "Utah",
  "Vermont",
  "Virginia",
  "Washington",
  "West Virginia",
  "Wisconsin",
  "Wyoming"
)


simulation_data <- tibble(
  voted_for = sample(voted_for_options, size = num_obs, replace = TRUE),
  gender = sample(gender_options, size = num_obs, replace = TRUE),
  age_bucket = sample(age_buckets_options, size = num_obs, replace = TRUE),
  race = sample(race_options, size = num_obs, replace = TRUE),
  education = sample(education_options, size = num_obs, replace = TRUE),
  employment_status = sample(employment_status_options, size = num_obs, replace = TRUE),
  income = sample(income_options, size = num_obs, replace = TRUE),
  region = sample(region_options, size = num_obs, replace = TRUE),
  urban_status = sample(urban_status_options, size = num_obs, replace = TRUE),
  state = sample(state_options, size = num_obs, replace = TRUE),
)
