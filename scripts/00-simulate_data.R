#### Preamble ####
# Purpose: Simulates... [...UPDATE THIS...]
# Author: Rohan Alexander [...UPDATE THIS...]
# Date: 11 February 2023 [...UPDATE THIS...]
# Contact: rohan.alexander@utoronto.ca [...UPDATE THIS...]
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]


#### Workspace setup ####
library(tidyverse)

#### Simulate data ####
set.seed(853)

num_obs <- 1000

us_political_preferences <- tibble(
  gender = sample(1:2, size = num_obs, replace = TRUE),
  birthyr = sample(0:100, size = num_obs, replace = TRUE),
  race = sample(c(1, 2, 3, 4, 5, 8), size = num_obs, replace = TRUE),
  educ = sample(1:6, size = num_obs, replace = TRUE),
  employ = sample(1:9, size = num_obs, replace = TRUE),
  faminc_new = sample(1:16, size = num_obs, replace = TRUE),
  region = sample(1:4, size = num_obs, replace = TRUE),
  urbancity = sample(1:4, size = num_obs, replace = TRUE),
  inputstate = sample(c(1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20,
                        21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 
                        41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56), size = num_obs, replace = TRUE),
  support_prob = ((gender + age + race + education + employment_status + income + region + urban_status + state) / 5),
) |>
  mutate(
    supports_biden = if_else(runif(n = num_obs) < support_prob, "yes", "no"),
    gender = if_else(gender == 1, "Male", "Female"),
    age = 2020 - birthyr,
    race = case_when(
      race == 1 ~ "White",
      race == 2 ~ "Black or African-American",
      race == 3 ~ "Hispanic or Latino",
      race == 4 ~ "Asian or Asian-American",
      race == 5 ~ "Native American",
      race == 8 ~ "Middle Eastern"
    ),
    race = factor(
      race,
      levels = c(
        "White",
        "Black or African-American",
        "Hispanic or Latino",
        "Asian or Asian-American",
        "Native American",
        "Middle Eastern"
      )
    ),
    education = case_when(
      educ == 1 ~ "No HS",
      educ == 2 ~ "High school graduate",
      educ == 3 ~ "Some college",
      educ == 4 ~ "2-year",
      educ == 5 ~ "4-year",
      educ == 6 ~ "Post-grad"
    ),
    education = factor(
      education,
      levels = c(
        "No HS",
        "High school graduate",
        "Some college",
        "2-year",
        "4-year",
        "Post-grad"
      )
    ),
    employment_status = case_when(
      employ == 1 ~ "Working full time now",
      employ == 2 ~ "Working part time now",
      employ == 3 ~ "Temporarily laid off",
      employ == 4 ~ "Unemployed",
      employ == 5 ~ "Retired",
      employ == 6 ~ "Permanently disabled",
      employ == 7 ~ "Taking care of home or family",
      employ == 8 ~ "Student",
      employ == 9 ~ "Other"
    ),
    employment_status = factor(
      employment_status,
      levels = c(
        "Working full time now",
        "Working part time now",
        "Temporarily laid off",
        "Unemployed",
        "Retired",
        "Permanently disabled",
        "Taking care of home or family",
        "Student",
        "Other"
      )
    ),
    income = case_when(
      faminc_new == 1 ~ "Less than 10,000",
      faminc_new == 2 ~ "10,000 - 19,999",
      faminc_new == 3 ~ "20,000 - 29,999",
      faminc_new == 4 ~ "30,000 - 39,999",
      faminc_new == 5 ~ "40,000 - 49,999",
      faminc_new == 6 ~ "50,000 - 59,999",
      faminc_new == 7 ~ "60,000 - 69,999",
      faminc_new == 8 ~ "70,000 - 79,999",
      faminc_new == 9 ~ "80,000 - 99,999",
      faminc_new == 10 ~ "100,000 - 119,999",
      faminc_new == 11 ~ "120,000 - 149,999",
      faminc_new == 12 ~ "150,000 - 199,999",
      faminc_new == 13 ~ "200,000 - 249,999",
      faminc_new == 14 ~ "250,000 - 349,999",
      faminc_new == 15 ~ "350,000 - 499,999",
      faminc_new == 16 ~ "500,000 or more"
    ),
    income = factor(
      income,
      levels = c(
        "Less than 10,000",
        "10,000 - 19,999",
        "20,000 - 29,999",
        "30,000 - 39,999",
        "40,000 - 49,999",
        "50,000 - 59,999",
        "60,000 - 69,999",
        "70,000 - 79,999",
        "80,000 - 99,999",
        "100,000 - 119,999",
        "120,000 - 149,999",
        "150,000 - 199,999",
        "200,000 - 249,999",
        "250,000 - 349,999",
        "350,000 - 499,999",
        "500,000 or more"
      )
    ),
    region = case_when(
      region == 1 ~ "Northeast",
      region == 2 ~ "Midwest",
      region == 3 ~ "South",
      region == 4 ~ "West",
    ),
    region = factor(
      region,
      levels = c(
        "Northeast",
        "Midwest",
        "South",
        "West"
      )
    ),
    urban_status = case_when(
      urbancity == 1 ~ "City",
      urbancity == 2 ~ "Suburb",
      urbancity == 3 ~ "Town",
      urbancity == 4 ~ "Rural Area",
    ),
    urban_status = factor(
      urban_status,
      levels = c(
        "City",
        "Suburb",
        "Town",
        "Rural Area"
      )
    ),
    state = case_when(
      inputstate == 1 ~ "Alabama",
      inputstate == 2 ~ "Alaska",
      inputstate == 4 ~ "Arizona",
      inputstate == 5 ~ "Arkansas",
      inputstate == 6 ~ "California",
      inputstate == 8 ~ "Colorado",
      inputstate == 9 ~ "Connecticut",
      inputstate == 10 ~ "Delaware",
      inputstate == 11 ~ "District of Columbia",
      inputstate == 12 ~ "Florida",
      inputstate == 13 ~ "Georgia",
      inputstate == 15 ~ "Hawaii",
      inputstate == 16 ~ "Idaho",
      inputstate == 17 ~ "Illinois",
      inputstate == 18 ~ "Indiana",
      inputstate == 19 ~ "Iowa",
      inputstate == 20 ~ "Kansas",
      inputstate == 21 ~ "Kentucky",
      inputstate == 22 ~ "Louisiana",
      inputstate == 23 ~ "Maine",
      inputstate == 24 ~ "Maryland",
      inputstate == 25 ~ "Massachusetts",
      inputstate == 26 ~ "Michigan",
      inputstate == 27 ~ "Minnesota",
      inputstate == 28 ~ "Mississippi",
      inputstate == 29 ~ "Missouri",
      inputstate == 30 ~ "Montana",
      inputstate == 31 ~ "Nebraska",
      inputstate == 32 ~ "Nevada",
      inputstate == 33 ~ "New Hampshire",
      inputstate == 34 ~ "New Jersey",
      inputstate == 35 ~ "New Mexico",
      inputstate == 36 ~ "New York",
      inputstate == 37 ~ "North Carolina",
      inputstate == 38 ~ "North Dakota",
      inputstate == 39 ~ "Ohio",
      inputstate == 40 ~ "Oklahoma",
      inputstate == 41 ~ "Oregon",
      inputstate == 42 ~ "Pennsylvania",
      inputstate == 44 ~ "Rhode Island",
      inputstate == 45 ~ "South Carolina",
      inputstate == 46 ~ "South Dakota",
      inputstate == 47 ~ "Tennessee",
      inputstate == 48 ~ "Texas",
      inputstate == 49 ~ "Utah",
      inputstate == 50 ~ "Vermont",
      inputstate == 51 ~ "Virginia",
      inputstate == 53 ~ "Washington",
      inputstate == 54 ~ "West Virginia",
      inputstate == 55 ~ "Wisconsin",
      inputstate == 56 ~ "Wyoming"
    ),
    state = factor(
      state,
      levels = c(
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
  ) |>
  select(-support_prob, supports_biden, gender, education)


