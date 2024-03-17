#### Preamble ####
# Purpose: Cleans and saves voter data obtained from Harvard's 2020 Cooperative Election Study (CES).
# Author: Sima Shmuylovich
# Date: 16 March 2024
# Contact: sima.shmuylovich@mail.utoronto.ca
# License: MIT
# Pre-requisites: Install tidyverse package. 
# Pre-requisites: Run 01-download_data.R 

#### Workspace setup ####
library(tidyverse)

#### Downlaod data ####
raw_data <-
  read_csv(
    "data/raw_data/raw_data.csv",
    col_types =
      cols(
        "votereg" = col_integer(),
        "CC20_410" = col_integer(),
        "gender" = col_integer(),
        "birthyr" = col_integer(),
        "race" = col_integer(),
        "educ" = col_integer(),
        "employ" = col_integer(),
        "faminc_new" = col_integer(),
        "region" = col_integer(),
        "urbancity" = col_integer(),
        "inputstate" = col_integer()
      )
  )
raw_data

#### Clean Data ####
analysis_data <-
  raw_data |>
  filter(votereg == 1,
         CC20_410 %in% c(1,2),
         gender %in% c(1,2),
         race %in% c(1, 2, 3, 4, 5, 8),
         educ %in% c(1:6),
         employ %in% c(1:8),
         faminc_new %in% c(1:16),
         region %in% c(1:4),
         urbancity %in% c(1:4),
         inputstate %in% c(1:56),
  ) |>
  mutate(
    voted_for = if_else(CC20_410 == 1, "Biden", "Trump"),
    voted_for = factor(voted_for),
    voted_for_bernoulli = if_else(CC20_410 == 1, 1, 0),
    gender = if_else(gender == 1, "Male", "Female"),
    age = 2020 - birthyr,
    age_bucket = case_when(
      2020 - birthyr >= 18 & age <= 29 ~ "18-29",
      2020 - birthyr >= 30 & age <= 44 ~ "30-44",
      2020 - birthyr >= 45 & age <= 64 ~ "45-64",
      2020 - birthyr > 65 ~ "65+",
    ),
    age_bucket = factor(
      age_bucket,
      levels = c(
        "18-29",
        "30-44",
        "45-64",
        "65+"
        )
    ),
    race = case_when(
      race == 1 ~ "White",
      race == 2 ~ "Black",
      race == 3 ~ "Hispanic",
      race == 4 ~ "Asian",
      race == 5 ~ "Other",
      race == 8 ~ "Other"
    ),
    race = factor(
      race,
      levels = c(
        "White",
        "Black",
        "Hispanic",
        "Asian",
        "Other"
      )
    ),
    education = case_when(
      educ == 1 ~ "High school or less",
      educ == 2 ~ "High school or less",
      educ == 3 ~ "Some college or assoc. degree",
      educ == 4 ~ "Some college or assoc. degree",
      educ == 5 ~ "College graduate",
      educ == 6 ~ "Postgraduate study"
    ),
    education = factor(
      education,
      levels = c(
        "High school or less",
        "Some college or assoc. degree",
        "College graduate",
        "Postgraduate study"
      )
    ),
    employment_status = case_when(
      employ == 1 ~ "Employed",
      employ == 2 ~ "Employed",
      employ == 3 ~ "Unemployed",
      employ == 4 ~ "Unemployed",
      employ == 5 ~ "Not in the Workforce",
      employ == 6 ~ "Not in the Workforce",
      employ == 7 ~ "Not in the Workforce",
      employ == 8 ~ "Not in the Workforce",
      employ == 9 ~ "Other"
    ),
    employment_status = factor(
      employment_status,
      levels = c(
        "Employed",
        "Unemployed",
        "Not in the Workforce",
        "Other"
      )
    ),
    income = case_when(
      faminc_new == 1 ~ "Less than 30,000",
      faminc_new == 2 ~ "Less than 30,000",
      faminc_new == 3 ~ "Less than 30,000",
      faminc_new == 4 ~ "30,000 - 49,999",
      faminc_new == 5 ~ "30,000 - 49,999",
      faminc_new == 6 ~ "50,000 - 99,999",
      faminc_new == 7 ~ "50,000 - 99,999",
      faminc_new == 8 ~ "50,000 - 99,999",
      faminc_new == 9 ~ "50,000 - 99,999",
      faminc_new == 10 ~ "100,000 - 199,999",
      faminc_new == 11 ~ "100,000 - 199,999",
      faminc_new == 12 ~ "100,000 - 199,999",
      faminc_new == 13 ~ "200,000 or more",
      faminc_new == 14 ~ "200,000 or more",
      faminc_new == 15 ~ "200,000 or more",
      faminc_new == 16 ~ "200,000 or more",
    ),
    income = factor(
      income,
      levels = c(
        "Less than 30,000",
        "30,000 - 49,999",
        "50,000 - 99,999",
        "100,000 - 199,999",
        "200,000 or more"
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
    )
  ) |>
  select(voted_for, voted_for_bernoulli, gender, age, age_bucket, race, education, employment_status, income, region, urban_status, state)

#### Save data ####
write_csv(analysis_data, "data/analysis_data/analysis_data.csv")

