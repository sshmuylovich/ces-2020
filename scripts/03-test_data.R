#### Preamble ####
# Purpose: Tests cleaned data to ensure robustness.
# Author: Sima Shmuylovich
# Date: 16 March 2024
# Contact: sima.shmuylovich@mail.utoronto.ca
# License: MIT
# Pre-requisites: Install tidyverse and testthat packages. 
# Pre-requisites: Run 01-download_data.R and 02-data_cleaning.R


#### Workspace setup ####
library(tidyverse)
library(testthat)

#### Test data ####
test_data <- read_csv("../data/analysis_data/analysis_data.csv")

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

test_that("No NULL values", {
  expect_false(any(is.null(test_data$voted_for)), "voted_for no NULLS")
  expect_false(any(is.null(test_data$voted_for_bernoulli)), "voted_for_bernoulli no NULLS")
  expect_false(any(is.null(test_data$gender)), "gender no NULLSs")
  expect_false(any(is.null(test_data$age)), "age no NULLS")
  expect_false(any(is.null(test_data$age_bucket)), "age_bucket no NULLS")
  expect_false(any(is.null(test_data$race)), "race no NULLS")
  expect_false(any(is.null(test_data$education)), "education no NULLS")
  expect_false(any(is.null(test_data$employment_status)), "employment_status no NULLS")
  expect_false(any(is.null(test_data$income)), "income no NULLS")
  expect_false(any(is.null(test_data$region)), "region no NULLS")
  expect_false(any(is.null(test_data$urban_status)), "urban_status no NULLS")
  expect_false(any(is.null(test_data$state)), "state no NULLS")
})

test_that("Expected type", {
  expect_type(test_data$voted_for, "character") 
  expect(is.numeric(test_data$voted_for_bernoulli))
  expect_type(test_data$gender, "character")
  expect(is.numeric(test_data$age))
  expect_type(test_data$age_bucket, "character")
  expect_type(test_data$race, "character")
  expect_type(test_data$education, "character")
  expect_type(test_data$employment_status, "character")
  expect_type(test_data$income, "character")
  expect_type(test_data$region, "character")
  expect_type(test_data$urban_status, "character")
  expect_type(test_data$state, "character")
})

test_that("Binary Variable Check", {
  expect(all(test_data$voted_for %in% c("Biden", "Trump")))
  expect(all(test_data$voted_for_bernoulli %in% c(0, 1)))
})

test_that("Numeric Variable Interval Check", {
  expect(all(test_data$age > 0), "age > 0")
  expect(all(test_data$age >= 18), "age >= 18")
})

test_that("Expected Values", {
  expect(all(test_data$voted_for %in% voted_for_options))
  expect(all(test_data$gender %in% gender_options))
  expect(all(test_data$age_buckets %in% age_buckets_options))
  expect(all(test_data$race %in% race_options))
  expect(all(test_data$education %in% education_options))
  expect(all(test_data$employment_status %in% employment_status_options))
  expect(all(test_data$income %in% income_options))
  expect(all(test_data$region %in% region_options))
  expect(all(test_data$urban_status %in% urban_status_options))
  expect(all(test_data$state %in% state_options))
  })

