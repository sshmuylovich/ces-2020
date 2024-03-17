#### Preamble ####
# Purpose: Downloads and saves the data from Harvard's 2020 Cooperative Election Study (CES).
# Author: Sima Shmuylovich
# Contact: sima.shmuylovich@mail.utoronto.ca
# License: MIT
# Pre-requisites: Install tidyverse and dataverse packages. 


#### Workspace setup ####
library(tidyverse)
library(dataverse)

#### Identify variables of interest ####
voting_var = c("votereg", "CC20_410")
identity_based_var = c("gender", "birthyr", "race")
socio_economic_var = c("educ", "employ", "faminc_new")
regional_var = c("region", "urbancity", "inputstate")

#### Download data ####
raw_data <-
  get_dataframe_by_name(
    filename = "CES20_Common_OUTPUT_vv.csv",
    dataset = "10.7910/DVN/E9N6PH",
    server = "dataverse.harvard.edu",
    .f = read_csv
  ) |>
  select(
    all_of(voting_var),
    all_of(identity_based_var),
    all_of(socio_economic_var),
    all_of(regional_var)
  ) 

#### Save data ####
write_csv(raw_data, "data/raw_data/raw_data.csv")
         
