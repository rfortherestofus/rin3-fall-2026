library(tidyverse)
library(readxl)
library(janitor)

source("R/functions.R")

obtn_gender_age_2019 <-
  tidy_gender_age_data(year = 2019)

obtn_gender_age_2020 <-
  tidy_gender_age_data(year = 2020)

tidy_gender_age_data(year = 2020)

years <- seq(from = 2019, to = 2023, by = 1)

map(years, tidy_gender_age_data) |>
  bind_rows()
