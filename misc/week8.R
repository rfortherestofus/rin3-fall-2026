# Load packages ----------------------------------------------------------

library(tidyverse)
library(readxl)
library(janitor)

# as.numeric() vs parse_number() -----------------------------------------

names_and_ages <-
  tribble(
    ~name                        ,
    ~age                         ,
    "David"                      ,
    "46"                         ,
    "Rachel"                     ,
    "46"                         ,
    "Leila"                      ,
    "9"                          ,
    "Elias"                      ,
    "9 years old (born in 2016)" ,
    "Diego"                      ,
    "No longer alive"
  )

names_and_ages

names_and_ages |>
  mutate(age_as_numeric = as.numeric(age))

names_and_ages |>
  mutate(age_parse_number = parse_number(age))

addresses <-
  tribble(
    ~address                                   ,
    "690 Omar Circle Yellow Springs, OH 45387"
  )

addresses |>
  mutate(zip_code = as.numeric(address))

addresses |>
  mutate(zip_code = parse_number(address))

addresses

# Pull out the five digits at the end of the address
addresses |>
  mutate(zip_code = str_extract(address, "\\d{5}$"))

tibble(
  name = c("David", "Rachel"),
  hometown = c("Philadelphia", "Seattle")
)

tribble(
  ~name    , ~hometown      ,
  "Rachel" , "Seattle"      ,
  "David"  , "Philadelphia"
)

# Why save as RDS vs CSV? ------------------------------------------------

# Read in the raw survey data

survey_responses <-
  read_csv("data-raw/survey_responses.csv")

# response is character (<chr>)
survey_responses

survey_responses |>
  count(response)

# Bars are in alphabetical order
survey_responses |>
  count(response) |>
  ggplot(aes(x = response, y = n)) +
  geom_col()

# Convert response to a factor

survey_responses_clean <-
  survey_responses |>
  mutate(
    response = fct(
      response,
      levels = c(
        "Strongly disagree",
        "Disagree",
        "Neutral",
        "Agree",
        "Strongly agree"
      )
    )
  )

# response is now a factor (<fct>)
survey_responses_clean

# Bars go from Strongly disagree to Strongly agree
survey_responses_clean |>
  count(response) |>
  ggplot(aes(x = response, y = n)) +
  geom_col()

# Save as CSV and read it back in

survey_responses_clean |>
  write_csv("data/survey_responses_clean.csv")

survey_responses_clean_csv <-
  read_csv("data/survey_responses_clean.csv")

# response is character (<chr>) again: the factor levels are gone
survey_responses_clean_csv

# Bars are back in alphabetical order
survey_responses_clean_csv |>
  count(response) |>
  ggplot(aes(x = response, y = n)) +
  geom_col()

# Save as RDS and read it back in

survey_responses_clean |>
  write_rds("data/survey_responses_clean.rds")

survey_responses_clean_rds <-
  read_rds("data/survey_responses_clean.rds")

# response is still a factor, with the levels in our order
survey_responses_clean_rds

# Bars are in the right order
survey_responses_clean_rds |>
  count(response) |>
  ggplot(aes(x = response, y = n)) +
  geom_col()

# Joins with mismatched variable types ------------------------------------

# ZIP codes stored as text, with leading zeros
zip_populations <-
  tribble(
    ~zip_code , ~population ,
    "02134"   ,       21000 ,
    "06511"   ,       33000 ,
    "10001"   ,       27000 ,
    "97201"   ,       16000
  )

zip_populations

# ZIP codes stored as numbers (leading zeros dropped)
clinics <-
  tribble(
    ~clinic                , ~zip_code ,
    "Riverside Health"     ,      2134 ,
    "Elm Street Clinic"    ,      6511 ,
    "Midtown Medical"      ,     10001 ,
    "Westside Family Care" ,     97201
  )

clinics

# Error: zip_code is <dbl> in one and <chr> in the other
left_join(
  clinics,
  zip_populations,
  join_by(zip_code)
)

# Convert to character: the join runs, but two clinics get NA
# because "2134" doesn't match "02134"
clinics |>
  mutate(zip_code = as.character(zip_code)) |>
  left_join(
    zip_populations,
    join_by(zip_code)
  )

# Pad with leading zeros so every ZIP code has five digits
clinics |>
  mutate(zip_code = str_pad(zip_code, width = 5, pad = "0")) |>
  left_join(
    zip_populations,
    join_by(zip_code)
  )

# Many-to-many joins ------------------------------------------------------

# Each county appears once per year
county_population <-
  tribble(
    ~county     , ~year , ~population ,
    "Deschutes" ,  2023 ,      208000 ,
    "Deschutes" ,  2024 ,      211000 ,
    "Lane"      ,  2023 ,      384000 ,
    "Lane"      ,  2024 ,      386000 ,
    "Multnomah" ,  2023 ,      795000 ,
    "Multnomah" ,  2024 ,      790000
  )

county_population

county_graduation <-
  tribble(
    ~county     , ~year , ~graduation_rate ,
    "Deschutes" ,  2023 , 0.84             ,
    "Deschutes" ,  2024 , 0.86             ,
    "Lane"      ,  2023 , 0.79             ,
    "Lane"      ,  2024 , 0.81             ,
    "Multnomah" ,  2023 , 0.78             ,
    "Multnomah" ,  2024 , 0.80
  )

county_graduation

# Join by county only: each row matches two rows (one per year)
# We get a warning, 12 rows instead of 6, and year.x and year.y columns
# where 2023 population gets paired with 2024 graduation rate
left_join(
  county_population,
  county_graduation,
  join_by(county)
)

# Join by county and year: each row matches exactly one row
left_join(
  county_population,
  county_graduation,
  join_by(county, year)
)

# Iteration --------------------------------------------------------------

# Import multiple years of data

tidy_gender_age_data <- function(year) {
  raw_data_path <-
    str_glue("data-raw/{year}-obtn-by-county.xlsx")

  read_excel(
    path = raw_data_path,
    sheet = "Gender Age"
  ) |>
    clean_names() |>
    pivot_longer(
      cols = -geography,
      values_to = "pct"
    ) |>
    separate_wider_delim(
      cols = name,
      delim = "_",
      names = c("gender", "min_age", "max_age"),
      too_few = "align_start"
    ) |>
    mutate(age = str_glue("{min_age}-{max_age}")) |>
    mutate(age = str_replace(age, pattern = "-NA", "+")) |>
    select(geography, gender, age, pct) |>
    mutate(age = fct_inorder(age)) |>
    mutate(data_year = year)
}

tidy_gender_age_data(2020)

obtn_gender_age_2019 <-
  tidy_gender_age_data(year = 2019)

obtn_gender_age_2020 <-
  tidy_gender_age_data(year = 2020)

bind_rows(obtn_gender_age_2019, obtn_gender_age_2020) |>
  view()

years <- seq(from = 2019, to = 2023, by = 1)

gender_age <-
  map(years, tidy_gender_age_data) |>
  bind_rows()

# Make multiple plots

gender_age_plot <- function(
  year_to_filter,
  gender_to_filter,
  geography_to_filter
) {
  gender_age |>
    filter(data_year == year_to_filter) |>
    filter(gender == gender_to_filter) |>
    filter(geography == geography_to_filter) |>
    ggplot(
      aes(
        x = pct,
        y = age
      )
    ) +
    geom_col()
}
gender_age_plot(
  year_to_filter = 2021,
  gender_to_filter = "males",
  geography_to_filter = "Multnomah"
)
