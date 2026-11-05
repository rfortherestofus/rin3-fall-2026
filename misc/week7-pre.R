# Function Arguments ------------------------------------------------------

library(tidyverse)

penguins <- read_csv("data-raw/penguins.csv")

ggplot(
  data = penguins,
  mapping = aes(
    x = bill_length_mm,
    y = bill_depth_mm
  )
) +
  geom_point()

ggplot(
  penguins,
  aes(
    bill_length_mm,
    bill_depth_mm
  )
) +
  geom_point()


# Read OBTN data ---------------------------------------------------------

library(readxl)
library(janitor)

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
    mutate(age = fct_inorder(age))
}

tidy_gender_age_data(year = 2021)

obtn_gender_age_2019 <-
  read_excel(
    path = "data-raw/2019-obtn-by-county.xlsx",
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
  mutate(age_category = str_glue("{min_age}-{max_age}")) |>
  mutate(age_category = str_replace(age_category, pattern = "-NA", "+")) |>
  select(geography, gender, age_category, pct) |>
  mutate(age_category = fct_inorder(age_category))


obtn_gender_age_2020 <-
  read_excel(
    path = "data-raw/2020-obtn-by-county.xlsx",
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
  mutate(age_category = str_replace(age_category, pattern = "-NA", "+")) |>
  select(geography, gender, age_category, pct) |>
  mutate(age_category = fct_inorder(age_category))


# Show in Excel ----------------------------------------------------------

library(tidyverse)
library(fs)

penguins <- read_csv("https://data.rfortherestofus.com/penguins-2007.csv")

show_in_excel_penguins <- function() {
  write_csv(
    x = penguins,
    file = "my-data.csv",
    na = ""
  )

  file_show(path = "my-data.csv")
}

show_in_excel_penguins()

show_in_excel <- function(data) {
  write_csv(
    x = data,
    file = "my-data.csv",
    na = ""
  )

  file_show(path = "my-data.csv")
}


# ACS Data ---------------------------------------------------------------

library(tidycensus)
library(janitor)
library(tidyverse)

race_ethnicity_data <-
  get_acs(
    geography = "state",
    variables = c(
      "B03002_003",
      "B03002_004",
      "B03002_005",
      "B03002_006",
      "B03002_007",
      "B03002_008",
      "B03002_009",
      "B03002_012"
    )
  )

race_ethnicity_data

# Basic function

get_acs_race_ethnicity <- function() {
  race_ethnicity_data <-
    get_acs(
      geography = "state",
      variables = c(
        "B03002_003",
        "B03002_004",
        "B03002_005",
        "B03002_006",
        "B03002_007",
        "B03002_008",
        "B03002_009",
        "B03002_012"
      )
    )

  race_ethnicity_data
}

get_acs_race_ethnicity()

# Change variable value text

get_acs_race_ethnicity <- function() {
  race_ethnicity_data <-
    get_acs(
      geography = "state",
      variables = c(
        "White" = "B03002_003",
        "Black/African American" = "B03002_004",
        "American Indian/Alaska Native" = "B03002_005",
        "Asian" = "B03002_006",
        "Native Hawaiian/Pacific Islander" = "B03002_007",
        "Other race" = "B03002_008",
        "Multi-Race" = "B03002_009",
        "Hispanic/Latino" = "B03002_012"
      )
    )

  race_ethnicity_data
}

get_acs_race_ethnicity()

# Add argument for clean_variable_name

get_acs_race_ethnicity <- function(clean_variable_names) {
  race_ethnicity_data <-
    get_acs(
      geography = "state",
      variables = c(
        "White" = "B03002_003",
        "Black/African American" = "B03002_004",
        "American Indian/Alaska Native" = "B03002_005",
        "Asian" = "B03002_006",
        "Native Hawaiian/Pacific Islander" = "B03002_007",
        "Other race" = "B03002_008",
        "Multi-Race" = "B03002_009",
        "Hispanic/Latino" = "B03002_012"
      )
    )
}

get_acs_race_ethnicity(clean_variable_names = TRUE)

# Show ... to add arguments to existing function

# Show mapping across multiple years
