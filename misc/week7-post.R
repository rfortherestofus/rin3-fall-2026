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

# Functions as Recipes ----------------------------------------------------

# https://joyfoodsunshine.com/the-most-amazing-chocolate-chip-cookies/#wprm-recipe-container-8678

library(tidyverse)

calculate_cookie_ingredients <- function(number_of_cookies) {
  
  cookie_ingredients <- read_csv("data-raw/cookie-ingredients.csv")
  
  default_number_of_cookies <- 36
  cookie_ratio <- number_of_cookies / 36
  
  cookie_ingredients |> 
    mutate(amount = amount * cookie_ratio)
  
}

calculate_cookie_ingredients(number_of_cookies = 1)

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

cookie_ingredients_1000 <- calculate_cookie_ingredients(number_of_cookies = 1000)

show_in_excel(data = cookie_ingredients_1000)

calculate_cookie_ingredients(number_of_cookies = 1000000) |> 
  show_in_excel()

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

get_acs_race_ethnicity() |> 
  group_by(NAME) |> 
  summarize(total_population = sum(estimate))

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
  
  if (clean_variable_names == TRUE) {
    race_ethnicity_data <- 
    race_ethnicity_data |> 
      clean_names() |> 
      rename(
        state = name
      )
  }
  
  race_ethnicity_data
}

get_acs_race_ethnicity(clean_variable_names = TRUE)

# Show ... to add arguments to existing function

get_acs(
  geography = "county",
  year = 2022, 
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


get_acs_race_ethnicity <- function(clean_variable_names, ...) {
  race_ethnicity_data <-
    get_acs(
      variables = c(
        "White" = "B03002_003",
        "Black/African American" = "B03002_004",
        "American Indian/Alaska Native" = "B03002_005",
        "Asian" = "B03002_006",
        "Native Hawaiian/Pacific Islander" = "B03002_007",
        "Other race" = "B03002_008",
        "Multi-Race" = "B03002_009",
        "Hispanic/Latino" = "B03002_012"
      ),
      ...
    )
  
  if (clean_variable_names == TRUE) {
    race_ethnicity_data <- 
      race_ethnicity_data |> 
      clean_names() |> 
      rename(
        state = name
      )
  }
  
  race_ethnicity_data
}

race_ethnicity_county_2019 <- 
get_acs_race_ethnicity(clean_variable_names = TRUE,
                       geography = "county",
                       year = 2019)

race_ethnicity_county_2020 <- 
  get_acs_race_ethnicity(clean_variable_names = TRUE,
                         geography = "county",
                         year = 2020)



get_acs_race_ethnicity(clean_variable_names = TRUE,
                       geography = "county")

get_acs_race_ethnicity <- function(year_to_use) {
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
      ),
      year = year_to_use
    )
  
  race_ethnicity_data |> 
    mutate(year = year_to_use)
}

years <- c(seq(2019, 2023, 1))

race_ethnicity_multiple_years <- 
map(years, get_acs_race_ethnicity) |> 
  bind_rows()


