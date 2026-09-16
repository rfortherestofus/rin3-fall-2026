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

# Show in Excel ----------------------------------------------------------

library(tidyverse)
library(fs)

penguins <- read_csv("https://data.rfortherestofus.com/penguins-2007.csv")

function_name <- function() {
  # Whatever code you want to run
}

function_name()


show_in_excel_penguins <- function() {
  write_csv(
    x = penguins,
    file = "penguins.csv",
    na = ""
  )

  file_show(path = "penguins.csv")
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

cbem <- read_csv("data-raw/cbem.csv")

addresses <- read_csv("data-raw/addresses.csv")

show_in_excel(data = addresses)


# ACS Data ---------------------------------------------------------------

library(tidycensus)
library(janitor)
library(tidyverse)

# race_ethnicity_data <-
#   get_acs(
#     geography = "state",
#     variables = c(
#       "B03002_003",
#       "B03002_004",
#       "B03002_005",
#       "B03002_006",
#       "B03002_007",
#       "B03002_008",
#       "B03002_009",
#       "B03002_012"
#     )
#   )

# race_ethnicity_data

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

  return(race_ethnicity_data)
}

race_ethnicity_data <- get_acs_race_ethnicity()

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

  if (clean_variable_names == TRUE) {
    race_ethnicity_data <-
      race_ethnicity_data |>
      clean_names()
  }

  race_ethnicity_data
}

get_acs_race_ethnicity(clean_variable_names = FALSE)

# Show ... to add arguments to existing function

get_acs_race_ethnicity <- function(
  clean_variable_names,
  ...
) {
  race_ethnicity_data <-
    get_acs(
      ...,
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
      clean_names()
  }

  race_ethnicity_data
}

get_acs_race_ethnicity(
  clean_variable_names = TRUE,
  geography = "state",
  year = 2024
)
