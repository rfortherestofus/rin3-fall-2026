# Load Packages ----------------------------------------------------------

library(tidyverse)

# Import Data ------------------------------------------------------------

penguins <- read_csv("data-raw/penguins.csv")

# The .by argument ------------------------------------------------------

penguins |>
  summarize(
    mean_bill_length = mean(bill_length_mm, na.rm = TRUE),
    .by = c(island, species)
  )

penguins |>
  group_by(island, species) |>
  summarize(
    mean_bill_length = mean(
      bill_length_mm,
      na.rm = TRUE
    )
  )


# Summary functions ------------------------------------------------------

penguins |>
  group_by(island, species) |>
  summarize(
    mean_bill_length = mean(bill_length_mm, na.rm = TRUE)
  )

mean(c(0, 1, 2))


# How to Import Excel Files ----------------------------------------------

library(readxl)

median_income <- read_excel(
  path = "data-raw/2019-obtn-by-county.xlsx",
  sheet = "Median Income"
)

# Projects vs Scripts ----------------------------------------------------

# Parentheses -------------------------------------------------------------

penguins |>
  select(-(species))

penguins |>
  select(-c(bill_length_mm:body_mass_g))

read_csv("data-raw/penguins_data.csv", na = c("-999", "-999.0"))

# select() issues ---------------------------------------------------------

penguins |>
  select(-island:year)

penguins |>
  select(c(-1, island:bill_length_mm))

# Does not remove the "species" variable but this does:

penguins |>
  select(island:year)

# NA values ---------------------------------------------------------------

read_csv("data-raw/penguins_data.csv", na = c("-999"))


# Rounding ----------------------------------------------------------------

penguins |>
  filter(island == "Biscoe") |>
  drop_na(body_mass_g, sex) |>
  group_by(sex) |>
  summarize(mean_body_mass = mean(body_mass_g)) |>
  mutate(mean_body_mass = round(mean_body_mass, digits = 0)) |>
  view()

library(scales)

penguins |>
  filter(island == "Biscoe") |>
  drop_na(body_mass_g, sex) |>
  group_by(sex) |>
  summarize(mean_body_mass = mean(body_mass_g)) |>
  mutate(mean_body_mass = comma(mean_body_mass, accuracy = 0.1))
