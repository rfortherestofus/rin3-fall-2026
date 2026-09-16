# Functions that take variables as arguments ------------------------------

library(tidyverse)

penguins <- read_csv("data-raw/penguins.csv")

filter_by_year <- function(year_to_filter) {
  penguins |>
    filter(year == year_to_filter)
}

filter_by_year(year_to_filter = 2009)

calculate_mean <- function(variable) {
  penguins |>
    summarize(avg = mean({{ variable }}, na.rm = TRUE))
}

calculate_mean(bill_depth_mm)


# Colors ------------------------------------------------------------------

penguins_bill_length_by_island <-
  penguins |>
  group_by(island) |>
  summarize(mean_bill_length = mean(bill_length_mm, na.rm = TRUE))

ggplot(
  data = penguins_bill_length_by_island,
  aes(
    x = island,
    y = mean_bill_length,
    fill = island,
    label = island
  )
) +
  geom_col() +
  geom_text(vjust = -1) +
  theme_minimal(base_family = "Georgia")


scale_fill_custom <- function() {
  scale_fill_manual(values = c(
    "#1f77b4",
    "#ff7f0e",
    "#2ca02c",
    "#d62728",
    "#9467bd",
    "#8c564b",
    "#e377c2"
  ))
}

ggplot(
  data = penguins_bill_length_by_island,
  aes(
    x = island,
    y = mean_bill_length,
    fill = island,
    label = island
  )
) +
  geom_col() +
  geom_text(vjust = -1) +
  # theme_minimal(base_family = "Georgia") +
  omni::theme_omni() +
  omni::scale_fill_omni_discrete() 
