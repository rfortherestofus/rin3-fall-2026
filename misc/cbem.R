#  Recreating this: https://show.rfor.us/cditiy

# Load Packages -----------------------------------------------------------

library(tidyverse)
library(ggchicklet)
library(scales)

# Import Data ------------------------------------------------------------

cbem <- read_csv("data-raw/cbem.csv")

# Plot -------------------------------------------------------------------

cbem_plot <- function(location_to_filter, age_group_to_filter) {
  state_rate <-
    cbem |>
    filter(location == location_to_filter) |>
    filter(group == "All Persons") |>
    filter(age_group == age_group_to_filter) |>
    pull(percent)

  cbem |>
    filter(location == location_to_filter) |>
    filter(group != "All Persons") |>
    filter(age_group == age_group_to_filter) |>
    mutate(
      group = fct(
        group,
        levels = c(
          "American Indian or Alaska Native",
          "Asian or Pacific Islander",
          "Black or African American",
          "White",
          "Hispanic or Latino"
        )
      )
    ) |>
    mutate(
      x_position = case_when(
        group == "American Indian or Alaska Native" ~ 1,
        group == "Asian or Pacific Islander" ~ 2,
        group == "Black or African American" ~ 3,
        group == "White" ~ 4,
        group == "Hispanic or Latino" ~ 5.5
      )
    ) |>
    mutate(percent_formatted = percent(percent)) |>
    ggplot(
      aes(
        x = x_position,
        y = percent,
        fill = group,
        label = percent_formatted
      )
    ) +
    geom_hline(
      yintercept = state_rate,
      linetype = "dashed",
      color = "grey40"
    ) +
    geom_chicklet() +
    geom_text(
      color = "white",
      vjust = 1.5
    ) +
    annotate(
      geom = "text",
      x = 5.5,
      y = state_rate,
      vjust = -1,
      color = "grey40",
      label = str_glue("CBEM State Rate\n{percent(state_rate)}")
    ) +
    scale_fill_manual(
      values = c(
        "American Indian or Alaska Native" = "#9CC892",
        "Asian or Pacific Islander" = "#0066cc",
        "Black or African American" = "#477A3E",
        "White" = "#6CC5E9",
        "Hispanic or Latino" = "#ff7400"
      )
    ) +
    theme_void() +
    theme(legend.position = "none")
}

cbem_plot(
  location_to_filter = "Alaska",
  age_group_to_filter = "Under 25"
)
