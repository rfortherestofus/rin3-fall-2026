#  Recreating this: https://show.rfor.us/cditiy

library(tidyverse)
library(ggchicklet)
library(scales)

cbem <-
  read_csv("https://rin3spring2026.rfortherestofus.com/data-raw/cbem.csv")

cbem_plot <- function(filter_state, filter_age_group) {
  cbem_filtered <-
    cbem |>
    filter(location == filter_state) |>
    filter(age_group == filter_age_group) |>
    filter(group != "All Persons") |>
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
    mutate(x_position = row_number()) |>
    mutate(
      x_position = case_when(
        x_position == max(x_position) ~ x_position + 0.5,
        .default = x_position
      )
    ) |>
    mutate(percent_formatted = percent(percent))

  state_average <-
    cbem |>
    filter(location == filter_state) |>
    filter(age_group == filter_age_group) |>
    filter(group == "All Persons") |>
    pull(percent)

  state_average_formatted <-
    str_glue("CBEM State Rate\n{percent(state_average, accuracy = 0.1)}")

  cbem_filtered |>
    ggplot(
      aes(
        x = x_position,
        y = percent,
        fill = group,
        label = percent_formatted
      )
    ) +
    geom_hline(
      yintercept = state_average,
      color = "grey40",
      linetype = "dashed"
    ) +
    geom_chicklet() +
    geom_text(
      vjust = 1.5,
      color = "white"
    ) +
    annotate(
      geom = "text",
      x = 5.5,
      y = state_average,
      label = state_average_formatted,
      vjust = -.75,
      color = "grey40"
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
  filter_state = "New Hampshire",
  filter_age_group = "Under 18"
)
