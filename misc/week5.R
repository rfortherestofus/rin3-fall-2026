library(tidyverse)

penguins <-
  read_csv("data-raw/penguins.csv")

# Overview ----------------------------------------------------------------

glimpse(penguins)

# Count rows per species/island
penguins |> count(species)
penguins |> count(island)
penguins |> count(species, island)

# Missing values per column
penguins |>
  summarise(across(everything(), \(x) sum(is.na(x))))

# Numeric summary stats
penguins |>
  summarise(
    across(
      where(is.numeric),
      list(
        min = \(x) min(x, na.rm = TRUE),
        mean = \(x) mean(x, na.rm = TRUE),
        max = \(x) max(x, na.rm = TRUE)
      ),
      .names = "{.col}__{.fn}"
    )
  ) |>
  pivot_longer(
    everything(),
    names_to = c("variable", "stat"),
    names_sep = "__"
  ) |>
  pivot_wider(names_from = stat, values_from = value)

# Numeric summaries by species
penguins |>
  summarise(
    across(where(is.numeric), \(x) mean(x, na.rm = TRUE)),
    .by = species
  )
