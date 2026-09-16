library(tidyverse)
library(palmerpenguins)

# Theme -------------------------------------------------------------------

theme_dk <- function(hide_gridlines = TRUE, hide_legend = TRUE) {
  my_theme <-
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_text(
        color = "grey60",
        size = 18
      )
    )

  if (hide_gridlines == TRUE) {
    my_theme <-
      my_theme +
      theme(panel.grid = element_blank())
  }

  if (hide_legend == TRUE) {
    my_theme <-
      my_theme +
      theme(legend.position = "none")
  }

  my_theme
}

library(omni)


# Colors -----------------------------------------------------------------

scale_color_rru <- function(number_of_colors) {
  if (number_of_colors == 2) {
    my_brand_colors <-
      c(
        "#6cabdd",
        "#ff7400"
      )
  }

  if (number_of_colors == 3) {
    my_brand_colors <-
      c(
        "#6cabdd",
        "#ff7400",
        "darkgreen"
      )
  }

  scale_color_manual(
    values = my_brand_colors
  )
}

theme_set(theme_dk())

penguins |>
  filter(island != "Dream") |>
  ggplot(
    aes(
      x = bill_length_mm,
      y = bill_depth_mm,
      color = island
    )
  ) +
  geom_point() +
  scale_color_rru(number_of_colors = 2) +
  theme_dk(hide_gridlines = FALSE)


# Plots -------------------------------------------------------------------

ggplot(
  data = penguins,
  aes(
    x = bill_length_mm,
    y = bill_depth_mm
  )
) +
  geom_point() +
  theme_dk(hide_gridlines = FALSE)

ggplot(
  data = penguins,
  aes(
    x = bill_length_mm,
    y = bill_depth_mm,
    color = island
  )
) +
  geom_point() +
  scale_color_manual(
    values = c(
      omni_colors("orange-red-600"),
      omni_colors("teal-200"),
      omni_colors("golden-yellow-400")
    )
  ) +
  theme_dk(hide_legend = FALSE) +
  theme(axis.title = element_text())

source("/users/davidkeyes/Desktop/functions.R")
