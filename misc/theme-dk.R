library(tidyverse)
library(ggtext)
library(palmerpenguins)


theme_dk <- function(show_gridlines = FALSE, show_axis_text = TRUE) {
  basic_theme <-
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      plot.title = element_markdown(),
      plot.title.position = "plot",
      panel.grid = element_blank(),
      axis.text = element_text(color = "grey60", size = 10)
    )

  if (show_gridlines == TRUE) {
    basic_theme <-
      basic_theme +
      theme(
        panel.grid.major = element_line(color = "grey80")
      )
  }
  
  if (show_axis_text == FALSE) {
    basic_theme <-
      basic_theme +
      theme(
        axis.text = element_blank()
      )
  }

  basic_theme
}

theme_dk_v2 <- function(plot_type) {
  basic_theme <-
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      plot.title = element_markdown(),
      plot.title.position = "plot",
      panel.grid = element_blank(),
      axis.text = element_text(color = "grey60", size = 10)
    )
  
  if (plot_type == "horizontal bar chart") {
    basic_theme <-
      basic_theme +
      theme(
        axis.text = element_blank()
      )
  }
  
  if (plot_type == "map") {
    basic_theme <-
      basic_theme +
      theme(
        axis.text = element_blank()
      )
  }

  
  basic_theme
}

# penguin_bar_chart <-
  penguins |>
  group_by(island) |>
  summarize(mean_bill_length = mean(bill_length_mm, na.rm = TRUE)) |>
  ggplot(
    aes(
      x = island,
      y = mean_bill_length,
      label = island,
      fill = island
    )
  ) +
  geom_col() +
    geom_text(size.unit = "pt",
              size = 12) +
  labs(title = "Biscoe penguins have the longest bills on average")

penguin_bar_chart

penguin_bar_chart +
  theme_dk(show_gridlines = TRUE)
