library(tidyverse)

penguins <-
  read_csv("data-raw/penguins.csv") |>
  filter(species == "Gentoo")
