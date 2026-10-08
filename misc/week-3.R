# Load Packages -----------------------------------------------------------

library(tidyverse)

# Import Data -------------------------------------------------------------

penguins <- read_csv("data-raw/penguins.csv")

penguins_bill_length_by_island <-
  penguins |>
  group_by(island) |>
  summarize(mean_bill_length = mean(bill_length_mm, na.rm = TRUE))

penguins_by_species <-
  penguins |>
  count(species)


# `==` and lowercase `x` and `y` ------------------------------------------

ggplot(
  data = penguins, # why can't I use == within this function?
  mapping = aes(
    x = flipper_length_mm,
    y = body_mass_g
  )
) +
  geom_point()


# Color vs Fill ----------------------------------------------------------

ggplot(
  data = penguins,
  mapping = aes(
    x = flipper_length_mm,
    y = body_mass_g,
    color = island
  )
) +
  geom_point()


ggplot(
  data = penguins_by_species,
  mapping = aes(
    x = species,
    y = n,
    fill = species
  )
) +
  geom_col()

ggplot(
  penguins,
  aes(
    x = flipper_length_mm,
    y = body_mass_g,
    fill = island,
    color = species
  )
) +
  geom_point(
    shape = 21
  )

ggplot(
  penguins,
  aes(
    x = bill_length_mm,
    y = bill_depth_mm
  )
) +
  geom_point(aes(color = island), fill = "black", shape = 21)


# `geom_bar()` vs `geom_col()` -------------------------------------------

penguins_by_species <-
  penguins |>
  count(species)

ggplot(
  data = penguins,
  mapping = aes(x = species)
) +
  geom_bar()

ggplot(
  data = penguins_by_species,
  mapping = aes(
    x = species,
    y = n
  )
) +
  geom_col()


# Ensuring legible labels ------------------------------------------------

ggplot(
  data = penguins_bill_length_by_island,
  aes(
    y = island,
    x = mean_bill_length,
    label = mean_bill_length,
    fill = island
  )
) +
  geom_col() +
  geom_text(hjust = 1.1, size = 10) +
  theme_minimal()

# Bar Chart Width ---------------------------------------------------------

ggplot(
  data = penguins_bill_length_by_island,
  aes(
    x = island,
    y = mean_bill_length,
    fill = island
  )
) +
  geom_col(width = 1) +
  theme_minimal()

# Reordering Bar Charts ---------------------------------------------------

penguins_by_species_arrange <-
  penguins_by_species |>
  arrange(desc(species))

ggplot(
  data = penguins_by_species_arrange,
  mapping = aes(
    x = species,
    y = n,
    fill = species
  )
) +
  geom_col()

ggplot(
  data = penguins_by_species,
  mapping = aes(
    x = reorder(species, n, decreasing = TRUE),
    y = n,
    fill = species
  )
) +
  geom_col()

penguins_by_species_factor <-
  penguins_by_species |>
  mutate(species = fct(species, levels = c("Chinstrap", "Adelie", "Gentoo")))

ggplot(
  data = penguins_by_species_factor,
  mapping = aes(
    x = species,
    y = n,
    fill = species
  )
) +
  geom_col()


# Hiding legend ----------------------------------------------------------

penguins_by_species |>
  ggplot(
    mapping = aes(
      x = species,
      y = n,
      fill = species
    )
  ) +
  geom_col() +
  theme_minimal() +
  theme(legend.position = "none")


# Wrapping Long Text ------------------------------------------------------

library(gapminder)

data("gapminder")

gapminder

gapminder_afghanistan <-
  gapminder |>
  filter(country == "Afghanistan")

ggplot(
  data = gapminder_afghanistan,
  aes(
    x = year,
    y = lifeExp,
    group = country
  )
) +
  geom_line()

ggplot(
  data = gapminder,
  aes(
    x = year,
    y = lifeExp,
    group = country
  )
) +
  geom_line() +
  facet_wrap(vars(country))

gapminder_wrapped <-
  gapminder |>
  mutate(country_wrapped = str_wrap(country, width = 10))

ggplot(
  data = gapminder_wrapped,
  aes(
    x = year,
    y = lifeExp
  )
) +
  geom_line() +
  facet_wrap(vars(country_wrapped))

ggsave("plots/gapminder-wrapped.png")
