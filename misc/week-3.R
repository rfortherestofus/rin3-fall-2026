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
    x = flipper_length_mm, # must be lowercase x and y
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
  geom_point(shape = 11)


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
    fill = island
  )
) +
  geom_point(
    shape = 21,
    color = "white",
    alpha = 0.75
  )


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

# Bar Chart Width ---------------------------------------------------------

ggplot(
  data = penguins_bill_length_by_island,
  aes(
    x = island,
    y = mean_bill_length,
    label = mean_bill_length,
    fill = island
  )
) +
  geom_col() +
  theme_minimal()

# Reordering Bar Charts ---------------------------------------------------

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
  data = penguins_by_species,
  mapping = aes(
    x = reorder(species, n, decreasing = TRUE),
    y = n,
    fill = species
  )
) +
  geom_col() +
  labs(x = NULL)

penguins_by_species_reordered <-
  penguins_by_species |>
  mutate(species = fct(species, levels = c("Adelie", "Chinstrap", "Gentoo")))

penguins_by_species_reordered

# mutate(species = fct(species, levels = c("Adelie", "Chinstrap", "Gentoo")))
# mutate(species = fct_reorder(species, n)) |>
# mutate(species = fct_rev(species))

ggplot(
  data = penguins_by_species_reordered,
  mapping = aes(
    x = species,
    y = n,
    fill = species
  )
) +
  geom_col()


# Wrapping Long Text ------------------------------------------------------

library(gapminder)

data("gapminder")

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

gapminder |>
  mutate(country_wrapped = str_wrap(country, width = 10)) |>
  ggplot(
    aes(
      x = year,
      y = lifeExp
    )
  ) +
  geom_line() +
  facet_wrap(vars(country_wrapped)) +
  theme(axis.text.x = element_blank())
