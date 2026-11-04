library(tidyverse)

life_expectancy_over_time <-
  gapminder::gapminder |>
  mutate(country = as.character(country)) |>
  select(country, year, lifeExp) |>
  pivot_wider(
    id_cols = country,
    names_from = year,
    values_from = lifeExp
  ) |>
  slice(1:10)

life_expectancy_over_time

ggplot(
  data = life_expectancy_over_time,
  aes(
    x =
  )
)

life_expectancy_over_time_tidy <-
  life_expectancy_over_time |>
  pivot_longer(
    cols = -country,
    names_to = "year",
    values_to = "life_expectancy"
  )

life_expectancy_over_time_tidy

ggplot(
  data = life_expectancy_over_time_tidy,
  aes(
    x = year,
    y = life_expectancy
  )
) +
  geom_col() +
  facet_wrap(vars(country))


life_expectancy_over_time |>
  pivot_longer(
    cols = -country,
    names_to = "year",
    values_to = "life_expectancy"
  ) |>
  group_by(country) |>
  summarize(avg_life_expectancy = mean(life_expectancy)) |>
  pivot_wider(
    id_cols = country,
    names_from = avg_life_expectancy,
    values_from = avg_life_expectancy
  )
