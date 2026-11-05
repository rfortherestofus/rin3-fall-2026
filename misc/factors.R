library(tidyverse)

gss_cat |>
  count(race)

gss_cat_race <-
  gss_cat |>
  count(race)

gss_cat_race |>
  pull(race)

ggplot(
  data = gss_cat_race,
  aes(
    x = race,
    y = n
  )
) +
  geom_col()

gss_cat_character <-
  gss_cat |>
  mutate(across(where(is.factor), as.character))

gss_cat_character_race <-
  gss_cat_character |>
  count(race)


ggplot(
  data = gss_cat_character_race,
  aes(
    x = race,
    y = n
  )
) +
  geom_col()
