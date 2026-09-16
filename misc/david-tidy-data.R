# https://shannonpileggi.github.io/rmedicine-data-cleaning-2023/exercises.html

library(tidyverse)
library(readxl)
library(janitor)

uc_data_raw <-
  read_excel(
    "data-raw/messy_uc.xlsx",
    sheet = "Data",
    skip = 6,
    na = c("-99", "not done")
  ) |>
  clean_names()

demographics <-
  uc_data_raw |>
  mutate(ethnic = str_to_title(ethnic)) |>
  mutate(
    ethnic = case_match(
      ethnic,
      "Hispamnic" ~ "Hispanic",
      .default = ethnic
    )
  ) |>
  select(pat_id:race, address)

weight <-
  uc_data_raw |>
  separate_wider_delim(
    pre_post_wt_kg,
    delim = "/",
    names = c("pre_weight", "post_weight")
  ) |>
  select(pat_id, pre_weight, post_weight) |>
  pivot_longer(
    cols = c(pre_weight, post_weight),
    names_to = "timing",
    values_to = "weight"
  ) |>
  mutate(timing = str_remove(timing, "_weight"))

scores <-
  uc_data_raw |>
  select(pat_id, start_mes:start_emo, end_mes:end_emo) |>
  pivot_longer(
    cols = -pat_id
  ) |>
  mutate(name = str_remove(name, "_score")) |>
  separate_wider_delim(
    cols = name,
    delim = "_",
    names = c("timing", "measure")
  )


# Plots -------------------------------------------------------------------

weight |>
  left_join(
    demographics,
    join_by(pat_id)
  ) |>
  ggplot(
    aes(
      x = weight,
      y = pat_id,
      color = timing
    )
  ) +
  geom_point() +
  facet_wrap(vars(ethnic)) +
  theme_minimal()

scores |>
  filter(measure != "mes") |>
  ggplot(
    aes(
      x = value,
      y = pat_id,
      color = timing
    )
  ) +
  geom_point() +
  facet_wrap(vars(measure)) +
  theme_minimal()
