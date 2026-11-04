library(tidyverse)

soccer_training_data <-
  read_csv(
    "data-raw/soccer-training-data.csv",
    na = c("No response", "Skipped", "Prefer not to say", "N/A")
  )

demographics <-
  soccer_training_data |>
  select(respondent_id, location) |>
  separate_wider_delim(
    cols = location,
    delim = ", ",
    names = c("city", "state")
  )

skill_assessment <-
  soccer_training_data |>
  select(respondent_id, pre_dribbling:post_passing) |>
  pivot_longer(
    cols = -respondent_id,
    values_to = "response"
  ) |>
  separate_wider_delim(
    cols = name,
    delim = "_",
    names = c("timing", "skill")
  )

satisfaction <-
  soccer_training_data |>
  select(respondent_id, contains("satisfaction"))

favorite_parts <-
  soccer_training_data |>
  select(respondent_id, favorite_parts) |>
  separate_longer_delim(
    cols = favorite_parts,
    delim = ", "
  )


# Analysis ---------------------------------------------------------------

favorite_parts |>
  count(favorite_parts)

skill_assessment |>
  left_join(demographics, join_by(respondent_id)) |>
  group_by(city, timing, skill) |>
  summarize(avg_response = mean(response, na.rm = TRUE)) |>
  ungroup() |>
  filter(timing == "post") |>
  pivot_wider(
    id_cols = city,
    names_from = skill,
    values_from = avg_response
  )
