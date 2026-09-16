library(tidyverse)
library(readxl)
library(janitor)

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

life_expectancy_over_time |>
  clean_names()

life_expectancy_over_time |>
  clean_names() |>
  pivot_longer(
    cols = -country,
    names_to = "year",
    values_to = "life_expectancy"
  )

favorite_sports <-
  tribble(
    ~name                          ,
    ~favorite_sport                ,
    "David"                        ,
    "Soccer, Basketball"           ,
    "Elias"                        ,
    "Baseball, Soccer, Skiing"     ,
    "Leila"                        ,
    "Aerial Dance, Roller Skating" ,
    "Rachel"                       ,
    "Soccer, Baseball"             ,
    "Scott"                        ,
    "Football"
  )

favorite_sports |>
  mutate(soccer = if_else(str_detect(favorite_sport, "Soccer"), 1, 0)) |>
  mutate(
    basketball = if_else(str_detect(favorite_sport, "Basketball"), 1, 0)
  ) |>
  mutate(
    football = if_else(str_detect(favorite_sport, "Football"), 1, 0)
  ) |>
  summarize(total_football_likes = sum(football))

favorite_sports |>
  separate_longer_delim(
    cols = favorite_sport,
    delim = ", "
  ) |>
  count(favorite_sport)


# Tidy Data Rule #1: Every Column is a Variable

data(billboard)

billboard_tidy <-
  billboard |>
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    values_to = "rank"
  ) |>
  mutate(week = parse_number(week))

billboard_tidy |>
  filter(track == "Baby Don't Cry (Keep...") |>
  ggplot(
    aes(
      x = week,
      y = rank
    )
  ) +
  geom_line()

# Tidy Data Rule #3: Every Cell is a Single Value

addresses <-
  read_csv("data-raw/addresses.csv")

addresses |>
  separate_wider_delim(
    cols = Address,
    delim = ", ",
    names = c("city", "state")
  ) |>
  filter(state == "Texas")


# Tidy Data Rule #2: Every Row is an Observation

survey_data <-
  read_csv("data-raw/survey_data.csv") |>
  separate_wider_delim(
    cols = location,
    delim = ", ",
    names = c("city", "state")
  )

demographics <-
  survey_data |>
  select(respondent_id, city, state)

pre_post_questions <-
  survey_data |>
  select(respondent_id, starts_with("pre"), starts_with("post")) |>
  pivot_longer(
    cols = -respondent_id,
    values_to = "response"
  ) |>
  separate_wider_delim(
    name,
    delim = "_",
    names = c("timing", "question", "question_number")
  ) |>
  select(-question) |>
  mutate(question_number = parse_number(question_number))

pre_post_questions |>
  group_by(timing, question_number) |>
  summarize(avg_response = mean(response))

pre_post_questions

demographics

left_join(pre_post_questions, demographics, join_by(respondent_id)) |>
  group_by(city, timing, question_number) |>
  summarize(avg_response = mean(response))

favorite_parts <-
  survey_data |>
  select(respondent_id, favorite_parts) |>
  separate_longer_delim(
    favorite_parts,
    delim = ", "
  )

favorite_parts |>
  left_join(demographics, join_by(respondent_id)) |>
  count(state, favorite_parts)

# Survey Monkey data

# read_xlsx("data-raw/survey-monkey-data.xlsx")
