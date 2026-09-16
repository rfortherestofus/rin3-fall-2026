library(tidyverse)
library(readxl)

# Tidy Data Rule #1: Every Column is a Variable

data(billboard)

billboard |> 
  pivot_longer(
    cols = contains("wk"),
    names_to = "week",
    values_to = "ranking"
  ) |> 
  mutate(week = parse_number(week)) |> 
  filter(track == "Baby Don't Cry (Keep...") |> 
  ggplot(
    aes(
      x = wk,
      y = ranking
    )
  ) +
  geom_line()

# Tidy Data Rule #3: Every Cell is a Single Value

addresses <-
  read_csv("data-raw/addresses.csv")

addresses |> 
  separate_wider_delim(
    Address,
    delim = ", ",
    names = c("city", "state")
  ) |> 
  filter(state == "Illinois")


# Tidy Data Rule #2: Every Row is an Observation

survey_data <-
  read_csv("data-raw/survey_data.csv")

survey_data

demographics <-
  survey_data |> 
  select(respondent_id, location) |> 
  separate_wider_delim(
    cols = location,
    delim = ", ",
    names = c("city", "state")
  )

pre_post_questions <- 
  survey_data |> 
  select(respondent_id, pre_question_1:post_question_2) |> 
  pivot_longer(
    cols = -respondent_id,
    values_to = "response"
  ) |> 
  separate_wider_delim(
    cols = name,
    delim = "_",
    names = c("timing", "question", "question_number")
  ) |> 
  select(-question)

pre_post_questions |> 
  left_join(demographics,
            join_by(respondent_id)) |> 
  # filter(city == "Portland") |> 
  group_by(timing, question_number, city) |> 
  summarize(avg_response = mean(response))

demographics |> 
  right_join(
    pre_post_questions,
    join_by(respondent_id)
  )

right_join(demographics, pre_post_questions)

favorite_parts <- 
  survey_data |> 
  select(respondent_id, favorite_parts) |> 
  separate_longer_delim(
    cols = favorite_parts,
    delim = ", "
  )

favorite_parts |> 
  count(favorite_parts)


# Survey Monkey data

read_xlsx("data-raw/survey-monkey-data.xlsx")
