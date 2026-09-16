
# Load packages -----------------------------------------------------------

library(tidyverse)


# Create dataset ----------------------------------------------------------

raw_data <- data.frame(
  id = 1:3,
  info = c("name: John; age: 30", "name: Jane; age: 25", "name: Doe; age: 40"),
  scores_math = c("score1: 90, score2: 85", "score1: 95, score2: 80", "score1: 88, score2: 92"),
  scores_science = c("score1: 85, score2: 80", "score1: 90, score2: 95", "score1: 78, score2: 88")
)


# Tidy process ------------------------------------------------------------

raw_data |> 
  # Separate the 'info' column into 'name' and 'age'
  separate(info, into = c("name", "age"), sep = "; ") |> 
  mutate(
    name = str_remove(name, "name: "),
    age = as.integer(str_remove(age, "age: "))
  ) |> 
  
  # Pivot the score columns longer
  pivot_longer(cols = starts_with("scores_"), 
               names_to = "subject", 
               values_to = "scores") |> 
  mutate(
    subject = str_remove(subject, "scores_")
  ) |> 
  
  # Separate the 'scores' column into multiple rows and columns
  separate_rows(scores, 
                sep = ", ") |> 
  separate_wider_delim(scores, 
                       delim = ": ", 
                       names = c("score_type", "score_value")) 