library(tidyverse)

gss_cat_character <-
  gss_cat |>
  mutate(across(where(is.factor), as.character))

gss_cat
