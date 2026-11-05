tidy_gender_age_data <- function(year) {
  raw_data_path <-
    str_glue("data-raw/{year}-obtn-by-county.xlsx")

  read_excel(
    path = raw_data_path,
    sheet = "Gender Age"
  ) |>
    clean_names() |>
    pivot_longer(
      cols = -geography,
      values_to = "pct"
    ) |>
    separate_wider_delim(
      cols = name,
      delim = "_",
      names = c("gender", "min_age", "max_age"),
      too_few = "align_start"
    ) |>
    mutate(age = str_glue("{min_age}-{max_age}")) |>
    mutate(age = str_replace(age, pattern = "-NA", "+")) |>
    select(geography, gender, age, pct) |>
    mutate(age = fct_inorder(age)) |>
    mutate(data_year = year)
}
