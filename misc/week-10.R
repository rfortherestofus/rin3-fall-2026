# https://data.census.gov/table/ACSST1Y2024.S1501

library(tidyverse)
library(readxl)
library(janitor)
library(fs)

dir_create("data-clean")

raw <-
  read_excel(
    "data-raw/ACSST1Y2024.S1501-2026-05-12T025542.xlsx",
    sheet = "Data",
    range = "A4:M71",
    col_names = c(
      "label",
      "total_estimate",
      "total_moe",
      "percent_estimate",
      "percent_moe",
      "male_estimate",
      "male_moe",
      "percent_male_estimate",
      "percent_male_moe",
      "female_estimate",
      "female_moe",
      "percent_female_estimate",
      "percent_female_moe"
    ),
    na = c("(X)", "-", "**", "***", "*****", "N")
  )

sections <- c(
  "AGE BY EDUCATIONAL ATTAINMENT" = "Age",
  "RACE AND HISPANIC OR LATINO ORIGIN BY EDUCATIONAL ATTAINMENT" = "Race and Hispanic Origin",
  "POVERTY RATE FOR THE POPULATION 25 YEARS AND OVER FOR WHOM POVERTY STATUS IS DETERMINED BY EDUCATIONAL ATTAINMENT LEVEL" = "Poverty Rate",
  "MEDIAN EARNINGS IN THE PAST 12 MONTHS (IN 2024 INFLATION-ADJUSTED DOLLARS)" = "Median Earnings"
)

race_subgroups <- c(
  "White alone",
  "White alone, not Hispanic or Latino",
  "Black alone",
  "American Indian or Alaska Native alone",
  "Asian alone",
  "Native Hawaiian and Other Pacific Islander alone",
  "Some other race alone",
  "Two or more races",
  "Hispanic or Latino Origin"
)

with_hierarchy <-
  raw |>
  mutate(
    is_section = label %in% names(sections),
    section = if_else(is_section, unname(sections[label]), NA_character_)
  ) |>
  fill(section, .direction = "down") |>
  filter(!is_section) |>
  mutate(
    is_subgroup = str_starts(label, "Population ") | label %in% race_subgroups,
    subgroup = if_else(is_subgroup, label, NA_character_)
  ) |>
  group_by(section) |>
  fill(subgroup, .direction = "down") |>
  ungroup() |>
  mutate(education_level = if_else(is_subgroup, NA_character_, label))

# Age / Race / Median Earnings store the value of interest in the count columns;
# Poverty Rate stores it in the percent columns. Pull the right columns per
# section so the tidy frame has one `estimate` per (section, subgroup, ed, sex).
counts_and_dollars <-
  with_hierarchy |>
  filter(
    section %in% c("Age", "Race and Hispanic Origin", "Median Earnings")
  ) |>
  mutate(unit = if_else(section == "Median Earnings", "Dollars", "Count")) |>
  select(
    section,
    subgroup,
    education_level,
    unit,
    total_estimate,
    total_moe,
    male_estimate,
    male_moe,
    female_estimate,
    female_moe
  )

rates <-
  with_hierarchy |>
  filter(section == "Poverty Rate") |>
  mutate(unit = "Percent") |>
  select(
    section,
    subgroup,
    education_level,
    unit,
    total_estimate = percent_estimate,
    total_moe = percent_moe,
    male_estimate = percent_male_estimate,
    male_moe = percent_male_moe,
    female_estimate = percent_female_estimate,
    female_moe = percent_female_moe
  )

educational_attainment <-
  bind_rows(counts_and_dollars, rates) |>
  pivot_longer(
    cols = total_estimate:female_moe,
    names_to = c("sex", ".value"),
    names_sep = "_"
  ) |>
  rename(margin_of_error = moe) |>
  mutate(
    sex = str_to_title(sex),
    estimate = parse_number(estimate),
    margin_of_error = parse_number(margin_of_error)
  )

educational_attainment |>
  write_rds("data-clean/educational_attainment.rds")


# Chart -------------------------------------------------------------------

library(scales)
library(marquee)
library(showtext)

font_add(
  family = "Georgia",
  regular = "/System/Library/Fonts/Supplemental/Georgia.ttf",
  bold = "/System/Library/Fonts/Supplemental/Georgia Bold.ttf",
  italic = "/System/Library/Fonts/Supplemental/Georgia Italic.ttf",
  bolditalic = "/System/Library/Fonts/Supplemental/Georgia Bold Italic.ttf"
)
showtext_auto()
showtext_opts(dpi = 200)

earnings_gap <-
  educational_attainment |>
  filter(
    section == "Median Earnings",
    !is.na(education_level),
    sex %in% c("Male", "Female")
  ) |>
  mutate(
    education_level = fct_reorder(
      education_level,
      estimate,
      .fun = max
    )
  )

earnings_gap_wide <-
  earnings_gap |>
  select(education_level, sex, estimate) |>
  pivot_wider(names_from = sex, values_from = estimate)

ggplot(earnings_gap, aes(x = estimate, y = education_level)) +
  geom_segment(
    data = earnings_gap_wide,
    aes(x = Female, xend = Male, y = education_level, yend = education_level),
    inherit.aes = FALSE,
    color = "grey70",
    linewidth = 1
  ) +
  geom_point(aes(color = sex), size = 5) +
  geom_text(
    aes(label = dollar(estimate, accuracy = 1), color = sex),
    vjust = -1.4,
    size = 3.4,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_x_continuous(
    labels = label_dollar(),
    expand = expansion(mult = c(0.05, 0.08))
  ) +
  scale_color_manual(
    values = c(Male = "#1a476f", Female = "#c8102e"),
    name = NULL
  ) +
  labs(
    title = "A diploma is worth tens of thousands of dollars a year",
    subtitle = paste0(
      "Median annual earnings of U.S. workers 25 and over, by education and sex. ",
      "At every level of schooling, {.male **men**} ",
      "out-earn {.female **women**}."
    ),
    x = NULL,
    y = NULL,
    caption = "Source: U.S. Census Bureau, American Community Survey, 2024 1-year estimates (Table S1501)."
  ) +
  theme_minimal(base_family = "Georgia", base_size = 12) +
  theme(
    plot.title = element_text(
      family = "Georgia",
      face = "bold",
      size = 18,
      margin = margin(b = 6)
    ),
    plot.subtitle = element_marquee(
      size = 11,
      color = "grey25",
      margin = margin(b = 16),
      lineheight = 1.3,
      style = classic_style() |>
        modify_style("male", color = "#1a476f", weight = "bold") |>
        modify_style("female", color = "#c8102e", weight = "bold")
    ),
    plot.caption = element_text(
      color = "grey45",
      hjust = 0,
      margin = margin(t = 12)
    ),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey90"),
    axis.text.y = element_text(color = "grey20", size = 11),
    axis.text.x = element_text(color = "grey45"),
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave(
  "misc/earnings-by-education.png",
  width = 9,
  height = 6,
  dpi = 200,
  bg = "white"
)
