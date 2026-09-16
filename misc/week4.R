library(tidyverse)
library(scales)

sales <-
  tribble(
    ~day_number,
    ~amount,
    1,
    3000,
    2,
    6000,
    3,
    4500
  ) |>
  mutate(amount_formatted = dollar(amount))

sales |>
  mutate(amount_formatted = str_glue("${amount}"))

sales |>
  mutate(amount_formatted_v2 = dollar(
    amount,
    accuracy = 1,
    scale = 1 / 1000,
    suffix = "K"
  ))

ggplot(
  data = sales,
  aes(
    x = day_number,
    y = amount,
    label = amount_formatted
  )
) +
  geom_col() +
  geom_text(vjust = -1) +
  scale_y_continuous(
    labels = dollar_format()
  )

penguins_islands <-
  read_csv("data-raw/penguins.csv") |>
  count(island) |>
  mutate(pct = n / sum(n)) |>
  mutate(pct_formatted = percent(pct))


ggplot(
  data = penguins_islands,
  aes(
    x = island,
    y = pct,
    label = pct_formatted
  )
) +
  geom_col() +
  geom_text(vjust = -1) +
  scale_y_continuous(
    labels = percent_format()
  )

??percent



# Ungrouping --------------------------------------------------------------

penguins <-
  read_csv("data-raw/penguins.csv")

penguins |>
  drop_na(sex) |>
  group_by(species, island, sex) |>
  summarize(avg_bill_length = mean(bill_length_mm, na.rm = TRUE)) |>
  arrange(desc(avg_bill_length))
ungroup() |>
  slice_max(
    order_by = avg_bill_length,
    n = 1
  )

library(tidyverse)

penguins <-
  read_csv("data-raw/penguins.csv") |> 
  drop_na(sex)

penguins
