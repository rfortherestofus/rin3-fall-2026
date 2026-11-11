# Load packages ----------------------------------------------------------

library(lubridate)
library(dplyr)
library(readr)


# Load data --------------------------------------------------------------

# Read everything as text so R doesn't guess the dates for us
orders <- read_csv("orders.csv", col_types = cols(.default = col_character()))
orders

# -------------------------------------------------------------
# 1. PARSING: the ymd() family
#    Name the function after the ORDER of the parts, not the separators.
# -------------------------------------------------------------
ymd("2024-01-15")
ymd("2024/04/09")
ymd("20240618") # no separators? still fine
mdy("01/22/2024")
mdy("March 3, 2024") # month names work too
dmy("08-May-2024")

# Same order, different separators -> one function handles them all
ymd(c("2024-01-15", "2024/04/09", "20240618"))

# Date-times: add _hms / _hm
ymd_hms("2024-01-17 09:30:00")

# -------------------------------------------------------------
# 2. THE MESSY COLUMN: parse_date_time() with several orders
# -------------------------------------------------------------
orders$order_date

# One function per format fails on most rows:
ymd(orders$order_date) # warning: most failed to parse

# Attempt 1: give lubridate a list of candidate orders
orders <- orders |>
  mutate(
    order_dt = as_date(parse_date_time(
      order_date,
      orders = c("ymd", "mdy", "dmy")
    )),
    shipped_at = ymd_hms(shipped_at)
  )

orders |> select(order_date, order_dt)
# Looks great... but sanity-check it: nothing should ship before it's ordered
orders |>
  filter(as_date(shipped_at) < order_dt) |>
  select(order_date, order_dt, shipped_at)
# "08-May-2024" became 2024-08-20. lubridate's parser is fuzzy:
# with "mdy" in the list it read 08 as the month and skipped "May".

# Attempt 2: exact = TRUE with the precise formats you saw in the data
orders <- orders |>
  mutate(
    order_dt = as_date(parse_date_time(
      order_date,
      exact = TRUE,
      orders = c(
        "%Y-%m-%d",
        "%Y/%m/%d",
        "%Y%m%d", # 2024-01-15, 2024/04/09, 20240618
        "%m/%d/%Y",
        "%m/%d/%y", # 01/22/2024, 7/4/24
        "%B %d, %Y",
        "%b %d %Y", # March 3, 2024 / Aug 19 2024
        "%d-%b-%Y"
      ) # 08-May-2024
    ))
  )

orders |> select(order_date, order_dt) # all correct now
orders |> filter(as_date(shipped_at) < order_dt) # 0 rows

# "TBD" -> NA, with a warning. That's what you want: check it!
orders |> filter(is.na(order_dt))

# -------------------------------------------------------------
# 3. EXTRACTING components
# -------------------------------------------------------------
orders |>
  transmute(
    order_dt,
    yr = year(order_dt),
    mon = month(order_dt, label = TRUE),
    day = day(order_dt),
    weekday = wday(order_dt, label = TRUE, week_start = 1),
    qtr = quarter(order_dt),
    wk = isoweek(order_dt)
  )

# Rounding to a period — handy for grouping
orders |> count(month = floor_date(order_dt, "month"))

# -------------------------------------------------------------
# 4. ARITHMETIC: periods vs durations vs intervals
# -------------------------------------------------------------
orders <- orders |>
  mutate(
    days_to_ship = as.numeric(difftime(shipped_at, order_dt, units = "days")),
    ship_interval = interval(order_dt, shipped_at),
    due_back = order_dt + days(30) # period: calendar days
  )

orders |> select(order_id, order_dt, shipped_at, days_to_ship, due_back)

# Period vs duration: the month-end / leap-year trap
ymd("2024-01-31") + months(1) # NA: there's no Feb 31
ymd("2024-01-31") %m+% months(1) # rolls back to 2024-02-29
ymd("2024-02-28") + years(1) # period: 2025-02-28
ymd("2024-02-28") + dyears(1) # duration: exactly 365.25 days

# Intervals: is a date inside a window?
q3 <- interval(ymd("2024-07-01"), ymd("2024-09-30"))
orders |> filter(order_dt %within% q3) |> select(order_id, order_dt)

# How long between two dates, in human units
time_length(interval(ymd("2024-01-15"), today()), "months")

# -------------------------------------------------------------
# 5. TIME ZONES
# -------------------------------------------------------------
t <- ymd_hms("2024-01-17 09:30:00", tz = "America/New_York")
with_tz(t, "Asia/Tokyo") # same instant, different clock
force_tz(t, "Asia/Tokyo") # same clock, different instant

# -------------------------------------------------------------
# 6. Which weekday gets the most orders? What's the average days_to_ship?
# -------------------------------------------------------------
orders |> count(wday(order_dt, label = TRUE))
mean(orders$days_to_ship, na.rm = TRUE)
