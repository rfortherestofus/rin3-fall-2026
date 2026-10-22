# =============================================================================
# WHAT THIS SCRIPT DOES
# =============================================================================
# This script takes a spreadsheet of race and ethnicity data for Oregon
# (from the 2019 "Oregon by the Numbers" report) and turns it into two charts:
#
#   Chart A: a grid of small pie charts, one per county.
#   Chart B: a grid of small bar charts, one per county.
#
# Both charts show the same information: what share of each county's
# population belongs to each racial/ethnic group. They are two different ways
# of looking at the same numbers, so you can compare which one is easier to
# read.
#
# A quick note on reading R code:
#   - Any line starting with "#" is a comment. R ignores it. Comments exist
#     only for humans.
#   - "<-" means "store the result in a named container" (called an object).
#     For example, `x <- 5` puts the number 5 into a container named x.
#   - "|>" is called the "pipe." Read it as "and then." It takes whatever is
#     on its left and hands it to the next step on its right. This lets us
#     write a series of steps top to bottom, like a recipe.
#   - "+" inside a chart works similarly to the pipe, but for charts: each
#     "+" adds another layer or setting to the chart being built.
# =============================================================================


# -----------------------------------------------------------------------------
# STEP 1: Load the toolkits (called "packages") we need
# -----------------------------------------------------------------------------
# R on its own can do a lot, but people have written add-on toolkits that make
# common jobs much easier. `library()` switches a toolkit on for this session.
# (Each toolkit has to be installed once on your computer before this works.)

# The tidyverse is a bundle of popular toolkits for tidying data and making
# charts. It includes ggplot2 (charts), dplyr (filtering and changing data),
# tidyr (reshaping data), and forcats (controlling the order of categories).
library(tidyverse)

# readxl lets R open Excel files (.xlsx).
library(readxl)

# janitor has handy cleanup tools. We use it below to tidy up messy column
# names.
library(janitor)


# -----------------------------------------------------------------------------
# STEP 2: Read in the data and get it into a tidy shape
# -----------------------------------------------------------------------------
# Everything from here until the end of Step 2 is one long recipe. The final
# result gets stored in an object named `race_ethnicity`.

# race_ethnicity <-
  # Open the Excel file. The file lives in the "data-raw" folder of this
  # project. An Excel file can have several tabs (called "sheets"), so we tell
  # R which one we want: the tab named "Race Ethnicity".
  #
  # In the original spreadsheet, each row is a place (a county, the whole
  # state, or the "Rural" and "Urban" totals) and there is one column per
  # racial/ethnic group. Each cell holds a proportion written as a decimal,
  # so 0.911 means 91.1%.
  read_excel(
  "data-raw/2019-obtn-by-county.xlsx",
  sheet = "Race Ethnicity"
) |>
  # Clean up the column names. The spreadsheet's headers look like
  # "Percentage of Population White, non-Latino." which are hard to type in
  # code because of the spaces, capital letters, and punctuation.
  # clean_names() converts them to all lowercase with underscores instead of
  # spaces and drops the punctuation. So that header becomes
  # "percentage_of_population_white_non_latino". The "Geography" column
  # becomes "geography".
  clean_names() |>
  # Reshape the data from "wide" to "long."
  #
  # Right now the data is wide: one row per place, with eight separate columns
  # for the eight groups. Like this (simplified):
  #
  #   geography | white | black | latino | ...
  #   Baker     | 0.91  | 0.01  | 0.04   | ...
  #
  # Charting tools work best when the data is long: one row per place AND
  # group, with the group name in one column and the number in another:
  #
  #   geography | race_ethnicity | pct
  #   Baker     | white          | 0.91
  #   Baker     | black          | 0.01
  #   Baker     | latino         | 0.04
  #
  # That is what pivot_longer() does. Its settings mean:
  #   cols = -geography     -> Reshape every column EXCEPT geography (the
  #                            minus sign means "leave this one alone").
  #   names_to = "race_ethnicity" -> The old column names (the group names)
  #                            go into a new column called race_ethnicity.
  #   values_to = "pct"     -> The numbers that were in those columns go into
  #                            a new column called pct (short for percent).
  pivot_longer(
    cols = -geography,
    names_to = "race_ethnicity",
    values_to = "pct"
  ) |>
  # Replace the long, code-friendly group names with short, readable labels
  # that will look good on a chart.
  #
  # mutate() means "change a column or create a new one." Here we overwrite
  # the existing race_ethnicity column.
  #
  # case_when() works like a list of "if this, then that" rules. Each line
  # reads: "if the value equals THIS (left side of ~), replace it with THAT
  # (right side of ~)." R checks each row against the rules in order.
  # The "==" means "is exactly equal to" (a single "=" is used for settings,
  # so R uses a double one for comparisons).
  mutate(
    race_ethnicity = case_when(
      race_ethnicity == "percentage_of_population_white_non_latino" ~ "White, non-Latino",
      race_ethnicity == "percentage_of_population_black_non_latino" ~ "Black, non-Latino",
      race_ethnicity == "percentage_of_population_american_indian_or_alaska_native_non_latino" ~ "American Indian or Alaska Native, non-Latino",
      race_ethnicity == "percentage_of_population_asian_non_latino" ~ "Asian, non-Latino",
      race_ethnicity == "percentage_of_population_native_hawaiian_pacific_islander_non_latino" ~ "Native Hawaiian/Pacific Islander, non-Latino",
      race_ethnicity == "percentage_of_population_other_race_non_latino" ~ "Other race, non-Latino",
      race_ethnicity == "percentage_of_population_multi_racial_non_latino" ~ "Multi-racial, non-Latino",
      race_ethnicity == "percentage_of_population_latino" ~ "Latino"
    )
  ) |>
  # Add a new column called geography_type that labels what KIND of place
  # each row is. The spreadsheet mixes three kinds of rows together:
  #   - "Oregon" is the whole state.
  #   - "Rural" and "Urban" are totals for all rural and all urban areas.
  #   - Everything else is an individual county.
  # We need this label so that later we can keep just the counties.
  #
  # The rules, again checked in order:
  #   - If geography is exactly "Oregon", label it "State".
  #   - If geography is either "Rural" or "Urban", label it "Rural/Urban".
  #     (%in% means "is any one of the items in this list"; c() builds a list.)
  #   - .default covers every row that didn't match a rule above. Those are
  #     the counties, so they get labeled "County".
  #
  # .after = geography just controls where the new column appears in the
  # table: right after the geography column, so the two sit side by side.
  # This is purely cosmetic and makes the table easier to look at.
  mutate(
    geography_type = case_when(
      geography == "Oregon" ~ "State",
      geography %in% c("Rural", "Urban") ~ "Rural/Urban",
      .default = "County"
    ),
    .after = geography
  )


# -----------------------------------------------------------------------------
# STEP 3: Keep only the counties and put the groups in a sensible order
# -----------------------------------------------------------------------------
# Start from the tidy data we just made and store the result in a new object
# called race_ethnicity_counties.

race_ethnicity_counties <- race_ethnicity |>
  # filter() keeps only the rows that pass a test. Here the test is "is this
  # row a county?" So the state total and the Rural/Urban totals are dropped,
  # leaving Oregon's 36 counties.
  filter(geography_type == "County") |>
  # Set the order in which the groups will appear in the charts and legends.
  #
  # By default, R would sort the group names alphabetically, which puts
  # "American Indian..." first and "White..." last. That isn't very useful.
  # Instead we want the biggest groups first.
  #
  # fct_reorder() reorders the groups based on the numbers in another column.
  # Its settings mean:
  #   race_ethnicity -> the column whose order we want to change.
  #   pct            -> the numbers to base the order on.
  #   .fun = sum     -> for each group, add up its pct across all counties to
  #                     get one overall score per group.
  #   .desc = TRUE   -> sort from largest to smallest ("desc" = descending).
  #
  # The result: White, non-Latino comes first (it's the largest group in most
  # counties), followed by Latino, and so on down to the smallest group.
  mutate(
    race_ethnicity = fct_reorder(race_ethnicity, pct, .fun = sum, .desc = TRUE)
  )


# -----------------------------------------------------------------------------
# STEP 4 (Chart A): A grid of pie charts, one per county
# -----------------------------------------------------------------------------
# ggplot2 builds charts in layers. You start with the data, describe how the
# data maps onto the visual (position, color, etc.), then keep adding layers
# and settings with "+".
#
# A surprising fact: ggplot2 has no direct "make a pie chart" command. The
# trick is to build a single stacked bar and then bend it into a circle. That
# is what the code below does.

race_ethnicity_counties |>
  # ggplot() starts the chart. aes() (short for "aesthetics") says which
  # columns control which visual features:
  #   x = 1                    -> Put every bar at the same horizontal spot.
  #                               The number 1 is arbitrary; any constant
  #                               works. This makes all the groups stack into
  #                               ONE bar instead of sitting side by side.
  #   y = pct                  -> The height of each slice is the group's
  #                               percentage.
  #   fill = race_ethnicity    -> Each group gets its own fill color.
  ggplot(aes(x = 1, y = pct, fill = race_ethnicity)) +
  # geom_col() draws the bars ("col" = column). Since every group sits at
  # x = 1, they stack on top of each other into one tall bar.
  #   width = 1         -> Make the bar fill its whole space. Once the bar is
  #                        bent into a circle, this makes the pie solid with
  #                        no hole in the middle.
  #   color = "white"   -> Draw a white outline around each slice so the
  #                        slices are visually separated.
  #   linewidth = 0.25  -> Keep that outline thin.
  geom_col(
    width = 1,
    color = "white",
    linewidth = 0.25
  ) +
  # This is the step that turns the stacked bar into a pie. coord_polar()
  # switches from a regular grid (left/right, up/down) to a circular one.
  # theta = "y" says "wrap the bar's height (y) around the circle." So the
  # taller a group's segment, the bigger its slice of the pie.
  coord_polar(theta = "y") +
  # Make a separate small chart for every county instead of one big chart.
  # These are called "facets" or "small multiples."
  #   vars(geography) -> make one mini-chart per value of geography (i.e. one
  #                      per county).
  #   ncol = 6        -> arrange them in a grid that is 6 charts wide.
  facet_wrap(vars(geography), ncol = 6) +
  # Choose the colors ourselves rather than using ggplot2's defaults.
  # scale_fill_manual() assigns these colors to the groups in order: the first
  # color goes to the first group (the largest one, thanks to Step 3), the
  # second color to the second group, and so on. There are eight colors for
  # the eight groups.
  #
  # Each color is written as a "hex code": a "#" followed by six characters
  # that describe a specific color. For example "#2a78d6" is a medium blue.
  scale_fill_manual(
    values = c(
      "#2a78d6",
      "#eb6834",
      "#1baf7a",
      "#eda100",
      "#e87ba4",
      "#008300",
      "#6250d6",
      "#e34948"
    )
  ) +
  # labs() sets the text labels on the chart.
  #   title = "..." -> the headline at the top.
  #   fill = NULL   -> remove the legend's heading (it would otherwise say
  #                    "race_ethnicity", which looks clunky). NULL means
  #                    "nothing."
  labs(
    title = "Race and ethnicity in Oregon counties, 2019",
    fill = NULL
  ) +
  # theme_void() strips away everything except the data itself: no gridlines,
  # no axes, no gray background. Pie charts don't need axes, so this gives a
  # clean look.
  theme_void() +
  # theme() fine-tunes the appearance of individual pieces of the chart.
  #   legend.position = "bottom" -> put the color key below the charts instead
  #                                 of to the right, leaving more room for the
  #                                 grid of pies.
  #   strip.text = ...           -> "strip" is the label above each mini-chart
  #                                 (the county name). Set its font size to 9.
  #   plot.title = ...           -> make the main title bold, and add a small
  #                                 gap (margin) below it. "b = 10" means 10
  #                                 points of space on the bottom.
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 9),
    plot.title = element_text(face = "bold", margin = margin(b = 10))
  ) +
  # Arrange the legend's entries in 2 columns. Some group names are long, so
  # a single row at the bottom would run off the edge of the chart.
  guides(fill = guide_legend(ncol = 2))


# -----------------------------------------------------------------------------
# STEP 5 (Chart B): A grid of horizontal bar charts, one per county
# -----------------------------------------------------------------------------
# Same data, different chart. Bar charts are often easier to read than pie
# charts because people judge lengths more accurately than angles. This
# version also handles a problem with the pies: in most Oregon counties, one
# group is very large and the rest are tiny, so the small slices are nearly
# invisible.

race_ethnicity_counties |>
  # Start the chart. This time:
  #   x = pct            -> bar length is the group's percentage (running
  #                         left to right).
  #   y = race_ethnicity -> each group gets its own row, so the bars are
  #                         horizontal. Horizontal bars leave room for long
  #                         group names to be read without tilting your head.
  # The rows follow the order we set in Step 3. With horizontal bars, ggplot2
  # draws the first group at the bottom.
  ggplot(aes(x = pct, y = race_ethnicity)) +
  # Draw the bars.
  #   fill = "#2a78d6" -> every bar is the same blue. Since each group already
  #                       has its own labeled row, color isn't needed to tell
  #                       them apart. (Note: this is set OUTSIDE of aes()
  #                       because it's a fixed color, not something driven by
  #                       the data.)
  #   width = 0.7      -> bars take up 70% of their row, leaving small gaps
  #                       between them.
  geom_col(fill = "#2a78d6", width = 0.7) +
  # One mini-chart per county again, this time in a grid 4 charts wide.
  #
  # scales = "free_x" is the key setting here. Normally every mini-chart
  # shares the same horizontal scale (say, 0% to 100%). "free_x" lets each
  # county's chart have its OWN horizontal scale, fitted to its own data.
  # Without this, counties where every group except one is small would show
  # tiny slivers. The trade-off: you can't compare bar lengths ACROSS counties
  # by eye, only within a county. Always read the axis numbers.
  facet_wrap(vars(geography), ncol = 4, scales = "free_x") +
  # Control how the horizontal axis (x) looks.
  #   labels = label_percent() -> show the numbers as percentages, so 0.5
  #                               displays as "50%".
  #   breaks = breaks_extended(n = 3) -> aim for about 3 tick marks per axis.
  #                               With so many small charts, fewer labels
  #                               keeps the axes from getting crowded.
  #   expand = expansion(mult = c(0, 0.05)) -> controls the padding at each
  #                               end of the axis. 0 on the left means the
  #                               bars start right at the edge with no gap.
  #                               0.05 on the right adds 5% of extra space so
  #                               the longest bar doesn't touch the edge.
  # The "scales::" prefix means "use the function from the scales toolkit."
  # This lets us use it without switching that whole toolkit on with
  # library() at the top.
  scale_x_continuous(
    labels = scales::label_percent(),
    breaks = scales::breaks_extended(n = 3),
    expand = expansion(mult = c(0, 0.05))
  ) +
  # Text labels.
  #   title    -> the headline.
  #   subtitle -> a smaller line under the headline. Here it warns readers
  #               that each panel has its own scale (see facet_wrap above).
  #   x = NULL, y = NULL -> remove the axis titles. The percentages and group
  #               names speak for themselves, so the titles would be clutter.
  labs(
    title = "Race and ethnicity in Oregon counties, 2019",
    subtitle = "Each panel has its own scale so smaller groups are readable",
    x = NULL,
    y = NULL
  ) +
  # theme_minimal() is a clean starting style: white background with light
  # gridlines and no box around the chart.
  theme_minimal() +
  # Fine-tune the details.
  #   panel.grid.major.y = element_blank() -> remove the horizontal gridlines.
  #                         With horizontal bars, those lines run through the
  #                         bars and add nothing. element_blank() means "don't
  #                         draw this."
  #   panel.grid.minor = element_blank()   -> remove the faint in-between
  #                         gridlines, keeping only the main ones at each
  #                         labeled tick mark.
  #   panel.spacing.x = unit(1.5, "lines") -> add more space between the
  #                         mini-charts side to side, so their axis labels
  #                         don't bump into each other. "1.5 lines" means
  #                         the height of 1.5 lines of text.
  #   strip.text = ...  -> the county name above each mini-chart: left-align
  #                         it (hjust = 0) and make it bold.
  #   plot.title = ...  -> make the main title bold.
  #   axis.text.y = ... -> shrink the group names on the left side to size 7
  #                         so the long names fit.
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing.x = unit(1.5, "lines"),
    strip.text = element_text(hjust = 0, face = "bold"),
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 7)
  )
