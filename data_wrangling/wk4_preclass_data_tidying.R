# week 4: R4DS Chap. 6 Data Tidying

# summary:
# lengthening data
# widening data

# progress:
# done with notes

library(tidyverse)

# lengthening data ----

billboard_longer <- billboard |>
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    values_to = "rank",
    values_drop_na = TRUE
  ) |>
  mutate(
    week = parse_number(week)
  )
# before: each column is one week, cell value is the rank
# now: week is a column, rank is a column
# "" because week and rank are new variables
# values_drop_na getting rid of NA
# parse_number extract first number from a string (clean wk1 to 1)

billboard_longer |>
  ggplot(aes(x = week, y = rank, group = track)) +
  geom_line(alpha = 0.25) +
  scale_y_reverse()
# plotting

df <- tribble(
  ~id, ~bp1, ~bp2,
  "A", 100, 120,
  "B", 140, 115,
  "C", 120, 125
)


df |>
  pivot_longer(
    cols = bp1:bp2,
    names_to = "measurement",
    values_to = "value"
  )
# cols specify the columns concerned
# storing the name under the new variable "measure"
# storing the value under another new variable "value"

who2 |>
  pivot_longer(
    cols = !(country:year),
    names_to = c("diagnosis", "gender", "age"),
    names_sep = "_",
    values_to = "count"
  )
# change column "sp_m_014" to "sp", "m", "014"

household |>
  pivot_longer(
    cols = !family,
    names_to = c(".value", "child"),
    names_sep = "_",
    values_drop_na = TRUE
  )
# when use ".value" in names_to
# column name in input contribute to both values and variable name

# widening data ----

# increasing columns and reducing rows
# need to provide the existing cols that define values and name

cms_patient_experience |>
  distinct(measure_cd, measure_title)

cms_patient_experience |>
  pivot_wider(
    id_cols = starts_with("org"),
    names_from = measure_cd,
    values_from = prf_rate
  )


# explaining pivot_wider()

df2 <- tribble(
  ~id, ~measurement, ~value,
  "A", "bp1", 100,
  "B", "bp1", 140,
  "B", "bp2", 115,
  "A", "bp2", 120,
  "A", "bp3", 105
)

df2 |>
  pivot_wider(
    names_from = measurement,
    values_from = value
  )

df2 |>
  distinct(measurement) |>
  pull()

df2 |>
  select(-measurement, -value) |>
  distinct()
# rows in the output decided by variables not going
# into names or values. they are called id_cols.
# in this case, there are only one column.

df2 |>
  select(-measurement, -value) |>
  distinct() |>
  mutate(x = NA, y = NA, z = NA)
# minus sigh -: excluding variables
# mutate: adding columns
# pivot_wider() generate an empty data frame using the
# method above, and fills in missing values
