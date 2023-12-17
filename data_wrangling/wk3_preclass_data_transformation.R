# week 3: r4ds chap.4 data transformation

# summary:
# rows: filter, arrange, distinct
# columns: mutate, select, rename, relocate
# groups: group_by, summarize, ungroup, slice_, .by
# corresponding to Lecture 4, Sept. 28

# progress:
# still missing *groups*

install.packages("nycflights13") 

library(nycflights13)
library(dplyr)

nycflights13::flights

print(nycflights13::flights, width = Inf)
# showing all columns

flights |>
  filter(dest == "IAH") |>
  group_by(year, month, day) |>
  summarize(
    arr_delay = mean(arr_delay, na.rm = TRUE)
  ) 

## rows: filter, arrange, distinct, count ----

flights |>
  filter(dep_delay > 120)
# keep rows based on the value of the columns
# all flights departed more than 120 mins

jan_1 <- flights |>
  filter(month == 1 & day == 2)
# all flights departed on Jan 2
# can store value
# and: &, or: |


flights |>
  arrange(year, month, day)
# change order of row based on value of columns
# earliest year first, then earliest month and day

flights |>
  arrange(desc(arr_delay))
# descending order

flights |>
  distinct()
# remove duplicate rows

flights |>
  distinct(origin, dest, .keep_all = TRUE)
# find all unique origin and destination pairs
# .keep_all keeps other columns

flights |>
  count(origin, dest, sort = TRUE)
# finding number of occurrences

flights |>
  distinct(carrier)

# exercise

flights |>
  filter(arr_delay > 50) |>
  filter(dest == "IAH" | dest == "HOU") |>
  filter(carrier == "UA" | carrier == "AA" | carrier == "DL")

flights |>
  distinct(arr_delay) |>
  arrange(arr_delay)

flights |>
  arrange(desc(dep_delay), dep_time)

flights$speed <- flights$air_time / flights$distance

flights |>
  distinct(speed) |> 
  arrange(desc(speed))

## columns: mutate, select, rename, relocate ----

flights |>
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60,
    .before = 1,
    .keep = "used"
  )
# add new columns
# (on the right hand by default, change to left by .before)
# .after = [column name] to specify place
# .keep = "used" only show columns involved in step

flights |>
  select(year, month, day)

flights |>
  select(year:dep_delay)
# columns between the two

flights |>
  select(!year:day)
# select columns except ...

flights |>
  select(where(is.character))

flights |>
  rename(tail_num = tailnum)
# need to use <- to overwrite in order to be recorded

flights |>
  relocate(arr_time, .before = dep_time)
# by default, move to front
# specify w/ .before and .after arguments

variables <- c("year", "month")
selected_columns <- flights |>
  select(any_of(variables))
# able to select based on a vector of names
# rather than type out each column name

flights |> select(contains("TIME"))

# use ctrl + shift + M to pipe

## groups: group_by(), summarize() ----

flights |>
  group_by(month)

