# week 5: Kabacoff chap. 4 univariate graphs

# summary:
# categorical: bar, pie, tree map, waffle
# quantitative: histogram, kernel density, dot

# progress:
# done w/ notes, need to ask questions

# dataset
install.packages("mosaicData")

data(Marriage, package = "mosaicData")

library(ggplot2)

## 1 Categorical ----

### 1.1 Bar Chart ----

ggplot(Marriage, aes(x = race)) +
  geom_bar(fill = "grey",
           color = "black") +
  labs(x = "Race",
       y = "Frequency",
       title = "Participants by Race") +
  scale_y_continuous(labels = scales::percent)
# Bar chart w/ color: red, blue, green, grey, etc.

#### 1.1.1 percent ----

ggplot(Marriage,
       aes(x = race,
           y = after_stat(count/sum(count)))) +
  geom_bar() +
  labs(x = "Race",
       y = "Percent",
       title = "Participants by race") +
  scale_y_continuous(labels = scales::percent)
# after_stat: doing stats within a plot
# scales package customize scales in a chart

#### 1.1.2 sorting categories ----

library(dplyr)

plotdata <- Marriage |>
  count(race)

ggplot(plotdata,
       aes(x = reorder(race, -n),
           y = n)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n),
            vjust = -0.5) +
  labs(x = "Race",
       y = "Frequency",
       title = "Participants by Race")
# "n" is in ascending order by default
# "-n" is in descending order
# stat = "identity": counts supplied directly, not calculated
# labeling bars through geom_text

#### 1.1.3 overlapping labels ----

ggplot(Marriage, aes(x=officialTitle)) +
  geom_bar()
# labels overlapping because too long

# make horizontal bar chart to solve
ggplot(Marriage, aes(x = officialTitle)) +
  geom_bar() +
  coord_flip()

# rotate axis labels to solve
ggplot(Marriage, aes(x = officialTitle)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))
# hjust: alignment of text within container
# between 0 - 1 (0 left, 1 right)

### 1.2 Pie Chart ----

library(ggpie)

ggpie(Marriage, race)

### 1.3 Tree Map ----

library(treemapify)

official_title <- Marriage |>
  count(officialTitle)

ggplot(official_title, aes(fill = officialTitle,
                           area = n,
                           label = officialTitle)) +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre") +
  labs(title = "Marriages by Officiate") +
  theme(legend.position = "none")

### 1.4 Waffle Chart ----

library(dbplyr)
library(waffle)
ggplot(official_title, aes(fill = officialTitle,
                           values = n)) +
  geom_waffle(na.rm = TRUE)

# customize

cap <- paste0("1 square = ", ceiling(sum(official_title$n)/100), 
              " case(s).")
#?paste0

library(waffle)
ggplot(official_title, aes(fill = officialTitle, values=n)) +
  geom_waffle(na.rm=TRUE,
              n_rows = 10,
              size = .4,
              color = "white") + 
  scale_fill_brewer(palette = "Spectral") +
  coord_equal() +
  theme_minimal() + 
  theme_enhance_waffle() +
  theme(legend.title = element_blank()) +
  labs(title = "Proportion of Wedding Officials",
       caption = cap)

## 2 Quantitative ----

### 2.1 histogram ----

# covered in intro_to_ggplot2

# binwidth = the width of the bins
# bins = number of bins (default = 30)

### 2.2 kernel density plot ----

ggplot(Marriage, aes(x = age)) +
  geom_density() + 
  labs(title = "Participants by age")

# smoothing parameter

ggplot(Marriage, aes(x = age)) +
  geom_density(fill = "deepskyblue", 
               bw = 1) + 
  labs(title = "Participants by age",
       subtitle = "bandwidth = 1")

### 2.3 dot chart

ggplot(Marriage, aes(x = age,
                     y = after_stat(count/sum(count)))) +
  geom_dotplot(binwidth = 3) + 
  scale_y_continuous(NULL, breaks = NULL) +
  labs(title = "Participants by age",
       y = "Proportion",
       x = "Age")
# y-axis inherently meaningless

ggsave("figure1.pdf", width = 5, height = 3)
ggsave("figure2.pdf", width = 3, height = 5)
