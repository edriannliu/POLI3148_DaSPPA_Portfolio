# week 6: Kabacoff Ch.5

# progress: done w/ notes

library(ggplot2)

# categorical v. categorical ----

data(mpg, package="ggplot2")

# staked bar chart
ggplot(mpg, aes(x = class, fill = drv)) +
  geom_bar(position = "stack")

# grouped bar chart (side-by-side)
ggplot(mpg, aes(x = class, fill = drv)) +
  geom_bar(position = "dodge")
# to have same width for all bars
ggplot(mpg, aes(x = class, fill = drv)) + 
  geom_bar(position = position_dodge(preserve = "single"))

# segmented bar chart
ggplot(mpg, aes(x = class, fill = drv)) + 
  geom_bar(position = "fill") +
  labs(y = "Proportion")

# improving color and presentation
# factor: modifies order of categories

library(scales)

ggplot(mpg,
       aes(x = factor(class,
                       levels = c("2seater", "subcompact",
                                  "compact", "midsize",
                                  "minivan", "suv", "pickup")),
           fill = factor(drv,
                          levels = c("f", "r", "4"),
                          labels = c("front-wheel",
                                     "rear-wheel",
                                     "4-wheel")))) +
  geom_bar(position = "fill") +
  scale_y_continuous(breaks = seq(0,1,.2),
                     label = percent) +
  scale_fill_brewer(palette = "Set3") +
  labs(y = "Percent",
       fill = "Drive Train",
       x = "Class",
       title = "Automobile Drive by Class") +
  theme_minimal()

# labeling segments
# first create a summary data set that has the labels
# then plot the new data set with geom_text()

library(dplyr)

plotdata <- mpg |>
  group_by(class, drv) |>
  summarize(n = n()) |>
  mutate(pct = n/sum(n),
         lbl = scales::percent(pct))

ggplot(plotdata,
       aes(x = factor(class,
                      levels = c("2seater", "subcompact",
                                 "compact", "midsize",
                                 "minivan", "suv", "pickup")),
           y = pct,
           fill = factor(drv,
                         levels = c("f", "r", "4"),
                         labels = c("front-wheel",
                                    "rear-wheel",
                                    "4-wheel")))) +
  geom_bar(stat = "identity",
           position = "fill") +
  scale_y_continuous(breaks = seq(0,1,.2),
                     label = percent) +
  geom_text(aes(label = lbl),
            size = 3,
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Percent",
       fill = "Drive Train",
       x = "Class",
       title = "Automobile Drive by Class") +
  theme_minimal()


# quantitative v. quantitative ----

# scatterplot: geom_point(), best fit lines geom_smooth()
# lineplot: geom_line()

# categorical v. quantitative ----

# bar chart on summary statistics

mpg_hwy <- mpg |>
  group_by(drv) |>
  summarize(mean_hwy = mean(hwy))

ggplot(mpg_hwy, aes(x = drv, y = mean_hwy)) +
  geom_bar(stat = "identity")

# grouped kernel density plots

ggplot(mpg, aes(x = hwy, fill = drv)) +
  geom_density(alpha = 0.4, bw = 1.5)

# box plots

ggplot(mpg, aes(x = drv, y = hwy)) +
  geom_boxplot()

ggplot(mpg, aes(x = drv, y = hwy)) +
  geom_boxplot(notch = TRUE, alpha = 0.7) +
  theme_bw()

# violin plots

ggplot(mpg, aes(x = drv, y = hwy)) +
  geom_violin() +
  geom_boxplot(width = .3, 
               fill = "orange",
               outlier.color = "orange",
               outlier.size = 2)

# ridgeline plots

library(ggridges)

# mean / sem plots

mpg_cty <- mpg |>
  group_by(drv, year) |>
  summarize(n = n(),
            mean = mean(cty),
            sd = sd(cty),
            se = sd/sqrt(n))

pd <- position_dodge(0.2)

ggplot(mpg_cty, aes(x = drv, y = mean,
                    group = year, color = year)) +
  geom_point(position = pd) +
  geom_line(position = pd) +
  geom_errorbar(aes(ymin = mean - se,
                    ymax = mean + se),
                width = .1,
                position = pd)
 
# strip plots & jittered plot

ggplot(mpg, aes(y = drv, x = hwy, color = drv)) +
  geom_point()

ggplot(mpg, aes(y = drv, x = hwy, color = drv)) +
  geom_jitter()

# cleveland dot charts

ggplot(mpg, aes(x = cty, y = reorder(model, cty))) +
  geom_point()


# ?ggplot
sample_df <- data.frame(
  group = factor(rep(letters[1:3], each = 10)),
  value = rnorm(30)
)

group_means_df <- setNames(
  aggregate(value ~ group, sample_df, mean),
  c("group", "group_mean")
)

ggplot(data = sample_df, mapping = aes(x = group, y = value)) +
  geom_point() +
  geom_point(
    mapping = aes(y = group_mean), data = group_means_df,
    colour = 'red', size = 3
  )

