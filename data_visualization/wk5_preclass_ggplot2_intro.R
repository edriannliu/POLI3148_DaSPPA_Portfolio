# week5: r4ds chap.2: data visualization

# progress:
# all notes done

library(tidyverse)
library(palmerpenguins)
library(ggthemes)

# Points ----

# start w/ ggplot(), then add layers
# mapping = aes() to define x and y
# geom: bar, line, boxplot, point, etc.

ggplot(
  data = penguins, 
  mapping = aes(x = flipper_length_mm,
                y = body_mass_g)
  ) + # creating empty graph
  geom_point() +
  geom_smooth(method = "lm") # plot on graph through geom_

# missing value warning is very common

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm,
                y = body_mass_g,
                color = species)
  ) +
  geom_point() +
  geom_smooth(method = "lm")

# create three lines for each species
# because "color" argument applied at global level

ggplot(
  data = penguins, # creating empty graph
  mapping = aes(x = flipper_length_mm,
                y = body_mass_g)
) +
  geom_point(aes(color = species,
                           shape = species)) +
  geom_smooth(method = "lm") +
  labs(
    title = "Body Mass and Fliper Length",
    subtitle = "Dimensions for Adelie, Chrinstrap, Gentoo",
    caption = "Data from the palmerpenguins package",
    x = "Flipper Length (mm)",
    y = "Body Mass (g)",
    color = "Species", shape = "Species"
  ) +
  scale_color_colorblind()
# apply "color" at local level, only generate 1 line
# colorblind function in ggthemes


# rewriting to be more concise

ggplot(penguins, aes(x = flipper_length_mm,
                     y = body_mass_g)) +
  geom_point()


# Distributions (bars) ----

## categorical ----

ggplot(penguins, aes(x = species)) +
  geom_bar()

# ordering
ggplot(penguins, aes(x = fct_infreq(species))) +
  geom_bar()

# fct_infreq(): order by number of observations with each level (largest first)


## numerical ----

ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 200)
# experiment w/ binwidth

ggplot(penguins, aes(x = body_mass_g)) +
  geom_density()

# Relationships ----

## one value ----

ggplot(penguins, aes(x = species,
                     y = body_mass_g)) +
  geom_boxplot()

ggplot(penguins, aes(x = body_mass_g,
                     color = species)) +
  geom_density(linewidth = 0.75)

ggplot(penguins, aes(x = body_mass_g,
                     color = species,
                     fill = species)) +
  geom_density(alpha = 0.5) # transparency 0-1

## two values ----

ggplot(penguins, aes(x = island,
                     fill = species)) +
  geom_bar()
ggplot(penguins, aes(x = island,
                     fill = species)) +
  geom_bar(position = "fill")

## three or more

# too much variables become confusing
# solution: creating facets

ggplot(penguins, aes(x = flipper_length_mm,
                     y = body_mass_g)) +
  geom_point(aes(color = species,
                shape = species)) +
  facet_wrap(~island)
