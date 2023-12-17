# week 6 Kabacoff Ch.6 Multivariate Graphs

# grouping ----

library(ggplot2)

ggplot(mpg, aes(x = cty, y = hwy,
                color = fl, shape = drv)) +
  geom_point()

# faceting ----

ggplot(mpg, aes(x = hwy)) +
  geom_histogram() +
  facet_wrap(~drv, ncol = 1) # number of columns of graphs

ggplot(mpg, aes(x = hwy)) +
  geom_histogram() +
  facet_grid(drv~cyl)

# grouping and faceting ----

ggplot(mpg, aes(x = hwy, y = cty, color = drv)) +
  geom_point() +
  facet_wrap(~fl, ncol = 5)
