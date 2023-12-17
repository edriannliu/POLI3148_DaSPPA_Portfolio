# Text Mining w R Chap. 1

# Aim: one token per row
# Token: a meaningful unit of text, such as a word

# unnest_tokens ----

library(dplyr)

text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")
text_df <- tibble(line = 1:4, text = text)

library(tidytext)

text_df |>
  unnest_tokens(word, text)

