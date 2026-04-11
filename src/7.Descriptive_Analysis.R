# Load libraries
library(tidyverse)

# Load dataset
df <- read_csv("../data/cinema_streaming_data.csv")

# Impression dataset
summary(df)
colSums(is.na(df))

# How many films per year in the dataset
df %>%
  group_by(releaseYear) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count / sum(count) * 100, 1))

# Descriptives of viewings
df %>%
  summarise(across(c(viewing_30days, viewing_60days, viewing_90days),
                   list(min = min, max = max, mean = mean),
                   na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = c("variable", "stat"), names_sep = "_(?=[^_]+$)") %>%
  pivot_wider(names_from = stat, values_from = value)



