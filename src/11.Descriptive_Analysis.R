# Load libraries
library(tidyverse)

# Load dataset
df <- read_csv("../data/finaldataset.csv")

# Impression dataset
summary(df)
colSums(is.na(df))

# How many films per year in the dataset
df %>%
  group_by(releaseYear) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count / sum(count) * 100, 1))

