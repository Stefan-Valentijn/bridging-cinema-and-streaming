# Load libraries
library(tidyverse)
library(lubridate)

# Load dataset
raw_thenumbers <- read_csv("../data/raw/raw_thenumbers.csv")

# Remove timestamp variable and rank, that is metadata
thenumbers <- raw_thenumbers %>%
  select(-timestamp,
         -rank)

# Filter movies with a release date since 2022
thenumbers <- thenumbers %>%
  mutate(release_date = mdy(release_year)) %>%
  filter(release_date >= as.Date("2022-01-01"))

# Set financial variables right (remove $-sign and numeric measurement level)
thenumbers <- thenumbers %>%
  mutate(
    production_budget = parse_number(production_budget),
    domestic_gross = parse_number(domestic_gross),
    worldwide_gross = parse_number(worldwide_gross)
  )

# Filter out movies that were original movies (not released in cinema) or those who have yet to be released
thenumbers <- thenumbers %>%
  filter(domestic_gross != 0 & worldwide_gross != 0)

# Inventory management
rm(raw_thenumbers)

# Convert the numbers dataset to a csv-file
write.csv(thenumbers, "../data/thenumbers.csv", row.names = FALSE)
