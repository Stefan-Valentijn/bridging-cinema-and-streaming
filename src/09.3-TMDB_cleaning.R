# Load libraries
library(tidyverse)
library(lubridate)

# Load dataset
part1_raw_tmdb <- read.csv("../data/part1_raw_tmdb.csv")
part2_raw_tmdb <- read.csv("../data/part2_raw_tmdb.csv")

# Merge based on tconst
tmdb <- part1_raw_tmdb %>%
  left_join(part2_raw_tmdb, by = "tconst")

# Inventory management
rm(part1_raw_tmdb, part2_raw_tmdb)

# Remove timestamp and title sanity check variables, that is metadata
tmdb <- tmdb %>%
  select(-title_sanitycheck, -starts_with("timestamp"))

# Set measurement levels good
tmdb$TMDB_votecount <- as.numeric(tmdb$TMDB_votecount)
tmdb$budget <- as.numeric(tmdb$budget)

# Make 0 an na
tmdb <- tmdb %>%
  mutate(budget = na_if(budget, 0))

# How does the dataset look
summary(tmdb)

# Save file
write.csv(tmdb, "../data/tmdb.csv", row.names = FALSE)
