# Load libraries
library(tidyverse)
library(lubridate)

# Load dataset
raw_tmdb <- read.csv("../data/raw/raw_tmdb.csv")

# Remove timestamp and title sanity check variables, that is metadata
tmdb <- raw_tmdb %>%
  select(-starts_with("timestamp"))

# Set measurement levels good
tmdb$TMDB_votecount <- as.numeric(tmdb$TMDB_votecount)
tmdb$budget <- as.numeric(tmdb$budget)

# Inventory management
rm(raw_tmdb)

# Save file
write.csv(tmdb, "../data/tmdb.csv", row.names = FALSE)
