# Load libraries
library(tidyverse)
library(lubridate)

# Load dataset
raw_tmdb <- read.csv("../data/cinema_streaming_data_plus.csv")

# Remove timestamp and title sanity check variables, that is metadata
tmdb <- raw_tmdb %>%
  select(-starts_with("timestamp"))

# Set measurement levels good
tmdb$TMDB_votecount <- as.numeric(tmdb$TMDB_votecount)
tmdb$budget <- as.numeric(tmdb$budget)

# Correct date format without hour timestamp thing
#tmdb <- tmdb %>%
#  mutate(cinema_release_TMDB = as.Date(substr(cinema_release, 1, 10)))

# Inventory management
rm(raw_tmdb)

# Variables not needed in hindsight
tmdb <- tmdb %>%
  select(-title_sanitycheck, -budget, -production_companies, -production_countries, -cinema_release, -digital_release)

# Save file
write.csv(tmdb, "../data/tmdb.csv", row.names = FALSE)
