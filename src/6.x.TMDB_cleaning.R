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


# Filter countries that have US as AT LEAST one of the production companies
tmdb <- tmdb %>%
  filter(grepl("US", production_countries))



# Rename
new_cinema_streaming_data <- tmdb

# Inventory management
rm(raw_tmdb, tmdb)

# Save file
write.csv(new_cinema_streaming_data, "../data/new_cinema_streaming_data.csv", row.names = FALSE)
