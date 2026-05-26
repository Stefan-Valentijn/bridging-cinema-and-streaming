# Load libraries
library(tidyverse)

# Load dataset
validated_data <- read_csv("../data/validated_data.csv")
tmdb <- read_csv("../data/tmdb.csv")

# Merge based on tconst
complete_data <- validated_data %>%
  left_join(tmdb, by = "tconst")

# Inventory management
rm(validated_data, tmdb)

# Simplify the dataset by creating weighted average rating
complete_data <- complete_data %>%
  mutate(
    averageRating = (IMDb_rating * IMDb_votecount + TMDB_rating * TMDB_votecount) / 
      (IMDb_votecount + TMDB_votecount),
    numVotes = (IMDb_votecount + TMDB_votecount)
  )

# Round it to 2
complete_data$averageRating <- round(complete_data$averageRating, 2)

# Remove original rating variables
complete_data <- complete_data %>%
  select(-IMDb_rating, -IMDb_votecount, -TMDB_rating, -TMDB_votecount)

# Reorder for clarity
complete_data <- complete_data %>%
  select(
    tconst,
    id,
    title,
    releaseYear,
    release_cinema_wide,
    release_streaming,
    release_window,
    viewing_30days,
    viewing_60days,
    viewing_90days,
    streaming_platform,
    budget,
    production_budget,
    domestic_gross,
    worldwide_gross,
    genres,
    blockbuster_score,
    runtimeMinutes,
    averageRating,
    numVotes
  )

# Sanity check dataset
colSums(is.na(complete_data)) #no issues

# Save dataset
write.csv(complete_data, "../data/complete_data.csv", row.names = FALSE)
