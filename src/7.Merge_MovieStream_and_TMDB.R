# Load libraries
library(tidyverse)

# Load dataset
cinema_streaming_data <- read_csv("../data/cinema_streaming_data.csv")
tmdb <- read_csv("../data/tmdb.csv")

# Merge based on tconst
complete_data <- cinema_streaming_data %>%
  left_join(tmdb, by = "tconst")

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
    releaseMonth,
    release_cinema,
    release_streaming,
    viewing_30days,
    streaming_platform,
    production_budget,
    domestic_gross,
    worldwide_gross,
    runtimeMinutes,
    genres,
    blockbuster_score,
    averageRating,
    numVotes,
    COVID,
    competition
  )

# Sanity check dataset
colSums(is.na(complete_data)) #no issues

# Inventory management
rm(cinema_streaming_data, tmdb)

# Save dataset
write.csv(complete_data, "../data/complete_data.csv", row.names = FALSE)
