# Load libraries
library(tidyverse)

# Load dataset
interdataset <- read_csv("../data/interdataset.csv")
tmdb <- read_csv("../data/tmdb.csv")

# Merge based on tconst
finaldataset <- interdataset %>%
  left_join(tmdb, by = "tconst")

# Inventory management
rm(interdataset, tmdb)

# Simplify the dataset by creating weighted average rating
finaldataset <- finaldataset %>%
  mutate(
    averageRating = (IMDb_rating * IMDb_votecount + TMDB_rating * TMDB_votecount) / 
      (IMDb_votecount + TMDB_votecount),
    numVotes = (IMDb_votecount + TMDB_votecount)
  )

# Round it to 2
finaldataset$averageRating <- round(finaldataset$averageRating, 2)

# Remove original rating variables
finaldataset <- finaldataset %>%
  select(-IMDb_rating, -IMDb_votecount, -TMDB_rating, -TMDB_votecount)

# Reorder for clarity
finaldataset <- finaldataset %>%
  select(
    tconst,
    id,
    title,
    releaseYear,
    new_releases,
    release_cinema_wide,
    release_streaming,
    release_window,
    viewing_30days,
    viewing_60days,
    viewing_90days,
    streaming_platform,
    budget,
    production_budget,
    domestic_opening,
    domestic_gross,
    worldwide_gross,
    genres,
    blockbuster_score,
    runtimeMinutes,
    averageRating,
    numVotes
  )

# Sanity check dataset
colSums(is.na(finaldataset)) #no issues

# Save dataset
write.csv(finaldataset, "../data/finaldataset.csv", row.names = FALSE)
