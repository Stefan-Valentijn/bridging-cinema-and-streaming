# Load libraries
library(tidyverse)

# Load dataset
cinema <- read_csv("../data/movie_data.csv")          # resulting from script 3
streaming <- read_csv("../data/streaming_data.csv")   # resulting from script 4

# Merge based on tconst
cinema_streaming_data <- streaming |> 
  left_join(cinema, by = "tconst")

# Inventory management
rm(cinema, streaming)

# Feature engineer release window
cinema_streaming_data <- cinema_streaming_data %>%
  mutate(
    release_cinema    = as.Date(release_cinema,    format = "%d-%m-%Y"),
    release_streaming = as.Date(release_streaming, format = "%d-%m-%Y"),
    release_window    = as.numeric(difftime(release_streaming, release_cinema, units = "days"))
  )

# Remove instances with negative release windows. that is not possible
cinema_streaming_data <- cinema_streaming_data %>%
  filter(release_window >= 0)

# Add COVID variable
cinema_streaming_data <- cinema_streaming_data %>%
  mutate(COVID = ifelse(releaseYear %in% c(2020, 2021), 1, 0))

# Reorder dataset for clarity
cinema_streaming_data <- cinema_streaming_data %>%
  select(
    tconst,
    title,
    releaseYear,
    releaseMonth,
    release_cinema,
    release_streaming,
    release_window,
    viewing_30days,
    viewing_60days,
    viewing_90days,
    streaming_platform,
    production_budget,
    domestic_gross,
    worldwide_gross,
    runtimeMinutes,
    genres,
    IMDb_rating,
    IMDb_votecount,
    COVID
  )

# Blockbuster score as a proxy of incorporating genres
genre_scores <- cinema_streaming_data %>%
  select(genres, worldwide_gross) %>%
  mutate(genre = strsplit(genres, ",")) %>%
  unnest(genre) %>%
  mutate(genre = trimws(genre)) %>%
  group_by(genre) %>%
  summarise(mean_bo = mean(worldwide_gross, na.rm = TRUE)) %>%
  arrange(desc(mean_bo))

# Then normalise
genre_weights <- genre_scores %>%
  mutate(score = (mean_bo - min(mean_bo)) / (max(mean_bo) - min(mean_bo))) %>%
  select(genre, score) %>%
  deframe()

cinema_streaming_data <- cinema_streaming_data %>%
  mutate(blockbuster_score = sapply(genres, function(g) {
    gs <- trimws(strsplit(g, ",")[[1]])
    mean(genre_weights[gs], na.rm = TRUE)  # average across genres per film
  }))

# Impression dataset
summary(cinema_streaming_data)
colSums(is.na(cinema_streaming_data)) #no missing values

# Inventory mangement
rm(genre_scores, genre_weights)

# Save dataset
write.csv(cinema_streaming_data, "../data/cinema_streaming_data.csv", row.names = FALSE)
