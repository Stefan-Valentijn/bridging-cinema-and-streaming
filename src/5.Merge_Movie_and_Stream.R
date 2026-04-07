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

# Calculate release window
cinema_streaming_data <- cinema_streaming_data %>%
  mutate(
    release_cinema    = as.Date(release_cinema,    format = "%d-%m-%Y"),
    release_streaming = as.Date(release_streaming, format = "%d-%m-%Y"),
    release_window    = as.numeric(difftime(release_streaming, release_cinema, units = "days"))
  )

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

# Impression dataset
summary(cinema_streaming_data)
colSums(is.na(cinema_streaming_data)) #no missing values

# Save dataset
write.csv(cinema_streaming_data, "../data/cinema_streaming_data.csv", row.names = FALSE)
