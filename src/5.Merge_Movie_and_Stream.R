# Load libraries
library(tidyverse)

# Load dataset
cinema <- read_csv("../data/movie_data.csv")          # resulting from script 3
streaming <- read_csv("../data/streaming_data.csv")   # resulting from script 4

###########
# MERGING #
###########

# Merge based on tconst
cinema_streaming_data <- streaming |> 
  left_join(cinema, by = "tconst")

# Inventory management
rm(cinema, streaming)

#######################
# FEATURE ENGINEERING #
#######################

# Feature engineer release window
cinema_streaming_data <- cinema_streaming_data %>%
  mutate(
    release_cinema    = as.Date(release_cinema,    format = "%d-%m-%Y"),
    release_streaming = as.Date(release_streaming, format = "%d-%m-%Y"),
    release_window    = as.numeric(difftime(release_streaming, release_cinema, units = "days"))
  )

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
    streaming_platform,
    production_budget,
    domestic_gross,
    worldwide_gross,
    runtimeMinutes,
    genres,
    blockbuster_score,
    IMDb_rating,
    IMDb_votecount,
    COVID,
    competition
  )

# There are some releases with negative release windows. that is not possible
cinema_streaming_data <- cinema_streaming_data %>%
  filter(release_window >= 0)

# Release windows that comprise years are not a strategic consideration and thus removed
filtered_set <- cinema_streaming_data %>%
  filter(release_window <= 365)








# Impression dataset
colSums(is.na(cinema_streaming_data)) #no missing values

# Save dataset
write.csv(cinema_streaming_data, "../data/cinema_streaming_data.csv", row.names = FALSE)
