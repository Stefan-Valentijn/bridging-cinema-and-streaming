# Load libraries
library(tidyverse)

# Load dataset
cinema <- read_csv("../data/cinema_data.csv")
streaming <- read_csv("../data/streaming_data.csv")

#Merge based on tconst
cinema_streaming_data <- streaming |> 
  left_join(cinema, by = "tconst")

# Inventory management
rm(cinema, streaming)

#Rename for clarity
cinema_streaming_data <- cinema_streaming_data %>%
  rename(
    release_streaming = release_day,
    release_cinema    = releaseDate
  )

#Calculate release window
cinema_streaming_data <- cinema_streaming_data %>%
  mutate(
    release_cinema    = as.Date(release_cinema,    format = "%d-%m-%Y"),
    release_streaming = as.Date(release_streaming, format = "%d-%m-%Y"),
    release_window    = as.numeric(difftime(release_streaming, release_cinema, units = "days"))
  )

cinema_streaming_data <- cinema_streaming_data %>%
  mutate(
    cinema_release  = as.Date(cinema_release,  format = "%d-%m-%Y"),
    digital_release = as.Date(digital_release, format = "%d-%m-%Y")
  )

#Add COVID variable
cinema_streaming_data <- cinema_streaming_data %>%
  mutate(COVID = ifelse(releaseYear %in% c(2020, 2021), 1, 0))



names(cinema_streaming_data)





#Reorder dataset for clarity
cinema_streaming_data <- cinema_streaming_data %>%
  select(
    tconst,
    id,
    title,
    release_cinema,
    cinema_release,
    release_streaming,
    digital_release,
    release_window,
    domestic_gross,
    worldwide_gross,
    production_companies,
    production_countries,
    streaming_platform,
    streamingService,
    viewing_90days,
    viewing_60days,
    viewing_30days,
    budget,
    production_budget,
    runtimeMinutes,
    genres,
    IMDb_rating,
    IMDb_votecount,
    TMDB_rating,
    TMDB_votecount,
    releaseYear,
    COVID
  )



#Minimised dataset without abundant TMDB info
minimised_data <- cinema_streaming_data %>%
  select(
    tconst,
    id,
    title,
    release_cinema,
    release_streaming,
    release_window,
    domestic_gross,
    worldwide_gross,
    streaming_platform,
    viewing_90days,
    viewing_60days,
    viewing_30days,
    production_budget,
    runtimeMinutes,
    genres,
    IMDb_rating,
    IMDb_votecount,
    TMDB_rating,
    TMDB_votecount,
    releaseYear,
    COVID
  )

#Inventory management
rm(cinema_streaming_data, streaming_data)

#Save minimislied dataset
write.csv(minimised_data, "../data/minimised_data.csv", row.names = FALSE)


