# Load libraries
library(tidyverse)
library(lubridate)

# Load dataset
data_crossvalidation <- read.csv("../data/release_dates.csv")
data_false <- read_csv("../data/crossvalidation.csv")
data_true  <- read_csv("../data/verified.csv")

# Merge files
crossval <- data_false %>% 
  left_join(data_crossvalidation, by = "tconst")

# Inventory mgt
rm(data_false, data_crossvalidation)

# Change release date to correct one when wide is provided
crossval <- crossval %>%
  mutate(
    wide_release_cinema = as.Date(if_else(notes != "wide", as.character(release_cinema), as.character(us_release_date)))
  )

# Adjust variables in both sets so they can be merged with ease
crossval <- crossval %>% select(
  tconst,
  id,
  title,
  wide_release_cinema,
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

# Select columns
data_true <- data_true %>%
  rename(wide_release_cinema = release_cinema) %>%
  select(
    tconst,
    id,
    title,
    wide_release_cinema,
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

# Now merge
complete_data <- bind_rows(crossval, data_true)

# Inventory management
rm(crossval, data_true)

# Save file
write.csv(complete_data, "../data/complete_data.csv", row.names = FALSE)
