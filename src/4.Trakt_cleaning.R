# Load libraries
library(tidyverse)
library(lubridate)

# Load dataset
raw_streaming <- read_csv("../data/raw/viewing_movies (3).csv")

# Impression dataset
summary(raw_streaming)
colSums(is.na(raw_streaming))

# There are more 
nrow(raw_streaming) #rows than
length(unique(raw_streaming$tconst)) #amount of movies

# For movies with multiple services releases, these are clustered
streaming_data <- raw_streaming %>%
  group_by(tconst, release_day, viewing_90days, viewing_60days, viewing_30days) %>%
  mutate(service = paste(service, collapse = " + ")) %>%
  distinct() %>%
  ungroup()

# If there are multiple release dates, the earliest one is taken
streaming_data <- streaming_data %>%
  group_by(tconst) %>%
  filter(release_day == min(release_day, na.rm = TRUE)) %>%
  ungroup()

# There are now 1 row 1 movie
nrow(streaming_data) # now the same as
length(unique(streaming_data$tconst)) # the amount of movie

# Assess the correlation between the different dependent variables
cor(raw_streaming[, c("viewing_30days", "viewing_60days", "viewing_90days")]) # all of them are highly correlated

# Rename variables for clarity
streaming_data <- streaming_data %>%
  rename(streaming_platform = service,
         release_streaming = release_day)

# Inventory management
rm(raw_streaming)

# Convert streaming dataset to a csv-file
write.csv(streaming_data, "../data/streaming_data.csv", row.names = FALSE)
