# Load libraries
library(tidyverse)
library(lubridate)

# Load dataset
raw_streaming <- read_csv("../data/viewing_movies.csv")

#Some tconst have double rows, these are combined so 1 row = 1 movie
streaming_data <- raw_streaming %>%
  group_by(tconst) %>%
  mutate(max_release = max(release_day)) %>%
  filter(release_day == max_release) %>%        # keep only most recent release_day rows
  summarise(
    release_day      = first(release_day),
    viewing_90days   = mean(viewing_90days, na.rm = TRUE),
    viewing_60days   = mean(viewing_60days, na.rm = TRUE),
    viewing_30days   = mean(viewing_30days, na.rm = TRUE),
    streaming_platform = paste(sort(unique(service)), collapse = " + "),
    .groups = "drop"
  )

# Inventory management
rm(raw_streaming)

# Convert streaming dataset to a csv-file
write.csv(streaming_data, "../data/streaming_data.csv", row.names = FALSE)
