# Load libraries
library(tidyverse)

# Load dataset
cinema_streaming_data <- read_csv("../data/cinema_streaming_data.csv")
tmdb <- read_csv("../data/tmdb.csv")

# Merge based on tconst
complete_data <- cinema_streaming_data %>%
  left_join(tmdb, by = "tconst")

# Sanity check dataset
colSums(is.na(complete_data)) #few issues

# Inventory management
rm(cinema_streaming_data, tmdb)

# Save dataset
write.csv(complete_data, "../data/complete_data.csv", row.names = FALSE)
