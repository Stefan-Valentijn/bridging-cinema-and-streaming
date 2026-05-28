# Load libraries
library(tidyverse)

# Load dataset
cinema <- read_csv("../data/movie_data.csv")          # resulting from script 3
streaming <- read_csv("../data/streaming_data.csv")   # resulting from script 4

# Merge based on tconst
cinema_streaming_data <- streaming |> 
  left_join(cinema, by = "tconst")

# Sanity check dataset
colSums(is.na(cinema_streaming_data)) #no missing values

# Inventory management
rm(cinema, streaming)

# Save dataset
write.csv(cinema_streaming_data, "../data/cinema_streaming_data.csv", row.names = FALSE)
