# Load libraries
library(tidyverse)

# Load dataset
moviedata <- read_csv("../data/moviedata.csv")
tmdb <- read.csv("../data/tmdb.csv")

# Merge datasets
merged_moviedata <- moviedata %>% left_join(tmdb, by = c("tconst" = "tconst"))

# Filter countries that have US as AT LEAST one of the production companies
merged_moviedata <- merged_moviedata %>%
  filter(grepl("US", production_countries))

# Reorder variables (title sanity check is left out)


# Inventory management
rm(tmdb, moviedata, merged_moviedata)

# Save file
write.csv(cinema_data, "../data/cinema_data.csv", row.names = FALSE)
