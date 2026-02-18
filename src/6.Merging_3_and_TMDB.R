# Load libraries
library(tidyverse)

# Load dataset
moviedata <- read_csv("../data/moviedata.csv")
tmdb <- read.csv("../data/tmdb.csv")

# Merge datasets
merged_moviedata <- moviedata %>% left_join(tmdb, by = c("tconst" = "tconst"))

# Inventory management
rm(tmdb, moviedata)

# Filter countries that have US as AT LEAST one of the production companies
merged_moviedata <- merged_moviedata %>%
  filter(grepl("US", production_countries))

# Reorder variables
merged_moviedata <- merged_moviedata %>%
  select(
    tconst,
    id,
    title,
    title_sanitycheck,
    releaseMonth,
    releaseYear,
    releaseDate,
    cinema_release,
    digital_release,
    streamingService,
    budget,
    production_budget,
    production_companies,
    production_countries,
    domestic_gross,
    worldwide_gross,
    runtimeMinutes,
    genres,
    IMDb_rating,
    IMDb_votecount,
    TMDB_rating,
    TMDB_votecount
  )

# Save file
write.csv(merged_moviedata, "../data/merged_moviedata.csv", row.names = FALSE)
