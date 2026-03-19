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
cinema_data <- merged_moviedata %>%
  select(
    tconst,
    id,
    title,
    title_sanitycheck,
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

# Inventory management
rm(tmdb, moviedata, merged_moviedata)

# Save file
write.csv(cinema_data, "../data/cinema_data.csv", row.names = FALSE)
