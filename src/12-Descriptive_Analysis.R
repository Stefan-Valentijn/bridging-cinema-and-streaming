# Load libraries
library(tidyverse)

# Load dataset
df <- read_csv("../data/bridging_cinema_and_streaming.csv")

# Impression dataset
summary(df)
colSums(is.na(df))

################################
# PART 1: CONTINUOUS VARIABLES #
################################

continuous_vars <- c(
  "viewing_30days",
  "log_viewing30",
  "domestic_opening",
  "log_domestic_opening",
  "release_window",
  "production_budget",
  "log_production_budget",
  "averageRating",
  "blockbuster_score"
)

# Minimum, maximum, mean and standard deviation
continuous_table <- df %>%
  select(all_of(continuous_vars)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
  group_by(Variable) %>%
  summarise(
    Min = round(min(Value,  na.rm = TRUE), 2),
    Max = round(max(Value,  na.rm = TRUE), 2),
    M   = round(mean(Value, na.rm = TRUE), 2),
    SD  = round(sd(Value,   na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(match(Variable, continuous_vars)); print(continuous_table)

#################################
# PART 2: CATEGORICAL VARIABLES #
#################################

n_total <- nrow(df)

# Streaming platform
platform_rows <- df %>%
  count(streaming_platform, sort = TRUE) %>%
  mutate(
    Group    = "Streaming Platform",
    Variable = streaming_platform,
    N        = n,
    Pct      = round(n / sum(n) * 100, 1)
  ) %>%
  select(Group, Variable, N, Pct); print(platform_rows)

# Release year
year_rows <- df %>%
  count(releaseYear, sort = FALSE) %>%
  mutate(
    Group    = "Release Year",
    Variable = as.character(releaseYear),
    N        = n,
    Pct      = round(n / sum(n) * 100, 1)
  ) %>%
  select(Group, Variable, N, Pct); print(year_rows)

# Genres
genre_rows <- df %>%
  select(tconst, genres) %>%
  separate_rows(genres, sep = ",") %>%
  mutate(genres = str_trim(genres)) %>%
  count(genres, sort = TRUE) %>%
  mutate(
    Group    = "Genre",
    Variable = genres,
    N        = n,
    Pct      = round(n / n_total * 100, 1)
  ) %>%
  select(Group, Variable, N, Pct); print(genre_rows) %>% print(n = Inf)
