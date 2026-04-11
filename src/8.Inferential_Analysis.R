# Load libraries
library(tidyverse)

# Load dataset
df <- read_csv("../data/cinema_streaming_data.csv")

# Mean-center the IV and moderator before creating the interaction
df <- df %>%
  mutate(
    domestic_gross_c  = scale(domestic_gross,  center = TRUE, scale = FALSE),
    release_window_c  = scale(release_window,  center = TRUE, scale = FALSE)
  )

# Estimates in model printed instead of scientific notation
options(scipen = 999)

# Run the model
model <- lm(viewing_30days ~ domestic_gross_c * release_window_c + runtimeMinutes + COVID + IMDb_rating + 
              production_budget + releaseYear + blockbuster_score, data = df); summary(model); 

# Cleaner results
round(summary(model)$coefficients, 3)

df$production_budget_z <- scale(df$production_budget)
df$domestic_gross_z    <- scale(df$domestic_gross)
df$release_window_z    <- scale(df$release_window)

# Moderated Mediation
process(
  data = df,
  y = "viewing_30days",
  x = "production_budget_z",
  m = "domestic_gross_z",
  w = "release_window_z",
  cov = "COVID",
  model = 14,
  boot = 5000,
  seed = 123
)