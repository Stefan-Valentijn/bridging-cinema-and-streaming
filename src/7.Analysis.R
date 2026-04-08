# Load libraries
library(tidyverse)

# Load dataset
df <- read_csv("../data/cinema_streaming_data.csv")

# Impression dataset
summary(df)
colSums(is.na(df))

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
            production_budget, data = df); summary(model); 

# Cleaner results
round(summary(model)$coefficients, 3)
