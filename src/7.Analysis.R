# Load libraries
library(tidyverse)

#Load dataset
df <- read_csv("../data/new_cinema_streaming_data.csv")

#Remove missign cases for now
df <- df %>%
  filter(!is.na(title))

#Mean-center the IV and moderator before creating the interaction
df <- df %>%
  mutate(
    domestic_gross_c  = scale(domestic_gross,  center = TRUE, scale = FALSE),
    release_window_c  = scale(release_window,  center = TRUE, scale = FALSE)
  )

#Run the model
model <- lm(viewing_30days ~ domestic_gross_c * release_window_c + runtimeMinutes + COVID + IMDb_rating + 
            production_budget, data = df); summary(model)
