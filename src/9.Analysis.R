# Load libraries
library(tidyverse)

#Load dataset
minimised_data <- read_csv("../data/minimised_data.csv")

#Remove missign cases for now
minimised_data <- minimised_data %>%
  filter(!is.na(title))

#Mean-center the IV and moderator before creating the interaction
minimised_data <- minimised_data %>%
  mutate(
    domestic_gross_c  = scale(domestic_gross,  center = TRUE, scale = FALSE),
    release_window_c  = scale(release_window,  center = TRUE, scale = FALSE)
  )

#Run the model
model <- lm(viewing_30days ~ domestic_gross_c * release_window_c + runtimeMinutes + COVID + IMDb_rating + 
            production_budget, data = minimised_data); summary(model)
