# Load libraries
library(tidyverse)
library(lubridate)

# Load dataset
data <- read.csv("../data/data.csv")

# Cross-validation for the release date makes another saving of a file
data <- data %>%
  mutate(date_difference = as.numeric(as.Date(cinema_release_TMDB) - as.Date(release_cinema)))

# There are cases with a different release date and some are missing, these are further crossvalidated with IMDb as a source
table(data$date_difference == 0, useNA = "ifany")

# For computational expensiveness, only the ones with a deviating or missing date difference are further crossvalidated
data_false <- data %>% filter(date_difference != 0 | is.na(date_difference))
data_true <- data %>% filter(date_difference == 0)

# Save dataset
write.csv(data_false, "../data/crossvalidation.csv", row.names = FALSE)
write.csv(data_true, "../data/verified.csv", row.names = FALSE)
