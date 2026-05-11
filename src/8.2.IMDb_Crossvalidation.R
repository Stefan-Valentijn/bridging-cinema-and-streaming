# Load libraries
library(tidyverse)
library(lubridate)

# Load dataset
complete_data <- read_csv("../data/complete_data.csv")
cross_validation_data <- read.csv("../data/cross_validation_data.csv")

# Merge datasets
validated_data <- merge(complete_data, cross_validation_data, by = "tconst", all.x = TRUE)

# Inventory management
rm(complete_data, cross_validation_data)

# Set to date format
validated_data <- validated_data %>%
  mutate(us_release_date = as.Date(us_release_date))

# Remove instances where the IMDb date is not retrieved as WIDE, the interest of the present study
validated_data <- validated_data %>%
  filter(is.na(notes) | !str_detect(notes, "fallback"))

# Sanity check to see how much the movies differ in release date
validated_data <- validated_data %>%
  mutate(date_diff = as.numeric(us_release_date - release_cinema))

table(cut(abs(validated_data$date_diff), 
          breaks = c(0, 1, 7, 14, 30, Inf), 
          labels = c("exact", "≤7 days", "≤14 days", "≤30 days", ">30 days"),
          right = FALSE,
          include.lowest = TRUE))

# Rename IMDb release date to contemporary one
validated_data <- validated_data %>%
  rename(release_cinema_wide = us_release_date) %>%
  relocate(release_cinema_wide, .before = release_streaming)

# Replace values
validated_data <- validated_data %>%
  mutate(
    releaseYear  = year(release_cinema_wide),
    releaseMonth = month(release_cinema_wide, label = TRUE, abbr = FALSE)
  )

# Remove variables
validated_data <- validated_data %>%
  select(-release_cinema, -us_release_date_raw, -notes, -date_diff)

# Save file
write.csv(validated_data, "../data/validated_data.csv", row.names = FALSE)
