# Load libraries
library(tidyverse)
library(lubridate)

# Load dataset
cinema_streaming_data <- read_csv("../data/cinema_streaming_data.csv")
cross_validation_data <- read.csv("../data/cross_validation_data.csv")

# Merge datasets
validated_data <- merge(cinema_streaming_data, cross_validation_data, by = "tconst", all.x = TRUE)

# Inventory management
rm(cinema_streaming_data, cross_validation_data)

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
  relocate(release_cinema_wide, .after = title)

# 2017 first reliable moment of measuring, therefore applied as a filter
validated_data <- validated_data %>% filter(release_cinema_wide >= as.Date("2017-01-01"))

# Replace values
validated_data <- validated_data %>%
  mutate(
    releaseYear  = year(release_cinema_wide),
    releaseMonth = month(release_cinema_wide, label = TRUE, abbr = FALSE)
  ) %>%
  relocate(releaseYear,  .after = title) %>%
  relocate(releaseMonth, .after = releaseYear)

# Remove variables
validated_data <- validated_data %>%
  select(-us_release_date_raw, -release_cinema, -notes, -date_diff)

# Feature engineer release window
validated_data <- validated_data %>%
  mutate(
    release_window      = as.numeric(difftime(release_streaming, release_cinema_wide, units = "days"))
  ) %>%
  relocate(release_window, .after = release_streaming)

# Inspection of release windows
ggplot(validated_data, aes(x = release_window)) +
  geom_histogram(binwidth = 30, fill = "#378ADD", color = "white") +
  theme_minimal()

# There are some releases with negative release windows. that is not possible
validated_data <- validated_data %>%
  filter(release_window >= 0)

# Crossvalidation of budget can now take place
#budget_deviations <- validated_data %>%
# filter(!is.na(budget) & !is.na(production_budget)) %>%
#filter(budget != production_budget) %>%
#select(tconst, title, budget, production_budget) %>%
#mutate(budget_diff = budget - production_budget)

#validated_data <- validated_data %>%
#filter(is.na(budget) | is.na(production_budget) | budget == production_budget)

# Save file
write.csv(validated_data, "../data/validated_data.csv", row.names = FALSE)
