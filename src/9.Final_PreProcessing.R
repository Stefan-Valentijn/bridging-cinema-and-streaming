# Load libraries
library(tidyverse)
library(lubridate)

# Load dataset
complete_data <- read_csv("../data/complete_data.csv")

# Feature engineer release window
cinestreamdata <- complete_data %>%
  mutate(
    wide_release_cinema = as.Date(wide_release_cinema, format = "%Y-%m-%d"),
    release_streaming   = as.Date(release_streaming,   format = "%Y-%m-%d"),
    release_window      = as.numeric(difftime(release_streaming, wide_release_cinema, units = "days"))
  ) %>%
  relocate(release_window, .after = release_streaming)

# Inventory management
rm(complete_data)

# Inspection of release windows
ggplot(cinestreamdata, aes(x = release_window)) +
  geom_histogram(binwidth = 30, fill = "#378ADD", color = "white") +
  labs(
    title = "Verdeling van Release Window",
    x = "Release window (dagen)",
    y = "Aantal films"
  ) +
  theme_minimal()




# There are some releases with negative release windows. that is not possible
cinestreamdata <- cinestreamdata %>%
  filter(release_window >= 0)



alldata <- cinestreamdata %>%
  filter(as.numeric(format(as.Date(release_streaming), "%d")) != 1)



# The input dataset was with movies from 2010, but the streaming metrics were first measured in 2017.
# Therefore filter that as the starting point of when movies had to be released
alldata <- cinestreamdata %>% filter(wide_release_cinema >= as.Date("2017-01-01"))

# Release windows that comprise years are not a strategic consideration and thus removed
alldata <- cinestreamdata %>%
  filter(release_window <= 365)



write.csv(cinestreamdata, "../data/cinestreamdata.csv", row.names = FALSE)
