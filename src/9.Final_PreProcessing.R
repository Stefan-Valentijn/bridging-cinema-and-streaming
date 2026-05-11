# Load libraries
library(tidyverse)
library(lubridate)

# Load dataset
validated_data <- read_csv("../data/validated_data.csv")

# Feature engineer release window
cinestreamdata <- validated_data %>%
  mutate(
    release_window      = as.numeric(difftime(release_streaming, release_cinema_wide, units = "days"))
  ) %>%
  relocate(release_window, .after = release_streaming)

# Inventory management
rm(validated_data)

# Inspection of release windows
ggplot(cinestreamdata, aes(x = release_window)) +
  geom_histogram(binwidth = 30, fill = "#378ADD", color = "white") +
  theme_minimal()

# There are some releases with negative release windows. that is not possible
cinestreamdata <- cinestreamdata %>%
  filter(release_window >= 0)





#filter 1
alldata <- cinestreamdata %>% filter(release_cinema_wide >= as.Date("2017-01-01"))

#filter2
alldata <- cinestreamdata %>%
  filter(release_window <= 472)





write.csv(cinestreamdata, "../data/cinestreamdata.csv", row.names = FALSE)
