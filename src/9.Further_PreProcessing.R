# Load libraries
library(tidyverse)
library(lubridate)

# Load dataset
validated_data <- read_csv("../data/validated_data.csv")

# 2017 first reliable moment of measuring, therefore applied as a filter
cinestreamdata <- validated_data %>% filter(release_cinema_wide >= as.Date("2017-01-01"))

# Inventory management
rm(validated_data)

# Feature engineer COVID variable
cinestreamdata <- cinestreamdata %>%
  mutate(COVID = ifelse(release_cinema_wide >= as.Date("2020-03-17") & 
                          release_cinema_wide <= as.Date("2021-12-31"), 1, 0))

# Feature engineer release window
cinestreamdata <- cinestreamdata %>%
  mutate(
    release_window      = as.numeric(difftime(release_streaming, release_cinema_wide, units = "days"))
  ) %>%
  relocate(release_window, .after = release_streaming)

# Inspection of release windows
ggplot(cinestreamdata, aes(x = release_window)) +
  geom_histogram(binwidth = 30, fill = "#378ADD", color = "white") +
  theme_minimal()

# There are some releases with negative release windows. that is not possible
cinestreamdata <- cinestreamdata %>%
  filter(release_window >= 0)

# Crossvalidation of budget can now take place
#budget_deviations <- cinestreamdata %>%
 # filter(!is.na(budget) & !is.na(production_budget)) %>%
  #filter(budget != production_budget) %>%
  #select(tconst, title, budget, production_budget) %>%
  #mutate(budget_diff = budget - production_budget)

#cinestreamdata <- cinestreamdata %>%
  #filter(is.na(budget) | is.na(production_budget) | budget == production_budget)

# Save dataset
write.csv(cinestreamdata, "../data/cinestreamdata.csv", row.names = FALSE)
