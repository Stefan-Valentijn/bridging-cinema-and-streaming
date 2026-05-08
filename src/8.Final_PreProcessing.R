# Load libraries
library(tidyverse)
library(lubridate)

# Load dataset
complete_data <- read.csv("../data/complete_data.csv")






# The input dataset was with movies from 2010, but the streaming metrics were first measured in 2017.
# Therefore filter that as the starting point of when movies had to be released
alldata <- complete_data %>% filter(release_cinema >= as.Date("2016-01-01"))





#test <- alldata %>% select(tconst, title, release_cinema, cinema_release)

#test <- test %>%
#  mutate(date_difference = as.numeric(as.Date(cinema_release) - as.Date(release_cinema)))





#######################
# FEATURE ENGINEERING #
#######################

# Feature engineer release window
alldata <- alldata %>%
  mutate(
    release_cinema    = as.Date(release_cinema,    format = "%d-%m-%Y"),
    release_streaming = as.Date(release_streaming, format = "%d-%m-%Y"),
    release_window    = as.numeric(difftime(release_streaming, release_cinema, units = "days"))
  )

# There are some releases with negative release windows. that is not possible
alldata <- alldata %>%
  filter(release_window >= 0)


# Release windows that comprise years are not a strategic consideration and thus removed
alldata <- complete_data %>%
  filter(release_window <= 365)











df2 <- df %>%
  filter(day(release_streaming) != 1)

pwr.f2.test(u = 6, f2 = 0.02, sig.level = 0.05, power = 0.80)
pwr.f2.test(u = 6, f2 = 0.15, sig.level = 0.05, power = 0.80)
pwr.f2.test(u = 6, f2 = 0.35, sig.level = 0.05, power = 0.80)

