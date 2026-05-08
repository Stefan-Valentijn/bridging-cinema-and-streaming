# Load libraries
library(tidyverse)
library(lubridate)

# Load dataset
alldata <- read.csv("../data/complete_data.csv")

test <- alldata %>% select(tconst, title, release_cinema, cinema_release)







#######################
# FEATURE ENGINEERING #
#######################

# Create weighted average rating
moviedata <- moviedata %>%
  mutate(
    averageRating = (IMDb_rating * IMDb_votecount + TMDB_rating * TMDB_votecount) / 
      (IMDb_votecount + TMDB_votecount),
    numVotes = (IMDb_votecount + TMDB_votecount)
  )

# Round it to 2
moviedata$averageRating <- round(moviedata$averageRating, 2)

# Remove original rating variables
moviedata <- moviedata %>%
  select(-IMDb_rating, -IMDb_votecount, -TMDB_rating, -TMDB_votecount)




# Feature engineer release window
alldata <- alldata %>%
  mutate(
    release_cinema    = as.Date(release_cinema,    format = "%d-%m-%Y"),
    release_streaming = as.Date(release_streaming, format = "%d-%m-%Y"),
    release_window    = as.numeric(difftime(release_streaming, release_cinema, units = "days"))
  )

# Reorder dataset for clarity
alldata <- alldata %>%
  select(
    tconst,
    title,
    releaseYear,
    releaseMonth,
    release_cinema,
    release_streaming,
    release_window,
    viewing_30days,
    streaming_platform,
    production_budget,
    domestic_gross,
    worldwide_gross,
    runtimeMinutes,
    genres,
    blockbuster_score,
    IMDb_rating,
    IMDb_votecount,
    COVID,
    competition
  )

# There are some releases with negative release windows. that is not possible
alldata <- alldata %>%
  filter(release_window >= 0)






# Despite input dataset from 2010, first streaming metrics were measured on 2017, therefore filter
df <- alldata %>% filter(release_cinema >= as.Date("2017-01-01"))

# Release windows that comprise years are not a strategic consideration and thus removed
df <- cinema_streaming_data %>%
  filter(release_window <= 180)





df2 <- df %>%
  filter(day(release_streaming) != 1)

pwr.f2.test(u = 6, f2 = 0.02, sig.level = 0.05, power = 0.80)
pwr.f2.test(u = 6, f2 = 0.15, sig.level = 0.05, power = 0.80)
pwr.f2.test(u = 6, f2 = 0.35, sig.level = 0.05, power = 0.80)



