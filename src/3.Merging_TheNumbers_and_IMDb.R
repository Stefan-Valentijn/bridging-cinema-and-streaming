# Load libraries
library(tidyverse)
library(lubridate)
library(stringdist)

# Load data
imdb <- read_csv("../data/imdb.csv")
thenumbers <- read_csv("../data/thenumbers.csv")

# Spelling of the movies is highly sensitive to errors in matching. This is solved with
thenumbers <- thenumbers %>%
  mutate(title = gsub("'|’|`|:|,|\\.", "", title) %>% str_to_title())

imdb <- imdb %>%
  mutate(primaryTitle = gsub("'|’|`|:|,|\\.", "", primaryTitle) %>% str_to_title())

#There are 3 movies with double titles that will give trouble. Rename
thenumbers <- thenumbers %>%
  mutate(title = case_when(
    title == "Robin Hood" & str_detect(release_year, "2010") ~ "Robin Hood (2010)",
    title == "Robin Hood" & str_detect(release_year, "2018") ~ "Robin Hood (2018)",
    title == "The Invitation" & str_detect(release_year, "2022") ~ "The Invitation (2022)",
    title == "The Invitation" & str_detect(release_year, "2016") ~ "The Invitation (2016)",
    title == "The Square" & str_detect(release_year, "2010") ~ "The Square (2010)",
    title == "The Square" & str_detect(release_year, "2013") ~ "The Square (2013)",
    TRUE ~ title
  ))

#Rename
imdb <- imdb %>%
  mutate(primaryTitle = case_when(
    primaryTitle == "Robin Hood" & startYear == 2010 ~ "Robin Hood (2010)",
    primaryTitle == "Robin Hood" & startYear == 2018 ~ "Robin Hood (2018)",
    primaryTitle == "The Invitation" & startYear == 2022 ~ "The Invitation (2022)",
    primaryTitle == "The Invitation" & startYear == 2016 ~ "The Invitation (2016)",
    primaryTitle == "The Square" & startYear == 2010 ~ "The Square (2010)",
    primaryTitle == "The Square" & startYear == 2013 ~ "The Square (2013)",
    TRUE ~ primaryTitle
  ))

# Merge for names that are the same
merged_moviedata <- thenumbers %>% left_join(imdb, by = c("title" = "primaryTitle"))

cat(sprintf("nrow(thenumbers) = %d is not equal to nrow(merged_moviedata) = %d", 
            nrow(thenumbers), nrow(merged_moviedata)))

# For movies with double info in the dataset, select the one corresponding to the release year of IMDb
merged_moviedata <- merged_moviedata %>%
  mutate(release_year_num = as.integer(str_extract(release_year, "\\d{4}"))) %>%
  group_by(title) %>%
  filter(n() == 1 | abs(release_year_num - startYear) == min(abs(release_year_num - startYear))) %>%
  ungroup() %>%
  select(-release_year_num)

# There are still some duplicates, select the ones with the highest number of votes
merged_moviedata <- merged_moviedata %>%
  group_by(title) %>%
  slice_max(numVotes, n = 1, with_ties = FALSE) %>%
  ungroup()

# Amount of rows thenumbers is now the same
nrow(merged_moviedata)
nrow(thenumbers)

# But there are still missing tconst movies
matched  <- merged_moviedata %>% filter(!is.na(tconst))
missing  <- merged_moviedata %>% filter(is.na(tconst)) %>% select(title, release_year, production_budget, domestic_gross, worldwide_gross, release_date)

missing_matched <- missing %>%
  rowwise() %>%
  mutate(
    best_match = imdb$primaryTitle[which.max(stringsim(str_to_lower(title),
                                                       str_to_lower(imdb$primaryTitle),
                                                       method = "jw"))],
    similarity = max(stringsim(str_to_lower(title),
                               str_to_lower(imdb$primaryTitle),
                               method = "jw"))
  ) %>%
  ungroup()

#Filter the ones that are significantly related to the title
missing_valid <- missing_matched %>%
  filter(similarity >= 0.95)

missing_complete <- missing_valid %>% left_join(imdb, by = c("best_match" = "primaryTitle"))

#Remove columns for merging
missing_complete <- missing_complete %>% select(-best_match, -similarity)


# Merging final set
moviedata <- bind_rows(matched, missing_complete)

# Inventory management
rm(imdb, matched, merged_moviedata, missing, missing_complete, missing_matched, missing_valid, thenumbers)

# Adjust date variables
final_moviedata <- moviedata %>%
  mutate(IMDb_rating = averageRating,
         IMDb_votecount = numVotes,
         releaseDate = format(release_date, "%d-%m-%Y"),
         releaseYear = format(release_date, "%Y"),
         releaseMonth = month(releaseDate, label = TRUE, abbr = FALSE))

# Reorder the dataset variables for clarity
final_moviedata <- final_moviedata %>%
  select(tconst, 
         title, 
         releaseDate,
         releaseYear,
         releaseMonth,
         production_budget, 
         domestic_gross, 
         worldwide_gross,
         runtimeMinutes, 
         genres,
         IMDb_rating, 
         IMDb_votecount
  )

# Dataset is now finalised
colSums(is.na(final_moviedata))

# Inventory mgt
rm(moviedata)

# Save merged file
write.csv(final_moviedata, "../data/moviedata.csv", row.names = FALSE)

#For input dataset
moviedata_newinput <- moviedata %>%
  select(tconst, title, releaseDate, releaseYear, releaseMonth)

write.csv(moviedata_newinput, "../data/moviedata_newinput.csv", row.names = FALSE)
