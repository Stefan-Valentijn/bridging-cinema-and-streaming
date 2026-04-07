# Load libraries
library(tidyverse)

# Original file is very big, therefore this has been narrowed done once
#raw_title_basics <- read_tsv("title.basics.tsv")
#raw_title_basics <- raw_title_basics %>% filter(titleType == "movie")
#write.csv(raw_title_basics, "raw_title_basics.csv", row.names = FALSE)

# Import datasets
raw_title_basics <- read_csv("../data/raw/raw_title_basics.csv")
raw_title_ratings <- read_tsv("../data/raw/raw_title.ratings.tsv")

# Merge the files
raw_imdb_total <- raw_title_basics %>%
  left_join(raw_title_ratings, by = "tconst")

# Select relevant columns for analysis (e.g there are variables specified to series, not movies)
merged_imdb <- raw_imdb_total %>% select(tconst,
                                      primaryTitle,
                                      startYear, 
                                      runtimeMinutes, 
                                      genres, 
                                      averageRating, 
                                      numVotes)

# Set variables right; convert character variable to numeric
merged_imdb$startYear <- as.numeric(merged_imdb$startYear)
merged_imdb$runtimeMinutes <- as.numeric(merged_imdb$runtimeMinutes)

# Attention: input dataset from 2010 to 2023
new_imdb <- merged_imdb %>% filter(startYear >= 2010 & startYear <= 2023,
                                   !is.na(averageRating),
                                   !is.na(runtimeMinutes),
                                   numVotes >= 100)

# Inventory management
rm(raw_title_basics, raw_title_ratings, raw_imdb_total, merged_imdb)

# Impression dataset
summary(new_imdb)
colSums(is.na(new_imdb))

# Convert imdb dataset to a csv-file
write.csv(new_imdb, "../data/imdb.csv", row.names = FALSE)
