# Load libraries
library(tidyverse)

# Load datasets
cinestreamdata <- read_csv("../data/cinestreamdata.csv")
bom1 <- read_csv("../data/part1_boxofficemojo.csv")
bom2 <- read_csv("../data/part2_boxofficemojo.csv")

# Select relevant variables
bom1 <- bom1 %>%
  select(tconst, total_playing, new_releases)

bom2 <- bom2 %>%
  select(tconst, domestic_opening)

# Merge
finaldataset <- cinestreamdata %>%
  left_join(bom1, by = "tconst") %>%
  left_join(bom2, by = "tconst")

# Inventory mgt
rm(bom1, bom2, cinestreamdata)

# Save dataset
write.csv(finaldataset, "../data/finaldataset.csv", row.names = FALSE)
