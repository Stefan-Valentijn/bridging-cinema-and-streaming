# Load libraries
library(tidyverse)

# Load datasets
validated_data <- read_csv("../data/validated_data.csv")
bom1 <- read_csv("../data/part1_boxofficemojo.csv")
bom2 <- read_csv("../data/part2_boxofficemojo.csv")

# Select relevant variables
bom1 <- bom1 %>%
  select(tconst, domestic_opening)

bom2 <- bom2 %>%
  select(tconst, total_playing, new_releases)

# Merge
interdataset <- validated_data %>%
  left_join(bom1, by = "tconst") %>%
  left_join(bom2, by = "tconst")

# Inventory mgt
rm(bom1, bom2, validated_data)

# Make it neat
interdataset <- interdataset %>%
  relocate(domestic_opening, .after = production_budget)

# Two movies have na, after research these are manually coded to
interdataset <- interdataset %>%
  mutate(domestic_opening = case_when(
    tconst == "tt1838556" ~ 3607966,
    tconst == "tt7734218" ~ 8225384,
    TRUE ~ domestic_opening
  ))

# Save dataset
write.csv(interdataset, "../data/interdataset.csv", row.names = FALSE)
