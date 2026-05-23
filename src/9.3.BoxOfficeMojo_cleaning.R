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

# Make it neat
finaldataset <- finaldataset %>%
  relocate(domestic_opening, .after = production_budget)

# Two movies have na, after research these are manually coded to
finaldataset <- finaldataset %>%
  mutate(domestic_opening = case_when(
    tconst == "tt1838556" ~ 3607966,
    tconst == "tt7734218" ~ 8225384,
    TRUE ~ domestic_opening
  ))

# See if opening DV is legit
cor.test(finaldataset$domestic_opening, finaldataset$domestic_gross)
# highly sig correlate so yes

# Save dataset
write.csv(finaldataset, "../data/finaldataset.csv", row.names = FALSE)

