# Load libraries
library(tidyverse)
library(lubridate)
library(stringdist)

# Load data
thenumbers <- read_csv("../data/thenumbers.csv")    # resulting from script 1.1 and 1.2
imdb <- read_csv("../data/imdb.csv")                # resulting from script 2

# The problem is that The Numbers does NOT have the identifier from IMDb or another platform, which is needed
# in order to get the input dataset. Therefore, this script merges the two data streams into one

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

# Rename double titled movies
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

# Check 
nrow(thenumbers) 
nrow(merged_moviedata) # is not equal, so there is double information

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
missing  <- merged_moviedata %>% filter(is.na(tconst)) %>% select(title, release_year, production_budget, domestic_gross, worldwide_gross, release_date, tconst)

# Ai claude has inspected the missing values and assessed whether the IMDb file has the right tconst
missing_solved <- missing %>%
  mutate(tconst = case_when(
    title == "13 Assassins (十三人の刺客)" ~ "tt1436045",
    title == "To Live!" ~ "tt1706433",
    title == "21 And Over" ~ "tt1711425",
    title == "A Very Harold & Kumar 3d Christmas" ~ "tt1268799",
    title == "Goa" ~ "tt1421036",
    title == "Jasmina" ~ "tt1482161",
    title == "Alexander And The Terrible Horrible No Good …" ~ "tt1698641",
    title == "An Inconvenient Sequel" ~ "tt6322922",
    title == "Antarctic Edge 70º South" ~ "tt2780714",
    title == "Carrie" ~ "tt1939659",
    title == "Atlas Shrugged Part Ii" ~ "tt0480239",
    title == "Axl" ~ "tt5709188",
    title == "Beyond The Brick A Lego Brickumentary" ~ "tt3214286",
    title == "Birds Of Prey (And The Fantabulous Emancipation…" ~ "tt7713068",
    title == "Celeste And Jesse Forever" ~ "tt1405365",
    title == "Cowboys And Aliens" ~ "tt0409847",
    title == "Dark Phoenix" ~ "tt6565702",
    title == "Daybreakers" ~ "tt1220627",
    title == "Dc League Of Super Pets" ~ "tt8912936",
    title == "Demon Slayer Kimetsu No Yaiba—The Movie Mugen…" ~ "tt11032374",
    title == "Dr Seuss The Grinch" ~ "tt2709692",
    title == "Dr Seuss The Lorax" ~ "tt1482459",
    title == "Dragon Ball Super Broly (ドラゴンボール超スーパー ブロリー)…" ~ "tt7961060",
    title == "Dune" ~ "tt11384400",
    title == "El Clan" ~ "tt4411504",
    title == "Ernest Et Celestine" ~ "tt1816518",
    title == "Estiu 1993" ~ "tt5897636",
    title == "Everybody Wants Some" ~ "tt2937696",
    title == "Extremely Loud And Incredibly Close" ~ "tt0477302",
    title == "Fast And Furious 6" ~ "tt1905041",
    title == "Gnomeo And Juliet" ~ "tt0377981",
    title == "Godzilla Minus One (ゴジラ最新作)" ~ "tt23289160",
    title == "Harry Potter And The Deathly Hallows Part I" ~ "tt0926084",
    title == "Harry Potter And The Deathly Hallows Part Ii" ~ "tt1201607",
    title == "Hoodwinked Too Hood Vs Evil" ~ "tt0844993",
    title == "How Do You Know?" ~ "tt1341188",
    title == "John Wick Chapter 3 — Parabellum" ~ "tt6146586",
    title == "John Wick Chapter Two" ~ "tt4425200",
    title == "L!Fe Happens" ~ "tt1726589",
    title == "Love And Other Drugs" ~ "tt0758752",
    title == "Mamma Mia Here We Go Again!" ~ "tt6911608",
    title == "Men In Black 3" ~ "tt1409024",
    title == "Men Women And Children" ~ "tt3179568",
    title == "Mission Impossible Dead Reckoning Part One" ~ "tt9603212",
    title == "Mission Impossible—Fallout" ~ "tt4912910",
    title == "Mission Impossible—Ghost Protocol" ~ "tt1229238",
    title == "Mission Impossible—Rogue Nation" ~ "tt2381249",
    title == "Mr Popperss Penguins" ~ "tt1396218",
    title == "Oceans 8" ~ "tt5164214",
    title == "Once Upon A Time…In Hollywood" ~ "tt7131622",
    title == "Parasite (기생충)" ~ "tt6751668",
    title == "Planes Fire And Rescue" ~ "tt2980706",
    title == "Prince Of Persia Sands Of Time" ~ "tt0473075",
    title == "Ratchet And Clank" ~ "tt2865120",
    title == "Scary Movie V" ~ "tt0795461",
    title == "Sh*Thouse" ~ "tt11618536",
    title == "Shaun The Sheep" ~ "tt2872750",
    title == "She\\S Out Of My League" ~ "tt0815236",
    title == "Silent Hill Revelation 3d" ~ "tt0938330",
    title == "Solitary Man" ~ "tt1327763",
    title == "Spider-Man Into The Spider-Verse 3d" ~ "tt4633694",
    title == "Spy Kids All The Time In The World" ~ "tt1517489",
    title == "Spy!" ~ "tt3079380",
    title == "Star Wars Ep Vii The Force Awakens" ~ "tt2488496",
    title == "Star Wars Ep Viii The Last Jedi" ~ "tt2527336",
    title == "Star Wars The Rise Of Skywalker" ~ "tt2527338",
    title == "Taylor Swift | The Eras Tour" ~ "tt28814949",
    title == "The Age Of Shadows (밀정)" ~ "tt4914580",
    title == "The Assassin (刺客聶隱娘)" ~ "tt3508840",
    title == "The Chronicles Of Narnia The Voyage Of The Daw…" ~ "tt0980970",
    title == "The Disappearance Of Alice Creed" ~ "tt1572781",
    title == "The Fantastic Four" ~ "tt1502712",
    title == "The Ghouls" ~ "tt3613314",
    title == "The Hangover 3" ~ "tt1951261",
    title == "The Hitmans Wifes Bodyguard" ~ "tt8385148",
    title == "The Old Man And The Gun" ~ "tt2837574",
    title == "The Monkey King 2 (西游记之孙悟空三打白骨精)…" ~ "tt4591310",
    title == "The Twilight Saga Breaking Dawn Part 1" ~ "tt1324999",
    title == "The Twilight Saga Breaking Dawn Part 2" ~ "tt1673434",
    title == "The Wandering Earth (流浪地球)" ~ "tt7605074",
    title == "Tom And Jerry" ~ "tt1361336",
    title == "Train To Busan (부산행)" ~ "tt5700672",
    title == "Tucker & Dale Vs Evil" ~ "tt1465522",
    title == "Victoria And Abdul" ~ "tt5816682",
    title == "Walking With Dinosaurs" ~ "tt1762399",
    title == "Wall Street 2 Money Never Sleeps" ~ "tt1027718",
    title == "Yip Man 3" ~ "tt2888046",
    TRUE ~ tconst  # keep existing if no match
  ))

# Remve obscure movies
missing_solved <- missing_solved %>% filter(!is.na(tconst))
missing_complete <- missing_solved %>% left_join(imdb, by = "tconst")

# Merging final set
moviedata <- bind_rows(matched, missing_complete)
moviedata <- moviedata %>% select(-primaryTitle)

# Inventory management
rm(matched, merged_moviedata, missing, missing_solved, missing_complete)

# Adjust date variables
moviedata <- moviedata %>%
  mutate(IMDb_rating = averageRating,
         IMDb_votecount = numVotes,
         releaseDate = format(release_date, "%d-%m-%Y"),
         releaseYear = format(release_date, "%Y"),
         releaseMonth = month(releaseDate, label = TRUE, abbr = FALSE))

# Reorder the dataset variables for clarity
moviedata <- moviedata %>%
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

# Rename for clarity
moviedata <- moviedata %>% rename(
  release_cinema = releaseDate
  )

# Inventory mgt
rm(imdb, thenumbers)

# Impression dataset
summary(moviedata)
colSums(is.na(moviedata))

# Save merged file
write.csv(moviedata, "../data/movie_data.csv", row.names = FALSE)
