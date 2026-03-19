# Load libraries
library(tidyverse)
library(lubridate)

# Load data
new_imdb <- read_csv("../data/imdb.csv")
new_thenumbers <- read_csv("../data/thenumbers.csv")

# Problems in merging arise because of inconsistencies in spelling (e.g. - and :)
# Therefore, adjustments are made

# Replace the ' in titles as this gives errors in merging both datasets
new_thenumbers <- new_thenumbers %>%
  mutate(title = gsub("’", "", title))

new_imdb <- new_imdb %>%
  mutate(primaryTitle = gsub("'", "", primaryTitle))

# Change names individually that cause merging problems
new_thenumbers <- new_thenumbers %>%
  mutate(title = case_when(
    title == "Mission: Impossible—The Final Reckoning" ~ "Mission: Impossible - The Final Reckoning",
    title == "Mission: Impossible Dead Reckoning Part One" ~ "Mission: Impossible - Dead Reckoning Part One",
    title == "Guardians of the Galaxy Vol 3" ~ "Guardians of the Galaxy Vol. 3",
    title == "Jurassic World Rebirth" ~ "Jurassic World: Rebirth",
    title == "Expend4bles" ~ "The Expendables 4",
    title == "DC League of Super Pets" ~ "DC League of Super-Pets",
    title == "From the World of John Wick: Ballerina" ~ "Ballerina",
    title == "AIR" ~ "AIR movie",
    title == "Ne Zha 2 (哪吒之魔童闹海)" ~ "Ne Zha 2",
    title == "Ruby Gillman, Teenage Kraken" ~ "Ruby Gillman: Teenage Kraken",
    title == "Gran Turismo: Based on a True Story" ~ "Gran Turismo",
    title == "Horizon: An American Saga Chapter 1" ~ "Horizon: An American Saga - Chapter 1",
    title == "Whitney Houston: I Wanna Dance With Somebody" ~ "Whitney Houston: I Wanna Dance with Somebody",
    title == "Den of Thieves 2: Pantera" ~ "Den of Thieves: Pantera",
    title == "Big George Foreman: The Miraculous Story of the…" ~ "Big George Foreman",
    title == "Godzilla Minus One (ゴジラ最新作)" ~ "Godzilla Minus One",
    title == "No Other Choice (어쩔수가없다)" ~ "No Other Choice",
    title == "jackass forever" ~ "Jackass Forever",
    title == "Anatomie dune chute" ~ "Anatomy of a Fall",
    title == "Talk To Me" ~ "Talk to Me",
    title == "Flow, le chat qui navait plus peur de leau" ~ "Flow",
    title == "TAYLOR SWIFT | THE ERAS TOUR" ~ "Taylor Swift: The Eras Tour",
    title == "Dark Phoenix" ~ "X-Men: Dark Phoenix",
    title == "Dune" ~ "Dune: Part One",
    title == "Venom: Let There be Carnage" ~ "Venom: Let There Be Carnage",
    title == "Ford v. Ferrari" ~ "Ford v Ferrari",
    title == "Bad Boys For Life" ~ "Bad Boys for Life",
    title == "Sonic The Hedgehog" ~ "Sonic the Hedgehog",
    title == "Once Upon a Time…in Hollywood" ~ "Once Upon a Time in... Hollywood",
    title == "Birds of Prey (And the Fantabulous Emancipation…" ~ "Birds of Prey",
    title == "John Wick: Chapter 3 — Parabellum" ~ "John Wick: Chapter 3 - Parabellum",
    title == "PLAYMOBIL" ~ "Playmobil: The Movie",
    title == "The Wandering Earth (流浪地球)" ~ "The Wandering Earth",
    title == "American Underdog: The Kurt Warner Story" ~ "American Underdog",
    title == "Tyler Perrys A Madea Family Funeral" ~ "A Madea Family Funeral",
    title == "Whered You Go Bernadette" ~ "Whered You Go, Bernadette",
    title == "Parasite (기생충)" ~ "Parasite",
    title == "Fighting With My Family" ~ "Fighting with My Family",
    title == "BrightBurn" ~ "Brightburn",
    title == "Sh*thouse" ~ "Shithouse",
    title == "Demon Slayer: Kimetsu no Yaiba—The Movie: Mugen…" ~ "Demon Slayer: Kimetsu no Yaiba - The Movie: Mugen Train",
    TRUE ~ title
  ))

# Change names individually that cause merging problems
new_imdb <- new_imdb %>%
  mutate(
    primaryTitle = case_when(
      tconst == "tt6208148" ~ "Disneys Snow White",
      tconst == "tt16419074" ~ "AIR movie",
      tconst == "tt30274401" ~ "Five Nights at Freddys 2",
      tconst == "tt2328678" ~ "The Kings Daughter",
      tconst == "tt4712810" ~ "Now You See Me: Now You Dont",
      tconst == "tt16280138" ~ "Magic Mikes Last Dance",
      tconst == "tt13521006" ~ "Beau is Afraid",
      tconst == "tt10731256" ~ "Dont Worry, Darling",
      tconst == "tt32214143" ~ "Gabbys Dollhouse: The Movie",
      tconst == "tt10655524" ~ "It Ends With Us",
      tconst == "tt6710474" ~ "Everything Everywhere All At Once",
      tconst == "tt31844586" ~ "SISU: Road to Revenge",
      tconst == "tt9185206" ~ "Are You There God? Its Me, Margaret",
      tconst == "tt2527338" ~ "Star Wars: The Rise of Skywalker",
      tconst == "tt6320628" ~ "Spider-Man: Far From Home",
      tconst == "tt3513498" ~ "The LEGO Movie 2: The Second Part",
      tconst == "tt8404256" ~ "Snake Eyes: G.I. Joe Origins",
      tconst == "tt7713068" ~ "Birds of Prey",
      tconst == "tt8385148" ~ "The Hitmans Wifes Bodyguard",
      tconst == "tt8332922" ~ "A Quiet Place: Part II",
      tconst == "tt2452244" ~ "Isnt it Romantic",
      tconst == "tt6423362" ~ "The Sun is Also a Star",
      tconst == "tt13925862" ~ "Bo Gia",
      tconst == "tt9845110" ~ "Deux",
      tconst == "tt7958736" ~ "MA",
      tconst == "tt8613070" ~ "Portrait de la jeune fille en feu",
      tconst == "tt10370710" ~ "Verdens verste menneske",
      tconst == "tt14850054" ~ "Greenland 2: Migration",
      tconst == "tt1987680" ~ "The Upside",
      tconst == "tt2140507" ~ "The Current War: Directors Cut",
      tconst == "tt22868010" ~ "Return to Silent Hill",
      tconst == "tt11285908" ~ "Baekdusan",
      tconst == "tt8291806" ~ "Dolor y gloria",
      tconst == "tt11655202" ~ "Retfærdighedens Ryttere",
      tconst == "tt10288566" ~ "Druk",
      tconst == "tt7131622" ~ "Once Upon a Time in... Hollywood",
      TRUE ~ primaryTitle
    )
  )


#Merge the two files together
merged_moviedata <- new_thenumbers %>% left_join(new_imdb, by = c("title" = "primaryTitle"))


#Solve duplicates
filtered_moviedata <- merged_moviedata %>%
  filter(
    !(title == "The Little Mermaid" & tconst != "tt5971474") &
      !(title == "Wicked" & tconst != "tt1262426") &
      !(title == "Fly Me to the Moon" & tconst != "tt1896747") &
      !(title == "Ballerina" & tconst != "tt7181546") &
      !(title == "Devotion" & tconst != "tt7693316") &
      !(title == "Nosferatu" & tconst != "tt5040012") &
      !(title == "Here" & tconst != "tt18272208") &
      !(title == "Anaconda" & tconst != "tt33244668") &
      !(title == "Beast" & tconst != "tt13223398") &
      !(title == "The Housemaid" & tconst != "tt27543632") &
      !(title == "Abigail" & tconst != "tt27489557") &
      !(title == "Love Hurts" & tconst != "tt30788842") &
      !(title == "Smile" & tconst != "tt15474916") &
      !(title == "Speak No Evil" & tconst != "tt27534307") &
      !(title == "Christy" & tconst != "tt32323252") &
      !(title == "Missing" & tconst != "tt10855768") &
      !(title == "Presence" & tconst != "tt28249919") &
      !(title == "Companion" & tconst != "tt26584495") &
      !(title == "Abominable" & tconst != "tt6324278") &
      !(title == "Afraid" & tconst != "tt24577462") &
      !(title == "After" & tconst != "tt4126476") &
      !(title == "Anna" & tconst != "tt7456310") &
      !(title == "Black Widow" & tconst != "tt3480822") &
      !(title == "Breakthrough" & tconst != "tt7083526") &
      !(title == "Cats" & tconst != "tt5697572") &
      !(title == "Countdown" & tconst != "tt10039344") &
      !(title == "Dog" & tconst != "tt11252248") &
      !(title == "Eden" & tconst != "tt23149780") &
      !(title == "Escape Room" & tconst != "tt5886046") &
      !(title == "Ferrari" & tconst != "tt3758542") &
      !(title == "Firestarter" & tconst != "tt1798632") &
      !(title == "Little Women" & tconst != "tt3281548") &
      !(title == "Love and Monsters" & tconst != "tt2222042") &
      !(title == "Mercy" & tconst != "tt26439204") &
      !(title == "Napoleon" & tconst != "tt13287846") &
      !(title == "No Hard Feelings" & tconst != "tt15671028") &
      !(title == "Nobody" & tconst != "tt7888964") &
      !(title == "Pinocchio" & tconst != "tt8333746") &
      !(title == "Reminiscence" & tconst != "tt3272066") &
      !(title == "Rocketman" & tconst != "tt2066051") &
      !(title == "Sketch" & tconst != "tt26238710") &
      !(title == "Soul" & tconst != "tt2948372") &
      !(title == "Spiral" & tconst != "tt10342730") &
      !(title == "Summerland" & tconst != "tt6841122") &
      !(title == "The Assistant" & tconst != "tt9000224") &
      !(title == "The Bad Guys" & tconst != "tt8115900") &
      !(title == "The Climb" & tconst != "tt5157682") &
      !(title == "The Father" & tconst != "tt10272386") &
      !(title == "The Hunt" & tconst != "tt8244784") &
      !(title == "The Intruder" & tconst != "tt6722030") &
      !(title == "The Invisible Man" & tconst != "tt1051906") &
      !(title == "The Kitchen" & tconst != "tt5822564") &
      !(title == "The Long Walk" & tconst != "tt10374610") &
      !(title == "The Reckoning" & tconst != "tt9182964") &
      !(title == "Trap" & tconst != "tt26753003") &
      !(title == "Unhinged" & tconst != "tt10059518") &
      !(title == "Wish" & tconst != "tt11304740") &
      !(title == "X" & tconst != "tt13560574")
  )


#Ensure there are no more duplicates; this table should be empty
filtered_moviedata %>%
  group_by(title) %>%
  filter(n() > 1) %>%
  arrange(title, tconst)

#Remove two movies with release year 2026, missing from IMDB
#filtered_moviedata <- filtered_moviedata %>%
#  filter(!title %in% c("Greenland 2: Migration", "Return to Silent Hill"))

#Hier nu code om te linken met streamen
filtered_moviedata <- filtered_moviedata %>%
  mutate(tconst = case_when(
    title == "Greenland 2: Migration" ~ "tt14850054",
    title == "Return to Silent Hill" ~ "tt22868010",
    TRUE ~ tconst  # Keep original tconst for all other rows
  ))


# Final dataset can be formed
moviedata <- filtered_moviedata

# Sanity check: is the amount of web scraped movies from The Numbers still the same
nrow(new_thenumbers) # original dataset
nrow(moviedata) # new dataset

# Inventory management
rm(new_imdb, merged_moviedata, filtered_moviedata, new_thenumbers)

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

# Save merged file
write.csv(moviedata, "../data/moviedata.csv", row.names = FALSE)
