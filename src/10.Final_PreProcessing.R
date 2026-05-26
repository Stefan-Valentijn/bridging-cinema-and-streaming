# Load libraries
library(tidyverse)
library(lm.beta)
library(ivreg)
library(lmtest)
library(sandwich)
library(car)

# Load dataset
df <- read_csv("../data/finaldataset.csv", show_col_types = FALSE)

# Log-transform domestic opening and dependent variable
df$log_domestic_opening <- log(df$domestic_opening)
df$log_viewing          <- log(df$viewing_30days)

# Mean center the variables that are involved in moderations
df$log_domestic_opening_c <- df$log_domestic_opening - mean(df$log_domestic_opening, na.rm = TRUE)
df$release_window_c       <- scale(df$release_window, center = TRUE, scale = FALSE)
df$new_releases_c         <- scale(df$new_releases,   center = TRUE, scale = FALSE)

# Make the interaction terms manually
df$domesticOpening_x_releaseWindow   <- df$log_domestic_opening_c * df$release_window_c
df$newReleases_x_releaseWindow       <- df$new_releases_c         * df$release_window_c

# Fixed effects of RELEASE YEAR - RELEASE MONTH combination found in the dataset
df <- df %>%
  mutate(
    month_year_fe = factor(format(release_cinema_wide, "%Y-%m")),
    month_year_fe = relevel(month_year_fe, ref = "2017-06")
  )

# 2017-06 is the reference as it is closests to the mean domestic opening:
df %>%
  group_by(month_year_fe) %>%
  summarise(n = n(), mean_domestic_opening = mean(domestic_opening, na.rm = TRUE)) %>%
  mutate(distance_from_mean = abs(mean_domestic_opening - mean(df$domestic_opening, na.rm = TRUE))) %>%
  arrange(distance_from_mean, desc(n)) %>%
  head(10)

# Fixed effects of STREAMING PLATFORM
df <- df %>%
  mutate(platform_fe = ifelse(streaming_platform %in% c("hbo + hulu", "amazon + hulu"), 
                              "multiplatform", streaming_platform),
         platform_fe = factor(platform_fe),
         platform_fe = relevel(platform_fe, ref = "disney"))



# For script 12 - robustness check

# Log-transform and mean-center domestic gross
df$log_domestic_gross   <- log(df$domestic_gross)
df$log_domestic_gross_c <- df$log_domestic_gross - mean(df$log_domestic_gross, na.rm = TRUE)

# Interaction term for robustness model
df$domesticGross_x_releaseWindow <- df$log_domestic_gross_c * df$release_window_c


# For robustness check 2
df <- df %>%
  mutate(
    log_viewing_60 = log(viewing_60days),
    log_viewing_90 = log(viewing_90days)
  )



# For additional exploratory analysis

# Create Disney+ dummy variable
df$platformDisney <- ifelse(df$streaming_platform == "disney", 1, 0)

# New interaction term
df$domesticOpening_x_Disney <- df$log_domestic_opening_c * df$platformDisney



# For moderated mediation

# Scale all variables involved in the mediation to avoid numerical singularity
df$production_budget_s    <- scale(df$production_budget,        center = TRUE, scale = TRUE)[,1]
df$log_domestic_opening_s <- scale(df$log_domestic_opening_c,  center = TRUE, scale = TRUE)[,1]
df$release_window_s       <- scale(df$release_window_c,        center = TRUE, scale = TRUE)[,1]




# Reorder dataset so it is as easy to understand as possible
df <- df %>%
  select(
    # identifiers
    tconst, id, title,
    
    # release dates
    releaseYear, month_year_fe, release_cinema_wide, 
    release_streaming, release_window,
    
    # dependent variables (raw and log-transformed)
    viewing_30days, viewing_60days, viewing_90days,
    log_viewing, log_viewing_60, log_viewing_90,
    
    # streaming platform
    streaming_platform, platform_fe, platformDisney,
    
    # box office performance (raw and log-transformed)
    domestic_opening, log_domestic_opening,
    domestic_gross, log_domestic_gross,
    worldwide_gross, budget, production_budget,
    
    # film characteristics
    genres, runtimeMinutes, averageRating, numVotes,
    blockbuster_score, total_playing, new_releases,
    
    # mean-centered variables
    log_domestic_opening_c, log_domestic_gross_c,
    release_window_c, new_releases_c,
    
    # interaction terms
    domesticOpening_x_releaseWindow, domesticOpening_x_Disney,
    domesticGross_x_releaseWindow, newReleases_x_releaseWindow,
    
    # fixed effects
  )

# Save dataset
write.csv(df, "../data/briding_cinema_and_streaming.csv", row.names = FALSE)
