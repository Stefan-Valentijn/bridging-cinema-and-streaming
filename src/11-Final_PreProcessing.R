# Load libraries
library(tidyverse)
library(moments)

# Load dataset
df <- read_csv("../data/finaldataset.csv", show_col_types = FALSE)

#######################################
# SECTION VARIABLE OPERATIONALISATION #
#######################################

# 30-DAY STREAMING FIGURES
# Log-transform
df$log_viewing30 <- log(df$viewing_30days)

# Histogram of raw variable
hist(df$viewing_30days,
     main = "Distribution viewing_30days",
     xlab = "Viewing 30 days",
     col = "steelblue",
     breaks = 30)

# Histogram of log-transformed variable
hist(log(df$viewing_30days),
     main = "Distibution log(viewing_30days)",
     xlab = "log(Viewing 30 days)",
     col = "steelblue",
     breaks = 30)

# Skewness comparison
skewness(df$viewing_30days)
skewness(log(df$viewing_30days))

# Decision: log further used in analysis

##########

# DOMESTIC BOX OFFICE OPENING
# Log-transform
df$log_domestic_opening <- log(df$domestic_opening)

# Histogram of raw variable
hist(df$domestic_opening,
     main = "Distribution domestic_opening",
     xlab = "Domestic opening ($)",
     col = "steelblue",
     breaks = 30)

# Histogram of log-transformed variable
hist(log(df$domestic_opening),
     main = "Distribution log(domestic_opening)",
     xlab = "log(Domestic opening)",
     col = "steelblue",
     breaks = 30)

# Skewness comparison
skewness(df$domestic_opening)
skewness(log(df$domestic_opening))

# Decision: log further used in analysis

##########

# PRODUCTION BUDGET
# Log-transform
df$log_production_budget <- log(df$production_budget)

# Histogram of raw variable
hist(df$production_budget,
     main = "Distribution production_budget",
     xlab = "Production budget ($)",
     col = "steelblue",
     breaks = 30)

# Histogram of log-transformed variable
hist(log(df$production_budget),
     main = "Distribution log(production_budget)",
     xlab = "log(Production budget)",
     col = "steelblue",
     breaks = 30)

# Skewness comparison
skewness(df$production_budget)
skewness(df$log_production_budget)
# Decision: log further used in analysis

#######################################
# SECTION OLS MODEL FIT AND ESTIMATES #
#######################################

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

# Mean center the variables that are involved in moderations in the OLS and instrumental variable procedure
df$log_domestic_opening_c <- df$log_domestic_opening - mean(df$log_domestic_opening, na.rm = TRUE)
df$release_window_c       <- scale(df$release_window, center = TRUE, scale = FALSE)
df$new_releases_c         <- scale(df$new_releases,   center = TRUE, scale = FALSE)

# Make the interaction terms manually
df$domesticOpening_x_releaseWindow   <- df$log_domestic_opening_c * df$release_window_c
df$newReleases_x_releaseWindow       <- df$new_releases_c         * df$release_window_c

#############################
# SECTION ROBUSTNESS CHECKS #
#############################

# REPLACEMENT DOMESTIC OPENING WITH DOMESTIC GROSS
# Log-transform and mean-center domestic gross
df$log_domestic_gross   <- log(df$domestic_gross)
df$log_domestic_gross_c <- df$log_domestic_gross - mean(df$log_domestic_gross, na.rm = TRUE)

# Interaction term for robustness model
df$domesticGross_x_releaseWindow <- df$log_domestic_gross_c * df$release_window_c

##########

# REPLACEMENT VIEWING 30 WITH 60 AND 90
df <- df %>%
  mutate(
    log_viewing_60 = log(viewing_60days),
    log_viewing_90 = log(viewing_90days)
  )

##########

# REPLACEMENT OF PRODUCTION BUDGET FROM TMDB INSTEAD OF THE NUMBERS

# First log transform
df$log_budget <- log(df$budget)

# Note that there are some missing values for this
summary(is.na(df$budget))

#############################
# SECTION ROBUSTNESS CHECKS #
#############################

# MODERATOR TEST
# Create interaction of Disney+ with domestic box office to test moderation
df$domesticOpening_x_Disney        <- df$log_domestic_opening_c * (df$streaming_platform == "disney")

##########

# MODERATED MEDIATION
# Scale all variables involved in the mediation to avoid numerical singularity
df$log_production_budget_s    <- scale(df$log_production_budget,   center = TRUE, scale = TRUE)[,1] #not mean centered because not in moderation involved
df$log_domestic_opening_s     <- scale(df$log_domestic_opening_c,  center = TRUE, scale = TRUE)[,1]
df$release_window_s           <- scale(df$release_window_c,        center = TRUE, scale = TRUE)[,1]

############
# ALL DONE #
############

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
    log_viewing30, log_viewing_60, log_viewing_90,
    
    # streaming platform
    streaming_platform, platform_fe,
    
    # box office performance (raw and log-transformed)
    domestic_opening, log_domestic_opening,
    domestic_gross, log_domestic_gross,
    worldwide_gross, budget, log_budget, production_budget, log_production_budget,
    
    # film characteristics
    genres, runtimeMinutes, averageRating, numVotes,
    blockbuster_score, new_releases,
    
    # standardised variables
    log_production_budget_s, log_domestic_opening_s, release_window_s,
    
    # mean-centered variables
    log_domestic_opening_c, log_domestic_gross_c,
    release_window_c, new_releases_c,
    
    # interaction terms
    domesticOpening_x_releaseWindow, domesticOpening_x_Disney,
    domesticGross_x_releaseWindow, newReleases_x_releaseWindow
  )

# Save dataset
write.csv(df, "../data/bridging_cinema_and_streaming.csv", row.names = FALSE)
