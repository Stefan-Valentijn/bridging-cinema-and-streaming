# Load libraries
library(tidyverse)

# Load dataset
df <- read_csv("../data/bridging_cinema_and_streaming.csv")

# Impression dataset
summary(df)
colSums(is.na(df))

################################
# PART 1: CONTINUOUS VARIABLES #
################################

continuous_vars <- c(
  "viewing_30days",
  "log_viewing30",
  "domestic_opening",
  "log_domestic_opening",
  "release_window",
  "production_budget",
  "log_production_budget",
  "averageRating",
  "blockbuster_score"
)

# Minimum, maximum, mean and standard deviation
continuous_table <- df %>%
  select(all_of(continuous_vars)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
  group_by(Variable) %>%
  summarise(
    Min = round(min(Value,  na.rm = TRUE), 2),
    Max = round(max(Value,  na.rm = TRUE), 2),
    M   = round(mean(Value, na.rm = TRUE), 2),
    SD  = round(sd(Value,   na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(match(Variable, continuous_vars)); print(continuous_table)

#################################
# PART 2: CATEGORICAL VARIABLES #
#################################

n_total <- nrow(df)

# Streaming platform
platform_rows <- df %>%
  count(streaming_platform, sort = TRUE) %>%
  mutate(
    Group    = "Streaming Platform",
    Variable = streaming_platform,
    N        = n,
    Pct      = round(n / sum(n) * 100, 1)
  ) %>%
  select(Group, Variable, N, Pct); print(platform_rows)

# Release year
year_rows <- df %>%
  count(releaseYear, sort = FALSE) %>%
  mutate(
    Group    = "Release Year",
    Variable = as.character(releaseYear),
    N        = n,
    Pct      = round(n / sum(n) * 100, 1)
  ) %>%
  select(Group, Variable, N, Pct); print(year_rows)

# COVID
covid_rows <- df %>%
  count(COVID) %>%
  mutate(
    Group    = "COVID Period",
    Variable = ifelse(COVID == 1, "COVID (2020-2021)", "Non-COVID"),
    N        = n,
    Pct      = round(n / sum(n) * 100, 1)
  ) %>%
  select(Group, Variable, N, Pct); print(covid_rows)

# Genres
genre_rows <- df %>%
  select(tconst, genres) %>%
  separate_rows(genres, sep = ",") %>%
  mutate(genres = str_trim(genres)) %>%
  count(genres, sort = TRUE) %>%
  mutate(
    Group    = "Genre",
    Variable = genres,
    N        = n,
    Pct      = round(n / n_total * 100, 1)
  ) %>%
  select(Group, Variable, N, Pct); print(genre_rows) %>% print(n = Inf)









library(ggplot2)

# Domestic opening quartile plot
p1 <- ggplot(df, aes(x = factor(domestic_opening_quartile), y = viewing_30days)) +
  geom_boxplot(fill = "steelblue", outlier.alpha = 0.3) +
  labs(
    title = "Streaming Performance by Domestic Opening Quartile",
    x = "Domestic Opening Quartile",
    y = "Views (first 30 days)"
  ) +
  theme_minimal()

# Release window quartile plot
p2 <- ggplot(df, aes(x = factor(release_window_quartile), y = viewing_30days)) +
  geom_boxplot(fill = "steelblue", outlier.alpha = 0.3) +
  labs(
    title = "Streaming Performance by Release Window Quartile",
    x = "Release Window Quartile",
    y = "Views (first 30 days)"
  ) +
  theme_minimal()

library(patchwork)
p1 / p2





################################
# PART 3: INTERESTING PATTERNS #
################################

options(scipen = 999)

df_split <- df %>%
  separate_rows(streaming_platform, sep = " \\+ ") %>%
  mutate(streaming_platform = str_trim(streaming_platform))


# ── DOMESTIC_OPENING BY STREAMING PLATFORM ───────────────────────────────────

# Descriptives per platform - domestic_opening
platform_opening <- df %>%
  separate_rows(streaming_platform, sep = " \\+ ") %>%
  mutate(streaming_platform = str_trim(streaming_platform)) %>%
  group_by(streaming_platform) %>%
  summarise(
    N   = n(),
    M   = round(mean(domestic_opening, na.rm = TRUE), 2),
    SD  = round(sd(domestic_opening,   na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(M)); print(platform_opening)

# Kruskal-Wallis test - domestic_opening
kruskal.test(domestic_opening ~ streaming_platform, data = df_split)

# Post-hoc pairwise Wilcoxon - domestic_opening
pairwise.wilcox.test(df_split$domestic_opening, df_split$streaming_platform,
                     p.adjust.method = "bonferroni")


# ── RELEASE_WINDOW BY STREAMING PLATFORM ─────────────────────────────────────

# Descriptives per platform
platform_window <- df %>%
  separate_rows(streaming_platform, sep = " \\+ ") %>%
  mutate(streaming_platform = str_trim(streaming_platform)) %>%
  group_by(streaming_platform) %>%
  summarise(
    N   = n(),
    M   = round(mean(release_window, na.rm = TRUE), 2),
    SD  = round(sd(release_window,   na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(M)); print(platform_window)

# Kruskal-Wallis test - release_window
kruskal.test(release_window ~ streaming_platform, data = df_split)


# ── VIEWING_30DAYS BY STREAMING PLATFORM (split combined platforms) ───────────

# Descriptives per platform
platform_viewing <- df %>%
  separate_rows(streaming_platform, sep = " \\+ ") %>%
  mutate(streaming_platform = str_trim(streaming_platform)) %>%
  group_by(streaming_platform) %>%
  summarise(
    N   = n(),
    M   = round(mean(viewing_30days, na.rm = TRUE), 2),
    SD  = round(sd(viewing_30days,   na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(M)); print(platform_viewing)

# Kruskal-Wallis test
kruskal.test(viewing_30days ~ streaming_platform, data = df_split)

# Post-hoc pairwise Wilcoxon
pairwise.wilcox.test(df_split$viewing_30days, df_split$streaming_platform,
                     p.adjust.method = "bonferroni")




# ── RELEASE WINDOW BY COVID ───────────────────────────────────────────────────

covid_window <- df %>%
  group_by(COVID) %>%
  summarise(
    N  = n(),
    M  = round(mean(release_window, na.rm = TRUE), 2),
    SD = round(sd(release_window,   na.rm = TRUE), 2),
    .groups = "drop"
  ); print(covid_window)

shapiro.test(df$release_window)
#sig, so wilcox

# Wilcoxon test (two groups so no Kruskal-Wallis needed)
wilcox.test(release_window ~ COVID, data = df)



# ── RELEASE WINDOW OVER TIME ──────────────────────────────────────────────────

release_window_year <- df %>%
  group_by(releaseYear) %>%
  summarise(
    M  = mean(release_window, na.rm = TRUE),
    SD = sd(release_window,   na.rm = TRUE),
    N  = n(),
    SE = SD / sqrt(N),
    .groups = "drop"
  );print(release_window_year)

ggplot(release_window_year, aes(x = releaseYear, y = M)) +
  geom_line(linewidth = 1, color = "steelblue") +
  geom_point(size = 3,    color = "steelblue") +
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE),
                width = 0.2, color = "steelblue") +
  scale_x_continuous(breaks = unique(release_window_year$releaseYear)) +
  scale_y_continuous(limits = c(0, 850)) +
  labs(
    title   = "Trend of Release Window over the years",
    x       = "Release Year",
    y       = "Mean Release Window (days)",
    caption = "Error bars represent standard error"
  ) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())



# Descriptives show Disney+ behaves differently
df$platformDisney <- ifelse(df$streaming_platform == "disney", 1, 0)
