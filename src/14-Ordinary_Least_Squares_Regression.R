# Load libraries
library(tidyverse)
library(lm.beta)
library(lmtest)
library(car)

# Load dataset
df <- read_csv("../data/bridging_cinema_and_streaming.csv", show_col_types = FALSE)

# Ensure fixed effects settings are correct
df <- df %>%
  mutate(
    month_year_fe = relevel(factor(month_year_fe), ref = "2017-06"),
    platform_fe   = relevel(factor(platform_fe),   ref = "disney")
  )

##############################################################################################################

# Step 1: PRE-ASSUMPTION: LINEARITY
# Are the relationships between each predictor and the DV roughly linear?
# (On menacnetered is no problem because axis change but not the data points)

plot(df$log_domestic_opening_c, df$log_viewing30,
     main = "Domestic Opening vs. Streaming Views",
     xlab = "Domestic Opening", ylab = "Streaming Views"); abline(lm(log_viewing30 ~ log_domestic_opening_c, data = df), col = "red")

plot(df$release_window_c, df$log_viewing30,
     main = "Release Window vs. Streaming Views",
     xlab = "Release Window", ylab = "Streaming Views"); abline(lm(log_viewing30 ~ release_window_c, data = df), col = "red")

# Conclusion: both linear so precondition met                                                                                                                                    

##############################################################################################################

# Step 2: ORDINARY LEAST SQUARES

# Model 1: control variables with fixed effects only (baseline)
model_1_log <- lm(log_viewing30 ~
                    averageRating + blockbuster_score + log_production_budget + # control variables
                    platform_fe + month_year_fe,                                # fixed effects
                  data = df); summary(model_1_log); lm.beta(model_1_log)

# Model 2: + interaction term (H3)
model_2_log <- lm(log_viewing30 ~
                    log_domestic_opening_c + release_window_c + domesticOpening_x_releaseWindow +   # H1, H2, H3
                    averageRating + blockbuster_score + log_production_budget +                     # control variables
                    platform_fe + month_year_fe,                                                    # fixed effects
                  data = df); summary(model_2_log); lm.beta(model_2_log)

# F-change: does model 2 significantly improve the model fit in comparison to baseline model 1?
anova(model_1_log, model_2_log)

##############################################################################################################

# Step 3: POST-ASSUMPTIONS AGAIN

# Linearity─────────────────────────────────────────────────────────────────────
# zpred vs zresid = residuals vs fitted plot
plot(model_2_log, which = 1)
# assumption met: red line is close to 0 + randomly scattered data points

# Homoscedasticity─────────────────────────────────────────────────────────────────────
bptest(model_2_log) #H0 = residuals are distributed with equal variance
# assumption met: non-significant test result

# Independence of errors─────────────────────────────────────────────────────────────────────
# Durbin-Watson: value ~2 = no autocorrelation; <1 or >3 = problematic
durbinWatsonTest(model_2_log) #H0 = no autocorrelation
# assumption met: non-significant test result

# Normality of residuals─────────────────────────────────────────────────────────────────────
# Shapiro-Wilk: p > .05 = normally distributed residuals (assumption met)
shapiro.test(residuals(model_2_log))   # H0 = the residuals are normally distributed
plot(model_2_log, which = 2)           # Q-Q plot
hist(residuals(model_2_log))           # histogram (Field's "Graphs: histogram")
# assumption violated: significant test result + tails deviate + histogram not normal

# Multicollinearity─────────────────────────────────────────────────────────────────────
# Model for testing the multicollinearity assumption WITHOUT the interaction
multicolmodel <- lm(log_viewing30 ~
                      log_domestic_opening_c + release_window_c +
                      averageRating + blockbuster_score + log_production_budget +
                      platform_fe + month_year_fe,                                         
                    data = df)

vif(multicolmodel) #= Variance Inflation Factor (greater than 10 = problem)
1 / vif(multicolmodel) #= tolerance (below 0.2 = problem)
# assumption met: GVIF all below 10 + tolerance for continuous variables above 0.2

# Influential cases─────────────────────────────────────────────────────────────────────
# Cook's distance: values > 1 are influential; calculate by 4/n is a common threshold
plot(model_2_log, which = 4)
cooksd <- cooks.distance(model_2_log)
threshold <- 4 / nrow(df); print(threshold)
influential <- which(cooksd > threshold)
df %>%
  slice(influential) %>%
  mutate(cooks_d = cooksd[influential],
         row_num = influential) %>%
  select(row_num, title, cooks_d) %>%
  arrange(desc(cooks_d)) %>%
  print(n = Inf)
# no influential cases: all values well below 1

##############################################################################################################

# Step 4. ROBUSTNESS CHECKS

# Robustness check 4A: TOTAL DOMESTIC GROSS INSTEAD OF OPENING DOMESTIC GROSS

# Robustness check model: domestic gross instead of domestic opening
model_robust <- lm(log_viewing30 ~
                     log_domestic_gross_c + release_window_c + domesticGross_x_releaseWindow +
                     averageRating + blockbuster_score + log_production_budget +
                     platform_fe + month_year_fe,
                   data = df); summary(model_robust); lm.beta(model_robust)

####################

# Robustness check 4B: DIFFERENT VIEWING INTERVALS

# First check dispersion
sd(df$log_viewing30, na.rm = TRUE) # reference
sd(df$log_viewing_60, na.rm = TRUE)
sd(df$log_viewing_90, na.rm = TRUE)

# 60-day viewing window as alternative DV
model_robust_60 <- lm(log_viewing_60 ~
                        log_domestic_opening_c + release_window_c + domesticOpening_x_releaseWindow +  # H1, H2, H3
                        averageRating + blockbuster_score + log_production_budget +                    # control variables
                        platform_fe + month_year_fe,                                                   # fixed effects
                      data = df); summary(model_robust_60); lm.beta(model_robust_60)

# 90-day viewing window as alternative DV
model_robust_90 <- lm(log_viewing_90 ~
                        log_domestic_opening_c + release_window_c + domesticOpening_x_releaseWindow +  # H1, H2, H3
                        averageRating + blockbuster_score + log_production_budget +                    # control variables
                        platform_fe + month_year_fe,                                                   # fixed effects
                      data = df); summary(model_robust_90); lm.beta(model_robust_90)


####################

# Robustness check 4C: REPLACEMENT OF PRODUCTION BUDGET FROM TMDB INSTEAD OF THE NUMBERS

# There are some inconsistencies
sum(df$budget == df$production_budget, na.rm = TRUE)
sum(df$budget != df$production_budget, na.rm = TRUE)

# Therefore run OLS with the alternative source
model_robustbudget <- lm(log_viewing30 ~
                           log_domestic_opening_c + release_window_c + domesticOpening_x_releaseWindow +   # H1, H2, H3
                           averageRating + blockbuster_score + log_budget +                                # control variables
                           platform_fe + month_year_fe,                                                    # fixed effects
                         data = df); summary(model_robustbudget); lm.beta(model_robustbudget)


##############################################################################################################

# Step 9. ADDITIONAL EXPLORATORY ANALYSIS

# Additional Exploratory Analysis 9A: PLATFORM DIFFERENCES

# Figures per streaming platform
df %>%
  group_by(platform_fe) %>%
  summarise(
    N           = n(),
    M_domestic  = median(domestic_opening, na.rm = TRUE),
    SD_domestic = sd(domestic_opening, na.rm = TRUE),
    M_window    = median(release_window, na.rm = TRUE),
    SD_window   = sd(release_window, na.rm = TRUE),
    M_viewing   = median(viewing_30days, na.rm = TRUE),
    SD_viewing  = sd(viewing_30days, na.rm = TRUE)
  ) %>%
  arrange(desc(M_domestic))

# Kruskal-Wallis tests
kruskal.test(domestic_opening ~ platform_fe, data = df)
kruskal.test(viewing_30days ~ platform_fe, data = df)
kruskal.test(release_window ~ platform_fe, data = df)

# Post-hoc pairwise Wilcoxon tests with Bonferroni correction
pairwise.wilcox.test(df$domestic_opening, df$platform_fe, p.adjust.method = "bonferroni")
pairwise.wilcox.test(df$viewing_30days,   df$platform_fe, p.adjust.method = "bonferroni")
pairwise.wilcox.test(df$release_window,   df$platform_fe, p.adjust.method = "bonferroni")

####################

# Additional Exploratory Analysis 9b: PLATFORM MODERATOR
model_exploratory <- lm(log_viewing30 ~
                          log_domestic_opening_c + release_window_c +                   # independent variables
                          domesticOpening_x_releaseWindow +                             # interaction
                          domesticOpening_x_Disney +                                    # exploratory moderation
                          averageRating + blockbuster_score + log_production_budget +   # control variables
                          platform_fe + month_year_fe,                                  # fixed effects
                        data = df); summary(model_exploratory); lm.beta(model_exploratory)

####################

# Additional Exploratory Analysis 9C: MODERATED MEDIATION

# In order for the code below to work, please run Hayes Process Tool in script 15 first (note: load time is long)

# Hayes process tool
process(data  = as.data.frame(df),
        y     = "log_viewing30",
        x     = "log_production_budget_s",
        m     = "log_domestic_opening_s",
        w     = "release_window_c",
        cov   = c("averageRating",
                  "blockbuster_score"),
        model = 14,
        boot  = 5000,
        seed  = 123,
        jn    = 1)
