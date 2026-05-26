# Load libraries
library(tidyverse)
library(lm.beta)
library(lmtest)
library(car)

# Load dataset
df <- read_csv("../data/briding_cinema_and_streaming.csv", show_col_types = FALSE)

##############################################################################################################

# Step 4: PRE-ASSUMPTION: LINEARITY
# Are the relationships between each predictor and the DV roughly linear?
# (On meancnetered is no problem because axis change but not the data points)

plot(df$domestic_opening_c, df$viewing_30days,
     main = "Domestic Opening vs. Streaming Views",
     xlab = "Domestic Opening (meancentered)", ylab = "Streaming Views (meancentered)"); abline(lm(viewing_30days ~ domestic_opening_c, data = df), col = "red")

plot(df$release_window_c, df$viewing_30days,
     main = "Release Window vs. Streaming Views",
     xlab = "Release Window (meancentered)", ylab = "Streaming Views (meancentered)"); abline(lm(viewing_30days ~ release_window_c, data = df), col = "red")

# Conclusion: both linear so no transformation needed                                                                                                                                    

##############################################################################################################

# Step 6: ORDINARY LEAST SQUARES

# Model 1: control variables with fixed effects only (baseline)
model_1_log <- lm(log_viewing ~
                    averageRating + blockbuster_score + production_budget +     # control variables
                    platform_fe + month_year_fe,                                # fixed effects
                  data = df); summary(model_1_log); lm.beta(model_1_log)

# Model 2: + interaction term (H3)
model_2_log <- lm(log_viewing ~
                    log_domestic_opening_c + release_window_c + domesticOpening_x_releaseWindow +  # H1, H2, H3
                    averageRating + blockbuster_score + production_budget +                         # control variables
                    platform_fe + month_year_fe,                                                    # fixed effects
                  data = df); summary(model_2_log); lm.beta(model_2_log)

# F-change: does model 2 significantly improe the model fit in comparison to baseline model 1?
anova(model_1_log, model_2_log)

##############################################################################################################

# Step 7: POST-ASSUMPTIONS AGAIN

# Linearity─────────────────────────────────────────────────────────────────────
# zpred vs zresid = residuals vs fitted plot
plot(model_2_log, which = 1)
# assumption met: red line is close to 0 

# Homoscedasticity─────────────────────────────────────────────────────────────────────
bptest(model_2_log) #H0 = residuals are distributed with equal variance
plot(model_2_log, which = 3)
# assumption met: non-significant test result + randomly scattered data points

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
multicolmodel <- lm(log_viewing ~
                      log_domestic_opening_c + release_window_c +
                      averageRating + blockbuster_score + production_budget +
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
  mutate(cooks_d = cooksd[influential]) %>%
  select(title, cooks_d) %>%
  arrange(desc(cooks_d)) %>%
  print(n = Inf)
# no influential cases: all values well below 1

##############################################################################################################

# Step 8. ROBUSTNESS CHECKS

# Check 1: TOTAL DOMESTIC GROSS INSTEAD OF OPENING DOMESTIC GROSS

# Robustness check model: domestic gross instead of domestic opening
model_robust <- lm(log_viewing ~
                     log_domestic_gross_c + release_window_c + domesticGross_x_releaseWindow +
                     averageRating + blockbuster_score + production_budget +
                     platform_fe + month_year_fe,
                   data = df); summary(model_robust); lm.beta(model_robust)

####################

# Check 2: DIFFERENT VIEWING INTERVALS

# 60-day viewing window as alternative DV
model_robust_60 <- lm(log_viewing_60 ~
                        log_domestic_opening_c + release_window_c + domesticOpening_x_releaseWindow +  # H1, H2, H3
                        averageRating + blockbuster_score + production_budget +                         # control variables
                        platform_fe + month_year_fe,                                                    # fixed effects
                      data = df); summary(model_robust_60); lm.beta(model_robust_60)

# 90-day viewing window as alternative DV
model_robust_90 <- lm(log_viewing_90 ~
                        log_domestic_opening_c + release_window_c + domesticOpening_x_releaseWindow +  # H1, H2, H3
                        averageRating + blockbuster_score + production_budget +                         # control variables
                        platform_fe + month_year_fe,                                                    # fixed effects
                      data = df); summary(model_robust_90); lm.beta(model_robust_90)

##############################################################################################################

# Step 9. ADDITIONAL EXPLORATORY ANALYSIS

# First one: PLATFORM MODERATOR

# New interaction term
df$domesticOpening_x_Disney <- df$domestic_opening_c * df$platformDisney

# Exploratory model with platform moderation
model_exploratory <- lm(log_viewing ~
                          domestic_opening_c +                # H1
                          release_window_c +                  # H2
                          domesticOpening_x_releaseWindow +   # H3
                          domesticOpening_x_Disney +          # exploratory moderation
                          platformDisney +                    # keep as main effect
                          COVID + averageRating + blockbuster_score + production_budget,
                        data = df); summary(model_exploratory); lm.beta(model_exploratory)

####################

# Second one: MODERATED MEDIATION

# In order for the code below to work, please run script 13 first

# Hayes process tool
process(data  = as.data.frame(df),
        y     = "log_viewing",
        x     = "production_budget_m",
        m     = "log_domestic_opening_c",
        w     = "release_window_c",
        cov   = c("averageRating",
                  "blockbuster_score"),
        model = 14,
        boot  = 5000,
        seed  = 123,
        jn    = 1)
