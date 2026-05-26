# Load libraries
library(tidyverse)
library(lm.beta)
library(ivreg)
library(lmtest)
library(sandwich)
library(car)

# Load dataset
df <- read_csv("../data/finaldataset.csv", show_col_types = FALSE)

###############################################################################################################

# Step 1: Reduced Form
# Domestic opening is not taken but instead the instruments
reducedform <- lm(log_viewing ~                                                  # dependent variable
                    release_window_c +                                             # moderator
                    new_releases_c +                                               # instrument
                    newReleases_x_releaseWindow +                                  # instrument x moderator
                    averageRating + blockbuster_score + production_budget +        # control variables
                    platform_fe + month_year_fe,                                   # fixed effects
                  data = df); summary(reducedform)

# Interpretation: new_releases_c and newReleases_x_releaseWindow should NOT have a significant effect on the dv, as this 
# supports the exclusion restriction (only affect dv INDIRECTLY through domestic opening)

###############################################################################################################

# Step 2a: First Stage 
# Domestic opening is now the predicted variable
firststage <- lm(log_domestic_opening_c ~                                        # dependent variable
                   release_window_c +                                             # moderator
                   new_releases_c +                                               # instrument
                   newReleases_x_releaseWindow +                                  # instrument x moderator
                   averageRating + blockbuster_score + production_budget +        # control variables
                   platform_fe + month_year_fe,                                   # fixed effects
                 data = df); summary(firststage)

# Interpretation: the instrument (and x moderator) should show a significant effect on the domestic opening

# F-test to see whether the instrument affects domestic opening (is it useful as an instrument)
linearHypothesis(firststage, "new_releases_c = 0", test = "F")

# Interpretation: Goldfarb cites Stock et al: F < 10 = weak instrument

###############################################################################################################

# Step 2b: First Stage
# Interaction term is now the predicted variable
interaction <- lm(domesticOpening_x_releaseWindow ~                                 # dependent variable
                    release_window_c +                                              # moderator
                    new_releases_c +                                                # instrument
                    newReleases_x_releaseWindow +                                   # instrument x moderator
                    averageRating + blockbuster_score + production_budget +         # control variables
                    platform_fe + month_year_fe,                                    # fixed effects
                  data = df); summary(interaction)

# Interpretation: the instrument interaction should show a significant effect on the interaction term

# F-test to see whether the instrument interaction affects the interaction term (is it useful as an instrument)
linearHypothesis(interaction, "newReleases_x_releaseWindow = 0", test = "F")

# Interpretation: Goldfarb cites Stock et al: F < 10 = weak instrument

###############################################################################################################

# Step 3: Hausman test
# Add residuals from both first stages to OLS
df$firststage_residuals        <- residuals(firststage)   # residuals from domestic_opening_c first stage
df$interaction_residuals       <- residuals(interaction)  # residuals from interaction term first stage

hausman_model <- lm(log_viewing ~                                                     # dependent variable
                      log_domestic_opening_c + domesticOpening_x_releaseWindow +        # main predictor + interaction
                      release_window_c +                                                # moderator
                      averageRating + blockbuster_score + production_budget +           # control variables
                      platform_fe + month_year_fe +                                     # fixed effects
                      firststage_residuals + interaction_residuals,                     # first stage residuals
                    data = df); summary(hausman_model)

# Interpretation: residuals NOT significant means endogeneity is NOT a serious problem, therefore use OLS instead of 2SLS

###############################################################################################################