# Load libraries
library(tidyverse)
library(ivreg)
library(lmtest)
library(sandwich)
library(car)

# Load dataset
df <- read_csv("../data/finaldataset.csv", show_col_types = FALSE)

# Ensure complete cases on the necessary variables
df <- df[complete.cases(df[, c("viewing_30days", "domestic_opening", "release_window", "total_playing", "new_releases", "production_budget", "blockbuster_score", "averageRating")]), ]

# Mean center the variables that are involved in moderations
df$domestic_opening_c  <- scale(df$domestic_opening,   center = TRUE, scale = FALSE)
df$release_window_c    <- scale(df$release_window,     center = TRUE, scale = FALSE)
df$new_releases_c      <- scale(df$new_releases,       center = TRUE, scale = FALSE)
df$viewing_30days_c    <- scale(df$viewing_30days,     center = TRUE, scale = FALSE)

# Make the interaction terms manually
df$domesticOpening_x_releaseWindow   <- df$domestic_opening_c * df$release_window_c
df$newReleases_x_releaseWindow       <- df$new_releases_c     * df$release_window_c

# Ensure complete cases on the necessary variables
df <- df[complete.cases(df[, c("viewing_30days", "domestic_opening", "release_window", "total_playing", "new_releases", "production_budget", "blockbuster_score", "averageRating")]), ]

###############################################################################################################

# Step 0: Reduced Form
# Domestic opening is not taken but instead the instruments
reducedform <- lm(viewing_30days_c ~                                               # dependent variable
                    release_window_c +                                             # moderator
                    new_releases_c +                                               # instrument
                    newReleases_x_releaseWindow +                                  # instrument x moderator
                    averageRating + blockbuster_score + production_budget + COVID, # control variables
                  data = df); summary(reducedform)

# Interpretation: none of the instrument (w/ x moderator) should show a significant effect on the dv, as this 
# supports the exclusion restriction (only affect dv INDIRECTLY through domestic opening)

###############################################################################################################

# Step 1a: First Stage 
# Domestic opening is now the predicted variable
firststage <- lm(domestic_opening_c ~                                             # dependent variable
                   release_window_c +                                             # moderator
                   new_releases_c +                                               # instrument
                   newReleases_x_releaseWindow +                                  # instrument x moderator
                   averageRating + blockbuster_score + production_budget + COVID, # control variables
                 data = df); summary(firststage)

# Interpretation: the instrument (and x moderator) should show a significant effect on the domestic opening

# F-test to see whether the instrument affects domestic opening (is it useful as an instrument)
linearHypothesis(firststage, "new_releases_c = 0", test = "F")

# Interpretation: Goldfarb/Stock et al: F < 10 signals weak instruments

###############################################################################################################

# Step 1b: First Stage
# Interaction term is now the predicted variable
interaction <- lm(domesticOpening_x_releaseWindow ~                                 # dependent variable
                    release_window_c +                                              # moderator
                    new_releases_c +                                                # instrument
                    newReleases_x_releaseWindow +                                   # instrument x moderator
                    averageRating + blockbuster_score + production_budget + COVID,  # control variables
                  data = df); summary(interaction)

# Interpretation: the instrument interaction should show a significant effect on the interaction term

# F-test to see whether the instrument interaction affects the interaction term (is it useful as an instrument)
linearHypothesis(interaction, "newReleases_x_releaseWindow = 0", test = "F")

# Interpretation: Goldfarb/Stock et al: F < 10 signals weak instruments

###############################################################################################################

# Step 2: Two-Stage Least Squares
# Structure of ivreg: dependent variable ~ exogenous + controls | endogenous | instruments
ivreg_model <- ivreg(viewing_30days_c ~                                                 # dependent variable
                    release_window_c +                                                  # exogenous
                    averageRating + blockbuster_score + production_budget + COVID       # controls
                  |
                    domestic_opening_c + domesticOpening_x_releaseWindow                # endogenous
                  |
                    new_releases_c + newReleases_x_releaseWindow,                       # instruments
                  data = df)

# Summary() does not work because of weak instrument, therefore
coeftest(ivreg_model, vcov = vcovHC(ivreg_model, type = "HC3"))

# Interpretation: like a normal multiple regression, check significance

###############################################################################################################

# Step 3: Hausman test
# Add residuals from both first stages to OLS
df$firststage_residuals        <- residuals(firststage)   # residuals from domestic_opening_c first stage
df$interaction_residuals       <- residuals(interaction)  # residuals from interaction term first stage

hausman_model <- lm(viewing_30days_c ~                                                  # dependent variable
                      domestic_opening_c + domesticOpening_x_releaseWindow +            # main predictor + interaction
                      release_window_c +                                                # moderator
                      averageRating + blockbuster_score + production_budget + COVID +   # control variables
                      firststage_residuals + interaction_residuals,                     # first stage residuals
                    data = df); summary(hausman_model)

# Interpretation: residuals NOT significant means endogeneity is NOT a serious problem, therefore use OLS instead of 2SLS

###############################################################################################################

# Step 4: OLS (= multiple linear regression) model
# Final interpretation because endogeneity is NOT a serious problem
ols_model <- lm(viewing_30days_c ~                                                  # dependent variable
                  domestic_opening_c + domesticOpening_x_releaseWindow +            # main predictor + interaction
                  release_window_c +                                                # moderator
                  averageRating + blockbuster_score + production_budget + COVID,    # control variables
                data = df); summary(ols_model)

###############################################################################################################

# Format of the regression output is changed to align with writing
summary_ok <- function(model) {
  
  format_num <- function(x) {
    formatted <- formatC(x, format = "f", digits = 3)
    formatted <- gsub("^0\\.",  ".",  formatted)
    formatted <- gsub("^-0\\.", "-.", formatted)
    return(formatted)
  }
  
  format_p <- function(p) {
    ifelse(p < 0.001, "< .001",
           gsub("^0\\.", ".", formatC(round(p, 3), format = "f", digits = 3)))
  }
  
  signif_stars <- function(p) {
    ifelse(p < 0.001, "***",
           ifelse(p < 0.01,  "**",
                  ifelse(p < 0.05,  "*",
                         ifelse(p < 0.1,   ".",
                                " "))))
  }
  
  s <- summary(model)
  coefs <- as.data.frame(s$coefficients)
  
  out <- data.frame(
    Estimate     = sapply(coefs[, 1], format_num),
    `Std. Error` = sapply(coefs[, 2], format_num),
    `t value`    = sapply(coefs[, 3], format_num),
    `Pr(>|t|)`   = sapply(coefs[, 4], format_p),
    ` `          = sapply(coefs[, 4], signif_stars),
    check.names  = FALSE
  )
  rownames(out) <- rownames(coefs)
  print(out)
  cat("---\n")
  cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
  cat("\nResidual standard error:", round(s$sigma, 3),
      "on", s$df[2], "degrees of freedom\n")
  cat("Multiple R-squared:", round(s$r.squared, 3),
      " Adjusted R-squared:", round(s$adj.r.squared, 3), "\n")
  cat("F-statistic:", round(s$fstatistic[1], 3),
      "on", s$fstatistic[2], "and", s$fstatistic[3], "DF\n\n")
}

# The output in clean format.
summary_ok(ols_model)
