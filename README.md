# Ready, Set… Stream? Exploring the Effect of Cinema Performance on Streaming Performance

## Research Motivation

The cinema-to-streaming relationship remains underexplored despite the growing academic and industry interest in how theatrical performance spills over into streaming consumption. While prior work has examined the effect of cinema performance on home entertainment sales (e.g. Hennig-Thurau et al., 2006), the role of the release window — the gap between a film's theatrical and streaming release — has received limited empirical attention in the streaming era. The present study addresses this gap by examining to what extent cinema performance predicts subsequent streaming performance, and what moderating role the release window plays.

### Research Question

*To what extent does the cinema performance of a movie predict its subsequent streaming performance, and what role does the release window play?*

## Data

Five data sources were consulted and merged into a single analytical dataset (*n* = 345), covering US-released Hollywood productions from 2017 to 2022:

| Source | Variables retrieved |
|---|---|
| [Trakt.tv](https://trakt.tv/) | 30-, 60-, and 90-day streaming viewing figures; streaming release date |
| [Box Office Mojo](https://www.boxofficemojo.com/) | Domestic opening weekend gross |
| [IMDb](https://developer.imdb.com/non-commercial-datasets/) | Wide US cinema release date; genre; runtime; consumer rating; vote count |
| [The Numbers](https://www.the-numbers.com/movie/budgets/) | Production budget; domestic gross; worldwide gross |
| [TMDB](https://developer.themoviedb.org/) | Consumer rating; vote count; production budget (robustness check) |

The analytical dataset contains the following variables:

| Variable | Type | Definition |
|---|---|---|
| `viewing_30days` | integer | Unique viewing instances in the first 30 days after streaming release |
| `domestic_opening` | integer | Domestic opening weekend gross in dollars |
| `release_window` | integer | Days between wide cinema release and streaming release |
| `production_budget` | integer | Cost of producing and distributing a movie in dollars |
| `blockbuster_score` | numeric | Genre-based normalised weight of historical domestic theatrical performance (0–1) |
| `averageRating` | numeric | Weighted average of IMDb and TMDB consumer ratings (0–10) |
| `runtimeMinutes` | integer | Runtime in minutes |
| `numVotes` | integer | Combined vote count across IMDb and TMDB |
| `streaming_platform` | character | Name of the streaming platform |
| `release_cinema_wide` | date | Wide US theatrical release date |
| `release_streaming` | date | Streaming platform release date |
| `new_releases` | integer | Count of wide-release films opening in the same week (instrument) |

## Method

Data collection and pre-processing were conducted in Python (version 3.13.5). All further analysis was performed in R (version 4.5.3) using RStudio (version 2026.1.1.403). Analysis consisted of four consecutive parts:

1. **Variable operationalisation** — log-transformations and mean-centring of key variables; construction of interaction terms and fixed effects for release month-year and streaming platform.

2. **Instrumental variable diagnostics** — a reduced form, two first-stage regressions, and a Durbin-Wu-Hausman test were conducted to assess the endogeneity of domestic opening gross. Both first-stage *F*-statistics fell below the weak instrument threshold, and the Hausman test indicated endogeneity was not severe; OLS (= multiple linear regression) was therefore retained as the final estimator.

3. **OLS models** — a baseline model (controls and fixed effects only) and a full model (adding cinema performance, release window, and their interaction) were estimated, alongside statistical assumption testing (linearity, homoscedasticity, independence of errors, normality of residuals, multicollinearity, influential cases).

4. **Robustness checks and exploratory analyses** — alternative operationalisations of the independent variable (domestic gross), dependent variable (60- and 90-day viewing figures), and production budget (TMDB) were tested; exploratory analyses examined platform differences and a moderated mediation.

## Repository Overview

```text
bridging-cinema-and-streaming/
├─ src/
│  ├─ 01.1-TheNumbers_webscrape.ipynb
│  ├─ 01.2-TheNumbers_cleaning.R
│  ├─ 02-IMDb_cleaning.R
│  ├─ 03-Merging_TheNumbers_and_IMDb.R
│  ├─ 04-Trakt_cleaning.R
│  ├─ 05-Merge_Movie_and_Stream.R
│  ├─ 06.1-IMDb_CrossValidation_webscrape.ipynb
│  ├─ 06.2-IMDb_Crossvalidation.R
│  ├─ 07.1-BoxOfficeMojo_webscrape.ipynb
│  ├─ 07.2-BoxOfficeMojo_webscrape.ipynb
│  ├─ 08-Merging_MovieStream_and_BOM.R
│  ├─ 09.1-TMDB_API_endpoint1.ipynb
│  ├─ 09.2-TMDB_API_endpoint2.ipynb
│  ├─ 09.3-TMDB_cleaning.R
│  ├─ 10-Merge_MovieStream_and_TMDB.R
│  ├─ 11-Final_PreProcessing.R
│  ├─ 12-Descriptive_Analysis.R
│  ├─ 13-Instrumental_Variable_procedure.R
│  ├─ 14-Ordinary_Least_Squares_Regression.R
│  └─ 15-HAYES process tool code.R
└─ README.md
```text

## Dependencies

By using the installation guides as found on [Tilburg University's Science Hub](http://tilburgsciencehub.com/), ensure your device has the following software installed:

- R / RStudio — [installation guide](https://tilburgsciencehub.com/building-blocks/configure-your-computer/statistics-and-computation/r/)
- Python — [installation guide](https://tilburgsciencehub.com/building-blocks/configure-your-computer/statistics-and-computation/python/)

Within RStudio, ensure the following packages are installed:

```r
install.packages("tidyverse")   # data wrangling and visualisation
install.packages("moments")     # skewness computation
install.packages("car")         # linearHypothesis(), vif(), durbinWatsonTest()
install.packages("lmtest")      # bptest() for heteroscedasticity testing
install.packages("lm.beta")     # standardised regression coefficients
```

The moderated mediation analysis additionally relies on the **Hayes PROCESS macro**, which is not available on CRAN and must be sourced manually via `source()` prior to running the relevant script. It can be downloaded from [processmacro.org](https://www.processmacro.org/download.html).

## About

This project is the Master's thesis for the [Marketing Analytics](https://www.tilburguniversity.edu/education/masters-programs/marketing-analytics) program at [Tilburg University](https://www.tilburguniversity.edu/), the Netherlands. The project is implemented by:

- Stefan Valentijn
