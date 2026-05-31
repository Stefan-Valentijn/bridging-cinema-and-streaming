# Ready, Set… Stream? Exploring the Effect of Cinema Performance on Streaming Performance

## Research Motivation

The streaming video on demand (SVOD) revolution has fundamentally changed how the success of a movie should be interpreted. Nonetheless, the relationship between cinema and streaming performance remains empirically underexplored. As release windows shrink and streaming platforms increasingly displace traditional home entertainment, understanding how cinema performance translates into streaming demand has become strategically critical for studios and distributors alike. Therefore, the present study aims to answer:
### Research Question

*To what extent does the cinema performance of a movie predict its subsequent streaming performance, and what role does the release window play?*

## Hypotheses

| Numer | Expectation |
|:-----------|:----------|
| **H1** | A stronger cinema performance has a positive effect on the subsequent streaming performance. |
| **H2** | A shorter release window is associated with greater streaming performance. |
| **H3** | A shorter release window amplifies the positive relationship between cinema performance and streaming performance. |
 
## Data

In order to test the hypotheses, five data sources were consulted  and merged into a single dataset (*n* = 345), covering historical data on movies released in the United States between 2017-2022:

| Source | Variables retrieved |
|---|---|
| [Trakt.tv](https://trakt.tv/) | 30-, 60-, and 90-day streaming viewing figures |
| [Box Office Mojo](https://www.boxofficemojo.com/) | Domestic opening box office |
| [IMDb](https://developer.imdb.com/non-commercial-datasets/) | Wide US cinema release date; genre; consumer rating; vote count |
| [The Numbers](https://www.the-numbers.com/movie/budgets/) | Production budget; domestic gross; worldwide gross |
| [TMDB](https://developer.themoviedb.org/) | Consumer rating; vote count; production budget (robustness check) |

## Method

Data collection and pre-processing were conducted in Python (version 3.13.5). All further analysis was performed in R (version 4.5.3) using RStudio (version 2026.1.1.403). Analysis consisted of four consecutive parts:

1. **Variable operationalisation** — log-transformations and mean-centring of key variables; construction of interaction terms and fixed effects for release month-year and streaming platform.

2. **Instrumental variable diagnostics** — a reduced form, two first-stage regressions, and a Durbin-Wu-Hausman test were conducted to assess the endogeneity of domestic opening gross. Both first-stage *F*-statistics fell below the weak instrument threshold, and the Hausman test indicated endogeneity was not severe; OLS (= multiple linear regression) was therefore retained as the final estimator.

3. **Ordinary Least Squares models** — a baseline model (controls and fixed effects only) and a full model (adding cinema performance, release window, and their interaction) were estimated, alongside statistical assumption testing (linearity, homoscedasticity, independence of errors, normality of residuals, multicollinearity, influential cases).

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
│  ├─ 14-Inferential_Analysis.R
│  └─ 15-HAYES process tool code.R
└─ README.md
```

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

The moderated mediation analysis additionally relies on the **Hayes PROCESS macro**, which has to be manually loaded before PROCESS can run. It can be downloaded from [processmacro.org](https://www.processmacro.org/download.html).

## About

This GitHub repository is a supplement to Stefan Valentijn's thesis for the [Master of Science Marketing Analytics](https://www.tilburguniversity.edu/education/masters-programs/marketing-analytics) program at [Tilburg University](https://www.tilburguniversity.edu/), the Netherlands.