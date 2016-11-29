<!-- README.md is generated from README.Rmd. Please edit that file -->

WWC: Weighing the Wisdom of Crowds
----------------------------------

**Authors:** [Heather Krause](http://idatassist.com/), [Julia Silge](http://juliasilge.com/)<br/>
**License:** [MIT](https://opensource.org/licenses/MIT)

[![Travis-CI Build Status](https://travis-ci.org/heathermkrause/WWC.svg?branch=master)](https://travis-ci.org/heathermkrause/WWC)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/WWC)](https://cran.r-project.org/package=WWC)
[![Codecov](https://codecov.io/gh/heathermkrause/WWC/branch/master/graph/badge.svg)](https://codecov.io/gh/heathermkrause/WWC)





This R package is part of the the Veracio survey tool developed by leaders from [Orb](http://www.orbmedia.org/), [Datassist](http://idatassist.com/), and [Cognite Labs](http://www.cognitelabs.com/), and supported by a [2016 Knight Foundation News Challenge on Data](http://idatassist.com/knight/). This survey tool provides a set of software tools and online services for nonprofits, local governments, social service agencies, journalists, and others to:

- frame proper sampling questions,
- help construct, link, and embed them into existing survey and Q&A platforms, and 
- visualize and then statistically adjust the collected results.

This open-source tool weights collected survey results for demographic and other factors to make them more scientifically sound. The Veracio tool is being developed and made available at no cost to anyone wishing to poll the crowd and share reliable, credible results.

## Installation

You can install the development version of this package from GitHub using [devtools](https://github.com/hadley/devtools):


```r
library(devtools)
install_github("heathermkrause/WWC")
```

## Examples

This package contains a simulated survey called `texassurvey` that contains 1000 respondents that have answered a yes/no question. This example survey is *biased*, meaning that the population in the survey does not match the true population in Texas. It has different proportions with respect to sex and race/ethnicity compared to the real population in Texas (2010-2014 5-year ACS population estimates). What does this survey look like?


```r
library(WWC)
texassurvey
#> # A tibble: 1,000 × 6
#>       sex                       raceethnicity               age
#>     <chr>                               <chr>             <chr>
#> 1    Male WHITE ALONE, NOT HISPANIC OR LATINO    25 to 44 years
#> 2  Female WHITE ALONE, NOT HISPANIC OR LATINO 65 years and over
#> 3  Female WHITE ALONE, NOT HISPANIC OR LATINO    25 to 44 years
#> 4    Male WHITE ALONE, NOT HISPANIC OR LATINO 65 years and over
#> 5    Male WHITE ALONE, NOT HISPANIC OR LATINO    25 to 44 years
#> 6    Male WHITE ALONE, NOT HISPANIC OR LATINO    25 to 44 years
#> 7  Female WHITE ALONE, NOT HISPANIC OR LATINO    45 to 64 years
#> 8    Male                         ASIAN ALONE 65 years and over
#> 9  Female WHITE ALONE, NOT HISPANIC OR LATINO 65 years and over
#> 10 Female WHITE ALONE, NOT HISPANIC OR LATINO    25 to 44 years
#>                                      education geography response
#>                                          <chr>     <chr>    <chr>
#> 1  High school graduate (includes equivalency)        TX       no
#> 2           Some college or associate's degree        TX       no
#> 3                  Bachelor's degree or higher        TX       no
#> 4                  Bachelor's degree or higher        TX      yes
#> 5                  Bachelor's degree or higher        TX       no
#> 6                  Bachelor's degree or higher        TX       no
#> 7  High school graduate (includes equivalency)        TX       no
#> 8           Some college or associate's degree        TX      yes
#> 9  High school graduate (includes equivalency)        TX       no
#> 10 High school graduate (includes equivalency)        TX       no
#> # ... with 990 more rows
```

What result would a person using the survey find if s/he looked at the raw result of the survey, without adjusting for the demographic differences between the survey respondents and the true population in Texas?


```r
library(dplyr)
library(ggplot2)
resultDF <- texassurvey %>% 
        group_by(response) %>% 
        summarize(n = n())
ggplot(resultDF, aes(x = response, y = n)) +
        geom_bar(stat = "identity", fill = "midnightblue")
```

![plot of chunk original](README-original-1.png)

Instead, the Veracio survey tool can be used to statistically weight each survey respondent relative to what proportion of Texas' real population he or she represents.


```r
weighted <- weight_wwc(texassurvey, sex, raceethnicity)
weighted
#> # A tibble: 1,000 × 7
#>    geography    sex                       raceethnicity               age
#>        <chr>  <chr>                               <chr>             <chr>
#> 1         TX   Male WHITE ALONE, NOT HISPANIC OR LATINO    25 to 44 years
#> 2         TX Female WHITE ALONE, NOT HISPANIC OR LATINO 65 years and over
#> 3         TX Female WHITE ALONE, NOT HISPANIC OR LATINO    25 to 44 years
#> 4         TX   Male WHITE ALONE, NOT HISPANIC OR LATINO 65 years and over
#> 5         TX   Male WHITE ALONE, NOT HISPANIC OR LATINO    25 to 44 years
#> 6         TX   Male WHITE ALONE, NOT HISPANIC OR LATINO    25 to 44 years
#> 7         TX Female WHITE ALONE, NOT HISPANIC OR LATINO    45 to 64 years
#> 8         TX   Male                         ASIAN ALONE 65 years and over
#> 9         TX Female WHITE ALONE, NOT HISPANIC OR LATINO 65 years and over
#> 10        TX Female WHITE ALONE, NOT HISPANIC OR LATINO    25 to 44 years
#>                                      education response    weight
#>                                          <chr>    <chr>     <dbl>
#> 1  High school graduate (includes equivalency)       no 0.5296229
#> 2           Some college or associate's degree       no 0.7062374
#> 3                  Bachelor's degree or higher       no 0.7062374
#> 4                  Bachelor's degree or higher      yes 0.5296229
#> 5                  Bachelor's degree or higher       no 0.5296229
#> 6                  Bachelor's degree or higher       no 0.5296229
#> 7  High school graduate (includes equivalency)       no 0.7062374
#> 8           Some college or associate's degree      yes 1.9861542
#> 9  High school graduate (includes equivalency)       no 0.7062374
#> 10 High school graduate (includes equivalency)       no 0.7062374
#> # ... with 990 more rows
```

Now what result on the survey question will we find?


```r
resultDF <- weighted %>% 
        group_by(response) %>% 
        summarize(n = sum(weight))
ggplot(resultDF, aes(x = response, y = n)) +
        geom_bar(stat = "identity", fill = "midnightblue")
```

![plot of chunk weighted](README-weighted-1.png)

To learn more about the survey weighting algorithm, see the main vignette.

## Code of Conduct

This project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
