---
title: "Introduction to the WWC Package"
author: "Heather Krause and Julia Silge"
date: "2016-11-28"
output:
  rmarkdown::html_vignette:
    toc: true
    keep_md: true
vignette: >
  %\VignetteIndexEntry{Introduction to the WWC Package}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---
  


The WWC package is part of the the [Veracio](http://veracio.org/) survey tool developed by leaders from [Orb](http://www.orbmedia.org/), [Datassist](http://idatassist.com/), and [Cognite Labs](http://www.cognitelabs.com/), and supported by a [2016 Knight Foundation News Challenge on Data](http://idatassist.com/knight/). This survey tool provides a set of software tools and online services for nonprofits, local governments, social service agencies, journalists, and others to:

- frame proper sampling questions,
- help construct, link, and embed them into a survey platform, and 
- visualize and then statistically adjust the collected results.

This open-source tool weights collected survey results for demographic and other factors to make them more scientifically sound. The Veracio tool is being developed and made available at no cost to anyone wishing to poll the crowd and share reliable, credible results.

This vignette explains how the functions are used in production online at [Veracio](http://veracio.org/).

## Surveys using the Veracio tool

A user of the Veracio tool sets up a survey via the website that s/he can share with members of a community. The survey will always include demographic questions, such as sex, race/ethnicity, education, and/or age, because the Veracio tool works by statistically weighting survey respondents by how much of the population they represent. During the survey period, responses are stored. At the end of the survey period, all the survey responses are ready to be analyzed.

The R package has several example surveys; let's use one that is stored as a `.csv` file, since that is how it is used in production. This simulated survey is *biased*, meaning that the population in the survey does not match the true population in the two states (TX and CA) where it is simulated. It has different proportions with respect to sex and race/ethnicity compared to the real population (2010-2014 5-year ACS population estimates). 


```r
library(readr)
survey <- read_csv(system.file("extdata/twostatessurvey.csv", package = "WWC"))

survey
```

```
## # A tibble: 1,000 × 6
##      sex                       raceethnicity               age
##    <chr>                               <chr>             <chr>
## 1   Male WHITE ALONE, NOT HISPANIC OR LATINO    45 to 64 years
## 2   Male WHITE ALONE, NOT HISPANIC OR LATINO    25 to 44 years
## 3   Male WHITE ALONE, NOT HISPANIC OR LATINO    25 to 44 years
## 4   Male WHITE ALONE, NOT HISPANIC OR LATINO 65 years and over
## 5   Male                         ASIAN ALONE    45 to 64 years
## 6   Male WHITE ALONE, NOT HISPANIC OR LATINO    25 to 44 years
## 7   Male                  HISPANIC OR LATINO    45 to 64 years
## 8   Male                         ASIAN ALONE    45 to 64 years
## 9   Male WHITE ALONE, NOT HISPANIC OR LATINO    25 to 44 years
## 10  Male WHITE ALONE, NOT HISPANIC OR LATINO 65 years and over
## # ... with 990 more rows, and 3 more variables: education <chr>,
## #   geography <chr>, response <int>
```

## Choosing the best set of weighting indicators

When Veracio users design a survey, they will not know ahead of time which set of demographic factors will be the best set of weighting indicators to use for their individual surveys, because they cannot predict how their survey respondents will be biased. The Veracio tool uses a function called `choose_best_weighting` to find the best set using bootstrap resampling of the survey itself. Based on our work preparing to implement the Veracio tool, we found that the set of weighting indicators that gives the most consistent result in post-stratification weighting (in the sense of lowest variance) is the right set. Let's implement that here.


```r
library(WWC)
best_indicators <- choose_best_weighting(inputfile = system.file("extdata/twostatessurvey.csv", 
                                                                 package = "WWC"),
                                         outputpath = "./wwc_weighted.csv",
                                         n = 500, 
                                         response, sex, raceethnicity, age)
```



In production for the Veracio tool, we use 500-1000 bootstrap resamplings to find the best set of weighting indicators. This part of the process calculates post-stratification weights for each bootstrap resampling of the real survey, for each possible combination of the weighting indicators, using 5-year ACS population estimates. In this case, the Veracio tool will try the following weighting indicators to calculate post-stratification weights.


```
## [[1]]
## [1] "sex"
## 
## [[2]]
## [1] "raceethnicity"
## 
## [[3]]
## [1] "age"
## 
## [[4]]
## [1] "sex"           "raceethnicity"
## 
## [[5]]
## [1] "sex" "age"
## 
## [[6]]
## [1] "raceethnicity" "age"          
## 
## [[7]]
## [1] "sex"           "raceethnicity" "age"
```

The function `choose_best_weighting` returns the best set of indicators to use, the ones most appropriate to the bias in the survey. What is that set for this example survey?


```r
best_indicators
```

```
## [[1]]
## [1] "sex"
## 
## [[2]]
## [1] "raceethnicity"
```

The algorithm here tried all possible sets of indicators and chose sex and race/ethnicity; this is correct because this is how this simulated survey was set up to be biased.

## Reporting post-stratification weights

The function `choose_best_weighting` also saves a `.csv` of the original survey data with two new columns added. (Depending on how many different geographical regions are included in the survey, the survey respondents and columns may be in a different order in the output than in the input.)


```r
weighted <- read_csv("./wwc_weighted.csv")

weighted
```


```
## # A tibble: 1,498 × 8
##    geography    sex                       raceethnicity               age
##        <chr>  <chr>                               <chr>             <chr>
## 1         CA   Male WHITE ALONE, NOT HISPANIC OR LATINO    45 to 64 years
## 2         CA   Male WHITE ALONE, NOT HISPANIC OR LATINO    25 to 44 years
## 3         CA   Male                  HISPANIC OR LATINO    45 to 64 years
## 4         CA   Male                         ASIAN ALONE    45 to 64 years
## 5         CA   Male WHITE ALONE, NOT HISPANIC OR LATINO 65 years and over
## 6         CA   Male WHITE ALONE, NOT HISPANIC OR LATINO    45 to 64 years
## 7         CA   Male                               OTHER    18 to 24 years
## 8         CA   Male                  HISPANIC OR LATINO    45 to 64 years
## 9         CA Female WHITE ALONE, NOT HISPANIC OR LATINO    25 to 44 years
## 10        CA Female WHITE ALONE, NOT HISPANIC OR LATINO    45 to 64 years
## # ... with 1,488 more rows, and 4 more variables: education <chr>,
## #   response <int>, weight_best <dbl>, weight_all <dbl>
```

There are two weights reported in the output. One is called `weight_best`; this is the weight for each survey respondent found using the set of weighting indicators identified by bootstrap resampling. The other is called `weight_all`; this weight for each survey respondent is found by using all the weighting indicators possible from the demographic questions posed on the survey. It is possible for these weights to be the same if the best set of weighting indicators is in fact all the demographic questions posed on the survey.


```r
library(dplyr)

weighted %>% 
        select(contains("weight"))
```

```
## # A tibble: 1,498 × 2
##    weight_best weight_all
##          <dbl>      <dbl>
## 1    0.6812547  0.5969613
## 2    0.6812547  0.5558195
## 3    2.2389717  0.7925856
## 4    7.5486263  4.8997215
## 5    0.6812547  0.5409507
## 6    0.6812547  0.5969613
## 7    0.5298126  0.3975543
## 8    2.2389717  0.7925856
## 9    1.0688964  0.8015454
## 10   1.0688964  0.9559243
## # ... with 1,488 more rows
```

When these weights are higher than 1, the survey respondent was from a group underrepresented on the survey; when these weights are lower than 1; the survey respondent was from a group overrepresented on the survey. Veracio users can use these weights to statistically adjust the results of their surveys.

## Reporting estimates of error

Survey users are also interested in understanding the margin of error of their surveys, and how the weighting procedure implemented by the Veracio tool affects the margin of error and uncertainty in their surveys.

Before finding the post-stratification weights, the raw survey has two main sources of uncertainty. One is random error, due to random sampling differences, and the other is bias, due to the survey sample not being an accurate reflection of the real population in question. The algorithm implemented by the Veracio tool reduces the second source of error and uncertainty as much as possible. One result of this is that the random error of the weighted survey results is somewhat higher than the raw survey results; we can think of this as an example of the bias-variance tradeoff. For many biased surveys, the overall error is still lower after applying post-stratification weights. Let's see how this works for the example survey we have here.


```r
margin_of_error("./wwc_weighted.csv", response)
```


```
## $raw_standard_error
## [1] 0.01070393
## 
## $raw_bias
## [1] 0.05252481
## 
## $raw_total_error
## [1] 0.05360438
## 
## $weighted_standard_error
## [1] 0.01691279
```

The raw survey has two sources of error, the standard error and the bias. The total error of the raw survey is the square root of the sum of the squares of these two quantities. We assume that the weighted survey has approximately no bias, since we removed the effect of bias in the survey sample by calculating post-stratification weights using ACS data. Thus, the total error of the weighted survey is approximately the same as the standard error. Notice that the standard error is higher for the weighted survey than for the raw survey; this is a consistent result of applying the algorithm here.
