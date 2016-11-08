# WWC 0.1.1

* Report estimates of margin of error in new `margin_of_error` function (reported
values include standard error of the weighted survey, and the standard error,
bias, and total error of the unweighted/raw survey)
* Added a new function to find the best weighting indicators for an individual
survey using bootstrap resampling, `choose_best_weighting`.
* Now allow multiple geographical areas per survey (for example, multiple states 
or multiple counties).
* Change binning of age data from ACS tables to new, bigger bins (only 5 age 
bins now) and replace negative "other" racial/ethnic values with 1.
* Deprecated functions that query Census API and process ACS data; moved those
functions to `data-raw`.
* Deprecated separate versions of main weighting function; all 
post-stratification weighting now done with `weight_wwc`.
* Changed weighting algorithm to remove strata with no survey respondents from 
population frequency table used for weighting.
* Added codecov and Travis.

# WWC 0.1.0

* Initial version of package, allowing one geography per survey.



