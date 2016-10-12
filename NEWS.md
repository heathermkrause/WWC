# WWC 0.1.1

* Now allow multiple geographical areas per survey (for example, multiple states 
or multiple counties)
* Deprecated functions that query Census API and process ACS data; moved those
functions to `data-raw`.
* Deprecated separate versions of main weighting function; all 
post-stratification weighting now done with `weight_wwc`.
* Changed weighing algorithm to remove strata with no survey respondents from 
population frequency table used for weighting (to avoid bias problem found in
simulations).
* Added codecov and Travis.

# WWC 0.1.0

* Initial version of package, allowing one geography per survey.



