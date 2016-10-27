#' Demographic and sex by age data from ACS tables for counties, states, and 
#' the U.S. as a whole
#' 
#' Demographic and sex by age data from ACS tables B01001 and B01001B/D/H/I for 
#' counties, states, and the U.S., fetched and processed by 
#' \code{process_acs_age} and \code{process_acs_age_all}.
#' 
#' @format A data frame with 163,600 rows and 8 variables:
#' \describe{
#' \item{sex}{Male or female.}
#' \item{age}{Under 18 years, 18 to 24 years, 25 to 44 years, 45 to 64 years,  
#' or 65 years and over.}
#' \item{raceethnicity}{White alone, Hispanic or Latino, black alone, Asian 
#' alone, or other.}
#' \item{population}{The number of people in this demographic bin.}
#' \item{sextotal}{The number of people in this sex and age bin.}
#' \item{geototal}{The total number of people tabulated in this geographic 
#' region.}
#' \item{prob}{\code{population/geototal}, or the proportion of the total that
#' is in this demographic bin.}
#' \item{region}{Geographic region, such as \code{US} for the entire United 
#' States, a two-letter abbrevation for a state (\code{TX} for Texas), or a 
#' 5-digit FIPS code for a county (\code{17031} for Cook County).}
#' }
#' @details Uses ACS 5-year estimate for 2010-2014; the data from table B01001 
#' is rebinned and used to find the "other" population. The "other" population
#' is set to 1 when it turns out negative because of measurement uncertainty.
#' (This is rare and more common in low population counties.)
#' 
"acsagetable"

#' Demographic and educational attainment data from ACS tables for counties,
#' states, and the U.S. as a whole
#' 
#' Demographic and educational attainment data from ACS tables B15002 and 
#' C15002B/D/H/I for counties, states, and the U.S., fetched and processed by 
#' \code{process_acs_education} and \code{process_acs_edu_all}. The educational 
#' attainment tables include only the population 25 and over.
#' 
#' @format A data frame with 130,880 rows and 8 variables:
#' \describe{
#' \item{sex}{Male or female.}
#' \item{education}{Less than high school diploma, high school graduate 
#' (includes equivalency), some college or associate's degree, bachelor's degree 
#' or higher.}
#' \item{raceethnicity}{White alone, Hispanic or Latino, black alone, Asian 
#' alone, or other.}
#' \item{population}{The number of people in this demographic bin.}
#' \item{sextotal}{The number of people in this sex and age bin.}
#' \item{geototal}{The total number of people tabulated in this geographic region.}
#' \item{prob}{\code{population/geototal}, or the proportion of the total that
#' is in this demographic bin.}
#' \item{region}{Geographic region, such as \code{US} for the entire United 
#' States, a two-letter abbrevation for a state (\code{TX} for Texas), or a 
#' 5-digit FIPS code for a county (\code{17031} for Cook County).}
#' }
#' @details Uses ACS 5-year estimate for 2010-2014; the data from table B15002 
#' is rebinned and used to find the "other" population. The "other" population
#' is set to 1 when it turns out negative because of measurement uncertainty.
#' (This is rare and more common in low population counties.)
#' 
"acsedutable"