#' Demographic and sex by age data from ACS tables for counties, states, and 
#' the U.S. as a whole
#' 
#' Demographic and sex by age data from ACS tables B01001 and B01001B/D/H/I for 
#' counties, states, and the U.S., fetched and processed by 
#' \code{process_acs_age} and \code{process_acs_age_all}.
#' 
#' @format A data frame with 458,080 rows and 8 variables:
#' \describe{
#' \item{sex}{Male or female.}
#' \item{age}{Under 5 years, 5 to 
#' 9 years, 10 to 14 years, 15 to 17 years, 18 and 19 years, 20 to 24 years, 25 
#' to 29 years, 30 to 34 years, 35 to 44 years, 45 to 54 years, 55 to 64 years, 
#' 65 to 74 years, 75 to 84 years, or 85 years and over.}
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
#' is rebinned and used to find the "other" population
#' 
"acsagetable"

#' Demographic and educational attainment data from ACS tables for counties,
#' states, and the U.S. as a whole
#' 
#' Demographic and educational attainment data from ACS tables B15002 and 
#' C15002B/D/H/I for the states and the U.S., fetched and processed by 
#' \code{process_acs_education}. The educational attainment tables include only
#' the population 25 and over.
#' 
#' @format A data frame with 2080 rows and 8 variables:
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
#' is rebinned and used to find the "other" population.
#' 
"acsedutable"