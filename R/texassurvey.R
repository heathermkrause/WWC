#' A simulated survey with 1000 respondents
#' 
#' A simulated survey created using the \code{simulate_survey} function for the
#' state of Texas. The respondents are biased compared to the real population
#' of Texas (2014 population estimates) with respect to sex and race/ethnicity
#' but are representative with respect to age and education.
#' 
#' @format A data frame with 1000 rows and 5 variables:
#' \describe{
#' \item{sex}{Either male or female}
#' \item{raceethnicity}{White alone (not Hispanic or 
#' Latino), Hispanic or Latino, black alone, Asian alone, or other}
#' \item{age}{Under 5 years, 5 to 9 years, 10 to 14 years, 15 to 17 years, 18 
#' and 19 years, 20 to 24 years, 25 to 29 years, 30 to 34 years, 35 to 44 years, 
#' 45 to 54 years, 55 to 64 years, 65 to 74 years, 75 to 84 years, 85 years and 
#' over}
#' \item{education}{Less than high school diploma, high school graduate 
#' (includes equivalency), some college or associate's degree, bachelor's 
#' degree or higher}
#' \item{response}{Yes or no}
#' }
"texassurvey"