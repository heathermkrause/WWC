#' A simulated survey with 1000 respondents
#' 
#' A simulated survey created using the \code{simulate_survey} function for the
#' state of Texas. The respondents are biased compared to the real population
#' of Texas (2014 population estimates) with respect to sex and race/ethnicity
#' but are representative with respect to education and age (except no children 
#' are in the survey).
#' 
#' @format A data frame with 1000 rows and 5 variables:
#' \describe{
#' \item{sex}{Either male or female}
#' \item{raceethnicity}{White alone (not Hispanic or 
#' Latino), Hispanic or Latino, black alone, Asian alone, or other}
#' \item{age}{Under 18 years (none of these), 18 to 24 years, 25 to 44 years, 
#' 45 to 64 years, or 65 years and over}
#' \item{education}{Less than high school diploma, high school graduate 
#' (includes equivalency), some college or associate's degree, bachelor's 
#' degree or higher}
#' \item{geography}{\code{TX} only}
#' \item{response}{Yes or no}
#' }
"texassurvey"