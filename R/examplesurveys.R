#' A simulated survey with 20 respondents
#' 
#' A very small simulated survey created using the 
#' \code{simulate_survey_continuous} function, for use in unit tests, etc.
#' 
#' @format A data frame with 20 rows and 5 variables:
#' \describe{
#' \item{sex}{Either male or female}
#' \item{raceethnicity}{White alone (not Hispanic or 
#' Latino), Hispanic or Latino, black alone, Asian alone, or other}
#' \item{age}{Under 18 years, 18 to 24 years, 25 to 44 years, 45 to 64 years,  
#' or 65 years and over}
#' \item{education}{Less than high school diploma, high school graduate 
#' (includes equivalency), some college or associate's degree, bachelor's 
#' degree or higher}
#' \item{geography}{\code{UT} only}
#' \item{response}{A numerical, continuous response to a survey question}
#' }
"tinysurvey"

#' A simulated survey with missing data
#' 
#' A simulated survey created using the \code{simulate_survey} function with 
#' 1000 respondents and about 20% missing data (\code{NA}s).
#' 
#' @format A data frame with 1000 rows and 5 variables:
#' \describe{
#' \item{sex}{Either male or female}
#' \item{raceethnicity}{White alone (not Hispanic or 
#' Latino), Hispanic or Latino, black alone, Asian alone, or other}
#' \item{age}{Under 18 years, 18 to 24 years, 25 to 44 years, 45 to 64 years,  
#' or 65 years and over}
#' \item{education}{Less than high school diploma, high school graduate 
#' (includes equivalency), some college or associate's degree, bachelor's 
#' degree or higher}
#' \item{geography}{\code{TX} only}
#' \item{response}{Yes or no}
#' }
"surveymissing"
