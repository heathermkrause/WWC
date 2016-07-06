#' Simulate a single survey to a question with a continuous numerical answer 
#' according to inputs
#' 
#' @param prop_sex Numeric vector specifying the gender characteristics of the
#' survey respondents as proportions in the order male, then female, for
#' example, \code{c(0.49, 0.51)}. Must sum to 1, i.e., all respondents fall into
#' one of these gender bins.
#' @param lambda_sex Numeric vector specifying lambda (Poisson distribution) 
#' for the survey respondents by sex in the order male, then female, for 
#' example, \code{c(25, 75)}.
#' @param prop_raceethnicity Numeric vector specifying the racial/ethnic 
#' characteristics of the survey respondendents as proportions in the order
#' white alone, Hispanic or Latino, black alone, Asian alone, and other, 
#' for example, \code{c(0.6, 0.22, 0.11, 0.05, 0.02)}. Must sum to 1, i.e., all
#' respondents fall into one of these racial/ethnic bins.
#' @param lambda_raceethnicity Numeric vector specifying lambda for the 
#' survey respondents by race/ethnicity in the same order as 
#' \code{prop_raceethnicity}, for example, \code{c(90, 10, 50, 50, 50)}.
#' @param prop_age Numeric vector specifying the age characteristics of the
#' survey respondents as proportions in the following bins: under 5 years, 5 to 
#' 9 years, 10 to 14 years, 15 to 17 years, 18 and 19 years, 20 to 24 years, 25 
#' to 29 years, 30 to 34 years, 35 to 44 years, 45 to 54 years, 55 to 64 years, 
#' 65 to 74 years, 75 to 84 years, 85 years and over, for example, \code{c(0, 
#' 0, 0, 0.05, 0.1, 0.1, 0.12, 0.13, 0.12, 0.11, 0.1, 0.1, 0.05, 0.02)}. Must
#' sum to 1, i.e., all respondents must fall into one of these age bins.
#' @param lambda_age Numeric vector specifying lambda for survey respondents 
#' by age in the same order bins as \code{prop_age}, for example, 
#' \code{c(50, 50, 50, 50, 50, 50, 50, 55, 55, 55, 65, 75, 85, 95)}.
#' @param prop_education Numeric vector specifying the educational attainment
#' of the survey respondents as proportions in the following bins: less than 
#' high school diploma, high school graduate (includes equivalency), some 
#' college or associate's degree, bachelor's degree or higher, for example,
#' \code{c(0.1, 0.2, 0.4, 0.3)}. Must sum to 1, i.e., all respondents must fall
#' into one of these educational attainment bins.
#' @param lambda_education Numeric vector specifying lambda for the survey 
#' respondents by educational attainment in the same order bins as 
#' \code{prop_education}, for example, \code{c(20, 40, 60, 80)}.
#' @param n Number of respondents in the survey (default is 1000)
#' 
#' @details The numerical value for each survey respondent is simulated using
#' the Poisson distribution. The \code{lambda} value for each respondent is
#' calculated by taking the mean of \code{lambda} for that respondent's sex,
#' race/ethnicity, etc.
#'
#' @import dplyr
#' @importFrom stats rpois
#'
#' @name simulate_survey_continuous
#' 
#' @examples 
#' 
#' # prop_sex specifies how many men/women are in the survey
#' # in this example, the survey is 48% men and 52% women
#' prop_sex <- c(0.48, 0.52)
#' # lambda_sex specifies the response weights of men/women
#' # in this example, women have a higher mean response than men
#' lambda_sex <- c(25, 75)
#' prop_raceethnicity <- c(0.55, 0.25, 0.1, 0.05, 0.05)
#' lambda_raceethnicity <- c(90, 10, 50, 50, 50)
#' prop_age <- c(0, 0, 0, 0.04, 0.1, 0.1, 0.12, 0.13, 0.12, 0.11, 0.11, 0.09, 0.06, 0.02)
#' lambda_age <- c(50, 50, 50, 50, 50, 50, 50, 55, 55, 60, 65, 70, 75, 80)
#' prop_education <- c(0.1, 0.3, 0.4, 0.2)
#' lambda_education <- c(20, 40, 60, 80)
#' mysurvey <- simulate_survey_continuous(prop_sex, lambda_sex,
#'                                      prop_raceethnicity, lambda_raceethnicity,
#'                                      prop_age, lambda_age,
#'                                      prop_education, lambda_education,
#'                                      n = 900)
#' 
#' @export

simulate_survey_continuous <- function(prop_sex, lambda_sex,
                            prop_raceethnicity, lambda_raceethnicity,
                            prop_age, lambda_age,
                            prop_education, lambda_education,
                            n = 1000) {
        
        if (sum(prop_sex) != 1) stop("prop_sex does not sum to 1")
        if (sum(prop_raceethnicity) != 1) stop("prop_raceethnicity does not sum to 1")
        if (sum(prop_age) != 1) stop("prop_age does not sum to 1")
        if (sum(prop_education) != 1) stop("prop_education does not sum to 1")
        
        sex_sample <- c("Male", "Female")
        raceethnicity_sample <- c("WHITE ALONE, NOT HISPANIC OR LATINO", 
                                  "HISPANIC OR LATINO", 
                                  "BLACK OR AFRICAN AMERICAN ALONE", 
                                  "ASIAN ALONE", 
                                  "OTHER")
        age_sample <- c("Under 5 years",
                        "5 to 9 years",
                        "10 to 14 years",
                        "15 to 17 years",
                        "18 and 19 years",
                        "20 to 24 years",
                        "25 to 29 years",
                        "30 to 34 years",
                        "35 to 44 years",
                        "45 to 54 years",
                        "55 to 64 years",
                        "65 to 74 years",
                        "75 to 84 years",
                        "85 years and over")
        education_sample <- c("Less than high school diploma",
                              "High school graduate (includes equivalency)",
                              "Some college or associate's degree",
                              "Bachelor's degree or higher")
        
        myList <- purrr::rerun(n, 
                               data_frame(sex = sample(sex_sample,
                                                       size = 1,
                                                       prob = prop_sex)) %>%
                                       mutate(raceethnicity = sample(raceethnicity_sample, 
                                                                     size = 1, 
                                                                     prob = prop_raceethnicity)) %>%
                                       mutate(age = sample(age_sample,
                                                           size = 1,
                                                           prob = prop_age)) %>%
                                       mutate(education = sample(education_sample,
                                                                 size = 1,
                                                                 prob = prop_education)))
        
        calc_response <- function(myDF, 
                                  sex_sample, raceethnicity_sample, 
                                  age_sample, education_sample,
                                  lambda_sex, lambda_raceethnicity,
                                  lambda_age, lambda_education) {
                # this function takes a 1 x 4 data frame with sex, race/ethnicity,
                # age, and education and calculates the yes/no response
                overalllambda <- mean(c(lambda_sex[which(sex_sample == myDF$sex)], 
                                    lambda_raceethnicity[which(raceethnicity_sample == myDF$raceethnicity)],
                                    lambda_age[which(age_sample == myDF$age)],
                                    lambda_education[which(education_sample == myDF$education)]),
                                    na.rm = TRUE)
                myDF <- myDF %>% mutate(response = rpois(1, overalllambda))
                myDF
        }
        
        
        
        myList <- purrr::map(myList, calc_response, 
                             sex_sample, raceethnicity_sample, 
                             age_sample, education_sample,
                             lambda_sex, lambda_raceethnicity,
                             lambda_age, lambda_education)        
        mySurvey <- purrr::map_df(myList, bind_rows)
        mySurvey        
}