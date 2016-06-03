#' Simulate a single survey to a yes/no response question according to inputs
#' 
#' @param n Number of respondents in the survey (default is 1000)
#' @param prob_sex Numeric vector specifying the gender characteristics of the
#' survey respondents as proportions in the order male, then female, for
#' example, \code{c(0.49, 0.51)}. Must sum to 1, i.e., all respondents fall into
#' one of these gender bins.
#' @param weight_sex Numeric vector specifying the opinion weights of the survey
#' respondents by sex in the order male, then female. For example, \code{c(0.8,
#' 1.25)} means that men are 0.8 times as likely to approve the survey question
#' and women are 1.25 times as likely to approve the survey question. The 
#' product of all elements (\code{prod()}) must equal 1.
#' @param prob_raceethnicity Numeric vector specifying the racial/ethnic 
#' characteristics of the survey respondendents as proportions in the order
#' white alone, Hispanic or Latino, black alone, Asian alone, and other, 
#' for example, \code{c(0.6, 0.22, 0.11, 0.05, 0.02)}. Must sum to 1, i.e., all
#' respondents fall into one of these racial/ethnic bins.
#' @param weight_raceethnicity Numeric vector specifying the opinion weights of
#' the survey respondents by race/ethnicity in the same order as 
#' \code{prob_raceethnicity}, for example, \code{c(0.2, 2, 2.5, 1, 1)}.
#' The product of all elements (\code{prod()}) must equal 1.
#' @param prob_age Numeric vector specifying the age characteristics of the
#' survey respondents as proportions in the following bins: under 5 years, 5 to 
#' 9 years, 10 to 14 years, 15 to 17 years, 18 and 19 years, 20 to 24 years, 25 
#' to 29 years, 30 to 34 years, 35 to 44 years, 45 to 54 years, 55 to 64 years, 
#' 65 to 74 years, 75 to 84 years, 85 years and over, for example, \code{c(0, 
#' 0, 0, 0.05, 0.1, 0.1, 0.12, 0.13, 0.12, 0.11, 0.1, 0.1, 0.05, 0.02)}. Must
#' sum to 1, i.e., all respondents must fall into one of these age bins.
#' @param weight_age Numeric vector specifying the opinion weights of the
#' survey respondents by age in the same order bins as \code{prob_age}, for
#' example, \code{c(1, 1, 1, 1, 1, 1, 1, 0.5, 2, 2, 2, 2.5, 0.5, 0.2)}. The 
#' product of all elements (\code{prod()}) must equal 1.
#' @param prob_education Numeric vector specifying the educational attainment
#' of the survey respondents as proportions in the following bins: less than 
#' high school diploma, high school graduate (includes equivalency), some 
#' college or associate's degree, bachelor's degree or higher, for example,
#' \code{c(0.1, 0.2, 0.4, 0.3)}. Must sum to 1, i.e., all respondents must fall
#' into one of these educational attainment bins.
#' @param weight_education Numeric vector specifying the opinion weights of the survey
#' respondents by educational attainment in the same order bins as 
#' \code{prob_education}, for example, \code{c(0.4, 0.5, 2, 2.5)}. The 
#' product of all elements (\code{prod()}) must equal 1.
#' 
#' @import dplyr
#'
#' @name simulate_survey
#' 
#' @examples 
#' 
#' # prob_sex specifies how many men/women are in the survey
#' # in this example, the survey is 48% men and 52% women
#' prob_sex <- c(0.48, 0.52)
#' # weight_sex specifies the opinions of men/women
#' # in this example, women are twice as likely to approve and men half as likely
#' weight_sex <- c(0.5, 2)
#' prob_raceethnicity <- c(0.55, 0.25, 0.1, 0.05, 0.05)
#' weight_raceethnicity <- c(0.2, 2, 2.5, 1, 1)
#' prob_age <- c(0, 0, 0, 0.04, 0.1, 0.1, 0.12, 0.13, 0.12, 0.11, 0.11, 0.09, 0.06, 0.02)
#' weight_age <- c(1, 1, 1, 1, 1, 1, 1, 0.5, 2, 2, 2, 2.5, 0.5, 0.2)
#' prob_education <- c(0.1, 0.3, 0.4, 0.2)
#' weight_education <- c(0.4, 0.5, 2, 2.5)
#' mySurvey <- simulate_survey(900, prob_sex, weight_sex,
#'                              prob_raceethnicity, weight_raceethnicity,
#'                              prob_age, weight_age,
#'                              prob_education, weight_education)
#' 
#' @export

simulate_survey <- function(n = 1000, prob_sex, weight_sex,
                            prob_raceethnicity, weight_raceethnicity,
                            prob_age, weight_age,
                            prob_education, weight_education) {
        
        if (sum(prob_sex) != 1) stop("prob_sex does not sum to 1")
        if (sum(prob_raceethnicity) != 1) stop("prob_raceethnicity does not sum to 1")
        if (sum(prob_age) != 1) stop("prob_age does not sum to 1")
        if (sum(prob_education) != 1) stop("prob_education does not sum to 1")
        if (prod(weight_sex) != 1) 
                stop("The product of the elements of weight_sex does not equal 1")
        if (prod(weight_raceethnicity) != 1) 
                stop("The product of the elements of weight_raceethnicity does not equal 1")
        if (prod(weight_age) != 1) 
                stop("The product of the elements of weight_age does not equal 1")
        if (prod(weight_education) != 1) 
                stop("The product of the elements of weight_education does not equal 1")
        
                
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
                                                       prob = prob_sex)) %>%
                                       mutate(raceethnicity = sample(raceethnicity_sample, 
                                                                     size = 1, 
                                                                     prob = prob_raceethnicity)) %>%
                                       mutate(age = sample(age_sample,
                                                           size = 1,
                                                           prob = prob_age)) %>%
                                       mutate(education = sample(education_sample,
                                                                 size = 1,
                                                                 prob = prob_education)))
        
        calc_response <- function(myDF) {
                # this function takes a 1 x 4 data frame with sex, race/ethnicity,
                # age, and education and calculates the yes/no response
                weight <- weight_sex[2] * weight_raceethnicity[2] * 
                        weight_age[8] * weight_education[3]
                prob_yes <- weight/(weight + 1)
                myDF <- myDF %>% mutate(response = sample(c("yes", "no"), 
                                                  size = 1,
                                                  prob = c(prob_yes, 1 - prob_yes)))
                myDF
        }
        

        
        myList <- purrr::map(myList, calc_response)        
        mySurvey <- purrr::map_df(myList, bind_rows)
        mySurvey        
}