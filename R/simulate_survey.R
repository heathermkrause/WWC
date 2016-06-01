#' Simulate a single survey to a yes/no response question in a U.S. state
#' 
#' @param n Number of respondents in the survey (default is 1000)
#' @param prob_sex Numeric vector specifying the gender characteristics of the
#' survey respondents as proportions in the order male, then female, for
#' example, \code{c(0.49, 0.51)}. Must sum to 1, i.e., all respondents fall into
#' one of these gender bins.
#' @param prob_raceethnicity Numeric vector specifying the racial/ethnic 
#' characteristics of the survey respondendents as proportions in the order
#' white alone, Hispanic or Latino, black alone, Asian alone, and other, 
#' for example, \code{c(0.6, 0.22, 0.11, 0.05, 0.02)}. Must sum to 1, i.e., all
#' respondents fall into one of these racial/ethnic bins.
#' @param prob_age Numeric vector specifying the age characteristics of the
#' survey respondents as proportions in the following bins: under 5 years, 5 to 
#' 9 years, 10 to 14 years, 15 to 17 years, 18 and 19 years, 20 to 24 years, 25 
#' to 29 years, 30 to 34 years, 35 to 44 years, 45 to 54 years, 55 to 64 years, 
#' 65 to 74 years, 75 to 84 years, 85 years and over, for example, \code{c(0, 
#' 0, 0, 0.05, 0.1, 0.1, 0.12, 0.13, 0.12, 0.11, 0.1, 0.1, 0.05, 0.02)}. Must
#' sum to 1, i.e., all respondents must fall into one of these age bins.
#' @param prob_education Numeric vector specifying the educational attainment
#' of the survey respondents as proportions in the following bins: less than 
#' high school diploma, high school graduate (includes equivalency), some 
#' college or associate's degree, bachelor's degree or higher, for example,
#' \code{c(0.1, 0.2, 0.4, 0.3)}. Must sum to 1, i.e., all respondents must fall
#' into one of these educational attainment bins.
#' 
#' @import dplyr
#'
#' @name simulate_survey
#' 
#' @examples 
#' 
#' prob_sex <- c(0.48, 0.52)
#' prob_raceethnicity <- c(0.65, 0.25, 0.1, 0.05, 0.05)
#' prob_age <- c(0, 0, 0, 0.04, 0.1, 0.1, 0.12, 0.13, 0.12, 0.11, 0.11, 0.09, 0.06, 0.02)
#' prob_education <- c(0.1, 0.3, 0.4, 0.2)
#' mySurvey <- simulate_survey(900, prob_sex, prob_raceethnicity, prob_age, prob_education)
#' 
#' @export

simulate_survey <- function(n = 1000, prob_sex, prob_raceethnicity,
                            prob_age, prob_education) {
        
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
        
        #statefetch <- acs::geo.make(state = state)
        #acseducationDF <- process_acs_education(geographyfetch = statefetch)
        #acsageDF <- process_acs_age(geographyfetch = statefetch)
        
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
                                       
        mySurvey <- purrr::map_df(myList, bind_rows)
        mySurvey        
}