#' Simulate a single survey to a yes/no response question in a U.S. state
#' 
#' @param n Number of respondents in the survey (default is 1000)
#' @param state Two letter string abbreviation for the state to simulate in, for
#' example, \code{"TX"}, \code{"UT"}, or \code{"WA"}
#' @param raceethnicity Numeric vector specifying the racial/ethnic 
#' characteristics of the survey respondendents as proportions in the order
#' white alone, all Hispanic, black alone, Asian alone, and other, 
#' for example, \code{c(0.6, 0.22, 0.11, 0.05, 0.02)}. Must sum to 1, i.e., all
#' respondents fall into one of these racial/ethnic bins.
#' @param gender Numeric vector specifying the gender characteristics of the
#' survey respondents as proportions in the order male, then female, for
#' example, \code{c(0.49, 0.51)}. Must sum to 1, i.e., all respondents fall into
#' one of these gender bins.
#' @param opinionyes Numeric vector specifying what proportion of each 
#' racial/ethnic group approves of the survey question, for example, 
#' \code{c(0.2, 0.7, 0.5, 0.1, 0.9)}
#' 
#' 
#' @import dplyr
#' @import acs
#'
#' @name simulate_survey
#' 
#' @examples 
#' 
#' myRaceEthnicity <- c(0.65, 0.25, 0.1, 0.05, 0.05)
#' myGender <- c(0.48, 0.52)
#' myOpinions <- c(0.8, 0.2, 0.9, 0.5, 0.5)
#' mySurvey <- simulate_survey(900, myraceethnicity, myopinions)
#' 
#' 
#' @export

simulate_survey <- function(n = 1000, state, opinionyes) {
        
        acseducationDF <- simulate_survey_ACS_education(state = state)
        acsageDF <- simulate_survey_ACS_age(state = state)
        
        
        # NOT WORKING FROM HERE ON DOWN RIGHT NOW
        opinionno <- 1 - opinionyes
        
        myList <- purrr::rerun(n, data_frame(ethnicity = sample(c("White Alone", "All Hispanic", 
                                                      "Black Alone", "Asian Alone", 
                                                      "Other"), 
                                                    size = 1, 
                                                    prob = raceethnicity)) %>%
                                       mutate(gender = sample(c("Male", "Female"),
                                                              size = 1,
                                                              prob = gender),
                                              response = ifelse(ethnicity == "White Alone", 
                                                       sample(c("yes", "no"), size = 1,
                                                              prob = c(opinionyes[1], 
                                                                       opinionno[1])),
                                                       ifelse(ethnicity == "All Hispanic", 
                                                              sample(c("yes", "no"), size = 1, 
                                                                     prob = c(opinionyes[2], 
                                                                              opinionno[2])), 
                                                              ifelse(ethnicity == "Black Alone",
                                                                     sample(c("yes", "no"), size = 1, 
                                                                            prob = c(opinionyes[3], 
                                                                                     opinionno[3])),
                                                                     ifelse(ethnicity == "Asian Alone",
                                                                            sample(c("yes", "no"), size = 1, 
                                                                                   prob = c(opinionyes[4], 
                                                                                            opinionno[4])),
                                                                            sample(c("yes", "no"), size = 1, 
                                                                                   prob = c(opinionyes[5], 
                                                                                            opinionno[5]))))))))
        mySurvey <- purrr::map_df(myList, bind_rows)
        mySurvey        
}