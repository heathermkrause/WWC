#' Simulate the true response to a yes/no response question in a given
#' geography
#' 
#' @param geographyfetch A geography created with the \code{geo.make()} function
#' of the acs package. The geography must be the entire U.S. or a single state
#' or a single county.
#' @param weight_sex Numeric vector specifying the opinion weights of the survey
#' respondents by sex in the order male, then female. For example, \code{c(0.8,
#' 1.25)} means that men are 0.8 times as likely to approve the survey question
#' and women are 1.25 times as likely to approve the survey question.
#' @param weight_raceethnicity Numeric vector specifying the opinion weights of
#' the survey respondents by race/ethnicity in the same order as 
#' \code{prob_raceethnicity}, for example, \code{c(0.2, 2, 2.5, 1, 1)}.
#' @param weight_age Numeric vector specifying the opinion weights of the
#' survey respondents by age in the same order bins as \code{prob_age}, for
#' example, \code{c(1, 1, 1, 1, 1, 1, 1, 0.5, 2, 2, 2, 2.5, 0.5, 0.2)}.
#' @param weight_education Numeric vector specifying the opinion weights of the survey
#' respondents by educational attainment in the same order bins as 
#' \code{prob_education}, for example, \code{c(0.4, 0.5, 2, 2.5)}.
#' 
#' @return A data frame with 3 columns (\code{response}, \code{answer}, and 
#' \code{result}) that tabulates the true opinion on the yes/no question 
#' in the given geography.
#' 
#' @import dplyr
#' 
#' @name simulate_true_response
#' 
#' @examples 
#' 
#' \dontrun{
#' library(acs)
#' # if you are new to using the acs package, you will need to get an API key
#' # and run api.key.install() one time to install your key on your system
#' unitedstates <- geo.make(us = TRUE)
#' # weight_sex specifies the opinions of men/women
#' # in this example, women are twice as likely to approve and men half as likely
#' weight_sex <- c(0.5, 2)
#' weight_raceethnicity <- c(0.2, 2, 2.5, 1, 1)
#' weight_age <- c(1, 1, 1, 1, 1, 1, 1, 0.8, 2, 2, 2.5, 3, 0.5, 0.2)
#' weight_education <- c(0.4, 0.5, 2, 2.5)
#' opinionDF <- simulate_true_response(unitedstates, 
#'                                      weight_sex, weight_raceethnicity,
#'                                      weight_age, weight_education)
#' }
#' 
#' @export


simulate_true_response <- function(geographyfetch, 
                                   weight_sex, weight_raceethnicity,
                                   weight_age, weight_education) {

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

        acseducationDF <- process_acs_education(geographyfetch)
        acsageDF <- process_acs_age(geographyfetch)
        
        calc_response <- function(myDF, 
                                  sex_sample, raceethnicity_sample, 
                                  age_sample, education_sample,
                                  weight_sex, weight_raceethnicity,
                                  weight_age, weight_education) {

                # this function takes a 1-row data frame with sex, race/ethnicity,
                # and age or education and calculates the yes/no response

                weight <- prod(weight_sex[which(sex_sample == myDF$sex)], 
                               weight_raceethnicity[which(raceethnicity_sample == myDF$raceethnicity)])
                if ("age" %in% colnames(myDF)) {
                        weight <- prod(weight, 
                                       weight_age[which(age_sample == myDF$age)])
                }
                if ("education" %in% colnames(myDF)) {
                        weight <- prod(weight,
                                       weight_education[which(education_sample == myDF$education)])
                }
                
                prob_yes <- weight/(weight + 1)
                myDF <- myDF %>% mutate(yes = prob_yes, 
                                        no = 1 - prob_yes) %>%
                        select(yes, no)
                myDF
        }
        

        acseducationDF <- acseducationDF %>% 
                purrr::by_row(calc_response, 
                           sex_sample, raceethnicity_sample, 
                           age_sample, education_sample,
                           weight_sex, weight_raceethnicity,
                           weight_age = rep(1, 14), weight_education,
                           .collate = "cols")
        acsageDF <- acsageDF %>% 
                purrr::by_row(calc_response, 
                              sex_sample, raceethnicity_sample, 
                              age_sample, education_sample,
                              weight_sex, weight_raceethnicity,
                              weight_age, weight_education = rep(1, 4),
                              .collate = "cols")

        opinionDF <- bind_rows(acseducationDF %>% 
                                       filter(education != "Total") %>%
                                       summarise(response = sum(yes1*prob)) %>% 
                                       mutate(answer = "yes", result = "Population (Edu)"),
                               acseducationDF %>% 
                                       filter(education != "Total") %>%
                                       summarise(response = sum(no1*prob)) %>% 
                                       mutate(answer = "no", result = "Population (Edu)"),
                               acsageDF %>% 
                                       filter(age != "Total") %>%
                                       summarise(response = sum(yes1*prob)) %>% 
                                       mutate(answer = "yes", result = "Population (Age)"),
                               acsageDF %>% 
                                       filter(age != "Total") %>%
                                       summarise(response = sum(no1*prob)) %>% 
                                       mutate(answer = "no", result = "Population (Age)"))
                               
        opinionDF
        
}