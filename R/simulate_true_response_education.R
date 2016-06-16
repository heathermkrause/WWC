#' Simulate the true response to a yes/no response question in a given
#' geography with specified opinions for sex, race/ethnicity, and education
#' 
#' @param geographyfetch A geography created with the \code{geo.make()} function
#' of the acs package. The geography must be the entire U.S. or a single state
#' or a single county.
#' @param weight_sex Numeric vector specifying the opinion weights of the survey
#' respondents by sex in the order male, then female. For example, \code{c(0.8,
#' 1.25)} means that men are 0.8 times as likely to approve the survey question
#' and women are 1.25 times as likely to approve the survey question.
#' @param weight_raceethnicity Numeric vector specifying the opinion weights of
#' the survey respondents by race/ethnicity in the order white alone, Hispanic 
#' or Latino, black alone, Asian alone, and other, for example, 
#' \code{c(0.2, 2, 2.5, 1, 1)}.
#' @param weight_education Numeric vector specifying the opinion weights of the 
#' survey respondents by educational attainment in the following bins: less than 
#' high school diploma, high school graduate (includes equivalency), some 
#' college or associate's degree, bachelor's degree or higher, for example, 
#' \code{c(0.4, 0.5, 2, 2.5)}.
#' 
#' @return A data frame with 3 columns (\code{value}, \code{answer}, and 
#' \code{result}) that tabulates the true opinion on the yes/no question 
#' in the given geography.
#' 
#' @import dplyr
#' 
#' @name simulate_true_response_education
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
#' weight_education <- c(0.4, 0.5, 2, 2.5)
#' opinionDF <- simulate_true_response_education(unitedstates, 
#'                                      weight_sex, 
#'                                      weight_raceethnicity,
#'                                      weight_education)
#' }
#' 
#' @export


simulate_true_response_education <- function(geographyfetch, 
                                             weight_sex, 
                                             weight_raceethnicity, 
                                             weight_education) {
        
        if (length(weight_sex) != 2) 
                stop("weight_sex must be a vector of length 2")
        if (length(weight_raceethnicity) != 5) 
                stop("weight_raceethnicity must be a vector of length 5")
        if (length(weight_education) != 4) 
                stop("weight_education must be a vector of length 4")
        sex_sample <- c("Male", "Female")
        names(weight_sex) <- sex_sample
        raceethnicity_sample <- c("WHITE ALONE, NOT HISPANIC OR LATINO", 
                                  "HISPANIC OR LATINO", 
                                  "BLACK OR AFRICAN AMERICAN ALONE", 
                                  "ASIAN ALONE", 
                                  "OTHER")
        names(weight_raceethnicity) <- raceethnicity_sample
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
        weight_age <- rep(NA, 14)
        names(weight_age) <- age_sample
        education_sample <- c("Less than high school diploma",
                              "High school graduate (includes equivalency)",
                              "Some college or associate's degree",
                              "Bachelor's degree or higher")
        names(weight_education) <- education_sample
        
        
        # fetch education and age data tables from ACS
        acseducationDF <- process_acs_education(geographyfetch)
        # acsageDF <- process_acs_age(geographyfetch)
        
        # find yes/no opinion proportions for education data table
        acsDF <- acseducationDF %>% 
                mutate(sex_wt = weight_sex[sex], 
                       race_wt = weight_raceethnicity[raceethnicity],
                       edu_wt = weight_education[education],
                       wt = sex_wt * race_wt * edu_wt,
                       yes = wt / (wt + 1),
                       no = 1 - yes) %>%
                select(-contains("wt"))
        
        opinionDF <- bind_rows(acsDF %>% 
                                       filter(education != "Total") %>%
                                       summarise(value = sum(yes*prob)) %>% 
                                       mutate(answer = "yes", result = "Population"),
                               acsDF %>% 
                                       filter(education != "Total") %>%
                                       summarise(value = sum(no*prob)) %>% 
                                       mutate(answer = "no", result = "Population"))
        opinionDF
}