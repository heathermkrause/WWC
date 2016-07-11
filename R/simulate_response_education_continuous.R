#' Simulate the true response to a continuous numerical response question in a 
#' given geography with specified characteristics for sex, race/ethnicity, 
#' and education
#' 
#' @param geographyfetch A geography created with the \code{geo.make()} function
#' of the acs package. The geography must be the entire U.S. or a single state
#' or a single county.
#' @param lambda_sex Numeric vector specifying lambda (Poisson distribution) 
#' for the survey respondents by sex in the order male, then female, for 
#' example, \code{c(25, 75)}.
#' @param lambda_raceethnicity Numeric vector specifying lambda for the survey 
#' respondents by race/ethnicity in the order white alone, Hispanic or Latino, 
#' black alone, Asian alone, and other, for example, 
#' \code{c(90, 10, 50, 50, 50)}.
#' @param lambda_education Numeric vector specifying lambda for survey 
#' respondents by educational attainment in the following bins: less than 
#' high school diploma, high school graduate (includes equivalency), some 
#' college or associate's degree, bachelor's degree or higher, for example, 
#' \code{c(20, 40, 60, 80)}.
#' 
#' @return A data frame with 3 columns (\code{value}, \code{answer}, and 
#' \code{result}) that tabulates the true response to the continuous, numerical 
#' survey question in the given geography.
#' 
#' @import dplyr
#' 
#' @name simulate_response_education_continuous
#' 
#' @examples 
#' 
#' \dontrun{
#' library(acs)
#' # if you are new to using the acs package, you will need to get an API key
#' # and run api.key.install() one time to install your key on your system
#' unitedstates <- geo.make(us = TRUE)
#' # lambda_sex specifies the response weights of men/women
#' # in this example, women have a higher mean response than men
#' lambda_sex <- c(25, 75)
#' lambda_raceethnicity <- c(90, 10, 50, 50, 50)
#' lambda_education <- c(20, 40, 60, 80)
#' opinionDF <- simulate_response_education_continuous(unitedstates, 
#'                                                      lambda_sex, 
#'                                                      lambda_raceethnicity,
#'                                                      lambda_education)
#' }
#' 
#' @export


simulate_response_education_continuous <- function(geographyfetch, 
                                                   lambda_sex, 
                                                   lambda_raceethnicity, 
                                                   lambda_education) {
        
        if (length(lambda_sex) != 2) 
                stop("lambda_sex must be a vector of length 2")
        if (length(lambda_raceethnicity) != 5) 
                stop("lambda_raceethnicity must be a vector of length 5")
        if (length(lambda_education) != 4) 
                stop("lambda_education must be a vector of length 4")
        sex_sample <- c("Male", "Female")
        names(lambda_sex) <- sex_sample
        raceethnicity_sample <- c("WHITE ALONE, NOT HISPANIC OR LATINO", 
                                  "HISPANIC OR LATINO", 
                                  "BLACK OR AFRICAN AMERICAN ALONE", 
                                  "ASIAN ALONE", 
                                  "OTHER")
        names(lambda_raceethnicity) <- raceethnicity_sample
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
        lambda_age <- rep(NA, 14)
        names(lambda_age) <- age_sample
        education_sample <- c("Less than high school diploma",
                              "High school graduate (includes equivalency)",
                              "Some college or associate's degree",
                              "Bachelor's degree or higher")
        names(lambda_education) <- education_sample
        
        
        # fetch education data tables from ACS
        acseducationDF <- process_acs_education(geographyfetch)
        
        # find yes/no opinion proportions for education data table
        acsDF <- acseducationDF %>% 
                mutate(sex_lambda = lambda_sex[sex], 
                       race_lambda = lambda_raceethnicity[raceethnicity],
                       education_lambda = lambda_education[education]) %>%
                rowwise() %>%
                mutate(response = mean(c(sex_lambda, 
                                         race_lambda, 
                                         education_lambda), na.rm = TRUE)) %>%
                select(-contains("lambda")) %>%
                ungroup()
        
        opinionDF <- acsDF %>% summarise(value = sum(response*prob)) %>% 
                mutate(answer = "response", result = "Population")
        opinionDF
}