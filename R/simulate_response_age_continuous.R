#' Simulate the true response to a continuous numerical response question in a 
#' given geography with specified characteristics for sex, race/ethnicity, 
#' and age
#' 
#' @param georegion A geographical region specified as a two letter abbreviation
#' or a 5-digit FIPS code. The geography must be the entire U.S. (\code{US}) 
#' or a single state (for example, \code{TX} or \code{CA}) or a single 
#' county (for example, \code{49035}).
#' @param lambda_sex Numeric vector specifying lambda (Poisson distribution) 
#' for the survey respondents by sex in the order male, then female, for 
#' example, \code{c(25, 75)}.
#' @param lambda_raceethnicity Numeric vector specifying lambda for the survey 
#' respondents by race/ethnicity in the order white alone, Hispanic or Latino, 
#' black alone, Asian alone, and other, for example, 
#' \code{c(90, 10, 50, 50, 50)}.
#' @param lambda_age Numeric vector specifying lambda for survey respondents by 
#' age in the following bins: under 5 years, 5 to 9 years, 10 to 14 years, 15 
#' to 17 years, 18 and 19 years, 20 to 24 years, 25 to 29 years, 30 to 34 years, 
#' 35 to 44 years, 45 to 54 years, 55 to 64 years, 65 to 74 years, 75 to 84 
#' years, 85 years and over, for example, 
#' \code{c(50, 50, 50, 50, 50, 50, 50, 55, 55, 55, 65, 75, 85, 95)}.
#' 
#' @return A data frame with 3 columns (\code{value}, \code{answer}, and 
#' \code{result}) that tabulates the true response to the continuous, numerical 
#' survey question in the given geography.
#' 
#' @import dplyr
#' 
#' @name simulate_response_age_continuous
#' 
#' @examples 
#' # lambda_sex specifies the response weights of men/women
#' # in this example, women have a higher mean response than men
#' lambda_sex <- c(25, 75)
#' lambda_raceethnicity <- c(90, 10, 50, 50, 50)
#' lambda_age <- c(50, 50, 50, 50, 50, 50, 50, 55, 55, 60, 65, 70, 75, 80)
#' opinionDF <- simulate_response_age_continuous(US, 
#'                                              lambda_sex, 
#'                                              lambda_raceethnicity,
#'                                              lambda_age)
#' 
#' @export


simulate_response_age_continuous <- function(georegion, lambda_sex, 
                                             lambda_raceethnicity, lambda_age) {
        
        if (length(lambda_sex) != 2) 
                stop("lambda_sex must be a vector of length 2")
        if (length(lambda_raceethnicity) != 5) 
                stop("lambda_raceethnicity must be a vector of length 5")
        if (length(lambda_age) != 14) 
                stop("lambda_age must be a vector of length 14")
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
        names(lambda_age) <- age_sample
        education_sample <- c("Less than high school diploma",
                              "High school graduate (includes equivalency)",
                              "Some college or associate's degree",
                              "Bachelor's degree or higher")
        lambda_education <- rep(NA, 4)
        names(lambda_education) <- education_sample

        # NSE magic
        georegion_ <- toupper(col_name(substitute(georegion)))
        
        # ACS age data tables
        acsageDF <- acsagetable %>% filter(region == georegion_)
        
        # find response in age data table
        acsDF <- acsageDF %>% 
                mutate(sex_lambda = lambda_sex[sex], 
                       race_lambda = lambda_raceethnicity[raceethnicity],
                       age_lambda = lambda_age[age]) %>%
                rowwise() %>%
                mutate(response = mean(c(sex_lambda, 
                                            race_lambda, 
                                            age_lambda), na.rm = TRUE)) %>%
                select(-contains("lambda")) %>%
                ungroup()
        
        opinionDF <- acsDF %>% summarise(value = sum(response*prob)) %>% 
                mutate(answer = "response", result = "Population")
        opinionDF
}