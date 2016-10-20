#' Simulate the true response to a continuous numerical response question in a 
#' given population with specified characteristics for sex, race/ethnicity, 
#' age, and geography
#' 
#' @param geovector A vector of geographies specified as a two letter 
#' abbreviation or a 5-digit FIPS code. Each geography must be the entire U.S. 
#' (\code{US}) or a single state (for example, \code{TX} or \code{CA}) or a 
#' single county (for example, \code{49035}). For example, \code{c("TX", "UT")}.
#' @param lambda_geography Numeric vector specifying lambda (Poisson 
#' distribution) for the survey respondents by geography in the same bins as 
#' \code{geovector}.
#' @param lambda_sex Numeric vector specifying lambda (Poisson distribution) 
#' for the survey respondents by sex in the order male, then female, for 
#' example, \code{c(25, 75)}.
#' @param lambda_raceethnicity Numeric vector specifying lambda for the survey 
#' respondents by race/ethnicity in the order white alone, Hispanic or Latino, 
#' black alone, Asian alone, and other, for example, 
#' \code{c(90, 10, 50, 50, 50)}.
#' @param lambda_age Numeric vector specifying lambda for survey respondents by 
#' age in the following bins: under 18 years, 18 to 24 years, 25 to 44 years, 
#' 45 to 64 years, 65 years and over, for example, \code{c(50, 55, 65, 75, 85)}.
#' 
#' @return A data frame with 3 columns (\code{value}, \code{answer}, and 
#' \code{result}) that tabulates the true response to the continuous, numerical 
#' survey question in the given population.
#' 
#' @import dplyr
#' 
#' @name simulate_response_age_continuous
#' 
#' @examples 
#' geovector <- c("WI", "WV")
#' lambda_geography <- c(50, 10)
#' # lambda_sex specifies the response weights of men/women
#' # in this example, women have a higher mean response than men
#' lambda_sex <- c(25, 75)
#' lambda_raceethnicity <- c(90, 10, 50, 50, 50)
#' lambda_age <- c(50, 60, 70, 75, 80)
#' opinionDF <- simulate_response_age_continuous(geovector,
#'                                              lambda_geography, 
#'                                              lambda_sex, 
#'                                              lambda_raceethnicity,
#'                                              lambda_age)
#' 
#' @export


simulate_response_age_continuous <- function(geovector, lambda_geography,
                                             lambda_sex, lambda_raceethnicity, 
                                             lambda_age) {
        
        if (length(geovector) != length(lambda_geography))
                stop("geovector and lambda_geography must have the same length")
        if (length(lambda_sex) != 2) 
                stop("lambda_sex must be a vector of length 2")
        if (length(lambda_raceethnicity) != 5) 
                stop("lambda_raceethnicity must be a vector of length 5")
        if (length(lambda_age) != 5) 
                stop("lambda_age must be a vector of length 5")
        sex_sample <- c("Male", "Female")
        names(lambda_sex) <- sex_sample
        raceethnicity_sample <- c("WHITE ALONE, NOT HISPANIC OR LATINO", 
                                  "HISPANIC OR LATINO", 
                                  "BLACK OR AFRICAN AMERICAN ALONE", 
                                  "ASIAN ALONE", 
                                  "OTHER")
        names(lambda_raceethnicity) <- raceethnicity_sample
        age_sample <- c("Under 18 years",
                        "18 to 24 years",
                        "25 to 44 years",
                        "45 to 64 years",
                        "65 years and over")
        names(lambda_age) <- age_sample
        education_sample <- c("Less than high school diploma",
                              "High school graduate (includes equivalency)",
                              "Some college or associate's degree",
                              "Bachelor's degree or higher")
        lambda_education <- rep(NA, 4)
        names(lambda_education) <- education_sample
        names(lambda_geography) <- geovector

        # fetch ACS age data tables
        acsDF <- acsagetable %>% filter(region %in% geovector)
        
        # find response in age data table
        acsDF <- acsDF %>% 
                mutate(sex_lambda = lambda_sex[sex], 
                       race_lambda = lambda_raceethnicity[raceethnicity],
                       age_lambda = lambda_age[age],
                       geo_lambda = lambda_geography[region]) %>%
                rowwise() %>%
                mutate(response = mean(c(sex_lambda, 
                                         race_lambda, 
                                         age_lambda,
                                         geo_lambda), na.rm = TRUE)) %>%
                select(-contains("lambda")) %>%
                ungroup()
        
        totalpop <- as.numeric(acsDF %>% distinct(geototal) %>% 
                                       summarise(sum = sum(geototal)))
        totals <- acsDF %>% group_by(region) %>% distinct(geototal) %>% 
                ungroup %>% 
                mutate(geoprob = geototal / totalpop)
        
        acsDF <- acsDF %>% left_join(totals, by = "region")
        
        opinionDF <- acsDF %>% summarise(value = sum(response*prob*geoprob)) %>% 
                mutate(answer = "response", result = "Population")
        opinionDF
}