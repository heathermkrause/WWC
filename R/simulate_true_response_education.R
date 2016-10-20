#' Simulate the true response to a yes/no response question in a given
#' population with specified opinions for sex, race/ethnicity, age, and 
#' geography
#' 
#' @param geovector A vector of geographies specified as a two letter 
#' abbreviation or a 5-digit FIPS code. Each geography must be the entire U.S. 
#' (\code{US}) or a single state (for example, \code{TX} or \code{CA}) or a 
#' single county (for example, \code{49035}). For example, \code{c("TX", "UT")}.
#' @param odds_geography Numeric vector specifying the opinion odds of the 
#' survey respondents by geography in the same bins as \code{geovector}.
#' @param odds_sex Numeric vector specifying the opinion odds of the survey
#' respondents by sex in the order male, then female. For example, \code{c(0.8,
#' 1.25)} means that men are 0.8 times as likely to approve the survey question
#' and women are 1.25 times as likely to approve the survey question.
#' @param odds_raceethnicity Numeric vector specifying the opinion odds of
#' the survey respondents by race/ethnicity in the order white alone, Hispanic 
#' or Latino, black alone, Asian alone, and other, for example, 
#' \code{c(0.2, 2, 2.5, 1, 1)}.
#' @param odds_education Numeric vector specifying the opinion odds of the 
#' survey respondents by educational attainment in the following bins: less than 
#' high school diploma, high school graduate (includes equivalency), some 
#' college or associate's degree, bachelor's degree or higher, for example, 
#' \code{c(0.4, 0.5, 2, 2.5)}.
#' 
#' @return A data frame with 3 columns (\code{value}, \code{answer}, and 
#' \code{result}) that tabulates the true opinion on the yes/no question 
#' in the given population.
#' 
#' @import dplyr
#' 
#' @name simulate_true_response_education
#' 
#' @examples 
#' geovector <- c("AL", "CT")
#' odds_geography <- c(1.5, 0.8)
#' odds_sex <- c(0.5, 2)
#' odds_raceethnicity <- c(0.2, 2, 2.5, 1, 1)
#' odds_education <- c(0.4, 0.5, 2, 2.5)
#' opinionDF <- simulate_true_response_education(geovector,
#'                                      odds_geography, 
#'                                      odds_sex, 
#'                                      odds_raceethnicity,
#'                                      odds_education)
#' 
#' @export


simulate_true_response_education <- function(geovector, odds_geography, 
                                             odds_sex, odds_raceethnicity, 
                                             odds_education) {
        
        if (length(geovector) != length(odds_geography))
                stop("geovector and odds_geography must have the same length")
        if (length(odds_sex) != 2) 
                stop("odds_sex must be a vector of length 2")
        if (length(odds_raceethnicity) != 5) 
                stop("odds_raceethnicity must be a vector of length 5")
        if (length(odds_education) != 4) 
                stop("odds_education must be a vector of length 4")
        sex_sample <- c("Male", "Female")
        names(odds_sex) <- sex_sample
        raceethnicity_sample <- c("WHITE ALONE, NOT HISPANIC OR LATINO", 
                                  "HISPANIC OR LATINO", 
                                  "BLACK OR AFRICAN AMERICAN ALONE", 
                                  "ASIAN ALONE", 
                                  "OTHER")
        names(odds_raceethnicity) <- raceethnicity_sample
        age_sample <- c("Under 18 years",
                        "18 to 24 years",
                        "25 to 44 years",
                        "45 to 64 years",
                        "65 years and over")
        odds_age <- rep(NA, 14)
        names(odds_age) <- age_sample
        education_sample <- c("Less than high school diploma",
                              "High school graduate (includes equivalency)",
                              "Some college or associate's degree",
                              "Bachelor's degree or higher")
        names(odds_education) <- education_sample
        names(odds_geography) <- geovector
        
        
        
        # fetch ACS education data tables
        acsDF <- acsedutable %>% filter(region %in% geovector)
        
        # find yes/no opinion proportions for education data table
        acsDF <- acsDF %>% 
                mutate(sex_od = odds_sex[sex], 
                       race_od = odds_raceethnicity[raceethnicity],
                       edu_od = odds_education[education],
                       geo_od = odds_geography[region],
                       odds = sex_od * race_od * edu_od * geo_od,
                       yes = odds / (odds + 1),
                       no = 1 - yes) %>%
                select(-contains("odds"))
        
        totalpop <- as.numeric(acsDF %>% distinct(geototal) %>% 
                                       summarise(sum = sum(geototal)))
        totals <- acsDF %>% group_by(region) %>% distinct(geototal) %>% 
                ungroup %>% 
                mutate(geoprob = geototal / totalpop)
        
        acsDF <- acsDF %>% left_join(totals, by = "region")
        
        opinionDF <- bind_rows(acsDF %>% 
                                       filter(education != "Total") %>%
                                       summarise(value = sum(yes*prob*geoprob)) %>% 
                                       mutate(answer = "responseyes", result = "Population"),
                               acsDF %>% 
                                       filter(education != "Total") %>%
                                       summarise(value = sum(no*prob*geoprob)) %>% 
                                       mutate(answer = "responseno", result = "Population"))
        opinionDF
}