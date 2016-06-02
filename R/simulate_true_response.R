#' Simulate the true response to a yes/no response question in a given
#' geography
#' 
#' @param geographyfetch A geography created with the \code{geo.make()} function
#' of the acs package. The geography must be the entire U.S. or a single state
#' or a single county.
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
#' unitedstates <- geo.make(us = TRUE)
#' opinionDF <- simulate_true_response(unitedstates)
#' }
#' 
#' @export


simulate_true_response <- function(geographyfetch) {

        acseducationDF <- process_acs_education(geographyfetch)
        acsageDF <- process_acs_age(geographyfetch)
        
        calc_response <- function(myDF) {
                # this function takes a data frame with sex, race/ethnicity,
                # age, and education and calculates the yes/no response
                myDF <- myDF %>% mutate(yes = 0.4 * population,
                                        no = 0.6 * population)
                myDF
        }
        
        acseducationDF <- calc_response(acseducationDF)
        acsageDF <- calc_response(acsageDF)
        
        opinionDF <- bind_rows(acseducationDF %>% 
                                       summarise(response = sum(yes)/sum(population)) %>% 
                                       mutate(answer = "yes", result = "Population (Edu)"),
                               acseducationDF %>% 
                                       summarise(response = sum(no)/sum(population)) %>% 
                                       mutate(answer = "no", result = "Population (Edu)"),
                               acsageDF %>% 
                                       summarise(response = sum(yes)/sum(population)) %>% 
                                       mutate(answer = "yes", result = "Population (Age)"),
                               acsageDF %>% 
                                       summarise(response = sum(no)/sum(population)) %>% 
                                       mutate(answer = "no", result = "Population (Age)"))
                               
        opinionDF
        
}