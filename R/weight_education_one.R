#' Weight a survey using post-stratification for a single weighting indicator 
#' from ACS tables with education attainment data
#' 
#' @param mysurvey A survey data frame, such as that created by 
#' \code{simulate_survey}
#' @param response A column in \code{mysurvey} that contains the quantity to be
#' weighted, such as the response to a yes/no question as in 
#' \code{simulate_survey}
#' @param indicator A weighting indicator to be used for post-stratification.
#' One of \code{sex}, \code{raceethnicity}, \code{education}
#' @param geographyfetch A geography created with the \code{geo.make()} function
#' of the acs package. The geography must be the entire U.S. or a single state
#' or a single county.
#' 
#' @return A data frame with 3 columns (\code{answer}, \code{value}, and 
#' \code{result}) that tabulates the weighted response on the yes/no question 
#' in the given geography.
#' 
#' @import dplyr
#' @importFrom reshape2 melt
#' @importFrom stats as.formula
#' 
#' @name weight_education_one
#' 
#' @examples 
#' 
#' \dontrun{
#' library(acs)
#' # if you are new to using the acs package, you will need to get an API key
#' # and run api.key.install() one time to install your key on your system
#' unitedstates <- geo.make(us = TRUE)
#' # prob_sex specifies how many men/women are in the survey
#' # in this example, the survey is 48% men and 52% women
#' prob_sex <- c(0.48, 0.52)
#' # weight_sex specifies the opinions of men/women
#' # in this example, women are twice as likely to approve and men half as likely
#' weight_sex <- c(0.5, 2)
#' prob_raceethnicity <- c(0.55, 0.25, 0.1, 0.05, 0.05)
#' weight_raceethnicity <- c(0.2, 2, 2.5, 1, 1)
#' prob_age <- c(0, 0, 0, 0.04, 0.1, 0.1, 0.12, 0.13, 0.12, 0.11, 0.11, 0.09, 0.06, 0.02)
#' weight_age <- c(1, 1, 1, 1, 1, 1, 1, 0.8, 2, 2, 2.5, 3, 0.5, 0.2)
#' prob_education <- c(0.1, 0.3, 0.4, 0.2)
#' weight_education <- c(0.4, 0.5, 2, 2.5)
#' mysurvey <- simulate_survey(prob_sex, weight_sex,
#'                              prob_raceethnicity, weight_raceethnicity,
#'                              prob_age, weight_age,
#'                              prob_education, weight_education,
#'                              n = 200)
#' weight_education_one(mysurvey, response, sex, unitedstates)
#' }
#' 
#' @export

weight_education_one <- function(mysurvey, response, indicator, geographyfetch) {
        
        # NSE magic
        indicator_col <- col_name(substitute(indicator))
        response_col <- col_name(substitute(response))
        
        # error handling for weighting indicator
        if (!(indicator_col %in% c("sex", "raceethnicity", "education"))) {
                stop("indicator must be one of sex, raceethnicity, or education") }
        
        # download and process ACS data
        acseducationDF <- process_acs_education(geographyfetch)
        
        # what are the population frequencies for post-stratification?
        popDF <- group_by_(acseducationDF, indicator_col) %>%
                summarise(Freq = sum(population))
        
        # what is the raw result on the survey question in the population?
        rawSurvey <- survey::svydesign(ids = ~0, data = mysurvey, weights = NULL)
        responseform <- as.formula(paste("~", response_col)) 
        rawresult <- survey::svymean(responseform, rawSurvey)        
        
        # now do the post-stratification?
        indicatorform <- as.formula(paste("~", indicator_col))
        psSurvey <- survey::postStratify(rawSurvey, indicatorform, 
                                         population = popDF)
        psresult <- survey::svymean(responseform, psSurvey)
        
        # bind raw and post-stratified results together
        results <- bind_rows(data_frame(answer = rownames(melt(rawresult))) %>% 
                                     mutate(value = melt(rawresult)$value) %>%
                                     mutate(result = "Raw"),
                             data_frame(answer = rownames(melt(psresult))) %>% 
                                     mutate(value = melt(psresult)$value) %>%
                                     mutate(result = "Weighted"))
        results
}