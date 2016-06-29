#' Summarize a survey data frame
#' 
#' @param mysurvey A survey data frame with post-stratification weights, such 
#' as those created by \code{weight_age} or \code{weight_education}. There must
#' be a column \code{weight} in the data frame.
#' @param response A column in \code{mysurvey} that contains the quantity to be
#' weighted, such as the response to a yes/no question as in 
#' \code{simulate_survey}
#' @param response_col Response column as string
#' 
#' @return A data frame with 3 columns (\code{answer}, \code{value}, and 
#' \code{result}) that tabulates the weighted response on the yes/no question 
#' in the given geography.
#' 
#' @details \code{summarize_survey} is given bare names while 
#' \code{summarize_survey_} is given strings and is therefore suitable for 
#' programming with.
#' 
#' @import dplyr
#' @importFrom stats as.formula
#' 
#' @name summarize_survey
#' 
#' @examples 
#' 
#' \dontrun{
#' library(acs)
#' # if you are new to using the acs package, you will need to get an API key
#' # and run api.key.install() one time to install your key on your system
#' texas <- geo.make(state = "TX")
#' data(texassurvey)
#' summarize_survey(weight_age(texassurvey, texas, sex, raceethnicity), response)
#' }
#' 
#' @export
summarize_survey <- function(mysurvey, response) {
        # NSE magic
        response_col <- col_name(substitute(response))
        summarize_survey_(mysurvey, response_col)
        
}

#' @rdname summarize_survey
#' @export
summarize_survey_ <- function(mysurvey, response_col) {
        
        # make two survey design objects, one without weighting and one with
        rawSurvey <- survey::svydesign(ids = ~0, data = mysurvey, 
                                       weights = NULL)
        psSurvey <- survey::svydesign(ids = ~0, data = mysurvey, 
                                      weights = ~ weight)
        
        responseform <- as.formula(paste("~", response_col)) 
        rawresult <- survey::svymean(responseform, rawSurvey)
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