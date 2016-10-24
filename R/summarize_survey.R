#' Summarize a survey data frame
#' 
#' @param mysurvey A survey data frame with post-stratification weights, such 
#' as those created by \code{weight_wwc}. There must be a column \code{weight} 
#' in the data frame.
#' @param response A column in \code{mysurvey} that contains the quantity to be
#' weighted, such as the response to a yes/no question as in 
#' \code{simulate_survey}
#' @param response_col Response column as string
#' 
#' @return A data frame with 4 columns (\code{answer}, \code{value}, \code{se}, 
#' and \code{result}) that tabulates the weighted response on the survey 
#' question. 
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
#' library(dplyr) 
#' data(texassurvey)
#' weight_wwc(texassurvey, sex, raceethnicity) %>%
#'     summarize_survey(response)
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
        results <- bind_rows(data.frame(rawresult) %>% 
                                     cbind(answer = rownames(data.frame(rawresult))) %>% 
                                     mutate(result = "Raw") %>% 
                                     select(answer, everything()),
                             data.frame(psresult) %>% 
                                     cbind(answer = rownames(data.frame(psresult))) %>% 
                                     mutate(result = "Weighted") %>% 
                                     select(answer, everything()))
        colnames(results) <- c("answer", "value", "se", "result")
        tbl_df(results)
}