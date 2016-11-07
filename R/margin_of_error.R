#' Calculate some estimates of the margin of error for a weighted survey 
#' data frame
#' 
#' @param mysurvey A survey data frame with post-stratification weights, such 
#' as those created by \code{weight_wwc}. There must be a column \code{weight} 
#' in the data frame.
#' @param response A column in \code{mysurvey} that contains the quantity to be
#' weighted, such as the response to a yes/no question as in 
#' \code{simulate_survey}
#' @param response_col Response column as string
#' 
#' @return A list with
#' \itemize{
#' \item the percent standard error of the raw, unweighted survey,
#' \item the percent bias of the raw, unweighted survey (compared to the weighted survey), and 
#' \item the percent standard error of the weighted survey.
#' }

#' @details \code{margin_of_error} is given bare names while 
#' \code{margin_of_error_} is given strings and is therefore suitable for 
#' programming with.
#' 
#' @import dplyr
#' 
#' @name margin_of_error
#' 
#' @examples
#' library(dplyr) 
#' data(texassurvey)
#' weight_wwc(texassurvey, sex, raceethnicity) %>%
#'     margin_of_error(response)
#'     
#' data(twostatessurvey)
#' weight_wwc(twostatessurvey, sex, raceethnicity) %>%
#'     margin_of_error(response)
#' 
#' @export
margin_of_error <- function(mysurvey, response) {
        # NSE magic
        response_col <- col_name(substitute(response))
        margin_of_error_(mysurvey, response_col)
        
}

#' @rdname margin_of_error
#' @export
margin_of_error_ <- function(mysurvey, response_col) {
        
        # summarize the survey
        su <- summarize_survey_(mysurvey, response_col)
        
        df <- su %>%
                mutate(percent_error = se / value) %>%
                group_by(result) %>%
                summarise(MOE = mean(percent_error))
        
        raw_standard_error <- df %>% 
                filter(result == "Raw")
        weighted_standard_error <- df %>% 
                filter(result == "Weighted")
        
        bias <- su %>% 
                group_by(answer, result) %>% 
                summarise(value = value) %>% 
                tidyr::spread(result, value) %>% 
                mutate(bias = abs(Raw - Weighted)/Weighted) %>% 
                ungroup %>% 
                summarise(bias = mean(bias))
        
        return(list(raw_standard_error = raw_standard_error$MOE,
                    raw_bias = bias$bias,
                    weighted_standard_error = weighted_standard_error$MOE))
}