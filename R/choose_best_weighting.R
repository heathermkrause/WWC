#' Use bootstrap resampling of survey to find best weighting indicators
#' 
#' @param mysurvey A survey data frame, such as that created by 
#' \code{simulate_survey}
#' @param response A column in \code{mysurvey} that contains the quantity to be
#' weighted, such as the response to a yes/no question as in 
#' \code{simulate_survey}
#' @param response_col Response column as string
#' @param ... Weighting indicator(s) to be tested for post-stratification.
#' One or more of \code{sex}, \code{raceethnicity}, \code{age}, and/or 
#' \code{education} which must be columns in the survey data frame. Both 
#' \code{age} and \code{education} cannot be used for post-stratification at the
#' same time because of how the ACS tables are organized.
#' @param dots List of weighting indicator(s) as string(s)
#' 
#' @return ??? WHAT WILL IT RETURN ???
#' 
#' @details \code{choose_best_weighting} is given bare names while 
#' \code{choose_best_weighting_} is given strings and is therefore suitable for 
#' programming with. One column of \code{mysurvey} must be \code{geography}, to 
#' indicate what ACS data to use for post-stratification weighting.
#' 
#' @import dplyr
#' 
#' @name choose_best_weighting
#' 
#' @examples 
#' data(texassurvey)
#' choose_best_weighting(texassurvey, response, sex, raceethnicity)
#' data(twostatessurvey)
#' choose_best_weighting(twostatessurvey, response, sex, education)
#' 
#' @export
choose_best_weighting <- function(mysurvey, response, ...) {
        # NSE magic
        dots <- eval(substitute(alist(...)))
        dots <- purrr::map(dots, col_name)
        response_col <- col_name(substitute(response))
        
        choose_best_weighting_(mysurvey, response_col, dots)
}

#' @rdname choose_best_weighting
#' @export
choose_best_weighting_ <- function(mysurvey, response_col, dots) {
        
        # error handling for weighting indicator
        force_edu <- FALSE
        test_indicators(dots, force_edu)
        
        # exclude rows/observations/respondents who have NA for geography
        mysurvey <- mysurvey[!is.na(mysurvey$geography),]

        weight_and_process <- function(mysurvey, response_col, dots) {
                boot <- modelr::bootstrap(mysurvey, 10)
                boot <- purrr::map(boot$strap, weight_wwc_, dots)
                ret <- purrr::map(boot, summarize_survey_, response_col)
                ret <- purrr::map_df(ret, bind_rows)
                ret <- ret %>% 
                        filter(result == "Weighted") %>% 
                        group_by(result, answer) %>% 
                        summarise(mean = mean(value), 
                                  stddev = stats::sd(value))
                ret
        }
        
        weight_and_process(mysurvey, response_col, dots)
}