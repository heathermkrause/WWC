#' Calculate some estimates of the margin of error for weighted survey 
#' results
#' 
#' @param inputfile A file containing the weighted survey data (a survey data 
#' frame with post-stratification weights, such as those created by 
#' \code{weight_wwc}). There must be a column \code{weight} or 
#' \code{weight_best} in the file. Either a path to a file, a connection, or 
#' literal data (either a single string or a raw vector). Files starting with 
#' \code{http://}, \code{https://}, \code{ftp://}, or \code{ftps://} will be 
#' automatically downloaded, and zipped files wll be uncompressed. 
#' @param response A column in \code{inputfile} that contains the quantity to be
#' weighted, such as the response to a yes/no question as in 
#' \code{simulate_survey}
#' @param response_col Response column as string
#' 
#' @return A list with
#' \itemize{
#' \item the percent standard error of the raw, unweighted survey,
#' \item the percent bias of the raw, unweighted survey (compared to the weighted survey),
#' \item the percent total error of the raw, unweighted survey (the square root of the sum of the squares of the bias and standard error), and
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
#' margin_of_error(system.file("extdata/weightedsurvey.csv", package = "WWC"),
#'     response)
#' 
#' @export
margin_of_error <- function(inputfile, response) {
        # NSE magic
        response_col <- col_name(substitute(response))
        margin_of_error_(inputfile, response_col)
        
}

#' @rdname margin_of_error
#' @export
margin_of_error_ <- function(inputfile, response_col) {
        
        mysurvey <- readr::read_csv(inputfile)
        
        if ('weight_all' %in% colnames(mysurvey))
                mysurvey <- mysurvey %>% 
                        rename(weight = weight_all)
        
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
        
        total <- sqrt(bias$bias * bias$bias + 
                              raw_standard_error$MOE * raw_standard_error$MOE)
        
        return(list(raw_standard_error = raw_standard_error$MOE,
                    raw_bias = bias$bias,
                    raw_total_error = total,
                    weighted_standard_error = weighted_standard_error$MOE))
}