#' Use bootstrap resampling of survey to find best weighting indicators
#' 
#' @param inputfile A file containing the input survey data (a survey data 
#' frame, such as that created by \code{simulate_survey}). Either a path to a 
#' file, a connection, or literal data (either a single string or a raw vector). 
#' Files starting with \code{http://}, \code{https://}, \code{ftp://}, or 
#' \code{ftps://} will be automatically downloaded, and zipped files wll be 
#' uncompressed. 
#' @param outputpath Path to write a csv file of the output weighted results. 
#' The new file will contain the original survey data with 2 columns added, 
#' \code{weight_best}, the post-stratification weight for each row in the 
#' survey using the best indicators, and \code{weight_all}, the 
#' post-stratification weight for each row using all the indicators available.
#' Default will write a csv to the working directory.
#' @param n Number of bootstrap resamplings to generate in order to find the
#' best weighting indicators
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
#' @return A list of the best indicators. This function also saves a csv file 
#' of the output weighted results
#' 
#' @details \code{choose_best_weighting} is given bare names while 
#' \code{choose_best_weighting_} is given strings and is therefore suitable for 
#' programming with. One column of the survey data in \code{inputfile} must be 
#' \code{geography}, to indicate what ACS data to use for post-stratification 
#' weighting.
#' 
#' @import dplyr
#' 
#' @name choose_best_weighting
#' 
#' @examples
#'  
#' tmp <- tempfile() 
#' choose_best_weighting(system.file("extdata/examplesurvey.csv", package = "WWC"), 
#'                       tmp, 5, response, sex, raceethnicity)
#' 
#' @export
choose_best_weighting <- function(inputfile, outputpath, n, response, ...) {
        # NSE magic
        dots <- eval(substitute(alist(...)))
        dots <- purrr::map(dots, col_name)
        response_col <- col_name(substitute(response))
        
        choose_best_weighting_(inputfile, outputpath, n, response_col, dots)
}

#' @rdname choose_best_weighting
#' @export
choose_best_weighting_ <- function(inputfile, outputpath = "./wwc_weighted.csv", 
                                   n, response_col, dots) {
        
        mysurvey <- readr::read_csv(inputfile)
        # error handling for weighting indicator
        force_edu <- FALSE
        test_indicators(dots, force_edu)
        
        # exclude rows/observations/respondents who have NA for geography
        mysurvey <- mysurvey[!is.na(mysurvey$geography),]

        all_weights <- weight_wwc_(mysurvey, dots, force_edu = FALSE) %>%
                rename(weight_all = weight)
        
        weight_and_process <- function(dots, mysurvey, n, response_col) {
                boot <- modelr::bootstrap(mysurvey, n)
                boot <- purrr::map(boot$strap, weight_wwc_, dots)
                ret <- purrr::map(boot, summarize_survey_, response_col)
                ret <- purrr::map_df(ret, bind_rows)
                ret <- ret %>% 
                        filter(result == "Weighted") %>% 
                        group_by(result, answer) %>% 
                        summarise(mean = mean(value), 
                                  stddev = stats::sd(value)) %>%
                        ungroup
                ret
        }
        
        if (length(dots) == 1) {
                ret <- weight_wwc_(mysurvey, dots, force_edu = FALSE)
                best_dots <- dots
        } else if (length(dots) == 2) {

                list_of_dots <- c(dots, 
                                  list(purrr::flatten_chr(dots)))
                results <- data_frame(dots = list_of_dots) %>%
                        mutate(results = purrr::map(list_of_dots, 
                                                    weight_and_process, 
                                                    mysurvey, n, response_col)) %>%
                        tidyr::unnest(results, .drop = FALSE)
                
                dots <- results %>% 
                        top_n(-1, stddev) 
                best_dots <- dots$dots %>%
                        purrr::transpose() %>%
                        purrr::map(purrr::flatten_chr)
                ret <- weight_wwc_(mysurvey, best_dots, force_edu = FALSE)

        } else if (length(dots) == 3) {

                list_of_dots <- c(dots, 
                                  utils::combn(dots, 2, FUN = as.character, 
                                               simplify = FALSE), 
                                  list(purrr::flatten_chr(dots)))
                results <- data_frame(dots = list_of_dots) %>%
                        mutate(results = purrr::map(list_of_dots, 
                                                    weight_and_process, 
                                                    mysurvey, n, response_col)) %>%
                        tidyr::unnest(results, .drop = FALSE)
                
                dots <- results %>% 
                        top_n(-1, stddev) %>%
                        sample_n(1)
                best_dots <- dots$dots %>%
                        purrr::transpose() %>%
                        purrr::map(purrr::flatten_chr)
                ret <- weight_wwc_(mysurvey, best_dots, force_edu = FALSE)

        } else {
                stop("indicators can only include three of sex, raceethnicity, age, and education")
                
        }
        
        ret <- ret %>%
                rename(weight_best = weight) %>%
                inner_join(all_weights)

        readr::write_csv(ret, outputpath)
        message("The set of indicators best suited to this survey is")
        message(cat(unlist(best_dots), sep = " "))

        return(best_dots)
}