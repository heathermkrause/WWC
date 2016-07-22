#' A wrapper for weight_wwc for CSV file input
#' 
#' @param file A file containing the survey data. Either a path to a file, a 
#' connection, or literal data (either a single string or a raw vector). Files 
#' starting with \code{http://}, \code{https://}, \code{ftp://}, or 
#' \code{ftps://} will be automatically downloaded, and zipped files wll be 
#' uncompressed. 
#' @param ... Arguments to be passed to \code{weight_wwc}, geographical region 
#' and weighting indicators. See \code{weight_wwc} for more details.
#' 
#' @return A data frame, the original survey data with 1 column added, 
#' \code{weight}, the post-stratification weight for each row in the survey.
#' 
#' @details See \code{weight_wwc} for more details on the weighting algorithm
#' arguments
#' 
#' @name weight_csv
#' 
#' @examples 
#' weight_csv(system.file("extdata/examplesurvey.csv", package = "WWC"), TX, sex, raceethnicity)
#' 
#' @export

weight_csv <- function(file, ...) {
        mysurvey <- readr::read_csv(file)
        weight_wwc(mysurvey, ...)
}
