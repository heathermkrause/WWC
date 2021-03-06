#' A wrapper for weight_wwc for CSV file input/output
#' 
#' @param inputfile A file containing the input survey data. Either a path to a 
#' file, a connection, or literal data (either a single string or a raw vector). 
#' Files starting with \code{http://}, \code{https://}, \code{ftp://}, or 
#' \code{ftps://} will be automatically downloaded, and zipped files wll be 
#' uncompressed. 
#' @param outputpath Path to write a csv file of the output weighted results. 
#' The new file will contain the original survey data with 1 column added, 
#' \code{weight}, the post-stratification weight for each row in the survey.
#' Default will write a csv to the working directory.
#' @param ... Arguments to be passed to \code{weight_wwc}, the weighting 
#' indicators. See \code{weight_wwc} for more details.
#' 
#' @details See \code{weight_wwc} for more details on the weighting algorithm
#' arguments
#' 
#' @name weight_csv
#' 
#' @examples
#' tmp <- tempfile() 
#' weight_csv(system.file("extdata/examplesurvey.csv", package = "WWC"), tmp,
#'      sex, raceethnicity)
#' 
#' @export

weight_csv <- function(inputfile, outputpath = "./wwcresults.csv", ...) {
        mysurvey <- readr::read_csv(inputfile)
        readr::write_csv(weight_wwc(mysurvey, ...), outputpath)
}
